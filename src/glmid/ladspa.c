/*
 * ladspa.c
 *
 * Copyright (C) 2000 Richard Furse
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 */

#include <dirent.h>
#include <dlfcn.h>
#include <ladspa.h>
#include <string.h>
#include <ladspa.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


/* Generic LADSPA filter wrapping function. */
static int ladspa_f(filter_node_t * n)
{
  nto1_state_t * psNTo1_State = NULL;
  int iNTo1_NR, iNTo1_Index;
  filter_pipe_t ** ppsAudioPorts;
  filter_pipe_t * psPipe;
  filter_param_t * psParam;
  filter_buffer_t ** ppsBuffers;
  LADSPA_Data * pfControlValues;
  LADSPA_Handle * psLADSPAPluginInstance;
  unsigned long lPortIndex, lPortCount;
  unsigned long lSampleRate, lNewSampleRate;
  unsigned long lRunSampleCount;
  const LADSPA_Descriptor * psDescriptor;
  LADSPA_PortDescriptor iPortDescriptor;

  psDescriptor = (const LADSPA_Descriptor *)(n->filter->private);
  lPortCount = psDescriptor->PortCount;
  if (!lPortCount)
    FILTER_ERROR_RETURN("a LADSPA plugin has no ports");

  iNTo1_NR = 0;
  lSampleRate = 0;
  for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {
    iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];
    if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)) {
      if (LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
	iNTo1_NR++;
	psPipe = filternode_get_input(n,
				      psDescriptor->PortNames[lPortIndex]);
	if (!psPipe)
	  FILTER_ERROR_RETURN("LADSPA plugins require all "
			      "inputs be connected");
	lNewSampleRate
	  = filterpipe_sample_rate(psPipe);
	if (lSampleRate != 0 && lNewSampleRate != lSampleRate)
	  FILTER_ERROR_RETURN("LADSPA plugins require all inputs "
			      "to be at the same sample rate");
	else
	  lSampleRate = lNewSampleRate;
      }
      else /* LADSPA_IS_PORT_OUTPUT */ {
	psPipe = filternode_get_output(n,
				       psDescriptor->PortNames[lPortIndex]);
	if (!psPipe)
	  FILTER_ERROR_RETURN("LADSPA plugins require all "
			      "outputs be connected");
      }
    }
  }

  if (lSampleRate == 0) {
    /* No audio channels incoming. The register method below will have
       added a "GLAME Sample Rate" parameter. */
    psParam = filternode_get_param(n, "GLAME Sample Rate");
    if (psParam)
      lSampleRate = filterparam_val_int(psParam);
    else
      lSampleRate = GLAME_DEFAULT_SAMPLERATE;
  }

  /* Construct pointers for GLAME Ports, but with slots corresponding
     to the numbering on the LADSPA plugin. Construct array for simple
     float data for the control ports. Note that for each LADSPA port
     there will be one entry in the ppsAudioPorts entry or one entry
     in pfControlValues, but never both. This means that some entries
     will never be used. */
  if (iNTo1_NR > 0)
    psNTo1_State = malloc(sizeof(nto1_state_t) * iNTo1_NR);
  ppsAudioPorts = malloc(sizeof(filter_pipe_t *) * lPortCount);
  pfControlValues = malloc(sizeof(LADSPA_Data) * lPortCount);
  ppsBuffers = malloc(sizeof(filter_buffer_t *) * lPortCount);
  if ((!psNTo1_State && iNTo1_NR > 0)
      || !ppsAudioPorts
      || !pfControlValues
      || !ppsBuffers)
    FILTER_ERROR_RETURN("malloc() failure");

  for (iNTo1_Index = 0; iNTo1_Index < iNTo1_NR; iNTo1_Index++) {
    psNTo1_State[iNTo1_Index].buf = NULL;
    psNTo1_State[iNTo1_Index].pos = 0;
  }

  /* Construct the LADSPA plugin instance itself. */
  psLADSPAPluginInstance = psDescriptor->instantiate(psDescriptor,
						     lSampleRate);
  if (!psLADSPAPluginInstance) {
    free(ppsAudioPorts);
    free(pfControlValues);
    free(ppsBuffers);
    FILTER_ERROR_RETURN("failed to create a LADSPA plugin filter");
  }

  iNTo1_Index = 0;
  for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {

    iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];

    if (LADSPA_IS_PORT_CONTROL(iPortDescriptor)) {
      if (LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
	/* Lookup the control value. */
	psParam = filternode_get_param(n,
				       psDescriptor->PortNames[lPortIndex]);
	/* psParam == NULL does not happen if params were registered
	 * appropriately. [richi] */
	pfControlValues[lPortIndex] = filterparam_val_float(psParam);
      }
      /* We now need to wire up the control port on the LADSPA plugin
         to the point in the control ports array. For an input port
         the value acquired by filterparam_val_float() will be
         used. For an output port the value will be written into the
         relevant spot in the array but never read again. Less than
         ideal, perhaps a FIXME for a later date... */
      psDescriptor->connect_port(psLADSPAPluginInstance,
				 lPortIndex,
				 pfControlValues + lPortIndex);
    }
    else /* i.e. LADSPA_IS_PORT_AUDIO(iPortDescriptor) */ {
      /* We simply want to hang on to the port input/output channel
         for use further along. */
      if (LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
	psNTo1_State[iNTo1_Index++].in
	  = ppsAudioPorts[lPortIndex]
	  = filternode_get_input(n,
				 psDescriptor->PortNames[lPortIndex]);
      }
      else /* i.e. LADSPA_IS_PORT_OUTPUT(iPortDescriptor) */
	ppsAudioPorts[lPortIndex]
	  = filternode_get_output(n,
				  psDescriptor->PortNames[lPortIndex]);
    }
    if (!ppsAudioPorts[lPortIndex])
      FILTER_ERROR_CLEANUP("port not connected");
  }

  FILTER_AFTER_INIT;

  /* Some LADSPA plugins have activate methods. If there is one, we
     need to call it now. */
  if (psDescriptor->activate)
    psDescriptor->activate(psLADSPAPluginInstance);

/* LADSPA plugins seem not to handle running out of data on one
 * input only, so the check nto1_tail() == 0 seems correct _if_
 * we do a simple "drop" loop after the main loop. */
  while (nto1_tail(psNTo1_State, iNTo1_NR) == 0) {

    FILTER_CHECK_STOP;

    /* Each time around, we link up all the audio ports. The control
       ports will still be linked up (from above). If this model is to
       be extended, it will probably best to changes the control
       values stored in the pfControlValues array. The LADSPA plugin
       should support these varying at this point. */

    /* Link up the audio input ports on the LADSPA plugin to the
       sample buffers coming in through each GLAME input port. */
    if (iNTo1_NR > 0) {

      lRunSampleCount = nto1_head(psNTo1_State, iNTo1_NR);

      iNTo1_Index = 0;
      for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {
	iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];
	if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)
	    && LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
	
	  if (!psNTo1_State[iNTo1_Index].buf) {
	    FILTER_ERROR_CLEANUP("nto1.buf is NULL");
	    /* This should not happen - under this circumstance the
               last repeat of the loop should have exited. */
	  }

	  /* We have audio coming in on this port. Link to it. */
	  psDescriptor->connect_port(psLADSPAPluginInstance,
				     lPortIndex,
				     psNTo1_State[iNTo1_Index].s);
	  /* Note that the above code will ONLY WORK IF
	     SAMPLE=LADSPA_Data (=float). If SAMPLE becomes something
	     different, it will be necessary to use intermediary
	     LADSPA_Data buffers to perform the translations to and
	     from as the LADSPA plugin itself only understands
	     LADSPA_Data (floats). */
	
	  iNTo1_Index++;
	}
      }
    }
    else {
      /* We have no buffers coming in. Therefore we can choose our own
	 sample count. Large counts are good. */
      lRunSampleCount = GLAME_WBUFSIZE;
    }

    /* Create GLAME output buffers for each audio output port on the
       LADSPA plugin. */
    for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {
      iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];
      if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)
	  && LADSPA_IS_PORT_OUTPUT(iPortDescriptor)) {
	/* Create the buffers and hang onto their whereabouts. We'll
	   need them later for when we send them on. */
	ppsBuffers[lPortIndex]
	  = sbuf_make_private(sbuf_alloc(lRunSampleCount, n));
	psDescriptor->connect_port(psLADSPAPluginInstance,
				   lPortIndex,
				   sbuf_buf(ppsBuffers[lPortIndex]));
	/* Note that the above code will ONLY WORK IF
	   SAMPLE=LADSPA_Data (=float). If SAMPLE becomes something
	   different, it will be necessary to use intermediary
	   LADSPA_Data buffers to perform the translations to and from
	   as the LADSPA plugin itself only understands LADSPA_Data
	   (floats). */
      }
    }

    /* The plugin now has somewhere to read and write to every port,
       be it audio or control. We can finally run it! */
    psDescriptor->run(psLADSPAPluginInstance,
		      lRunSampleCount);

    /* Having done this, we need to forward the audio sample buffers
       we wrote to and release the input buffers we read from. If were
       supporting control outputs we would handle these here too. */
    for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {

      iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];
      if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)
	  && LADSPA_IS_PORT_OUTPUT(iPortDescriptor))
	sbuf_queue(ppsAudioPorts[lPortIndex],
		   ppsBuffers[lPortIndex]);

#if 0
      /* Test code: output the data in any control outputs to stdout: */
      if (LADSPA_IS_PORT_CONTROL(iPortDescriptor)
	  && LADSPA_IS_PORT_OUTPUT(iPortDescriptor))
	printf("Control output (%s/%s): %g\n",
	       psDescriptor->Name,
	       psDescriptor->PortNames[lPortIndex],
	       pfControlValues[lPortIndex]);
#endif

    }
  }

  /* FIXME! need to drop remaining input buffers on still active
   * ports! */

  /* Queue EOF's */
  for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {
    iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];
    if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)
	&& LADSPA_IS_PORT_OUTPUT(iPortDescriptor))
      sbuf_queue(ppsAudioPorts[lPortIndex], NULL);
  }

  FILTER_BEFORE_STOPCLEANUP;

  /* Some plugins have deactivate methods. If there is one, we need to
     call it now. */
  if (psDescriptor->deactivate)
    psDescriptor->deactivate(psLADSPAPluginInstance);

  FILTER_BEFORE_CLEANUP;

  /* Cleanup the LADSPA plugin instance. */
  psDescriptor->cleanup(psLADSPAPluginInstance);

  if (iNTo1_NR > 0)
    free(psNTo1_State);
  free(ppsAudioPorts);
  free(pfControlValues);
  free(ppsBuffers);

  FILTER_RETURN;

}


/* This call examines a LADSPA plugin descriptor and uses it to set up
   a of GLAME plugin. */
int installLADSPAPlugin(const LADSPA_Descriptor * psDescriptor,
			plugin_t * psPlugin)
{
  char * pcBuffer;
  LADSPA_Data fBound1, fBound2, fRecommendation;
  unsigned long lPortIndex;
  /* Use a hard-coded sample rate here based on likely human use
     rather than actual system sample rate. This is only used to
     generate a default value on a slider when range values are WRT a
     sample rate. The plugin itself is obliged to handle any values
     correctly. */
  unsigned long lSampleRate = 48000;
  int bHasAudioInput;
  LADSPA_PortDescriptor iPortDescriptor;
  filter_portdesc_t * psPort;
  filter_t * psFilter;

  psFilter = filter_alloc(ladspa_f);
  if (!psFilter)
    return -1;

  /* Link the LADSPA_Descriptor to the filter itself as private
     data. This allows the ladspa_f call to work out what is going
     on. */
  psFilter->private = (void *)psDescriptor;

  bHasAudioInput = 0;
  for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount; lPortIndex++) {

    iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];

    /* LADSPA audio ports are translated directly to GLAME sample
       ports. */
    if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)) {
      if (LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
	psPort = filter_add_input(psFilter,
				  psDescriptor->PortNames[lPortIndex],
				  psDescriptor->PortNames[lPortIndex],
				  FILTER_PORTTYPE_SAMPLE);
	bHasAudioInput = 1;
      }
      else /* LADSPA_IS_PORT_OUTPUT(iPortDescriptor) */ {
	psPort = filter_add_output(psFilter,
				   psDescriptor->PortNames[lPortIndex],
				   psDescriptor->PortNames[lPortIndex],
				   FILTER_PORTTYPE_SAMPLE);
      }
      if (!psPort)
	return -1;
    }

    /* Interpret input controls as parameters. In fact they could be
       varied, but we're using a simple model for now. Perhaps this is
       a FIXME for a later date. */

    if (LADSPA_IS_PORT_INPUT(iPortDescriptor)
	&& LADSPA_IS_PORT_CONTROL(iPortDescriptor)) {
	  fBound1 = 0;
	  fBound2 = 0;
      if (LADSPA_IS_HINT_BOUNDED_BELOW
	  (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor))
	fBound1 = psDescriptor->PortRangeHints[lPortIndex].LowerBound;
      if (LADSPA_IS_HINT_BOUNDED_ABOVE
	  (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor))
	fBound2 = psDescriptor->PortRangeHints[lPortIndex].UpperBound;
      if (LADSPA_IS_HINT_SAMPLE_RATE
	  (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor)) {
	fBound1 *= lSampleRate;
	fBound2 *= lSampleRate;
      }
      if (fBound1 > 0
	  && fBound2 > 0
	  && LADSPA_IS_HINT_LOGARITHMIC
	  (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor))
	fRecommendation = sqrt(fBound1 * fBound2);
      else
	fRecommendation = 0.5 * (fBound1 + fBound2);
      if (LADSPA_IS_HINT_INTEGER
	  (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor))
	fRecommendation = (LADSPA_Data)(long)(fRecommendation + 0.5);
      filterpdb_add_param_float(filter_pdb(psFilter),
				psDescriptor->PortNames[lPortIndex],
				FILTER_PARAMTYPE_FLOAT,
				fRecommendation,
				FILTERPARAM_END);
    }
  }

  if (!bHasAudioInput) {
    /* The plugin cannot work out its sample rate from its input
       channels as it has none. GLAME therefore requires us to choose
       one ourselves. This requires us to provide a sample-rate
       parameter on the plugin itself. */
    filterpdb_add_param_int(filter_pdb(psFilter),
			    "GLAME Sample Rate",
			    FILTER_PARAMTYPE_INT,
			    GLAME_DEFAULT_SAMPLERATE,
			    FILTERPARAM_END);
  }

  pcBuffer = malloc(strlen(psDescriptor->Name)
		    + strlen(psDescriptor->Maker)
		    + strlen(psDescriptor->Copyright)
		    + 100);
  if (pcBuffer) {
    sprintf(pcBuffer,
	    "LADSPA Plugin Name: %s\n"
	    "Maker: %s\n"
	    "Copyright: %s",
	    psDescriptor->Name,
	    psDescriptor->Maker,
	    psDescriptor->Copyright);
    plugin_set(psPlugin,
	       PLUGIN_DESCRIPTION,
	       pcBuffer);
    /* We deliberately do not call free() here. */
  }

  filter_attach(psFilter, psPlugin);

  return 0;
}

