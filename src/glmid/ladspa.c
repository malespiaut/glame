/*
 * ladspa.c
 *
 * $Id: ladspa.c,v 1.26 2003/05/25 13:00:42 richi Exp $
 * 
 * Copyright (C) 2000-2003 Richard Furse, Alexander Ehlert, Richard Guenther
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <dirent.h>
#include <dlfcn.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <ladspa.h>
#ifdef HAVE_LRDF
#include <stdio.h>
#include <lrdf.h>
#endif
#include "filter.h"
#include "util.h"
#include "glplugin.h"


/* Generic LADSPA filter wrapping function. */

static int ladspa_connect_out(filter_port_t *port, filter_pipe_t *p)
{
	filter_t *n = filterport_filter(port);
	int rate;
	float pos;

	rate = filterparam_val_long(filterparamdb_get_param(
				filter_paramdb(n), "GLAME Sample Rate"));
	pos = filterparam_val_double(filterparamdb_get_param(
				filter_paramdb(n), "GLAME Position"));
	filterpipe_settype_sample(p, rate, pos);
	return 0;
}

static int ladspa_f(filter_t * n)
{
	nto1_state_t *psNTo1_State = NULL;
	int iNTo1_NR, iNTo1_Index, i;
	filter_pipe_t **ppsAudioPorts;
	filter_param_t *psParam;
	filter_buffer_t **ppsBuffers;
	LADSPA_Data *pfControlValues;
	LADSPA_Handle *psLADSPAPluginInstance;
	unsigned long lPortIndex, lPortCount;
	unsigned long lSampleRate, lNewSampleRate;
	unsigned long lRunSampleCount;
	const LADSPA_Descriptor *psDescriptor;
	LADSPA_PortDescriptor iPortDescriptor;
	SAMPLE **dummy;
	long glame_timer;
	
	psDescriptor = (const LADSPA_Descriptor *) (n->priv);
	lPortCount = psDescriptor->PortCount;
	DPRINTF("%ld ports found\n", lPortCount);

	if (!lPortCount)
		FILTER_ERROR_RETURN("a LADSPA plugin has no ports");

	iNTo1_NR = 0;
	lSampleRate = 0;
	for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {
		filter_port_t *psPort;
		filter_pipe_t *psPipe;
		psPort = filterportdb_get_port
			(filter_portdb(n), psDescriptor->PortNames[lPortIndex]);
		if (!psPort)
			continue;
		if (filterport_get_property(psPort, "!CONTROL")) {
			if (filterport_get_pipe(psPort))
				iNTo1_NR++;
		} else {
			psPipe = filterport_get_pipe(psPort);
			if (!psPipe && filterport_is_input(psPort))
				FILTER_ERROR_RETURN
					("LADSPA plugins require all "
					 "inputs be connected");
			if (filterport_is_input(psPort)) {
				iNTo1_NR++;
				lNewSampleRate =
				    filterpipe_sample_rate(psPipe);
				if (lSampleRate != 0
				    && lNewSampleRate != lSampleRate)
					FILTER_ERROR_RETURN
					    ("LADSPA plugins require all inputs "
					     "to be at the same sample rate");
				else
					lSampleRate = lNewSampleRate;
			}
		}
	}

	DPRINTF("%d input port(s)\n", iNTo1_NR);

	if (lSampleRate == 0) {
		/* No audio channels incoming. The register method below will have
		   added a "GLAME Sample Rate" parameter. */
		psParam =
		    filterparamdb_get_param(filter_paramdb(n),
					    "GLAME Sample Rate");
		if (psParam)
			lSampleRate = filterparam_val_long(psParam);
		else
			lSampleRate = GLAME_DEFAULT_SAMPLERATE;
	}

	psParam = filterparamdb_get_param(filter_paramdb(n),
					  "GLAME Duration");
	if (psParam != NULL) {
		/* ok, we have a sound generator there
		 * give it some time to rumble */
		glame_timer = TIME2CNT(long,
				       filterparam_val_double(psParam)*1000.0,
				       lSampleRate);
		/* 0 means infinity. */
		if (glame_timer == 0)
			glame_timer = LONG_MAX;
	}
	else 
		glame_timer = 1;

	/* Construct pointers for GLAME Ports, but with slots corresponding
	   to the numbering on the LADSPA plugin. Construct array for simple
	   float data for the control ports. Note that for each LADSPA port
	   there will be one entry in the ppsAudioPorts entry or one entry
	   in pfControlValues, but never both. This means that some entries
	   will never be used. */
	if (iNTo1_NR > 0)
		psNTo1_State =
		    (nto1_state_t *) malloc(sizeof(nto1_state_t) *
					    iNTo1_NR);

	ppsAudioPorts	= ALLOCN(lPortCount, filter_pipe_t *);
	pfControlValues = ALLOCN(lPortCount, LADSPA_Data);
	ppsBuffers 	= ALLOCN(lPortCount, filter_buffer_t *);
	dummy 		= ALLOCN(lPortCount, SAMPLE *);

	if ((!psNTo1_State && iNTo1_NR > 0)
	    || !ppsAudioPorts || !pfControlValues || !ppsBuffers)
		FILTER_ERROR_RETURN("malloc() failure");

	for (iNTo1_Index = 0; iNTo1_Index < iNTo1_NR; iNTo1_Index++) {
		psNTo1_State[iNTo1_Index].n = n;
		psNTo1_State[iNTo1_Index].buf = NULL;
		psNTo1_State[iNTo1_Index].s = NULL;
		psNTo1_State[iNTo1_Index].pos = 0;
	}

	/* Construct the LADSPA plugin instance itself. */
	psLADSPAPluginInstance =
	    (LADSPA_Handle *) psDescriptor->instantiate(psDescriptor,
							lSampleRate);
	if (!psLADSPAPluginInstance) {
		free(ppsAudioPorts);
		free(pfControlValues);
		free(ppsBuffers);
		FILTER_ERROR_RETURN
		    ("failed to create a LADSPA plugin filter");
	}

	iNTo1_Index = 0;
	for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {
		filter_port_t *psPort;

		iPortDescriptor =
		    psDescriptor->PortDescriptors[lPortIndex];

		/* First handle control ports as params. Always. */
		if (LADSPA_IS_PORT_CONTROL(iPortDescriptor)
		    && LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
			/* Lookup the control value. */
			psParam =
				filterparamdb_get_param(filter_paramdb
							(n),
							psDescriptor->
							PortNames
							[lPortIndex]);
			/* psParam == NULL does not happen if params were registered
			 * appropriately. [richi] */
			pfControlValues[lPortIndex] =
				filterparam_val_double(psParam);
/* 
 * We now need to wire up the control port on the LADSPA plugin
 * to the point in the control ports array. For an input port
 * the value acquired by filterparam_val_double() will be
 * used. For an output port the value will be written into the
 * relevant spot in the array but never read again. Less than
 * ideal, perhaps a FIXME for a later date... 
 */
			psDescriptor->connect_port(psLADSPAPluginInstance,
						   lPortIndex,
						   pfControlValues +
						   lPortIndex);
		}

		/* Second handle audio ports and control ports as streams. */
		psPort = filterportdb_get_port
			(filter_portdb(n), psDescriptor->PortNames[lPortIndex]);
		if (!psPort)
			continue;

		if (LADSPA_IS_PORT_CONTROL(iPortDescriptor)
		    && !filterport_get_pipe(psPort))
			continue;

		/* now LADSPA_IS_PORT_AUDIO(iPortDescriptor) or
		 * a connected control port. */

/* 
 * We simply want to hang on to the port input/output channel
 * for use further along. 
 */
		if (LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
			DPRINTF("Mapping audio input port %ld",
				lPortIndex);
			psNTo1_State[iNTo1_Index++].in
				= ppsAudioPorts[lPortIndex]
				= filterport_get_pipe(psPort);
		} 
		else	/* i.e. LADSPA_IS_PORT_OUTPUT(iPortDescriptor) */
		{
			DPRINTF("Mapping audio output port %ld\n",
				lPortIndex);
			ppsAudioPorts[lPortIndex] =
				filterport_get_pipe(psPort);
		}
	}

	FILTER_AFTER_INIT;

	/* Some LADSPA plugins have activate methods. If there is one, we
	   need to call it now. */
	if (psDescriptor->activate)
		psDescriptor->activate(psLADSPAPluginInstance);

/* LADSPA plugins seem not to handle running out of data on one
 * input only, so the check nto1_tail() == 0 seems correct _if_
 * we do a simple "drop" loop after the main loop. */

	while ((nto1_tail(psNTo1_State, iNTo1_NR) == 0) && (glame_timer>0)) {

		FILTER_CHECK_STOP;

		/* Each time around, we link up all the audio ports. The control
		   ports will still be linked up (from above). If this model is to
		   be extended, it will probably best to changes the control
		   values stored in the pfControlValues array. The LADSPA plugin
		   should support these varying at this point. */
		for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {

			iPortDescriptor =
			    psDescriptor->PortDescriptors[lPortIndex];

			if (LADSPA_IS_PORT_CONTROL(iPortDescriptor)
			    && LADSPA_IS_PORT_INPUT(iPortDescriptor)
			    && !ppsAudioPorts[lPortIndex]) {
				/* Lookup the control value - if not connected through a pipe. */
				psParam = filterparamdb_get_param(
					filter_paramdb(n),
					psDescriptor->PortNames[lPortIndex]);
				/* psParam == NULL does not happen if params were registered
				 * appropriately. [richi] */
				pfControlValues[lPortIndex] =
					filterparam_val_double(psParam);
				psDescriptor->connect_port(psLADSPAPluginInstance,
							   lPortIndex,
							   pfControlValues +
							   lPortIndex);

			} else if (LADSPA_IS_PORT_CONTROL(iPortDescriptor)
				   && LADSPA_IS_PORT_OUTPUT(iPortDescriptor)) {
				/* Output controls get just written to and ignored then... */
				psDescriptor->connect_port(psLADSPAPluginInstance,
							   lPortIndex,
							   pfControlValues +
							   lPortIndex);
			}
		}

		/* Link up the audio input ports on the LADSPA plugin to the
		   sample buffers coming in through each GLAME input port. */
		if (iNTo1_NR > 0) {

			lRunSampleCount =
			    nto1_head(psNTo1_State, iNTo1_NR);
			iNTo1_Index = 0;
			for (lPortIndex = 0; lPortIndex < lPortCount;
			     lPortIndex++) {
				iPortDescriptor =
				    psDescriptor->PortDescriptors[lPortIndex];
				if ((LADSPA_IS_PORT_AUDIO(iPortDescriptor)
				     && LADSPA_IS_PORT_INPUT(iPortDescriptor))
				    || (LADSPA_IS_PORT_CONTROL(iPortDescriptor)
					&& LADSPA_IS_PORT_INPUT(iPortDescriptor)
					&& ppsAudioPorts[lPortIndex]))
				{

					if (!psNTo1_State[iNTo1_Index].buf) {
						FILTER_ERROR_CLEANUP
						    ("nto1.buf is NULL");
						/* This should not happen - under this circumstance the
						   last repeat of the loop should have exited. */
					}

					/* We have audio coming in on this port. Link to it. */
					if (psNTo1_State[iNTo1_Index].s != NULL) {
						psDescriptor->
						    connect_port
						    (psLADSPAPluginInstance,
						     lPortIndex,
						     psNTo1_State
						     [iNTo1_Index].s);

						/* adjust pointer */
						psNTo1_State[iNTo1_Index].
						    s += lRunSampleCount;
					} else {
						dummy[lPortIndex] =
						    ALLOCN(lRunSampleCount,
							   SAMPLE);
						psDescriptor->
						    connect_port
						    (psLADSPAPluginInstance,
						     lPortIndex,
						     dummy[lPortIndex]);
					}

/* Note that the above code will ONLY WORK IF
   SAMPLE=LADSPA_Data (=float). If SAMPLE becomes something
   different, it will be necessary to use intermediary
   LADSPA_Data buffers to perform the translations to and
   from as the LADSPA plugin itself only understands
   LADSPA_Data (floats). */

					iNTo1_Index++;
				}
			}
		} else {
	/* We have no buffers coming in. Therefore we can choose our own
			   sample count. Large counts are good. */
			lRunSampleCount = GLAME_WBUFSIZE;
			if  (iNTo1_NR==0)
				glame_timer -= GLAME_WBUFSIZE;
		}

		/* Create GLAME output buffers for each audio output port on the
		   LADSPA plugin. */
		for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {
			iPortDescriptor =
			    psDescriptor->PortDescriptors[lPortIndex];
			if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)
			    && LADSPA_IS_PORT_OUTPUT(iPortDescriptor)) {
				/* Create the buffers and hang onto their whereabouts. We'll
				   need them later for when we send them on. */
				ppsBuffers[lPortIndex]
				    =
				    sbuf_make_private(sbuf_alloc
						      (lRunSampleCount,
						       n));
				psDescriptor->
				    connect_port(psLADSPAPluginInstance,
						 lPortIndex,
						 sbuf_buf(ppsBuffers
							  [lPortIndex]));
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

		psDescriptor->run(psLADSPAPluginInstance, lRunSampleCount);

		/* free dummy buffers if there are any */
		for (i = 0; i < lPortCount; i++)
			if (dummy[i] != NULL) {
				free(dummy[i]);
				dummy[i] = NULL;
			}

		/* Having done this, we need to forward the audio sample buffers
		   we wrote to and release the input buffers we read from. If were
		   supporting control outputs we would handle these here too. */
		for (lPortIndex = 0; lPortIndex < lPortCount; lPortIndex++) {

			iPortDescriptor =
			    psDescriptor->PortDescriptors[lPortIndex];
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
		iPortDescriptor =
		    psDescriptor->PortDescriptors[lPortIndex];
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
	free(dummy);
	FILTER_RETURN;
}


/* This call examines a LADSPA plugin descriptor and uses it to set up
   a of GLAME plugin. */
int installLADSPAPlugin(const LADSPA_Descriptor * psDescriptor,
			plugin_t * psPlugin)
{
	char *pcBuffer;
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
	filter_port_t *psPort;
	filter_t *psFilter;
	char category[16];

	psFilter = filter_creat(NULL);
	if (!psFilter)
		return -1;
	psFilter->f = ladspa_f;

	/* Link the LADSPA_Descriptor to the filter itself as private
	   data. This allows the ladspa_f call to work out what is going
	   on. */
	psFilter->priv = (void *) psDescriptor;

	bHasAudioInput = 0;
	for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount;
	     lPortIndex++) {

		iPortDescriptor =
		    psDescriptor->PortDescriptors[lPortIndex];

		/* LADSPA audio ports are translated directly to GLAME sample
		   ports. */
		if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)
		    || (LADSPA_IS_PORT_INPUT(iPortDescriptor)
			&& LADSPA_IS_PORT_CONTROL(iPortDescriptor))) {
			if (LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
				psPort =
				    filterportdb_add_port(filter_portdb(psFilter),
							  psDescriptor->PortNames[lPortIndex],
							  FILTER_PORTTYPE_SAMPLE,
							  FILTER_PORTFLAG_INPUT,
							  FILTERPORT_DESCRIPTION,
							  psDescriptor->PortNames[lPortIndex],
							  FILTERPORT_END);
				if (LADSPA_IS_PORT_AUDIO(iPortDescriptor))
					bHasAudioInput = 1;
				else
					filterport_set_property(psPort, "!CONTROL", "true");
			} else {	/* LADSPA_IS_PORT_OUTPUT(iPortDescriptor) */

				psPort =
				    filterportdb_add_port(filter_portdb
							  (psFilter),
							  psDescriptor->
							  PortNames
							  [lPortIndex],
							  FILTER_PORTTYPE_SAMPLE,
							  FILTER_PORTFLAG_OUTPUT,
							  FILTERPORT_DESCRIPTION,
							  psDescriptor->
							  PortNames
							  [lPortIndex],
							  FILTERPORT_END);
			}
			if (!psPort)
				return -1;
		}

		/* Interpret input controls as parameters. In fact they could be
		   varied, but we're using a simple model for now. Perhaps this is
		   a FIXME for a later date. */

		if (LADSPA_IS_PORT_INPUT(iPortDescriptor)
		    && LADSPA_IS_PORT_CONTROL(iPortDescriptor)) {
			filter_param_t *param;
			int bound_below = 0, bound_above = 0, isint = 0, toggled = 0;
			fBound1 = 0;
			fBound2 = 0;
			if (LADSPA_IS_HINT_BOUNDED_BELOW
			    (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor)) {
				bound_below = 1;
				fBound1 =
				    psDescriptor->
				    PortRangeHints[lPortIndex].LowerBound;
				if (LADSPA_IS_HINT_SAMPLE_RATE
				    (psDescriptor->PortRangeHints[lPortIndex].
				     HintDescriptor))
					fBound1 *= GLAME_DEFAULT_SAMPLERATE;
			}
			if (LADSPA_IS_HINT_BOUNDED_ABOVE
			    (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor)) {
				bound_above = 1;
				fBound2 =
				    psDescriptor->
				    PortRangeHints[lPortIndex].UpperBound;
				if (LADSPA_IS_HINT_SAMPLE_RATE
				    (psDescriptor->PortRangeHints[lPortIndex].
				     HintDescriptor))
					fBound1 *= GLAME_DEFAULT_SAMPLERATE;
			}
			if (LADSPA_IS_HINT_SAMPLE_RATE
			    (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor)) {
				fBound1 *= lSampleRate;
				fBound2 *= lSampleRate;
			}
			if (fBound1 > 0
			    && fBound2 > 0
			    && LADSPA_IS_HINT_LOGARITHMIC
			    (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor))
				fRecommendation = 0.0; /* -- assume db, 0 is sane default, then. (was: sqrt(fBound1 * fBound2);) */
			else
				fRecommendation =
				    0.5 * (fBound1 + fBound2);
			if (LADSPA_IS_HINT_INTEGER
			    (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor)) {
				isint = 1;
				fRecommendation =
				    (LADSPA_Data) (long) (fRecommendation +
							  0.5);
			}
			if (LADSPA_IS_HINT_TOGGLED
			    (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor)) {
				toggled = 1;
				fRecommendation = 0.0;
			}
			if (LADSPA_IS_HINT_HAS_DEFAULT
			    (psDescriptor->PortRangeHints[lPortIndex].HintDescriptor)) {
				const LADSPA_PortRangeHintDescriptor *d;
				d = &psDescriptor->PortRangeHints[lPortIndex].HintDescriptor;
				if (LADSPA_IS_HINT_DEFAULT_MINIMUM(*d))
					fRecommendation = fBound1;
				else if (LADSPA_IS_HINT_DEFAULT_MAXIMUM(*d))
					fRecommendation = fBound2;
				else if (LADSPA_IS_HINT_DEFAULT_LOW(*d))
					fRecommendation = LADSPA_IS_HINT_LOGARITHMIC(*d)
						? exp(log(fBound1)*0.75 + log(fBound2)*0.25)
						: fBound1*0.75 + fBound2*0.25;
				else if (LADSPA_IS_HINT_DEFAULT_MIDDLE(*d))
					fRecommendation = LADSPA_IS_HINT_LOGARITHMIC(*d)
						? exp(0.5*(log(fBound1) + log(fBound2)))
						: 0.5*(fBound1+fBound2);
				else if (LADSPA_IS_HINT_DEFAULT_HIGH(*d))
					fRecommendation = LADSPA_IS_HINT_LOGARITHMIC(*d)
						? exp(log(fBound1)*0.25 + log(fBound2)*0.75)
						: fBound1*0.25 + fBound2*0.75;
				else if (LADSPA_IS_HINT_DEFAULT_0(*d))
					fRecommendation = 0.0;
				else if (LADSPA_IS_HINT_DEFAULT_1(*d))
					fRecommendation = 1.0;
				else if (LADSPA_IS_HINT_DEFAULT_100(*d))
					fRecommendation = 100.0;
				else if (LADSPA_IS_HINT_DEFAULT_440(*d))
					fRecommendation = 440.0;
			}
			if  (isint || toggled) {
				param = filterparamdb_add_param_long(
					filter_paramdb(psFilter),
					psDescriptor->PortNames[lPortIndex],
					FILTER_PARAMTYPE_LONG, fRecommendation,
					FILTERPARAM_END);
			} else {
				param = filterparamdb_add_param_double(
					filter_paramdb(psFilter),
					psDescriptor->PortNames[lPortIndex],
					FILTER_PARAMTYPE_DOUBLE, fRecommendation,
					FILTERPARAM_END);
			}
			if (!param) {
				DPRINTF("Cannot add param for port %i (%s)\n",
					(int)lPortIndex, psDescriptor->PortNames[lPortIndex]);
				continue;
			}

			if (bound_below && bound_above && !isint) {
				/* Use GtkHScale */
				char xml[1024];
				float step, page;
				int lrange, lstep, lpage;
				/* optimize step/page dependend on min-max range */
				lrange = floor(log10(fBound2-fBound1));
				lstep = MIN(0, lrange-2);
				lpage = (lrange + lstep) / 2;
				if (lstep == lpage)
					lpage++;
				step = pow(10.0, lstep);
				page = pow(10.0, lpage);
#if 1
				snprintf(xml, 1023,
"<?xml version=\"1.0\"?><GTK-Interface>"
"  <widget>"
"    <class>GtkHScale</class>"
"    <name>widget</name>"
"    <can_focus>True</can_focus>"
"    <draw_value>True</draw_value>"
"    <value_pos>GTK_POS_LEFT</value_pos>"
"    <digits>2</digits>"
"    <policy>GTK_UPDATE_CONTINUOUS</policy>"
"    <value>%.3f</value>"
"    <lower>%.3f</lower>"
"    <upper>%.3f</upper>"
"    <step>%.3f</step>"
"    <page>%.3f</page>"
"    <page_size>%.3f</page_size>"
"  </widget>"
"</GTK-Interface>",
					 fRecommendation, fBound1, fBound2, step, page, 0.0);
#else
				snprintf(xml, 1023,
"<?xml version=\"1.0\"?><GTK-Interface>"
" <widget>"
"  <class>GtkHBox</class>"
"  <name>root</name>"
"  <widget>"
"    <class>GtkKnob</class>"
"    <name>widget</name>"
"    <value>%.3f</value>"
"    <lower>%.3f</lower>"
"    <upper>%.3f</upper>"
"  </widget>"
"  <widget>"
"    <class>GtkHScale</class>"
"    <name>widget2</name>"
"    <can_focus>True</can_focus>"
"    <draw_value>True</draw_value>"
"    <value_pos>GTK_POS_LEFT</value_pos>"
"    <digits>3</digits>"
"    <policy>GTK_UPDATE_CONTINUOUS</policy>"
"    <value>0.0</value>"
"    <lower>0.0</lower>"
"    <upper>1.0</upper>"
"    <step>0.1</step>"
"    <page>0.1</page>"
"    <page_size>0.0</page_size>"
"  </widget>"
" </widget>"
"</GTK-Interface>",
					 fRecommendation, fBound1, fBound2);
#endif
				filterparam_set_property(param, FILTERPARAM_GLADEXML, strdup(xml));
			} else if ((bound_below || bound_above) && !isint) {
				/* Use GtkSpinButton */
				char xml[1024];
				snprintf(xml, 1023,
"<?xml version=\"1.0\"?><GTK-Interface>"
"  <widget>"
"    <class>GtkSpinButton</class>"
"    <name>widget</name>"
"    <can_focus>True</can_focus>"
"    <climb_rate>1</climb_rate>"
"    <digits>2</digits>"
"    <numeric>True</numeric>"
"    <update_policy>GTK_UPDATE_ALWAYS</update_policy>"
"    <snap>False</snap>"
"    <wrap>False</wrap>"
"    <value>%.3f</value>"
"    <lower>%.3f</lower>"
"    <upper>%.3f</upper>"
"    <step>%.3f</step>"
"    <page>%.3f</page>"
"    <page_size>0.0</page_size>"
"  </widget>"
"</GTK-Interface>",
					 fRecommendation,
					 bound_below ? fBound1 : -1e19,
					 bound_above ? fBound2 : 1e19, 0.01, 0.1);
				filterparam_set_property(param, FILTERPARAM_GLADEXML, strdup(xml));
			} else if ((bound_above || bound_below) && isint) {
				/* Use GtkSpinButton */
				char xml[1024];
				snprintf(xml, 1023,
"<?xml version=\"1.0\"?><GTK-Interface>"
"  <widget>"
"    <class>GtkSpinButton</class>"
"    <name>widget</name>"
"    <can_focus>True</can_focus>"
"    <climb_rate>1</climb_rate>"
"    <digits>0</digits>"
"    <numeric>True</numeric>"
"    <update_policy>GTK_UPDATE_ALWAYS</update_policy>"
"    <snap>False</snap>"
"    <wrap>False</wrap>"
"    <value>%.0f</value>"
"    <lower>%.0f</lower>"
"    <upper>%.0f</upper>"
"    <step>%.0f</step>"
"    <page>%.0f</page>"
"    <page_size>0.0</page_size>"
"  </widget>"
"</GTK-Interface>",
					 fRecommendation,
					 bound_below ? fBound1 : -1000,
					 bound_above ? fBound2 : 1000,
					 1.0, 1.0);
				filterparam_set_property(param, FILTERPARAM_GLADEXML, strdup(xml));
			} else if (toggled) {
				/* Use GtkSpinButton */
				char xml[1024];
				snprintf(xml, 1023,
"<?xml version=\"1.0\"?><GTK-Interface>"
"  <widget>"
"    <class>GtkSpinButton</class>"
"    <name>widget</name>"
"    <can_focus>True</can_focus>"
"    <climb_rate>1</climb_rate>"
"    <digits>0</digits>"
"    <numeric>True</numeric>"
"    <update_policy>GTK_UPDATE_ALWAYS</update_policy>"
"    <snap>False</snap>"
"    <wrap>False</wrap>"
"    <value>%.0f</value>"
"    <lower>%.0f</lower>"
"    <upper>%.0f</upper>"
"    <step>%.0f</step>"
"    <page>%.0f</page>"
"    <page_size>0.0</page_size>"
"  </widget>"
"</GTK-Interface>",
					 fRecommendation,
					 0.0,
					 1.0,
					 1.0, 1.0);
				filterparam_set_property(param, FILTERPARAM_GLADEXML, strdup(xml));
			}
		}
	}

	if (!bHasAudioInput) {
		/* The plugin cannot work out its sample rate from its input
		   channels as it has none. GLAME therefore requires us to choose
		   one ourselves. This requires us to provide a sample-rate
		   parameter on the plugin itself. */
		filterportdb_foreach_port(filter_portdb(psFilter), psPort)
			if (filterport_is_output(psPort))
				psPort->connect = ladspa_connect_out;
		
		filterparamdb_add_param_long(filter_paramdb(psFilter),
					    "GLAME Sample Rate",
					    FILTER_PARAMTYPE_RATE,
					    GLAME_DEFAULT_SAMPLERATE,
					    FILTERPARAM_END);
		
		filterparamdb_add_param_double(filter_paramdb(psFilter),
					    "GLAME Position",
					    FILTER_PARAMTYPE_POSITION,
					    0.0,
					    FILTERPARAM_END);
		
		filterparamdb_add_param_double(filter_paramdb(psFilter),
					    "GLAME Duration",
					    FILTER_PARAMTYPE_TIME_S,
					    0.0,
					    FILTERPARAM_END);
	}

	pcBuffer = (char *) malloc(strlen(psDescriptor->Name)
				   + strlen(psDescriptor->Maker)
				   + strlen(psDescriptor->Copyright)
				   + 100);
	if (pcBuffer) {
		sprintf(pcBuffer,
			"LADSPA Plugin Name: %s\n"
			"Maker: %s\n"
			"Copyright: %s",
			psDescriptor->Name,
			psDescriptor->Maker, psDescriptor->Copyright);
		plugin_set(psPlugin, PLUGIN_DESCRIPTION, pcBuffer);
		/* We deliberately do not call free() here. */
	}

	/* Set PLUGIN_LABEL, if desc->Name is available, set
	 * PLUGIN_CATEGORY to first character of label/name. */
	if (psDescriptor->Name)
		plugin_set(psPlugin, PLUGIN_LABEL, psDescriptor->Name);
	sprintf(category, "LADSPA/%c",
		tolower(psDescriptor->Name ? psDescriptor->Name[0]
                                           : psPlugin->name[0]));
	plugin_set(psPlugin, PLUGIN_CATEGORY, strdup(category));
#if 0 && defined HAVE_LRDF
	{
		uri = lrdf_get_default_uri(psDescriptor->UniqueID);
		if (uri) {
			lrdf_get_setting_metadata(uri, "");
		}
	}
#endif

	filter_register(psFilter, psPlugin);

	return 0;
}
