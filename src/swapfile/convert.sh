#!/bin/sh

if test -z "$1"; then
	echo Usage: $0 swapfile	
	exit 1
fi

if test ! -d "$1/clusters.data" || test ! -d "$1/clusters.meta"; then
	echo No swapfile at $1
	exit 1
fi

if test -f "$1/.lock"; then
	echo Swapfile busy
	exit 1
fi

# called with $1 directory to convert (non-rec.)
do_convert_dec2hex()
{
	mkdir $1/.newfiles
	files=`find $1 -type f -maxdepth 1`
	for i in $files; do
		dirn=`dirname $i`
		basen=`basename $i`
		mv $i $1/.newfiles/`printf "%X" $basen`
	done
	mv $1/.newfiles/* $1/
	rm -Rf $1/.newfiles
}

# called with $1 directory to convert (non-rec.)
do_convert_hex2dec()
{
	mkdir $1/.newfiles
	files=`find $1 -type f -maxdepth 1`
	for i in $files; do
		dirn=`dirname $i`
		basen=`basename $i`
		mv $i $1/.newfiles/$[0x$basen + 0]
	done
	mv $1/.newfiles/* $1/
	rm -Rf $1/.newfiles
}

# called with $1 directory to hash (non-rec.)
do_hash()
{
	# create hash dirs (with leading . to prevent clashes)
	for i in `seq 0 255`; do
		mkdir $1/.`printf "%X" $i`
	done
	# insert all files into their hash slots (renaming them, too)
	files=`find $1 -type f -maxdepth 1`
	for i in $files; do
		basen=`basename $i`
		hashname=$[0x$basen / 256]
		hashdir=$[0x$basen - $hashname*256]
		mv $i $1/.`printf "%X" $hashdir`/`printf "%X" $hashname`
	done
	# move . hash dirs to real names
	for i in `seq 0 255`; do
		mv $1/.`printf "%X" $i` $1/`printf "%X" $i`
	done
}

# called with $1 directory to unhash
do_unhash_and_hex2dec()
{
	# move hash dirs to .hash to avoid clashes
	dirs=`find $1/ -type d -maxdepth 1 -name '[^d.]*'`
	for d in $dirs; do
		mv $d $1/.`basename $d`
	done
	# unhash / rename files
	for d in $dirs; do
		hashdir=`basename $d`
		files=`find $1/.$hashdir -type f -maxdepth 1`
		for i in $files; do
			hashfile=`basename $i`
			name=$[0x$hashfile*256 + 0x$hashdir]
			mv $i $1/$name
		done
	done
	# remove hash dirs
	for d in $dirs; do
		rm -Rf $1/.`basename $d`
	done
}


if test -d $1/clusters.meta/FF; then
	echo Converting from 0.5 format to 0.4 format

	echo Converting $1
	do_convert_hex2dec $1/
	echo Unhashing $1/clusters.data
	do_unhash_and_hex2dec $1/clusters.data
	echo Unhashing $1/clusters.meta
	do_unhash_and_hex2dec $1/clusters.meta

else
	echo Converting from 0.4 format to 0.5 format

	echo Converting $1
	do_convert_dec2hex $1/
	echo Converting $1/clusters.data
	do_convert_dec2hex $1/clusters.data
	echo Converting $1/clusters.meta
	do_convert_dec2hex $1/clusters.meta
	echo Hashing $1/clusters.data
	do_hash $1/clusters.data
	echo Hashing $1/clusters.meta
	do_hash $1/clusters.meta
fi
