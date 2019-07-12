#! /bin/bash

BINFILE=$(which arm-eabi-gcc)
if [ -z "$BINFILE" ] ; then
    echo "Maybe GNAT environment is not properly established"
    exit 1
fi

INSTALLDIR=$(dirname $(dirname $BINFILE))

if [ ! -d $INSTALLDIR ] ; then
    echo "Invalid GNAT directory '$INSTALLDIR'"
    echo "Maybe GNAT environment is not properly established"
    exit 1
else
    echo "GNAT directory: $INSTALLDIR";
fi

SRCDIR=${INSTALLDIR}/arm-eabi/include/rts-sources

if [ ! -f files.txt ] ; then
    echo "No installation file 'files.txt' found"
    exit 1
fi

FILES=$(cat files.txt)

for i in $FILES
do
    b=$(basename $i)

    if [ ! -f $b ] ; then
	continue
    fi
    
    f=${SRCDIR}/$i
    if [ -f $f ] ; then
	if [ ! -f ${f}.org ] ; then
	    cp -v ${f} ${f}.org
	else
	    echo "File '${f}' already backed up"
	fi
    fi

    cp -v $b $f
done

