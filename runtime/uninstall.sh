#! /bin/bash

INSTALLDIR=$(dirname $(dirname $(which arm-eabi-gcc)))

if [ ! -d $INSTALLDIR ] ; then
    echo "Invalid GNAT directory '$INSTALLDIR'"
    echo "Maybe GNAT environment is not properly established"
    exit 1
else
    echo "GNAT directory: $INSTALLDIR";
fi

SRCDIR=${INSTALLDIR}/arm-eabi/include/rts-sources

FILES=$(cat files.txt)

for i in $FILES
do
    b=$(basename $i)

    if [ ! -f $b ] ; then
	continue
    fi
    
    f=${SRCDIR}/$i
    
    rm -vf $f

    if [ -f ${f}.org ] ; then
	cp -v ${f}.org ${f}
    fi
done

