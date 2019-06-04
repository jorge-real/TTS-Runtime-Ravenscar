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
    if [ -f $f ] ; then
	cp -v ${f} ${f}.org
    fi

    cp -v $b $f
done

