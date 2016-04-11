#!/bin/sh

CP=lib/commons-logging-1.0.4.jar:lib/fuse-j.jar
JAR=lib/flashix.jar
MAIN=integration.Mount

M=`uname -m`
case $M in
    x86_64)
    LD_LIBRARY_PATH=lib64;;
    *)
    LD_LIBRARY_PATH=lib;;
esac

if [ -z "$1" ]
then
    ./usage.sh
else
    export LD_LIBRARY_PATH
    echo "native lib in " $LD_LIBRARY_PATH
    echo "mounting " $1
    scala -classpath $CP:$JAR $MAIN $@
fi
