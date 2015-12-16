#!/bin/sh

CP=lib/commons-logging-1.0.4.jar:lib/fuse-j.jar
JAR=lib/flashix.jar

mkdir -p bin bin/META-INF

echo "running scala build..."
SRC=`find src -iname "*.scala"`
scalac -cp $CP -d bin $SRC

echo "writing manifest"
cat > bin/META-INF/MANIFEST.MF <<EOF
Manifest-Version: 1.0"
Class-Path: lib/fuse-j.jar lib/commons-logging-1.0.4.jar
Main-Class: de.uniaugsburg.flashix.fuse.Mount
EOF

echo "creating jar"
jar cfm $JAR bin/META-INF/MANIFEST.MF -C bin .
