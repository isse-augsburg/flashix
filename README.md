# Flashix - a verified file system for flash memory

The Flashix project aims at the construction and verification of a
state-of-the-art file system for flash memory.

(c) 2015 Institute for Software & Systems Engineering <http://isse.de>.
This code is licensed under MIT license (see LICENSE for details).

The work is part of the project [*Verifcation of Flash File Systems*](http://isse.de/flashix)
(orig.  Verifikation von Flash-Dateisystemen, RE828/13-1, DFG/German Research Council).

This repository contains the [Scala](http://scala-lang.org) code automatically
derived from the [KIV](http://isse.de/kiv) specifications.
It provides a fully working prototype and simulation of the file system that can
be mounted into the Linux directory tree via [FUSE](http://fuse.sourceforge.net/).
Storage is either a plain file or an [MTD](http://www.linux-mtd.infradead.org/)-device.

See the [technical documentation](https://swt.informatik.uni-augsburg.de/swt/projects/flash.html)
for an overview of the concepts realized and a list of [publications](http://isse.de/flashix/publications).

## Quick Start

Mount flashix (on a 64-bit system) as follows:

    ./run.sh [-odebug] [-obig_writes] <mountpoint>

Options

- `-odebug`: dump a lot of debug information and don't fork to background
- `-obig_writes`: instruct FUSE to write in large chunks instead of the default of 4K
  (should improve write performance)
- See `man /etc/fuse.conf` for further options.

Umount with

    fusermount -u <mountpoint>

## Compiling

To compile flashix on your machine, simply type

    ./build.sh

Dependencies

- [Scala](http://scala-lang.org)
- [FUSE](http://fuse.sourceforge.net)
- [FUSE-J](http://sourceforge.net/projects/fuse-j/)
- [Apache Commons Logging](https://commons.apache.org/proper/commons-logging/)
  (used by FUSE-j)

Pre-built binaries for the latter two are included in the `lib` folder.

The native component of FUSE-J, `libjavafs.so` is provided for x68-64 in `lib64`,
on 32 bit systems it must be in `lib`.
It is linked against `/usr/lib/libjvm.so` which means that you have to provide
this file at this location if you want to use the prebuilt binary.
You can for example link the file as follows:

    $ locate libjvm.so
    /usr/lib/jvm/java-7-openjdk/jre/lib/amd64/libjava.so
    ...

    $ ln -s /usr/lib/jvm/java-7-openjdk/jre/lib/amd64/libjava.so /usr/lib/libjvm.so

The respective licenses apply:
FUSE-J is distributed under the LGPL.
Apache Commons Logging is distributed under the Apache License v2.0.

`fuse-j-2.4-prerelease1` has a compile bug with Java 7. You can copy the updated
build script files in folder `patches` and the patch to the `jni` subfolder of
FUSE-J to work around this issue.

- `patches/jni/duplicate-array.patch` removes a duplicate line in the JNI bindings
- `patches/jni/build.xml` applies this patch automatically during build time

Please contact Gidon Ernst <ernst@isse.de> or Jörg Pfähler <pfaehler@isse.de> us if you have any trouble.

## Open Issues

- File permissions are not integrated with FUSE and won't work
- The `nlink`s for directories are not correct at the moment
