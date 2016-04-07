# Flashix - a verified file system for flash memory

The Flashix project aims at the construction and verification of a
state-of-the-art file system for flash memory.

(c) 2015 Institute for Software & Systems Engineering <http://isse.de>.
This code is licensed under MIT license (see LICENSE for details).

The work is part of the project [*Verifcation of Flash File Systems*](http://isse.de/flashix)
(orig.  Verifikation von Flash-Dateisystemen, RE828/13-1, DFG/German Research Council).

This repository contains the [Scala](http://scala-lang.org) code automatically
derived from the [KIV](http://isse.de/kiv) specifications.
It provides a working prototype and simulation of the file system that can
be mounted into the Linux directory tree via [FUSE](http://fuse.sourceforge.net/).
Storage is either a plain file
(or an [MTD](http://www.linux-mtd.infradead.org/)-device in the future).

**DISCLAIMER: no claim about the correctness of the generated Scala code is made.
Please read the documentation provided below and make sure you understand the
background of this work and its assumptions.**

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

Further Flashix specific options will be introduced in the future (see the list
of open issues).

Umount with

    fusermount -u <mountpoint>

## Compiling

To compile flashix on your machine, simply type

    ./build.sh

Alternatively, you can import the code as an eclipse project,
provided that the [Scala IDE](http://scala-ide.org) plugin has been installed.
The main file is `integration.Mount`.

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

Please contact Gidon Ernst <ernst@isse.de> or Jörg Pfähler <pfaehler@isse.de> if you have any trouble.

## Code Structure

### Generated Code

- `algebraic/Algebraic.scala` contains definitions of algebraic functions.
  In case they have been left abstract in the formal model,
  or if the code generator was unable to translate them,
  they are abstract here as well. These are implemented in
  `integration/Algebraic.scala`.

- `asm` contains the executable formal models derived from the
  Abstract State Machine specifications in KIV.
  Some are interface specifications, for which we currently don't provide the code.

- `encoding` contains encoding/decoding functions between data structures and
  their representations as byte sequences.

- `types` contains definitions of all free data types that are used in the case study.

### Manually Implemented Code

- `integration` contains all the glue code.
- `integration/fuse` contains the wrappers for FUSE
- `helpers` contains predefined data structures such as lists and sets

## Open Issues

- File permissions are not integrated with FUSE and won't work
- The `nlink`s for directories are not correct at the moment
- Missing option to turn of flushing of the write cache at the end of operations
- The support for symbolic links is flaky and relies on the Linux VFS/FUSE
  to resolve these. They're implemented in the glue layer (unverified)
  simply as files with a special attribute.
- Some glue code will move to the models, in particular, the decision when to
  invoke on flash garbage collection.

## Test Runs with SibylFS

[SibylFS](http://sibylfs.io/) is a tool for conformance checking and test
generation for file systems. It runs a series of POSIX system operations 
on existing file systems. Besides tests on loopback devices that are
automatically set up, formatted by a file system of the user's choice,
released after the test, it supports a *path* model that can be pointed to an
existing mount. It is thus easy to run test these suites on Flashix.
Note that a couple of tests will fail due to the open issues outlined above.
This is expected and will be fixed.

**"WARNING!!! Executing test scripts directly on your machine has the potential to
destroy your data."** Read the SibylFS documentation first, in particular this
page, before you run the commands below.

In particular, the authors of SibylFS recommend to run test (in particular with
the path model) in a `chroot` environment.

Replace `/mnt/flashix` with the mountpoint of your choice
and the output folder `2015-12-14_wLc` with the one created in by
`fs_test exec`. Mount the flashix file system with

    $ ./run.sh /mnt/flashix

(without the `-d`/`-odebug` option it should fork to background)
Then exectue the following commands

    $ fs_test exec --model path=/mnt/flashix/ --suites test-suite 2> exec.err.md
    $ fs_test check linux_spec 2015-12-14_wLc 2> check.err.md
    $ fs_test html 2015-12-14_wLc

The first line runs a preconfigured test suite from `test-suite` against Flashix
(the suite is avaliable from the SibylFS website). It will collect a bunch of
traces as results in a subfolder *date-XYZ* for some random *XYZ*.
The second line compares the generated traces against a specification shipped
with SibylFS (`linux_spec` in this case, there is also `posix_spec` and some others).
Results of the conformance check will go to the same output folder.
The third line generates a nice HTML report that you can open in your browser
(file `2015-12-14_wLc/html/index.html`).

A sample report can be found
[here](https://swt.informatik.uni-augsburg.de/swt/projects/sibylfs-20151214/html).
It only contains the `html` subfolder, so a couple of links will be dead.

Running Flashix on top of a simulated flash device introduces some overhead.
The write cache is currently flushed at least at the end of each POSIX operation
which produces space waste and leads to increased pressure on the flash garbage
collector. Also, the Scala code and integration into FUSE are not very efficient
(it took about 2:45 minutes for to run `exec` for the report above).
