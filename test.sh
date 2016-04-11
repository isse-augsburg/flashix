#!/bin/sh

if [ -z "$1" ]
then
    ./usage.sh
elif [ -z `which fs_test` ]
then
    echo
    echo "Please install 'fs_test' from https://sibylfs.github.io/"
else
    echo "WARNING!!! Executing test scripts directly on your machine has the potential to destroy your data."
    echo "Use at your own risk and perform the tests inside a chroot environment if in doubt."
    read -p "Continue? [yN] " yn
    case $yn in
        [Yy]* )
            fs_test exec --model path="$1" --suites "$2" 2> exec.err.md
# to generate HTML (repalce <output folder!>
# fs_test check linux_spec <output folder> 2> check.err.md
# fs_test html <output folder>
        ;;
        *)
            echo "aborted."
        ;;
    esac
fi

