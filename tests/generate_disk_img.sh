#!/bin/sh
dd if=/dev/zero of=buffered:$1.img count=$2
