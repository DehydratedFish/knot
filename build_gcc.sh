#! /bin/bash

mkdir -p build
cd source

sources="$PWD/knot.cpp $PWD/linux/platform.cpp $PWD/parser.cpp"


g++ -DDEVELOPER -DBOUNDS_CHECKING -I. -o ../build/knot $sources

