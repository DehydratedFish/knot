#! /bin/bash

mkdir -p build
cd source

sources="$PWD/knot.cpp $PWD/linux/platform.cpp $PWD/parser.cpp $PWD/type_check.cpp $PWD/io.cpp $PWD/bytecode.cpp"


g++ -DDEVELOPER -DBOUNDS_CHECKING -g -I. -o ../build/knot $sources

