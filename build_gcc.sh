#/bin/bash

mkdir -p build/debug

sources="source/knot.cpp source/parser.cpp"

g++ -DDEVELOPER -DBOUNDS_CHECKING -g -Isource -Idependencies/mountain/source -o build/debug/knot $sources

