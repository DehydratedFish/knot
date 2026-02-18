#/bin/bash

mkdir -p build/debug

sources="source/knot.cpp source/parser.cpp source/type_check.cpp"
mountain="dependencies/mountain/source/linux/platform.cpp dependencies/mountain/source/io.cpp"

g++ -DDEVELOPER -DBOUNDS_CHECKING -g -Isource -Idependencies/mountain/source -o build/debug/knot $sources $mountain

