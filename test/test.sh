#!/bin/bash

echo "=========================================================================="

rm -rf test/dst/*
rm -rf test/src/*
for i in `seq 1 9`; do touch "test/src/${i}-file"; done

echo "src before:"
ls test/src -R

echo "dst before:"
ls test/dst -R

echo "--------------------------------------------------------------------------"

echo "running massmove"
stack exec massmove-exe 2 test/src/ test/dst/

echo "--------------------------------------------------------------------------"

echo "src after:"
ls test/src -R

echo "dst after:"
ls test/dst -R

echo "=========================================================================="