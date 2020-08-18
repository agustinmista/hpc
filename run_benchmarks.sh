#!/bin/bash

rm -r bench/results
mkdir -p bench/results
mkdir -p bench/results/seq
mkdir -p bench/results/par

cabal build

for n in $(seq 1 6); do
	cabal exec bench -- +RTS -N$n -K200m -H200m -A200m -RTS
done
