#!/bin/bash

start=`date +%s`
n=$1

for i in {10..500..1}; do
    cd /src
    ./cppcorr "data_${rho}/data_${n}_${i}.json" | awk -v prefix="${n} ${i} " '$0=prefix$0' >> /output/${model}_${rho}.txt
done
