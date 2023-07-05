#!/bin/bash

export rho="$1"
export model="$2"
export n_sim="$3"
export n_cores="$4"

if [ $model = "freq" ]
then
    parallel -j $n_cores /src/freq_sim.sh ::: $(seq 1 $n_sim)
else
    parallel -j $n_cores /src/bayes_sim.sh ::: $(seq 1 $n_sim)
fi

