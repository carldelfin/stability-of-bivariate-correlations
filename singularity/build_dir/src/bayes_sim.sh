#!/bin/bash

n=$1

for i in {10..500..1}; do 

    # create output directory
    mkdir /output/output_${n}_${i}
    cd /cmdstan

    # run model
    ./models/${model} \
        method=sample \
        algorithm=hmc \
        stepsize=0.05 \
        num_warmup=1000 \
        num_samples=5000 \
        num_chains=4 \
        num_threads=1 \
        data file=data_${rho}/data_${n}_${i}.json \
        random seed=${n} \
        output refresh=0 \
        file=/output/output_${n}_${i}/output_${n}_${i}.csv >/dev/null
    
    # gather summary data
    bin/stansummary /output/output_${n}_${i}/output_* -s 3 -p 2.5,5,17,83,95,97.5 | awk 'NR==17' | awk -v prefix="${n} ${i} " '$0=prefix$0' >> /output/${model}_${rho}.txt
    
    # gather diagnostic data
    bin/diagnose /output/output_${n}_${i}/output_* | awk 'NR==16' | awk -v prefix="${n} ${i} " '$0=prefix$0' >> /output/dx_${model}_${rho}.txt
    
    # remove temporary files
    rm -rf /output/output_${n}_${i}
    
done
