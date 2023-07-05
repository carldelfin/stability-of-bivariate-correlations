# Improving the stability of bivariate correlations using informative Bayesian priors: A Monte Carlo simulation study

This repository contains all code used for the study [Improving the stability of bivariate correlations using informative Bayesian priors: A Monte Carlo simulation study](https://google.com).

* R code for data generation is available in `singularity/generate_data.R`
* R code relevant for processing the simulation results, creating figures and so on is availble in the `manuscript/src/` directory
* R session information is available in `manuscript/output/session_info.txt`
* Stan code for the Bayesian models is available in the `singularity/build_dir/models/` directory
* C++ code for the frequentist model is available in `singularity/build_dir/src/corr.cpp` (note the compilation instructions!)
* The definition file used to create the Singularity container is available in `singularity/build_dir/bayesian_correlations.def`
* Bash scripts that run the simulations (through Singularity) are available in the `singularity/build_dir/src/` directory
