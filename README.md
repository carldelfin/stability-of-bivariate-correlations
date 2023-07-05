# Improving the stability of bivariate correlations using informative Bayesian priors: A Monte Carlo simulation study

This repository contains all code used for the study [Improving the stability of bivariate correlations using informative Bayesian priors: A Monte Carlo simulation study](https://google.com).

Roughly in the order they are used, you can find:

* R code for JSON data generation in `singularity/generate_data.R`
* Stan code for the Bayesian models in the `singularity/build_dir/models/` directory
* C++ code for the frequentist model in `singularity/build_dir/src/corr.cpp` (note the compilation instructions)
* The definition file used to create the Singularity container in `singularity/build_dir/bayesian_correlations.def`
* Bash scripts that run the simulations (through Singularity, so these are not accessed directly) in the `singularity/build_dir/src/` directory
* The Singularity container at [https://zenodo.org/record/7857972](https://zenodo.org/record/7857972)
* The raw simulation output at [https://zenodo.org/record/7994203](https://zenodo.org/record/7994203)
* R code relevant for processing the raw simulation output, creating tables and figures and so on in the `manuscript/src/` directory
* R session information in `manuscript/output/session_info.txt`
