if (overwrite_results == TRUE) {

    # prior overview
    beta_grid <- expand.grid(a = seq(0.1, 20, 0.1),
                             b = seq(0.1, 20, 0.1))
    
    # evaluate each combination in grid
    beta_out <- mapply(FUN = beta_mode,
                       a = beta_grid[, 1],
                       b = beta_grid[, 2],
                       SIMPLIFY = FALSE,
                       USE.NAMES = FALSE)
    
    beta_out <- as.data.frame(do.call(rbind, beta_out)) %>%
        mutate(rho = round(rho, 3))
    
    saveRDS(beta_out, here("manuscript/tmp/beta_out.rds"))
    
    # read all output files
    all_files <- list.files(here("singularity/raw_output"), 
                            pattern = ".txt", 
                            full.names = TRUE, 
                            recursive = TRUE)

    # cmdstan diagnostics
    mcmc_dx_files <- all_files[grep("dx", all_files)]
    mcmc_dx <- mcmc_dx_files %>% map_dfr(check_mcmc_dx)
    saveRDS(mcmc_dx, here("manuscript/tmp/mcmc_dx.rds"))

    # bayesian simulation results
    res_bayes <- all_files[grep("dx|freq|date|elapsed", all_files, invert = TRUE)] %>% 
        map_dfr(get_bayes_results)
    saveRDS(res_bayes, here("manuscript/tmp/res_bayes.rds"))

    # mcmc diagnostics
    neff_all <- res_bayes %>% 
        summarize(min = min(neff),
                  max = max(neff),
                  mean = mean(neff),
                  sd = sd(neff)) %>%
        ungroup()
    
    saveRDS(neff_all, here("manuscript/tmp/neff_all.rds"))
    
    neff_grp <- res_bayes %>% 
        group_by(rho, model) %>%
        summarize(min = min(neff),
                  max = max(neff),
                  mean = mean(neff),
                  sd = sd(neff)) %>%
        ungroup()
    
    saveRDS(neff_grp, here("manuscript/tmp/neff_grp.rds"))
    
    mcse_all <- res_bayes %>% 
        summarize(min = min(mcse),
                  max = max(mcse),
                  mean = mean(mcse),
                  sd = sd(mcse)) %>%
        ungroup()
    saveRDS(mcse_all, here("manuscript/tmp/mcse_all.rds"))
    
    mcse_grp <- res_bayes %>% 
        group_by(rho, model) %>%
        summarize(min = min(mcse),
                  max = max(mcse),
                  mean = mean(mcse),
                  sd = sd(mcse)) %>%
        ungroup()
    saveRDS(mcse_grp, here("manuscript/tmp/mcse_grp.rds"))

    # frequentist simulation results
    res_freq <- all_files[grep("freq", all_files)] %>% 
        map_dfr(get_freq_results)
    saveRDS(res_freq, here("manuscript/tmp/res_freq.rds"))

    # combined
    res <- bind_rows(res_bayes, res_freq)
    saveRDS(res, here("manuscript/tmp/res.rds"))
    
} else {

    mcmc_dx <- readRDS(here("manuscript/tmp/mcmc_dx.rds"))
    neff_all <- readRDS(here("manuscript/tmp/neff_all.rds"))
    neff_grp <- readRDS(here("manuscript/tmp/neff_grp.rds"))
    mcse_all <- readRDS(here("manuscript/tmp/mcse_all.rds"))
    mcse_grp <- readRDS(here("manuscript/tmp/mcse_grp.rds"))

}

if (restore_res == TRUE) {

    res <- readRDS(here("manuscript/tmp/res.rds"))

}
