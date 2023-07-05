# ==================================================================================================
#
# Generate JSON data prior to simulation
# 
# The `here` package is used to keep track of where you are. The data is saved in subdirectories 
# of a directory called `build_dir`, which will be created if it does not already exist.
# Make sure to update the number of cores used, as this function is run in parallel.
# The time required to create all data depends on computational resources, but 50-100 hours or more
# is likely.
#
# ==================================================================================================

gen_dat <- function(i, pop_rho, initial_size, sample_out, step_size, n_sim) {

    outer_seed <- 2022 * pop_rho * 10
    inner_seed <- 2022 * pop_rho * 10
    out_length <- 1 + ((sample_out - initial_size) / step_size)

    set.seed(outer_seed)
    data <- as.data.frame(MASS::mvrnorm(1000000,
                                        mu = c(0, 0),
                                        Sigma = matrix(c(1, pop_rho, pop_rho, 1), ncol = 2),
                                        empirical = TRUE)) 

    colnames(data) <- c("x", "y")
    data$id <- seq(1, 1000000, 1)

    set.seed(inner_seed + i)

    initial_sample <- data[sample(nrow(data), initial_size), ]
    remaining_sample <- data[!data$id %in% initial_sample$id, ]

    data_for_stan <- list(x = initial_sample[[1]],
                          y = initial_sample[[2]],
                          N = nrow(initial_sample))

    cmdstanr::write_stan_json(data_for_stan, 
                              paste0(here("build_dir/data_"),
                                     data_dir,
                                     "/data_",
                                     i, "_",
                                     data_for_stan$N,
                                     ".json"))

    for (j in 2:out_length) {

        set.seed(inner_seed + i)
        
        new_sample <- remaining_sample[sample(nrow(remaining_sample), step_size), ]
        initial_sample <- rbind(initial_sample, new_sample)
        remaining_sample <- data[!data$id %in% initial_sample$id, ]

        data_for_stan <- list(x = initial_sample[[1]],
                              y = initial_sample[[2]],
                              N = nrow(initial_sample))

        cmdstanr::write_stan_json(data_for_stan, 
                                  paste0(here("build_dir/data_"),
                                         data_dir,
                                         "/data_",
                                         i, "_",
                                         data_for_stan$N,
                                         ".json"))
    }
}

library(here)

# --------------------------------------------------------------------------------------------------
# 0.1
# --------------------------------------------------------------------------------------------------

# simulation parameters 
pop_rho <- 0.1
initial_size <- 10 
sample_out <- 500
step_size <- 1
n_sim <- 1
n_cores <- 22L

data_dir <- gsub("\\.", "", pop_rho)
dir.create(here("build_dir"))
dir.create(paste0(here("build_dir/data_"), data_dir))

parallel::mcmapply(gen_dat,
                   i = 1:n_sim,
                   MoreArgs = list(pop_rho = pop_rho,
                                   initial_size = initial_size,
                                   sample_out = sample_out,
                                   step_size = step_size,
                                   n_sim = n_sim),
                   mc.cores = n_cores)

# --------------------------------------------------------------------------------------------------
# 0.2
# --------------------------------------------------------------------------------------------------


# simulation parameters 
pop_rho <- 0.2
initial_size <- 10 
sample_out <- 500
step_size <- 1
n_sim <- 10000
n_cores <- 22L

data_dir <- gsub("\\.", "", pop_rho)
dir.create(paste0(here("build_dir/data_"), data_dir))

parallel::mcmapply(gen_dat,
                   i = 1:n_sim,
                   MoreArgs = list(pop_rho = pop_rho,
                                   initial_size = initial_size,
                                   sample_out = sample_out,
                                   step_size = step_size,
                                   n_sim = n_sim),
                   mc.cores = n_cores)

# --------------------------------------------------------------------------------------------------
# 0.3
# --------------------------------------------------------------------------------------------------

# simulation parameters 
pop_rho <- 0.3
initial_size <- 10 
sample_out <- 500
step_size <- 1
n_sim <- 10000
n_cores <- 22L

data_dir <- gsub("\\.", "", pop_rho)
dir.create(paste0(here("build_dir/data_"), data_dir))

parallel::mcmapply(gen_dat,
                   i = 1:n_sim,
                   MoreArgs = list(pop_rho = pop_rho,
                                   initial_size = initial_size,
                                   sample_out = sample_out,
                                   step_size = step_size,
                                   n_sim = n_sim),
                   mc.cores = n_cores)

# --------------------------------------------------------------------------------------------------
# 0.4
# --------------------------------------------------------------------------------------------------

# simulation parameters 
pop_rho <- 0.4
initial_size <- 10 
sample_out <- 500
step_size <- 1
n_sim <- 10000
n_cores <- 22L

data_dir <- gsub("\\.", "", pop_rho)
dir.create(paste0(here("build_dir/data_"), data_dir))

parallel::mcmapply(gen_dat,
                   i = 1:n_sim,
                   MoreArgs = list(pop_rho = pop_rho,
                                   initial_size = initial_size,
                                   sample_out = sample_out,
                                   step_size = step_size,
                                   n_sim = n_sim),
                   mc.cores = n_cores)
