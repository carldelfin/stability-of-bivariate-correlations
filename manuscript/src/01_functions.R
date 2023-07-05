# --------------------------------------------------------------------------------------------------
# Examine various combinations of a and b for the beta distribution
# --------------------------------------------------------------------------------------------------

beta_mode <- function(a, b) {
    rho = ((a - 1) / (a + b - 2) * 2) - 1
    out = data.frame(a = a,
                     b = b,
                     rho = rho)
    return(out)
}

# --------------------------------------------------------------------------------------------------
# Plot beta priors (transformed to -1, 1)
# --------------------------------------------------------------------------------------------------

plot_beta <- function(a, b, rho) {
    
    x <- seq(from = 0, to = 1, by = .001)
    subtitle <- paste0("\u03B1 = ", a, ", \u03B2 = ", b, ", \u03C1 = ", rho)
    data <- data.frame(x = (x * 2) - 1, y = dbeta(x, a, b)) 
    
    plot <- data %>%
        ggplot(aes(x = x, y = y)) +
        geom_area(color = "#440154", fill = "#483677", alpha = 0.3, linewidth = 0.8) +
        geom_segment(aes(x = rho, 
                         y = -Inf, 
                         xend = rho, 
                         yend = max(data$y)),
                     color = "#440154", linetype = "dashed", linewidth = 0.8) +
        scale_x_continuous(expand = c(0.1, 0.1), limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
        scale_y_continuous(expand = c(0.01, 0.01, 0.01, 0.4)) +
        labs(x = "\u03c1",
             y = "",
             subtitle = subtitle) +
        theme_bw() +
        theme(plot.title = element_text(size = 11, color = "black"),
              plot.subtitle = element_text(size = 9, color = "black", hjust = 0.5),
              plot.margin = margin(0, 0, 1, 0, "mm"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 9, color = "black"),
              axis.text.y = element_blank(),
              axis.title.x = element_text(size = 10, color = "black"),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    return(plot)
}

# --------------------------------------------------------------------------------------------------
# Check MCMC diagnostics 
# --------------------------------------------------------------------------------------------------

check_mcmc_dx <- function(path) {
    
    model <- gsub(".*bayesian/", "", path)
    model <- gsub("/.*", "", model)
    
    tmp <- read_delim(path,
                      delim = "\t",
                      escape_double = FALSE, 
                      col_names = FALSE,
                      show_col_types = FALSE) 
    
    tmp <- do.call(rbind.data.frame,
                  strsplit(grep("no problems detected", 
                                tmp$X1, 
                                invert = TRUE, 
                                value = TRUE), 
                           " "))
    
    if (nrow(tmp) > 0) {
        names(tmp) <- c("sim_num", "n")
        tmp$rho <- paste0("0.", parse_number(path))
        tmp$model <- model
    }
    
    return(tmp)
}

# --------------------------------------------------------------------------------------------------
# Get bayesian results 
# --------------------------------------------------------------------------------------------------

get_bayes_results <- function(path) {
    
    tmp <- read_table(path, col_names = FALSE) %>%
        select(-X3) %>%
        as.data.frame()
    
    tmp$rho <- str_sub(path, -6, -5)
    tmp$model <- sub("\\_.*", "", basename(path))

    colnames(tmp) <- c("sim", "n", "est", "mcse", "stdev",
                       "pi2.5", "pi5.0", "pi17", "pi83", "pi95", "pi97.5",
                       "neff", "neffs", "rhat", "rho", "model")

    tmp <- tmp %>%
        arrange(rho, model, sim, n) %>%
        select(c(rho, model, everything()))

    return(tmp)
}

# --------------------------------------------------------------------------------------------------
# Get frequentist results 
# --------------------------------------------------------------------------------------------------

get_freq_results <- function(path) {
    tmp <- read_table(path, col_names = FALSE) %>%
        as.data.frame()
    
    tmp$rho <- str_sub(path, -6, -5)
    tmp$model <- sub("\\_.*", "", basename(path))

    # note different order of columns here compared to Bayesian output
    colnames(tmp) <- c("sim", "n", "est", 
                       "pi2.5", "pi97.5",
                       "pi5.0", "pi95", 
                       "pi17", "pi83", 
                       "rho", "model")

    tmp <- tmp %>%
        arrange(rho, model, sim, n) %>%
        select(c(rho, model, sim, n, est, pi2.5, pi5.0, pi17, pi83, pi95, pi97.5)) 

    return(tmp)

}

# --------------------------------------------------------------------------------------------------
# Get proportion of values above zero (type S or sign error)
# --------------------------------------------------------------------------------------------------

get_sign_error <- function(prop) {

    tmp <- res %>%
        group_by(rho, model, n) %>%
        summarize(proportion = mean(est > 0)) %>%
        filter(proportion >= prop) %>%
        group_by(rho, model) %>%
        slice_min(n) %>%
        ungroup()

    if (nchar(prop) == 3) {
        tmp <- tmp %>% mutate_if(is.numeric, round, 1)
    } else {
        tmp <- tmp %>% mutate_if(is.numeric, round, 2)
    }

    tmp <- tmp %>%
        mutate(n2 = ifelse(proportion > prop, paste0("< 10"), as.character(n))) %>%
        group_by(rho) %>% 
        arrange(model, .by_group = TRUE) %>%
        mutate(pct_change = round((n / n[1] - 1) * 100, 0)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(pct_change = ifelse(n2 == "< 10", paste0(pct_change, "%*"), paste0(pct_change, "%"))) %>%
        ungroup() %>%
        mutate(n2 = paste0(n2, " (", pct_change, ")")) %>%
        rowwise() %>%
        mutate(n2 = ifelse(model == "freq", gsub(" (0%)", "", n2, fixed = TRUE), n2)) %>%
        ungroup() %>%
        select(-c(n, proportion, pct_change)) %>%
        pivot_wider(names_from = model,
                    values_from = n2) %>%
        mutate(prop = prop) %>%
        select(prop, rho, freq, weakly, mod, highly) %>%
        replace_na(list(freq = "> 500",
                        weakly = "> 500",
                        mod = "> 500"))
    return(tmp)
}

# --------------------------------------------------------------------------------------------------
# Get proportion of lower pis above zero ("power")
# --------------------------------------------------------------------------------------------------

get_power <- function(prop, lower_interval) {

    tmp <- res[, c("rho", "model", "n", lower_interval)]
    tmp$lower_interval <- tmp[, lower_interval]
    tmp <- tmp %>% select(rho, n, model, lower_interval)

    tmp <- tmp %>% 
        group_by(rho, n, model) %>%
        summarise(proportion = mean(lower_interval > 0)) %>%
        ungroup() %>%
        arrange(rho, model)

    out <- NULL

    for (m in c("freq", "weakly", "mod", "highly")) {

        for (r in c("01", "02", "03", "04")) {

            tmp_out <- tmp %>% 
                filter(model == m) %>%
                filter(rho == r) %>%
                filter(proportion >= prop) %>%
                arrange(n) %>%
                slice(1) %>%
                mutate(n = as.character(n)) %>%
                as.data.frame()

            if (nchar(prop) == 3) {
                tmp_out <- tmp_out %>% mutate_if(is.numeric, round, 1)
            } else {
                tmp_out <- tmp_out %>% mutate_if(is.numeric, round, 2)
            }

            if (nrow(tmp_out) != 0 && tmp_out$proportion > prop) {
            
                tmp_out <- data.frame(rho = r,
                                      n = "< 10", 
                                      model = m,
                                      proportion = NA)

            }

            if (nrow(tmp_out) == 0) {
            
                tmp_out <- data.frame(rho = r,
                                      n = "> 500",
                                      model = m,
                                      proportion = NA)

            }

            out <- rbind(out, tmp_out)
        }
    }

    out <- out %>%
        rename(n2 = n) %>%
        mutate(n = as.numeric(gsub("> ", "", gsub("< ", "", n2)))) %>%
        group_by(rho) %>% 
        arrange(model, .by_group = TRUE) %>%
        mutate(pct_change = round((n / n[1] - 1) * 100, 0)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(pct_change = ifelse(n2 == "< 10", paste0(pct_change, "%*"), paste0(pct_change, "%"))) %>%
        ungroup() %>%
        mutate(n2 = paste0(n2, " (", pct_change, ")")) %>%
        rowwise() %>%
        mutate(n2 = ifelse(model == "freq", gsub(" (0%)", "", n2, fixed = TRUE), n2)) %>%
        ungroup() %>%
        select(rho, n2, model) %>%
        pivot_wider(names_from = model,
                    values_from = n2) %>%
        mutate(prop = as.character(prop)) %>%
        mutate(lower_interval = lower_interval) %>% 
        arrange(rho, lower_interval)

    return(out)
}

# --------------------------------------------------------------------------------------------------
# Get proportion of estimates within COS
# --------------------------------------------------------------------------------------------------

get_cos_lim <- function(r, w) {
    lower_bound <- round(tanh(atanh(r) - w), 3)
    upper_bound <- round(tanh(atanh(r) + w), 3)
    return(c(lower_bound, upper_bound))
}

get_cos_data <- function(rhoc, w) {
    
    r <- as.numeric(paste0(strsplit(rhoc, "")[[1]][1], ".", strsplit(rhoc, "")[[1]][2]))
    bounds <- get_cos_lim(r, w)
    
    tmp <- res %>% 
        select(rho, model, sim, n, est) %>%
        filter(grepl(rhoc, rho)) %>% 
        group_by(model, n) %>% 
        summarise(proportion = mean(est > bounds[1] & est < bounds[2])) %>%
        ungroup() %>%
        arrange(model, n, proportion) %>%
        mutate(w = w,
               rho = r) %>%
        select(model, w, rho, n, proportion)

    return(tmp)
   
}

get_cos_est <- function(rhoc, w, prop) {
    
    r <- as.numeric(paste0(strsplit(rhoc, "")[[1]][1], ".", strsplit(rhoc, "")[[1]][2]))
    bounds <- get_cos_lim(r, w)
    
    tmp <- res %>% 
        select(rho, model, n, est) %>%
        filter(grepl(rhoc, rho)) %>% 
        group_by(model, n) %>% 
        summarise(proportion = mean(est > bounds[1] & est < bounds[2])) %>%
        ungroup() %>%
        arrange(model, n, proportion) 
   
    out <- NULL

    for (m in c("freq", "weakly", "mod", "highly")) {

        tmp_out <- tmp %>%
            filter(model == m) %>%
            filter(proportion >= prop) %>%
            arrange(n) %>%
            slice(1) %>%
            mutate(n = as.character(n)) %>%
            as.data.frame()

        if (nchar(prop) == 3) {
            tmp_out <- tmp_out %>% mutate_if(is.numeric, round, 1)
        } else {
            tmp_out <- tmp_out %>% mutate_if(is.numeric, round, 2)
        }

        if (nrow(tmp_out) != 0 && tmp_out$proportion > prop) {
            tmp_out <- data.frame(model = m,
                                  n = "< 10", 
                                  proportion = NA)
        }

        if (nrow(tmp_out) == 0) {
            tmp_out <- data.frame(model = m,
                                  n = "> 500",
                                  proportion = NA)
        }

        out <- rbind(out, tmp_out)

    }

    out <- out %>%
        rename(n2 = n) %>%
        mutate(n = as.numeric(gsub("> ", "", gsub("< ", "", n2)))) %>%
        arrange(model) %>%
        mutate(pct_change = round((n / n[1] - 1) * 100, 0)) %>%
        rowwise() %>%
        mutate(pct_change = ifelse(n2 == "< 10", paste0(pct_change, "%*"), paste0(pct_change, "%"))) %>%
        ungroup() %>%
        mutate(n2 = paste0(n2, " (", pct_change, ")")) %>%
        rowwise() %>%
        mutate(n2 = ifelse(model == "freq", gsub(" (0%)", "", n2, fixed = TRUE), n2)) %>%
        ungroup() %>%
        select(n2, model) %>%
        pivot_wider(names_from = model,
                    values_from = n2) %>%
        mutate(prop = prop,
               w = w,
               rho = rhoc) %>%
        select(rho, w, prop, freq, weakly, mod, highly)

    return(out)
}

# --------------------------------------------------------------------------------------------------
# Get proportion of intervals within COS
# --------------------------------------------------------------------------------------------------


get_int_data <- function(rhoc, w, lower_interval, upper_interval) {
    
    r <- as.numeric(paste0(strsplit(rhoc, "")[[1]][1], ".", strsplit(rhoc, "")[[1]][2]))
    bounds <- get_cos_lim(r, w)
    
    tmp <- res[, c("rho", "model", "n", lower_interval, upper_interval)]
    tmp$lower_interval <- tmp[, lower_interval]
    tmp$upper_interval <- tmp[, upper_interval]
    tmp <- tmp %>% select(rho, model, n, lower_interval, upper_interval)
    
    tmp <- tmp %>% 
        filter(grepl(rhoc, rho)) %>% 
        group_by(model, n) %>% 
        summarise(proportion = mean(lower_interval > bounds[1] & upper_interval < bounds[2])) %>%
        ungroup() %>%
        arrange(model, n, proportion) %>%
        mutate(w = w,
               rho = r) %>%
        select(model, w, rho, n, proportion)

    return(tmp)
   
}

get_int_prop <- function(rhoc, w, prop, lower_interval, upper_interval, int_name) {
    
    r <- as.numeric(paste0(strsplit(rhoc, "")[[1]][1], ".", strsplit(rhoc, "")[[1]][2]))
    bounds <- get_cos_lim(r, w)
    
    tmp <- res[, c("rho", "model", "n", lower_interval, upper_interval)]
    tmp$lower_interval <- tmp[, lower_interval]
    tmp$upper_interval <- tmp[, upper_interval]
    tmp <- tmp %>% select(rho, model, n, lower_interval, upper_interval)

    tmp <- tmp %>% 
        filter(grepl(rhoc, rho)) %>% 
        group_by(model, n) %>% 
        summarise(proportion = mean(lower_interval > bounds[1] & upper_interval < bounds[2])) %>%
        ungroup() %>%
        arrange(model, n, proportion) 
   
    out <- NULL

    for (m in c("freq", "weakly", "mod", "highly")) {

        tmp_out <- tmp %>%
            filter(model == m) %>%
            filter(proportion >= prop) %>%
            arrange(n) %>%
            slice(1) %>%
            mutate(n = as.character(n)) %>%
            as.data.frame()

        if (nchar(prop) == 3) {
            tmp_out <- tmp_out %>% mutate_if(is.numeric, round, 1)
        } else {
            tmp_out <- tmp_out %>% mutate_if(is.numeric, round, 2)
        }

        if (nrow(tmp_out) != 0 && tmp_out$proportion > prop) {
            tmp_out <- data.frame(model = m,
                                  n = "< 10", 
                                  proportion = NA)
        }

        if (nrow(tmp_out) == 0) {
            tmp_out <- data.frame(model = m,
                                  n = "> 500",
                                  proportion = NA)
        }

        out <- rbind(out, tmp_out)

    }

    out <- out %>%
        rename(n2 = n) %>%
        mutate(n = as.numeric(gsub("> ", "", gsub("< ", "", n2)))) %>%
        arrange(model) %>%
        mutate(pct_change = round((n / n[1] - 1) * 100, 0)) %>%
        rowwise() %>%
        mutate(pct_change = ifelse(n2 == "< 10", paste0(pct_change, "%*"), paste0(pct_change, "%"))) %>%
        ungroup() %>%
        mutate(n2 = paste0(n2, " (", pct_change, ")")) %>%
        rowwise() %>%
        mutate(n2 = ifelse(model == "freq", gsub(" (0%)", "", n2, fixed = TRUE), n2)) %>%
        ungroup() %>%
        select(n2, model) %>%
        pivot_wider(names_from = model,
                    values_from = n2) %>%
        mutate(prop = prop,
               w = w,
               rho = rhoc,
               interval = int_name) %>%
        select(rho, w, interval, prop, freq, weakly, mod, highly)

    return(out)
}
