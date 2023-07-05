if (overwrite_figure_data == TRUE) {

    if (!exists("res")) {
        res <- readRDS(here("manuscript/tmp/res.rds"))    
    }
    
    # ----------------------------------------------------------------------------------------------
    # Figure 2 data
    # Extreme trajectories
    # ----------------------------------------------------------------------------------------------
    
    tmp <- res %>%
        select(model, rho, sim, n, est) %>%
        filter(sim %% 50 == 0) %>%
        mutate(mod2 = factor(case_when(model == "freq" ~ "Frequentist",
                                       model == "weakly" ~ "Weakly informative",
                                       model == "mod" ~ "Moderately informative",
                                       model == "highly" ~ "Highly informative"))) %>%
        mutate(mod2 = fct_relevel(mod2, c("Frequentist",
                                          "Weakly informative",
                                          "Moderately informative",
                                          "Highly informative")))
    
    saveRDS(tmp, here("manuscript/tmp/fig_2_data.rds"))
    
    rm(tmp)
    
    
    # ----------------------------------------------------------------------------------------------
    # Figure 3 data
    # Proportion of estimates above zero
    # ----------------------------------------------------------------------------------------------
    
    tmp <- res %>%
        select(model, rho, sim, n, est) %>%
        group_by(rho, model, n) %>% 
        summarise(prop = mean(est > 0)) %>%
        mutate(rho2 = paste0("\u03c1 = ", paste0(str_split(rho, "")[[1]], collapse = ".")),
               mod2 = factor(case_when(model == "freq" ~ "Frequentist",
                                       model == "weakly" ~ "Weakly informative",
                                       model == "mod" ~ "Moderately informative",
                                       model == "highly" ~ "Highly informative"))) %>%
        mutate(mod2 = fct_relevel(mod2, c("Frequentist",
                                          "Weakly informative",
                                          "Moderately informative",
                                          "Highly informative")))
    
    saveRDS(tmp, here("manuscript/tmp/fig_3_data.rds"))
    
    rm(tmp)
    
    # ----------------------------------------------------------------------------------------------
    # Figure 4 data
    # Proportion of lower interval bound above zero
    # ----------------------------------------------------------------------------------------------
    
    tmp1 <- res %>%
        select(model, rho, sim, n, pi2.5) %>%
        group_by(rho, model, n) %>% 
        summarise(prop = mean(pi2.5 > 0)) %>%
        ungroup() %>%
        mutate(pi_width = "95%",
               mod2 = factor(case_when(model == "freq" ~ "Frequentist",
                                       model == "weakly" ~ "Weakly informative",
                                       model == "mod" ~ "Moderately informative",
                                       model == "highly" ~ "Highly informative"))) %>%
        mutate(mod2 = fct_relevel(mod2, c("Frequentist",
                                          "Weakly informative",
                                          "Moderately informative",
                                          "Highly informative")))
    
    tmp2 <- res %>%
        select(model, rho, sim, n, pi5.0) %>%
        group_by(rho, model, n) %>% 
        summarise(prop = mean(pi5.0 > 0)) %>%
        ungroup() %>%
        mutate(pi_width = "90%",
               mod2 = factor(case_when(model == "freq" ~ "Frequentist",
                                       model == "weakly" ~ "Weakly informative",
                                       model == "mod" ~ "Moderately informative",
                                       model == "highly" ~ "Highly informative"))) %>%
        mutate(mod2 = fct_relevel(mod2, c("Frequentist",
                                          "Weakly informative",
                                          "Moderately informative",
                                          "Highly informative")))
    
    tmp3 <- res %>%
        select(model, rho, sim, n, pi17) %>%
        group_by(rho, model, n) %>% 
        summarise(prop = mean(pi17 > 0)) %>%
        ungroup() %>%
        mutate(pi_width = "66%",
               mod2 = factor(case_when(model == "freq" ~ "Frequentist",
                                       model == "weakly" ~ "Weakly informative",
                                       model == "mod" ~ "Moderately informative",
                                       model == "highly" ~ "Highly informative"))) %>%
        mutate(mod2 = fct_relevel(mod2, c("Frequentist",
                                          "Weakly informative",
                                          "Moderately informative",
                                          "Highly informative")))
    
    tmp <- rbind(tmp1, tmp2, tmp3)
    
    saveRDS(tmp, here("manuscript/tmp/fig_4_data.rds"))
    
    rm(tmp, tmp1, tmp2, tmp3)
    
    # ----------------------------------------------------------------------------------------------
    # Figure 5-6 data
    # Proportion of estimates within COS
    # ----------------------------------------------------------------------------------------------
    
    tmp <- rbind(get_cos_data("01", 0.1),
                 get_cos_data("02", 0.1),
                 get_cos_data("03", 0.1),
                 get_cos_data("04", 0.1),

                 get_cos_data("01", 0.15),
                 get_cos_data("02", 0.15),
                 get_cos_data("03", 0.15),
                 get_cos_data("04", 0.15),

                 get_cos_data("01", 0.2),
                 get_cos_data("02", 0.2),
                 get_cos_data("03", 0.2),
                 get_cos_data("04", 0.2))

    tmp <- tmp %>% 
        mutate(mod2 = factor(case_when(model == "freq" ~ "Frequentist",
                                       model == "weakly" ~ "Weakly informative",
                                       model == "mod" ~ "Moderately informative",
                                       model == "highly" ~ "Highly informative"))) %>%
        mutate(mod2 = fct_relevel(mod2, c("Frequentist",
                                          "Weakly informative",
                                          "Moderately informative",
                                          "Highly informative")))
    
    saveRDS(tmp, here("manuscript/tmp/fig_5_data.rds"))
    
    rm(tmp)
    
    # ----------------------------------------------------------------------------------------------
    # Figure 7 data
    # Proportion of intervals within COS, 95%
    # ----------------------------------------------------------------------------------------------
     
    tmp <- rbind(get_int_data("01", 0.1, "pi2.5", "pi97.5"),
                 get_int_data("02", 0.1, "pi2.5", "pi97.5"),
                 get_int_data("03", 0.1, "pi2.5", "pi97.5"),
                 get_int_data("04", 0.1, "pi2.5", "pi97.5"),

                 get_int_data("01", 0.15, "pi2.5", "pi97.5"),
                 get_int_data("02", 0.15, "pi2.5", "pi97.5"),
                 get_int_data("03", 0.15, "pi2.5", "pi97.5"),
                 get_int_data("04", 0.15, "pi2.5", "pi97.5"),

                 get_int_data("01", 0.2, "pi2.5", "pi97.5"),
                 get_int_data("02", 0.2, "pi2.5", "pi97.5"),
                 get_int_data("03", 0.2, "pi2.5", "pi97.5"),
                 get_int_data("04", 0.2, "pi2.5", "pi97.5"))

    tmp <- tmp %>% 
        mutate(mod2 = factor(case_when(model == "freq" ~ "Frequentist",
                                       model == "weakly" ~ "Weakly informative",
                                       model == "mod" ~ "Moderately informative",
                                       model == "highly" ~ "Highly informative"))) %>%
        mutate(mod2 = fct_relevel(mod2, c("Frequentist",
                                          "Weakly informative",
                                          "Moderately informative",
                                          "Highly informative")))
    
    saveRDS(tmp, here("manuscript/tmp/fig_7_data.rds"))
    
    rm(tmp)
     
    # ----------------------------------------------------------------------------------------------
    # Figure S3 data
    # Proportion of intervals within COS, 90%
    # ----------------------------------------------------------------------------------------------
     
    tmp <- rbind(get_int_data("01", 0.1, "pi5.0", "pi95"),
                 get_int_data("02", 0.1, "pi5.0", "pi95"),
                 get_int_data("03", 0.1, "pi5.0", "pi95"),
                 get_int_data("04", 0.1, "pi5.0", "pi95"),

                 get_int_data("01", 0.15, "pi5.0", "pi95"),
                 get_int_data("02", 0.15, "pi5.0", "pi95"),
                 get_int_data("03", 0.15, "pi5.0", "pi95"),
                 get_int_data("04", 0.15, "pi5.0", "pi95"),

                 get_int_data("01", 0.2, "pi5.0", "pi95"),
                 get_int_data("02", 0.2, "pi5.0", "pi95"),
                 get_int_data("03", 0.2, "pi5.0", "pi95"),
                 get_int_data("04", 0.2, "pi5.0", "pi95"))

    tmp <- tmp %>% 
        mutate(mod2 = factor(case_when(model == "freq" ~ "Frequentist",
                                       model == "weakly" ~ "Weakly informative",
                                       model == "mod" ~ "Moderately informative",
                                       model == "highly" ~ "Highly informative"))) %>%
        mutate(mod2 = fct_relevel(mod2, c("Frequentist",
                                          "Weakly informative",
                                          "Moderately informative",
                                          "Highly informative")))
    
    saveRDS(tmp, here("manuscript/tmp/fig_s3_data.rds"))
    
    rm(tmp)
     
    # ----------------------------------------------------------------------------------------------
    # Figure S4 data
    # Proportion of intervals within COS, 66%
    # ----------------------------------------------------------------------------------------------
     
    tmp <- rbind(get_int_data("01", 0.1, "pi17", "pi83"),
                 get_int_data("02", 0.1, "pi17", "pi83"),
                 get_int_data("03", 0.1, "pi17", "pi83"),
                 get_int_data("04", 0.1, "pi17", "pi83"),

                 get_int_data("01", 0.15, "pi17", "pi83"),
                 get_int_data("02", 0.15, "pi17", "pi83"),
                 get_int_data("03", 0.15, "pi17", "pi83"),
                 get_int_data("04", 0.15, "pi17", "pi83"),

                 get_int_data("01", 0.2, "pi17", "pi83"),
                 get_int_data("02", 0.2, "pi17", "pi83"),
                 get_int_data("03", 0.2, "pi17", "pi83"),
                 get_int_data("04", 0.2, "pi17", "pi83"))

    tmp <- tmp %>% 
        mutate(mod2 = factor(case_when(model == "freq" ~ "Frequentist",
                                       model == "weakly" ~ "Weakly informative",
                                       model == "mod" ~ "Moderately informative",
                                       model == "highly" ~ "Highly informative"))) %>%
        mutate(mod2 = fct_relevel(mod2, c("Frequentist",
                                          "Weakly informative",
                                          "Moderately informative",
                                          "Highly informative")))
    
    saveRDS(tmp, here("manuscript/tmp/fig_s4_data.rds"))
    
    rm(tmp)
   
} else {
    
    fig_2_data <- readRDS(here("manuscript/tmp/fig_2_data.rds"))
    fig_3_data <- readRDS(here("manuscript/tmp/fig_3_data.rds"))
    fig_4_data <- readRDS(here("manuscript/tmp/fig_4_data.rds"))
    fig_5_data <- readRDS(here("manuscript/tmp/fig_5_data.rds"))
    fig_7_data <- readRDS(here("manuscript/tmp/fig_7_data.rds"))
    fig_s3_data <- readRDS(here("manuscript/tmp/fig_s3_data.rds"))
    fig_s4_data <- readRDS(here("manuscript/tmp/fig_s4_data.rds"))

}

if (overwrite_figures == TRUE) {
   
    # labels
    labs <- c("Frequentist",
              "Weakly informative",
              "Moderately informative",
              "Highly informative")

    # ----------------------------------------------------------------------------------------------
    # Figure 1
    # Overview of Bayesian priors
    # ----------------------------------------------------------------------------------------------
    
    # weakly inf
    w_01 <- plot_beta(2, 2, 0.1) + labs(title = "Weakly informative")
    w_02 <- plot_beta(2, 2, 0.2)
    w_03 <- plot_beta(2, 2, 0.3)
    w_04 <- plot_beta(2, 2, 0.4)
    
    # moderately inf
    m_01 <- plot_beta(8.7, 7.3, 0.1) + labs(title = "Moderately informative")
    m_02 <- plot_beta(8.8, 6.2, 0.2)
    m_03 <- plot_beta(8.8, 5.2, 0.3)
    m_04 <- plot_beta(8.7, 4.3, 0.4)
    
    # highly inf
    h_01 <- plot_beta(19.7, 16.3, 0.1) + labs(title = "Highly informative")
    h_02 <- plot_beta(19.9, 13.6, 0.2)
    h_03 <- plot_beta(19.4, 10.9, 0.3)
    h_04 <- plot_beta(19.9, 9.1, 0.4)
    
    fig_1 <- 
        w_01 + w_02 + w_03 + w_04 + 
        m_01 + m_02 + m_03 + m_04 + 
        h_01 + h_02 + h_03 + h_04 + 
        plot_layout(nrow = 3)
    
    saveRDS(fig_1, here("manuscript/tmp/fig_1.rds"))
    
    ggsave(here("manuscript/output/figures/fig_1.pdf"),
           fig_1,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 160,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_1.png"),
           fig_1,
           dpi = 300,
           width = 180,
           height = 160,
           units = "mm")
    
    rm(w_01, w_02, w_03, w_04,
       m_01, m_02, m_03, m_04,
       h_01, h_02, h_03, h_04)
    
    # ----------------------------------------------------------------------------------------------
    # Figure 2
    # Extreme trajectories
    # ----------------------------------------------------------------------------------------------
    
    fig_2_data <- readRDS(here("manuscript/tmp/fig_2_data.rds"))
    
    tmp_diff <- fig_2_data %>% 
        filter(rho == "02") %>%
        group_by(sim) %>%
        summarize(min = min(est),
                  max = max(est)) %>%
        mutate(diff = max - min) %>%
        arrange(diff) %>%
        slice(which.max(diff))
    
    high1 <- fig_2_data %>% 
        filter(rho == "02") %>% 
        filter(sim == fig_2_data %>% filter(rho == "02") %>% slice(which.min(est)) %>% pull(sim))
    
    high2 <- fig_2_data %>% 
        filter(rho == "02") %>% 
        filter(sim == fig_2_data %>% filter(rho == "02") %>% slice(which.max(est)) %>% pull(sim))
    
    high3 <- fig_2_data %>% 
        filter(rho == "02") %>% 
        filter(sim == tmp_diff$sim)
    
    fig_2_a <- fig_2_data %>%
        filter(rho == "02") %>%
        ggplot(aes(x = n, y = est, group = sim)) +
        geom_line(size = 0.5, color = "#c1c1c1", alpha = 0.1) +
        geom_line(data = high1, size = 0.8, color = "#33a02c") +
        geom_line(data = high2, size = 0.8, color = "#e31a1c") +
        geom_line(data = high3, size = 0.8, color = "#1f78b4") +
        geom_hline(yintercept = 0.2, color = "#262626", linetype = "dashed") +
        scale_x_continuous(breaks = c(10, seq(100, 500, 100)), expand = c(0.05, 0.05)) + 
        scale_y_continuous(breaks = seq(-0.6, 0.8, 0.2),
                           labels = scales::comma) +
        facet_wrap(~mod2, ncol = 4) +
        labs(y = "Estimate") +
        theme_bw() +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_2_b <- fig_2_data %>%
        filter(n <= 100) %>%
        filter(rho == "02") %>%
        ggplot(aes(x = n, y = est, group = sim)) +
        geom_line(size = 0.5, color = "#c1c1c1", alpha = 0.1) +
        geom_line(data = high1, size = 0.8, color = "#33a02c") +
        geom_line(data = high2, size = 0.8, color = "#e31a1c") +
        geom_line(data = high3, size = 0.8, color = "#1f78b4") +
        geom_hline(yintercept = 0.2, color = "#262626", linetype = "dashed") +
        scale_x_continuous(limits = c(10, 100), 
                           breaks = c(10, seq(25, 100, 25)), 
                           expand = c(0.05, 0.05)) + 
        scale_y_continuous(breaks = seq(-0.6, 0.8, 0.2), 
                           labels = scales::comma) +
        labs(y = "Estimate",
             x = "Sample size") +
        facet_wrap(~mod2, ncol = 4) +
        theme_bw() +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_2 <- fig_2_a / fig_2_b + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_2, here("manuscript/tmp/fig_2.rds"))
    
    ggsave(here("manuscript/output/figures/fig_2.pdf"),
           fig_2,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_2.png"),
           fig_2,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")
    
    rm(tmp_diff, high1, high2, high3, fig_2_a, fig_2_b)
    
    # ----------------------------------------------------------------------------------------------
    # Figure 3
    # Proportion of estimates > 0
    # ----------------------------------------------------------------------------------------------
    
    fig_3_data <- readRDS(here("manuscript/tmp/fig_3_data.rds"))
    
    fig_3_a <- ggplot(fig_3_data, aes(x = n, y = prop, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(breaks = seq(0.6, 1, 0.1)) +
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) +
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_3_b <- fig_3_data %>%
        filter(n <= 100) %>%
        ggplot(aes(x = n, y = prop, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(breaks = seq(0.6, 1, 0.1)) +
        scale_x_continuous(breaks = c(10, seq(25, 100, 25))) +
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_3 <- fig_3_a / fig_3_b + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_3, here("manuscript/tmp/fig_3.rds"))
    
    ggsave(here("manuscript/output/figures/fig_3.pdf"),
           fig_3,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_3.png"),
           fig_3,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")
    
    rm(fig_3_a, fig_3_b)
    
    # ----------------------------------------------------------------------------------------------
    # Figure 4
    # Proportion of lower CrI > 0
    # ----------------------------------------------------------------------------------------------

    fig_4_data <- readRDS(here("manuscript/tmp/fig_4_data.rds"))

    fig_4_a <- fig_4_data %>%
        filter(pi_width == "95%") %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("\u03c1 = ", 
                                    paste0(str_split(rho, "")[[1]], collapse = ".")))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = prop, color = mod2)) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_4_b <- fig_4_data %>%
        filter(pi_width == "95%") %>%
        filter(n <= 100) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("\u03c1 = ", 
                                    paste0(str_split(rho, "")[[1]], collapse = ".")))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = prop, color = mod2)) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(25, 100, 25))) +
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_4 <- fig_4_a / fig_4_b + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_4, here("manuscript/tmp/fig_4.rds"))
    
    ggsave(here("manuscript/output/figures/fig_4.pdf"),
           fig_4,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_4.png"),
           fig_4,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")
    
    rm(fig_4_a, fig_4_b)
    
    # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    # Figure S1
    # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    
    # proportion of 90% interval above zero
    fig_s1_a <- fig_4_data %>%
        filter(pi_width == "90%") %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("\u03c1 = ", 
                                    paste0(str_split(rho, "")[[1]], collapse = ".")))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = prop, color = mod2)) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s1_b <- fig_4_data %>%
        filter(pi_width == "90%") %>%
        filter(n <= 100) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("\u03c1 = ",
                                    paste0(str_split(rho, "")[[1]], collapse = ".")))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = prop, color = mod2)) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(25, 100, 25))) +
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s1 <- fig_s1_a / fig_s1_b + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_s1, here("manuscript/tmp/fig_s1.rds"))
    
    ggsave(here("manuscript/output/figures/fig_s1.pdf"),
           fig_s1,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_s1.png"),
           fig_s1,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")

    rm(fig_s1_a, fig_s1_b)
    
    # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    # Figure S2
    # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

    fig_s2_a <- fig_4_data %>%
        filter(pi_width == "66%") %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("\u03c1 = ",
                                    paste0(str_split(rho, "")[[1]], collapse = ".")))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = prop, color = mod2)) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s2_b <- fig_4_data %>%
        filter(pi_width == "66%") %>%
        filter(n <= 100) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("\u03c1 = ", 
                                    paste0(str_split(rho, "")[[1]], collapse = ".")))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = prop, color = mod2)) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(25, 100, 25))) +
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s2 <- fig_s2_a / fig_s2_b + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_s2, here("manuscript/tmp/fig_s2.rds"))
    
    ggsave(here("manuscript/output/figures/fig_s2.pdf"),
           fig_s2,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_s2.png"),
           fig_s2,
           dpi = 300,
           width = 180,
           height = 140,
           units = "mm")

    rm(fig_s2_a, fig_s2_b)
    
    # ----------------------------------------------------------------------------------------------
    # Figure 5
    # Proportion of estimates within w
    # ----------------------------------------------------------------------------------------------
    
    fig_5_data <- readRDS(here("manuscript/tmp/fig_5_data.rds"))
    
    fig_5_a <- fig_5_data %>%
        filter(w == 0.1) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_5_b <- fig_5_data %>%
        filter(w == 0.15) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_5_c <- fig_5_data %>%
        filter(w == 0.2) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_5 <- fig_5_a / fig_5_b / fig_5_c + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_5, here("manuscript/tmp/fig_5.rds"))

    ggsave(here("manuscript/output/figures/fig_5.pdf"),
           fig_5,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_5.png"),
           fig_5,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    rm(fig_5_a, fig_5_b, fig_5_c)
    
    # ----------------------------------------------------------------------------------------------
    # Figure 6
    # Proportion of estimates within w, N = 10 to N = 100
    # ----------------------------------------------------------------------------------------------
    
    fig_6_a <- fig_5_data %>%
        filter(n <= 100) %>%
        filter(w == 0.1) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
        scale_x_continuous(breaks = c(10, seq(25, 100, 25))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_6_b <- fig_5_data %>%
        filter(n <= 100) %>%
        filter(w == 0.15) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
        scale_x_continuous(breaks = c(10, seq(25, 100, 25))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_6_c <- fig_5_data %>%
        filter(n <= 100) %>%
        filter(w == 0.2) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
        scale_x_continuous(breaks = c(10, seq(25, 100, 25))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_6 <- fig_6_a / fig_6_b / fig_6_c + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_6, here("manuscript/tmp/fig_6.rds"))
    
    ggsave(here("manuscript/output/figures/fig_6.pdf"),
           fig_6,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_6.png"),
           fig_6,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    rm(fig_6_a, fig_6_b, fig_6_c)
     
    # ----------------------------------------------------------------------------------------------
    # Figure 7
    # Proportion of intervals within w, 95%
    # ----------------------------------------------------------------------------------------------
    
    fig_7_data <- readRDS(here("manuscript/tmp/fig_7_data.rds"))
    
    fig_7_a <- fig_7_data %>%
        filter(w == 0.1) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_7_b <- fig_7_data %>%
        filter(w == 0.15) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_7_c <- fig_7_data %>%
        filter(w == 0.2) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_7 <- fig_7_a / fig_7_b / fig_7_c + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_7, here("manuscript/tmp/fig_7.rds"))

    ggsave(here("manuscript/output/figures/fig_7.pdf"),
           fig_7,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_7.png"),
           fig_7,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    rm(fig_7_a, fig_7_b, fig_7_c)
    
    # ----------------------------------------------------------------------------------------------
    # Figure S3
    # Proportion of intervals within w, 90%
    # ----------------------------------------------------------------------------------------------
    
    fig_s3_data <- readRDS(here("manuscript/tmp/fig_s3_data.rds"))
    
    fig_s3_a <- fig_s3_data %>%
        filter(w == 0.1) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s3_b <- fig_s3_data %>%
        filter(w == 0.15) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s3_c <- fig_s3_data %>%
        filter(w == 0.2) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s3 <- fig_s3_a / fig_s3_b / fig_s3_c + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_s3, here("manuscript/tmp/fig_s3.rds"))

    ggsave(here("manuscript/output/figures/fig_s3.pdf"),
           fig_s3,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_s3.png"),
           fig_s3,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    rm(fig_s3_a, fig_s3_b, fig_s3_c)
    
    # ----------------------------------------------------------------------------------------------
    # Figure S4
    # Proportion of intervals within w, 66%
    # ----------------------------------------------------------------------------------------------
    
    fig_s4_data <- readRDS(here("manuscript/tmp/fig_s4_data.rds"))
    
    fig_s4_a <- fig_s4_data %>%
        filter(w == 0.1) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s4_b <- fig_s4_data %>%
        filter(w == 0.15) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s4_c <- fig_s4_data %>%
        filter(w == 0.2) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
        scale_x_continuous(breaks = c(10, seq(100, 500, 100))) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_s4 <- fig_s4_a / fig_s4_b / fig_s4_c + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_s4, here("manuscript/tmp/fig_s4.rds"))

    ggsave(here("manuscript/output/figures/fig_s4.pdf"),
           fig_s4,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_s4.png"),
           fig_s4,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    rm(fig_s4_a, fig_s4_b, fig_s4_c)
 
    # ----------------------------------------------------------------------------------------------
    # Figure 8
    # Proportion of intervals within w, 95%, specific N
    # ----------------------------------------------------------------------------------------------
    
    fig_7_data <- readRDS(here("manuscript/tmp/fig_7_data.rds"))
    
    fig_8_a <- fig_7_data %>%
        filter(w == 0.1) %>%
        filter(n >= 300) %>%
        filter(n <= 400) %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 0.02)) + 
        scale_x_continuous(breaks = seq(300, 400, 25)) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_8_b <- fig_7_data %>%
        filter(w == 0.15) %>%
        filter(n >= 100) %>%
        filter(n <= 200) %>%
        rowwise() %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 0.05)) + 
        scale_x_continuous(breaks = seq(100, 200, 25)) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "none",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_8_c <- fig_7_data %>%
        filter(w == 0.2) %>%
        filter(n >= 50) %>%
        filter(n <= 150) %>%
        rowwise() %>%
        rowwise() %>%
        mutate(rho2 = factor(paste0("w = ", w, ", \u03c1 = ", rho))) %>%
        ungroup() %>%
        ggplot(aes(x = n, y = proportion, color = mod2)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1)) + 
        scale_x_continuous(breaks = seq(50, 150, 25)) + 
        scale_color_manual(values = c("#666666", "#fde725", "#20a387", "#481567"),
                           labels = labs) +
        labs(y = "Proportion",
             x = "Sample size",
             color = "Model") + 
        theme_bw() +
        facet_wrap(~rho2, ncol = 4) +
        theme(legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(0.6, 'lines'),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              strip.background = element_rect(color = "white", fill = "white"),
              strip.text = element_text(size = 10, color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(size = 9, color = "black"),
              axis.title = element_text(size = 10, color = "black"),
              panel.grid.major = element_line(size = 0.2, color = "#e6e6e6"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    fig_8 <- fig_8_a / fig_8_b / fig_8_c + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag = element_text(size = 11))
    
    saveRDS(fig_8, here("manuscript/tmp/fig_8.rds"))

    ggsave(here("manuscript/output/figures/fig_8.pdf"),
           fig_8,
           device = cairo_pdf,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    ggsave(here("manuscript/output/figures/fig_8.png"),
           fig_8,
           dpi = 300,
           width = 180,
           height = 180,
           units = "mm")
    
    rm(fig_8_a, fig_8_b, fig_8_c)

} 
