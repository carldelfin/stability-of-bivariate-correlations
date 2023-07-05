if (overwrite_tables == TRUE) {

    if (!exists("res")) {
        res <- readRDS(here("manuscript/tmp/res.rds"))
    }
    
    # ----------------------------------------------------------------------------------------------
    # Supplementary table 1
    # Neff by group
    # ----------------------------------------------------------------------------------------------
    
    table_s1 <- readRDS(here("manuscript/tmp/neff_grp.rds"))
    
    saveRDS(table_s1, here("manuscript/tmp/table_s1.rds"))
    save_as_docx(flextable(table_s1), path = here("manuscript/output/tables/table_s1.docx"))

    # ----------------------------------------------------------------------------------------------
    # Supplementary table 2
    # MCSE by group
    # ----------------------------------------------------------------------------------------------

    table_s2 <- readRDS(here("manuscript/tmp/mcse_grp.rds"))
    
    saveRDS(table_s2, here("manuscript/tmp/table_s2.rds"))
    save_as_docx(flextable(table_s2), path = here("manuscript/output/tables/table_s2.docx"))
    
    # ----------------------------------------------------------------------------------------------
    # Table 1
    # Proportion of estimates > 0
    # ----------------------------------------------------------------------------------------------
    
    table_1 <- rbind(get_sign_error(0.8),
                     get_sign_error(0.9),
                     get_sign_error(0.95)) %>%
        mutate(rho = gsub("0", "0.", rho)) %>%
        select(rho, prop, freq, weakly, mod, highly)

    saveRDS(table_1, here("manuscript/tmp/table_1.rds"))
    save_as_docx(flextable(table_1), path = here("manuscript/output/tables/table_1.docx"))
    
    # ----------------------------------------------------------------------------------------------
    # Table 2
    # Proportion of lower interval > 0, 95% interval
    # ----------------------------------------------------------------------------------------------

    tmp <- rbind(get_power(0.8, "pi2.5"),
                 get_power(0.9, "pi2.5"),
                 get_power(0.95, "pi2.5"),

                 get_power(0.8, "pi5.0"),
                 get_power(0.9, "pi5.0"),
                 get_power(0.95, "pi5.0"),

                 get_power(0.8, "pi17"),
                 get_power(0.9, "pi17"),
                 get_power(0.95, "pi17"))

    tmp <- tmp %>%
        select(prop, lower_interval, rho, freq, weakly, mod, highly) %>%
        mutate(prop = as.numeric(prop),
               lower_interval = factor(case_when(lower_interval == "pi2.5" ~ "95%",
                                                 lower_interval == "pi5.0" ~ "90%",
                                                 lower_interval == "pi17" ~ "66%")),
               rho = gsub("0", "0.", rho)) %>%
        select(rho, lower_interval, prop, freq, weakly, mod, highly)

    table_2 <- tmp %>%
        filter(lower_interval == "95%")

    saveRDS(table_2, here("manuscript/tmp/table_2.rds"))
    save_as_docx(flextable(table_2), path = here("manuscript/output/tables/table_2.docx"))
    
    # ----------------------------------------------------------------------------------------------
    # Supplementary table 3
    # Proportion of estimates > 0, 90% interval
    # ----------------------------------------------------------------------------------------------
    
    table_s3 <- tmp %>%
        filter(lower_interval == "90%")
    
    saveRDS(table_s3, here("manuscript/tmp/table_s3.rds"))
    save_as_docx(flextable(table_s3), path = here("manuscript/output/tables/table_s3.docx"))
    
    # ----------------------------------------------------------------------------------------------
    # Supplementary table 4
    # Proportion of estimates > 0, 66% interval
    # ----------------------------------------------------------------------------------------------
    
    table_s4 <- tmp %>%
        filter(lower_interval == "66%")

    saveRDS(table_s4, here("manuscript/tmp/table_s4.rds"))
    
    save_as_docx(flextable(table_s4), path = here("manuscript/output/tables/table_s4.docx"))
    
    rm(tmp)

    # ----------------------------------------------------------------------------------------------
    # Table 3
    # Proportion of estimates within COS 
    # ----------------------------------------------------------------------------------------------

    table_3 <- rbind(# P = 0.8
                     get_cos_est("01", 0.2, 0.8),
                     get_cos_est("02", 0.2, 0.8),
                     get_cos_est("03", 0.2, 0.8),
                     get_cos_est("04", 0.2, 0.8),

                     get_cos_est("01", 0.15, 0.8),
                     get_cos_est("02", 0.15, 0.8),
                     get_cos_est("03", 0.15, 0.8),
                     get_cos_est("04", 0.15, 0.8),

                     get_cos_est("01", 0.1, 0.8),
                     get_cos_est("02", 0.1, 0.8),
                     get_cos_est("03", 0.1, 0.8),
                     get_cos_est("04", 0.1, 0.8),

                     # P = 0.90
                     get_cos_est("01", 0.2, 0.9),
                     get_cos_est("02", 0.2, 0.9),
                     get_cos_est("03", 0.2, 0.9),
                     get_cos_est("04", 0.2, 0.9),

                     get_cos_est("01", 0.15, 0.9),
                     get_cos_est("02", 0.15, 0.9),
                     get_cos_est("03", 0.15, 0.9),
                     get_cos_est("04", 0.15, 0.9),

                     get_cos_est("01", 0.1, 0.9),
                     get_cos_est("02", 0.1, 0.9),
                     get_cos_est("03", 0.1, 0.9),
                     get_cos_est("04", 0.1, 0.9),

                     # P = 0.95
                     get_cos_est("01", 0.2, 0.95),
                     get_cos_est("02", 0.2, 0.95),
                     get_cos_est("03", 0.2, 0.95),
                     get_cos_est("04", 0.2, 0.95),

                     get_cos_est("01", 0.15, 0.95),
                     get_cos_est("02", 0.15, 0.95),
                     get_cos_est("03", 0.15, 0.95),
                     get_cos_est("04", 0.15, 0.95),

                     get_cos_est("01", 0.1, 0.95),
                     get_cos_est("02", 0.1, 0.95),
                     get_cos_est("03", 0.1, 0.95),
                     get_cos_est("04", 0.1, 0.95))

    saveRDS(table_3, here("manuscript/tmp/table_3.rds"))
    save_as_docx(flextable(table_3), path = here("manuscript/output/tables/table_3.docx"))
    
    # ----------------------------------------------------------------------------------------------
    # Table 4
    # Proportion of intervals within COS, 95% interval 
    # ----------------------------------------------------------------------------------------------
    
    table_4 <- rbind(# P = 0.8
                     get_int_prop("01", 0.2, 0.8, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("02", 0.2, 0.8, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("03", 0.2, 0.8, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("04", 0.2, 0.8, "pi2.5", "pi97.5", "95%"),

                     get_int_prop("01", 0.15, 0.8, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("02", 0.15, 0.8, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("03", 0.15, 0.8, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("04", 0.15, 0.8, "pi2.5", "pi97.5", "95%"),

                     get_int_prop("01", 0.1, 0.8, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("02", 0.1, 0.8, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("03", 0.1, 0.8, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("04", 0.1, 0.8, "pi2.5", "pi97.5", "95%"),

                     # P = 0.90
                     get_int_prop("01", 0.2, 0.9, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("02", 0.2, 0.9, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("03", 0.2, 0.9, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("04", 0.2, 0.9, "pi2.5", "pi97.5", "95%"),

                     get_int_prop("01", 0.15, 0.9, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("02", 0.15, 0.9, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("03", 0.15, 0.9, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("04", 0.15, 0.9, "pi2.5", "pi97.5", "95%"),

                     get_int_prop("01", 0.1, 0.9, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("02", 0.1, 0.9, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("03", 0.1, 0.9, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("04", 0.1, 0.9, "pi2.5", "pi97.5", "95%"),

                     # P = 0.95
                     get_int_prop("01", 0.2, 0.95, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("02", 0.2, 0.95, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("03", 0.2, 0.95, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("04", 0.2, 0.95, "pi2.5", "pi97.5", "95%"),

                     get_int_prop("01", 0.15, 0.95, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("02", 0.15, 0.95, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("03", 0.15, 0.95, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("04", 0.15, 0.95, "pi2.5", "pi97.5", "95%"),

                     get_int_prop("01", 0.1, 0.95, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("02", 0.1, 0.95, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("03", 0.1, 0.95, "pi2.5", "pi97.5", "95%"),
                     get_int_prop("04", 0.1, 0.95, "pi2.5", "pi97.5", "95%"))

    saveRDS(table_4, here("manuscript/tmp/table_4.rds"))
    save_as_docx(flextable(table_4), path = here("manuscript/output/tables/table_4.docx"))
    
    # ----------------------------------------------------------------------------------------------
    # Supplementary table 5
    # Proportion of intervals within COS, 90% interval 
    # ----------------------------------------------------------------------------------------------
 
    table_s5 <- rbind(# P = 0.8
                     get_int_prop("01", 0.2, 0.8, "pi5.0", "pi95", "90%"),
                     get_int_prop("02", 0.2, 0.8, "pi5.0", "pi95", "90%"),
                     get_int_prop("03", 0.2, 0.8, "pi5.0", "pi95", "90%"),
                     get_int_prop("04", 0.2, 0.8, "pi5.0", "pi95", "90%"),

                     get_int_prop("01", 0.15, 0.8, "pi5.0", "pi95", "90%"),
                     get_int_prop("02", 0.15, 0.8, "pi5.0", "pi95", "90%"),
                     get_int_prop("03", 0.15, 0.8, "pi5.0", "pi95", "90%"),
                     get_int_prop("04", 0.15, 0.8, "pi5.0", "pi95", "90%"),

                     get_int_prop("01", 0.1, 0.8, "pi5.0", "pi95", "90%"),
                     get_int_prop("02", 0.1, 0.8, "pi5.0", "pi95", "90%"),
                     get_int_prop("03", 0.1, 0.8, "pi5.0", "pi95", "90%"),
                     get_int_prop("04", 0.1, 0.8, "pi5.0", "pi95", "90%"),

                     # P = 0.90
                     get_int_prop("01", 0.2, 0.9, "pi5.0", "pi95", "90%"),
                     get_int_prop("02", 0.2, 0.9, "pi5.0", "pi95", "90%"),
                     get_int_prop("03", 0.2, 0.9, "pi5.0", "pi95", "90%"),
                     get_int_prop("04", 0.2, 0.9, "pi5.0", "pi95", "90%"),

                     get_int_prop("01", 0.15, 0.9, "pi5.0", "pi95", "90%"),
                     get_int_prop("02", 0.15, 0.9, "pi5.0", "pi95", "90%"),
                     get_int_prop("03", 0.15, 0.9, "pi5.0", "pi95", "90%"),
                     get_int_prop("04", 0.15, 0.9, "pi5.0", "pi95", "90%"),

                     get_int_prop("01", 0.1, 0.9, "pi5.0", "pi95", "90%"),
                     get_int_prop("02", 0.1, 0.9, "pi5.0", "pi95", "90%"),
                     get_int_prop("03", 0.1, 0.9, "pi5.0", "pi95", "90%"),
                     get_int_prop("04", 0.1, 0.9, "pi5.0", "pi95", "90%"),

                     # P = 0.95
                     get_int_prop("01", 0.2, 0.95, "pi5.0", "pi95", "90%"),
                     get_int_prop("02", 0.2, 0.95, "pi5.0", "pi95", "90%"),
                     get_int_prop("03", 0.2, 0.95, "pi5.0", "pi95", "90%"),
                     get_int_prop("04", 0.2, 0.95, "pi5.0", "pi95", "90%"),

                     get_int_prop("01", 0.15, 0.95, "pi5.0", "pi95", "90%"),
                     get_int_prop("02", 0.15, 0.95, "pi5.0", "pi95", "90%"),
                     get_int_prop("03", 0.15, 0.95, "pi5.0", "pi95", "90%"),
                     get_int_prop("04", 0.15, 0.95, "pi5.0", "pi95", "90%"),

                     get_int_prop("01", 0.1, 0.95, "pi5.0", "pi95", "90%"),
                     get_int_prop("02", 0.1, 0.95, "pi5.0", "pi95", "90%"),
                     get_int_prop("03", 0.1, 0.95, "pi5.0", "pi95", "90%"),
                     get_int_prop("04", 0.1, 0.95, "pi5.0", "pi95", "90%"))

    saveRDS(table_s5, here("manuscript/tmp/table_s5.rds"))
    save_as_docx(flextable(table_s5), path = here("manuscript/output/tables/table_s5.docx"))

    # ----------------------------------------------------------------------------------------------
    # Supplementary table 6
    # Proportion of intervals within COS, 66% interval 
    # ----------------------------------------------------------------------------------------------
 
    table_s6 <- rbind(# P = 0.8
                     get_int_prop("01", 0.2, 0.8, "pi17", "pi83", "66%"),
                     get_int_prop("02", 0.2, 0.8, "pi17", "pi83", "66%"),
                     get_int_prop("03", 0.2, 0.8, "pi17", "pi83", "66%"),
                     get_int_prop("04", 0.2, 0.8, "pi17", "pi83", "66%"),

                     get_int_prop("01", 0.15, 0.8, "pi17", "pi83", "66%"),
                     get_int_prop("02", 0.15, 0.8, "pi17", "pi83", "66%"),
                     get_int_prop("03", 0.15, 0.8, "pi17", "pi83", "66%"),
                     get_int_prop("04", 0.15, 0.8, "pi17", "pi83", "66%"),

                     get_int_prop("01", 0.1, 0.8, "pi17", "pi83", "66%"),
                     get_int_prop("02", 0.1, 0.8, "pi17", "pi83", "66%"),
                     get_int_prop("03", 0.1, 0.8, "pi17", "pi83", "66%"),
                     get_int_prop("04", 0.1, 0.8, "pi17", "pi83", "66%"),

                     # P = 0.90
                     get_int_prop("01", 0.2, 0.9, "pi17", "pi83", "66%"),
                     get_int_prop("02", 0.2, 0.9, "pi17", "pi83", "66%"),
                     get_int_prop("03", 0.2, 0.9, "pi17", "pi83", "66%"),
                     get_int_prop("04", 0.2, 0.9, "pi17", "pi83", "66%"),

                     get_int_prop("01", 0.15, 0.9, "pi17", "pi83", "66%"),
                     get_int_prop("02", 0.15, 0.9, "pi17", "pi83", "66%"),
                     get_int_prop("03", 0.15, 0.9, "pi17", "pi83", "66%"),
                     get_int_prop("04", 0.15, 0.9, "pi17", "pi83", "66%"),

                     get_int_prop("01", 0.1, 0.9, "pi17", "pi83", "66%"),
                     get_int_prop("02", 0.1, 0.9, "pi17", "pi83", "66%"),
                     get_int_prop("03", 0.1, 0.9, "pi17", "pi83", "66%"),
                     get_int_prop("04", 0.1, 0.9, "pi17", "pi83", "66%"),

                     # P = 0.95
                     get_int_prop("01", 0.2, 0.95, "pi17", "pi83", "66%"),
                     get_int_prop("02", 0.2, 0.95, "pi17", "pi83", "66%"),
                     get_int_prop("03", 0.2, 0.95, "pi17", "pi83", "66%"),
                     get_int_prop("04", 0.2, 0.95, "pi17", "pi83", "66%"),

                     get_int_prop("01", 0.15, 0.95, "pi17", "pi83", "66%"),
                     get_int_prop("02", 0.15, 0.95, "pi17", "pi83", "66%"),
                     get_int_prop("03", 0.15, 0.95, "pi17", "pi83", "66%"),
                     get_int_prop("04", 0.15, 0.95, "pi17", "pi83", "66%"),

                     get_int_prop("01", 0.1, 0.95, "pi17", "pi83", "66%"),
                     get_int_prop("02", 0.1, 0.95, "pi17", "pi83", "66%"),
                     get_int_prop("03", 0.1, 0.95, "pi17", "pi83", "66%"),
                     get_int_prop("04", 0.1, 0.95, "pi17", "pi83", "66%"))

    saveRDS(table_s6, here("manuscript/tmp/table_s6.rds"))
    save_as_docx(flextable(table_s6), path = here("manuscript/output/tables/table_s6.docx"))

} 
