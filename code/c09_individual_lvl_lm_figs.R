
# plot estimated coefficients for income group categorical variable

# load(file = paste0(edir_atus, "atus_", tfirst, "_", tlast, "_individual_lm_all.Rdata"))

# activity_chosen <- c("t_shop_ttl")
activity_chosen <- c("t_shop_food", "t_shop_gas", "t_shop_groceries", "t_shop_other", "t_shop_travel")
# activity_chosen <- c("t_shop_food", "t_shop_gas", "t_shop_groceries", "t_shop_other", "t_shop_travel", "t_shop_ttl")
# activity_chosen <- c("t_ahk_leisure_tv", "t_ahk_leisure_oth", "t_leisure", "t_ahk_sleep")
# activity_chosen <- c("t_work_ttl", "t_ahk_childcare", "t_meals", "t_ahk_homeproduction")
# activity_chosen <- c("t_ahk_education", "t_ahk_civic", "t_other")
# activity_chosen <- c("t_eat", "t_personal_care", "t_ahk_ownmdcare", "t_ahk_othercare")

# plot estimated coefficients for income
g <- df_lm_results_all %>%
    filter(activity %in% activity_chosen) %>%
    select(activity_number, activity_label, activity, lm_spc_number, lm_coefs) %>%
    unnest() %>%
    filter(str_sub(term, 1, 6) == "faminc") %>%
    mutate(term = term %>%
               str_sub(11, -1) %>%
               str_replace(",Inf", "+") %>%
               str_replace(",", " - ") %>%
               str_replace_all("[\\[()]", "")) %>%
    group_by(lm_spc_number) %>%
    mutate(faminc_position = row_number()) %>%
    ungroup() %>%
    ggplot(aes(x = estimate, y = reorder(term, faminc_position))) +
        geom_point() +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.2), col = "red") +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        labs(x = "Hours per week relative to family with income under 12.5 thousands",
             y = "Family Income (in thousands)") +
        # facet_grid(lm_spc_number ~ activity) +
        facet_grid(lm_spc_number ~ activity_label, switch = "y") +
        # theme_minimal() +
        theme(# strip.background = element_blank(),
              # strip.text_y = element_text(angle = 90),
              strip.placement = "outside")
g


# file_path <- paste0(odir_atus, "fig_individual_coefs_t_shop_ttl.pdf")
# ggsave(filename = file_path, plot = g, width = 3.25, height = 6.05)

file_path <- paste0(odir_atus, "fig_individual_coefs_t_shop_all.pdf")
ggsave(filename = file_path, plot = g, width = 9, height = 9)

# file_path <- paste0(odir_atus, "fig_individual_coefs_t_leisure.pdf")
# file_path <- paste0(odir_atus, "fig_individual_coefs_t_work.pdf")
# file_path <- paste0(odir_atus, "fig_individual_coefs_t_care.pdf")
# ggsave(filename = file_path, plot = g, width =  7, height = 9)
