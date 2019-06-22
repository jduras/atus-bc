
# plot estimated coefficients for income group categorical variable

# load(file = paste0(edir_atus, "atus_", tfirst, "_", tlast, "_individual_lm_all.Rdata"))

activity_chosen <- c("t_shop_ttl")
# activity_chosen <- c("t_shop_food", "t_shop_gas", "t_shop_groceries", "t_shop_other", "t_shop_travel")
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
        facet_grid(lm_spc_number ~ ., switch = "y") +
        # theme_minimal() +
        theme(# strip.background = element_blank(),
              # strip.text_y = element_text(angle = 90),
              strip.placement = "outside")
g


file_path <- paste0(odir_atus, "fig_individual_coefs_t_shop_ttl.pdf")
ggsave(filename = file_path, plot = g, width = 3.25, height = 6.05)

# file_path <- paste0(odir_atus, "fig_individual_coefs_t_shop_all.pdf")
# ggsave(filename = file_path, plot = g, width = 9, height = 9)

# file_path <- paste0(odir_atus, "fig_individual_coefs_t_leisure.pdf")
# file_path <- paste0(odir_atus, "fig_individual_coefs_t_work.pdf")
# file_path <- paste0(odir_atus, "fig_individual_coefs_t_care.pdf")
# ggsave(filename = file_path, plot = g, width =  7, height = 9)


df_lm_results_all %>%
    filter(parse_number(lm_spc_number) %in% c(1:5, 7:8)) %>%
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
        # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.1)) +
        geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.1), col = "black", alpha = 0.25) +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        geom_point(color = "red") +
            labs(title = "Hours per week relative to family with income under 12.5 thousands",
                 x = "",
             y = "Family Income (in thousands)") +
        scale_x_continuous(sec.axis = dup_axis()) +
        # facet_grid(lm_spc_number ~ activity) +
        facet_grid(lm_spc_number ~ ., switch = "y") +
        # facet_grid(~lm_spc_number, switch = "y") +
        # coord_flip() +
        # theme_minimal() +
        theme_bw() +
        # theme_classic() +
        theme(strip.background = element_blank(),
            axis.text.x.bottom = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            strip.text.y = element_text(angle = 180),
            strip.placement = "outside")

g <- df_lm_results_all %>%
    filter(parse_number(lm_spc_number) %in% c(1,4,7,8)) %>%
    mutate(lm_spc_label = case_when(lm_spc_number == "Model 1" ~ "no controls",
                                    lm_spc_number == "Model 3" ~ "add demographics",
                                    lm_spc_number == "Model 4" ~ "add demographics and LF status",
                                    lm_spc_number == "Model 7" ~ "add fixed effects",
                                    lm_spc_number == "Model 8" ~ "add unemployment rate"),
           lm_spc_number = factor(lm_spc_number,
                                  labels = c("no controls", "add demographics and LF status", "add fixed effects", "add unemployment rate"),
                                  ordered = TRUE)) %>%
    filter(activity %in% activity_chosen) %>%
    select(activity_number, activity_label, activity, lm_spc_number, lm_spc_label, lm_coefs) %>%
    unnest() %>%
    filter(str_sub(term, 1, 6) == "faminc") %>%
    mutate(term = term %>%
               str_sub(11, -1) %>%
               str_replace(",Inf", "+") %>%
               str_replace(",", " - ") %>%
               str_replace_all("[\\[()]", "") %>%
               str_replace_all(" ", "")) %>%
    group_by(lm_spc_number) %>%
    mutate(faminc_position = row_number()) %>%
    ungroup() %>%
    ggplot(aes(x = estimate, y = reorder(term, faminc_position))) +
        # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.1)) +
        geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.1), col = "black", alpha = 0.75) +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        geom_point(size = 2, color = "red") +
        labs(title = "Hours Spent Shopping per Week",
             subtitle = "Relative to Families with Income Under 12.5 thousands",
             x = "",
             y = "Family Income (in thousands)") +
        scale_x_continuous(sec.axis = dup_axis()) +
        # facet_grid(lm_spc_number ~ activity) +
        # facet_grid(lm_spc_number ~ ., switch = "y") +
        facet_grid(~lm_spc_number, switch = "y") +
        coord_flip() +
        # theme_minimal() +
        theme_bw() +
        # theme_classic() +
        theme(strip.text = element_text(hjust = 0, face = "bold"),
              text = element_text(size = 10),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 7),
              strip.background = element_blank(),
              # axis.text.x.bottom = element_blank(),
              # axis.text.y.right = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              strip.text.y = element_text(angle = 180),
              strip.placement = "outside")
g

file_path <- paste0(odir_atus, "fig_individual_coefs_t_shop_all_slides.pdf")
ggsave(filename = file_path, plot = g, width = 15, height = 3)



# g <-
df_lm_results_all %>%
    filter(parse_number(lm_spc_number) %in% c(8)) %>%
    # mutate(lm_spc_label = case_when(lm_spc_number == "Model 1" ~ "no controls",
    #                                 lm_spc_number == "Model 3" ~ "add demographics",
    #                                 lm_spc_number == "Model 4" ~ "add demographics and LF status",
    #                                 lm_spc_number == "Model 7" ~ "add fixed effects",
    #                                 lm_spc_number == "Model 8" ~ "add unemployment rate"),
    #        lm_spc_number = factor(lm_spc_number,
    #                               labels = c("no controls", "add demographics and LF status", "add fixed effects", "add unemployment rate"),
    #                               ordered = TRUE)) %>%
    filter(activity %in% activity_chosen) %>%
    # select(activity_number, activity_label, activity, lm_spc_number, lm_spc_label, lm_coefs) %>%
    select(activity_number, activity_label, activity, lm_spc_number, lm_coefs) %>%
    unnest() %>%
    filter(str_sub(term, 1, 6) == "faminc") %>%
    mutate(term = term %>%
               str_sub(11, -1) %>%
               str_replace(",Inf", "+") %>%
               str_replace(",", " - ") %>%
               str_replace_all("[\\[()]", "") %>%
               str_replace_all(" ", "")) %>%
    group_by(lm_spc_number) %>%
    mutate(faminc_position = row_number()) %>%
    ungroup() %>%
    ggplot(aes(x = estimate, y = reorder(term, faminc_position))) +
    # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.1)) +
    geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.1), col = "black", alpha = 0.75) +
    geom_vline(aes(xintercept = 0), linetype = "dotted") +
    geom_point(size = 2, color = "red") +
    labs(title = "Hours Spent Shopping per Week",
         subtitle = "Relative to Families with Income Under 12.5 thousands",
         x = "",
         y = "Family Income (in thousands)") +
    scale_x_continuous(sec.axis = dup_axis()) +
    # facet_grid(lm_spc_number ~ activity) +
    # facet_grid(lm_spc_number ~ ., switch = "y") +
    # facet_grid(~lm_spc_number, switch = "y") +
    facet_grid(activity ~ ., switch = "y", scales = "free") +
    coord_flip() +
    # theme_minimal() +
    theme_bw() +
    # theme_classic() +
    theme(strip.text = element_text(hjust = 0, face = "bold"),
          text = element_text(size = 10),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          strip.background = element_blank(),
          # axis.text.x.bottom = element_blank(),
          # axis.text.y.right = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.text.y = element_text(angle = 180),
          strip.placement = "outside")
g

# file_path <- paste0(odir_atus, "fig_individual_coefs_t_leisure.pdf")
# file_path <- paste0(odir_atus, "fig_individual_coefs_t_work.pdf")
# file_path <- paste0(odir_atus, "fig_individual_coefs_t_care.pdf")
# ggsave(filename = file_path, plot = g, width =  7, height = 9)


g <- df_lm_results_all %>%
    filter(parse_number(lm_spc_number) %in% c(8)) %>%
    # mutate(lm_spc_label = case_when(lm_spc_number == "Model 1" ~ "no controls",
    #                                 lm_spc_number == "Model 3" ~ "add demographics",
    #                                 lm_spc_number == "Model 4" ~ "add demographics and LF status",
    #                                 lm_spc_number == "Model 7" ~ "add fixed effects",
    #                                 lm_spc_number == "Model 8" ~ "add unemployment rate"),
    #        lm_spc_number = factor(lm_spc_number,
    #                               labels = c("no controls", "add demographics and LF status", "add fixed effects", "add unemployment rate"),
    #                               ordered = TRUE)) %>%
    #filter(activity %in% activity_chosen) %>%
    #
    filter(activity %in%  c("t_shop_groceries", "t_shop_gas", "t_shop_food", "t_shop_other", "t_shop_travel")) %>%
    mutate(activity_label  = recode(activity,
                                    "t_shop_groceries"       = "Groceries",
                                    "t_shop_gas"             = "Gas",
                                    "t_shop_food"            = "Food",
                                    # "t_shop_ggf"             = "Groceries, gas, food",
                                    "t_shop_other"           = "Other",
                                    "t_shop_travel"          = "Travel") %>%
               factor(levels = c("Groceries", "Gas", "Food", "Other", "Travel"),
                      ordered = TRUE)) %>%
    # select(activity_number, activity_label, activity, lm_spc_number, lm_spc_label, lm_coefs) %>%
    select(activity_number, activity_label, activity, lm_spc_number, lm_coefs) %>%
    unnest() %>%
    # filter(str_sub(term, 1, 6) == "faminc" | term == "(Intercept)") %>%
    # group_by(lm_spc_number, activity) %>%
    # mutate(estimate = if_else(term != "(Intercept)", estimate + estimate[term == "(Intercept)"], estimate),
    #        conf_low_clustered = conf_low_clustered + estimate[term == "(Intercept)"],
    #        conf_high_clustered = conf_high_clustered + estimate[term == "(Intercept)"]) %>%
    # ungroup() %>%
    filter(str_sub(term, 1, 6) == "faminc") %>%
    mutate(term = term %>%
               str_sub(11, -1) %>%
               str_replace(",Inf", "+") %>%
               str_replace(",", " - ") %>%
               str_replace_all("[\\[()]", "") %>%
               str_replace_all(" ", "")) %>%
    group_by(lm_spc_number) %>%
    mutate(faminc_position = row_number()) %>%
    ungroup() %>%
    ggplot(aes(x = estimate, y = reorder(term, faminc_position))) +
        # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.1)) +
        geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.1), col = "black", alpha = 0.75) +
        geom_vline(aes(xintercept = 0)) +
        geom_point(size = 2, color = "red") +
        labs(title = "Hours Spent Shopping per Week",
             subtitle = "Relative to Families with Income Under 12.5 thousands",
             x = "",
             y = "Family Income (in thousands)") +
        scale_x_continuous(sec.axis = dup_axis()) +
        # facet_grid(lm_spc_number ~ activity) +
        # facet_grid(lm_spc_number ~ ., switch = "y") +
        # facet_grid(~lm_spc_number, switch = "y") +
        # facet_grid(activity_label ~ activity_group, switch = "y", scales = "free") +
        facet_wrap( ~ activity_label, ncol = 3, scales = "free") +
        # facet_wrap( ~ activity_label, ncol = 3) +
        coord_flip() +
        # theme_minimal() +
        theme_bw() +
        # theme_classic() +
        theme(strip.text = element_text(hjust = 0, face = "bold"),
              text = element_text(size = 10),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 7),
              strip.background = element_blank(),
              # axis.text.x.bottom = element_blank(),
              # axis.text.y.right = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              strip.text.y = element_text(angle = 180),
              strip.placement = "outside")
g

file_path <- paste0(odir_atus, "fig_individual_coefs_t_shop_categories_slides.pdf")
ggsave(filename = file_path, plot = g, width = 10, height = 5)



g <- df_lm_results_all %>%
    filter(parse_number(lm_spc_number) %in% c(8)) %>%
    # mutate(lm_spc_label = case_when(lm_spc_number == "Model 1" ~ "no controls",
    #                                 lm_spc_number == "Model 3" ~ "add demographics",
    #                                 lm_spc_number == "Model 4" ~ "add demographics and LF status",
    #                                 lm_spc_number == "Model 7" ~ "add fixed effects",
    #                                 lm_spc_number == "Model 8" ~ "add unemployment rate"),
    #        lm_spc_number = factor(lm_spc_number,
    #                               labels = c("no controls", "add demographics and LF status", "add fixed effects", "add unemployment rate"),
    #                               ordered = TRUE)) %>%
    #filter(activity %in% activity_chosen) %>%
    #
    filter(activity %in%  c("t_ahk_leisure_tv", "t_ahk_leisure_oth", "t_leisure", "t_ahk_sleep",
                            "t_work_ttl", "t_ahk_childcare", "t_meals", "t_ahk_homeproduction",
                            "t_eat", "t_personal_care", "t_ahk_ownmdcare", "t_ahk_othercare")) %>%
    mutate(activity_label  = recode(activity,
                                    "t_ahk_leisure_tv"       = "Leisure: TV",
                                    "t_ahk_leisure_oth"      = "Leisure: Other",
                                    "t_leisure"              = "Leisure: Total",
                                    "t_ahk_sleep"            = "Sleeping",
                                    "t_work_ttl"             = "Work",
                                    "t_ahk_childcare"        = "Childcare",
                                    "t_meals"                = "Preparing meal",
                                    "t_ahk_homeproduction"   = "Home production",
                                    "t_eat"                  = "Eating",
                                    "t_personal_care"        = "Personal care",
                                    "t_ahk_ownmdcare"        = "Own medical care",
                                    "t_ahk_othercare"        = "Taking care of others") %>%
               factor(levels = c("Leisure: TV", "Leisure: Other", "Leisure: Total", "Sleeping",
                                 "Work", "Childcare", "Preparing meal", "Home production",
                                 "Eating", "Personal care", "Own medical care", "Taking care of others"),
                      ordered = TRUE)) %>%
    mutate(activity_group = case_when(activity %in% c("t_ahk_leisure_tv", "t_ahk_leisure_oth", "t_leisure", "t_ahk_sleep") ~ "leisure",
                                      activity %in% c("t_work_ttl", "t_ahk_childcare", "t_meals", "t_ahk_homeproduction") ~ "work",
                                      activity %in% c("t_eat", "t_personal_care", "t_ahk_ownmdcare", "t_ahk_othercare") ~ "care")) %>%
    # select(activity_number, activity_label, activity, lm_spc_number, lm_spc_label, lm_coefs) %>%
    select(activity_number, activity_label, activity, activity_group, lm_spc_number, lm_coefs) %>%
    unnest() %>%
    # filter(str_sub(term, 1, 6) == "faminc" | term == "(Intercept)") %>%
    # group_by(lm_spc_number, activity) %>%
    # mutate(estimate = if_else(term != "(Intercept)", estimate + estimate[term == "(Intercept)"], estimate),
    #        conf_low_clustered = conf_low_clustered + estimate[term == "(Intercept)"],
    #        conf_high_clustered = conf_high_clustered + estimate[term == "(Intercept)"]) %>%
    ungroup() %>%
    filter(str_sub(term, 1, 6) == "faminc") %>%
    mutate(term = term %>%
               str_sub(11, -1) %>%
               str_replace(",Inf", "+") %>%
               str_replace(",", " - ") %>%
               str_replace_all("[\\[()]", "") %>%
               str_replace_all(" ", "")) %>%
    group_by(lm_spc_number) %>%
    mutate(faminc_position = row_number()) %>%
    ungroup() %>%
    ggplot(aes(x = estimate, y = reorder(term, faminc_position))) +
    # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.1)) +
    geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.1), col = "black", alpha = 0.75) +
    geom_vline(aes(xintercept = 0)) +
    geom_point(size = 2, color = "red") +
    labs(title = "Hours Spent Shopping per Week",
         subtitle = "Relative to Families with Income Under 12.5 thousands",
         x = "",
         y = "Family Income (in thousands)") +
    scale_x_continuous(sec.axis = dup_axis()) +
    # facet_grid(lm_spc_number ~ activity) +
    # facet_grid(lm_spc_number ~ ., switch = "y") +
    # facet_grid(~lm_spc_number, switch = "y") +
    # facet_grid(activity_label ~ activity_group, switch = "y", scales = "free") +
    facet_wrap( ~ activity_label, ncol = 4, scales = "free") +
    coord_flip() +
    # theme_minimal() +
    theme_bw() +
    # theme_classic() +
    theme(strip.text = element_text(hjust = 0, face = "bold"),
          text = element_text(size = 10),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          strip.background = element_blank(),
          # axis.text.x.bottom = element_blank(),
          # axis.text.y.right = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.text.y = element_text(angle = 180),
          strip.placement = "outside")
g

file_path <- paste0(odir_atus, "fig_individual_coefs_combined.pdf")
ggsave(filename = file_path, plot = g, width =  13, height = 6.5)
