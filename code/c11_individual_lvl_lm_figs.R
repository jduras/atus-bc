
message(str_c("Plotting the results from individual level regressions: time spent on various activities"))

if (!exists("df_lm_results_coefs_all")) load(file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_individual_lm_coefs_all.Rdata"))

#### dataset to plot estimated coefficients for income group categorical variable for selected models ####

# all estimates are relative to family with lowest income
df_lm_results_coefs_all_toplot <-
    df_lm_results_coefs_all %>%
    mutate(activity_number = row_number(),
           activity_label  = recode(activity,
                                    "t_shop_groceries"       = "Groceries",
                                    "t_shop_gas"             = "Gas",
                                    "t_shop_food"            = "Food",
                                    "t_shop_ggf"             = "Groceries, gas, food",
                                    "t_shop_other"           = "Other",
                                    "t_shop_other_shop"      = "Other: shopping",
                                    "t_shop_other_research"  = "Other: research",
                                    "t_shop_other_wait"      = "Other: waiting",
                                    "t_shop_travel"          = "Travel",
                                    "t_shop_ttl"             = "Total shopping time",
                                    # "t_ahk_shop"             = "Shopping",
                                    "t_ahk_leisure_tv"       = "Leisure: TV",
                                    "t_ahk_leisure_oth"      = "Leisure: Other",
                                    "t_leisure"              = "Leisure: Total",
                                    "t_eat"                  = "Eating",
                                    "t_ahk_sleep"            = "Sleeping",
                                    "t_personal_care"        = "Personal care",
                                    "t_ahk_esp"              = "Eating, sleeping, personal care",
                                    "t_ahk_espleisure"       = "Eating, sleeping, personal care, leisure",
                                    "t_ahk_ownmdcare"        = "Own medical care",
                                    "t_ahk_othercare"        = "Taking care of others",
                                    "t_ahk_childcare"        = "Childcare",
                                    "t_childcare_play"       = "Childcare: playing",
                                    "t_meals"                = "Preparing meal",
                                    "t_housework"            = "Housework",
                                    "t_home_car_maintenance" = "Home and vehicle maintenance and repair",
                                    "t_ahk_homeproduction"   = "Home production",
                                    "t_nonmarket_work_ttl"   = "Non-market Work: Total",
                                    "t_ahk_work"             = "Work",
                                    "t_ahk_work_search"      = "Work: Search",
                                    "t_work_ttl"             = "Work: Total",
                                    "t_ahk_education"        = "Education",
                                    "t_ahk_civic"            = "Civic",
                                    "t_other"                = "Other activities",
                                    "t_ttl"                  = "Total time") %>%
               factor(levels = c("Groceries", "Gas", "Food", "Groceries, gas, food",
                                 "Other", "Other: shopping", "Other: research", "Other: waiting",
                                 "Travel", "Total shopping time",
                                 "Leisure: TV", "Leisure: Other", "Leisure: Total",
                                 "Eating", "Sleeping", "Personal care", "Eating, sleeping, personal care", "Eating, sleeping, personal care, leisure",
                                 "Own medical care", "Taking care of others", "Childcare", "Childcare: playing",
                                 "Preparing meal", "Housework", "Home and vehicle maintenance and repair", "Home production", "Non-market Work: Total",
                                 "Work", "Work: Search", "Work: Total",
                                 "Education", "Civic", "Other activities", "Total time"),
                      ordered = TRUE)) %>%
    ungroup() %>%
    select(activity_number, activity_label, activity, lm_spc_number, lm_spc_label, lm_coefs) %>%
    unnest() %>%
    filter(str_sub(term, 1, 6) == "faminc") %>%
    mutate(term = term %>%
               str_sub(11, -1) %>%
               str_replace(",Inf", "+") %>%
               str_replace(",", " - ") %>%
               str_replace_all("[\\[()]", "")) %>%
    group_by(lm_spc_number) %>%
    mutate(faminc_position = row_number()) %>%
    ungroup()

#### plot estimated coefficients for income group categorical variable for each estimated model ####

# chart template
g <- ggplot() +
        geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, y = reorder(term, faminc_position)), height = 0.1, alpha = 0.3) +
        # geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, y = reorder(term, faminc_position))), height = 0, col = "black", alpha = 0.25) +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        geom_point(aes(x = estimate, y = reorder(term, faminc_position)), color = "red") +
        # labs(title = "Minutes per day compared to family with income under 12.5 thousands",
        labs(title = "",
             #title = "Hours per day compared to family with income under 12.5 thousands",
             x = "Hours per day compared to family with income under 12.5 thousands",
             y = "Family Income (in thousands)") +
        scale_x_continuous(sec.axis = dup_axis()) +
        theme_bw() +
        theme(axis.title.x = element_text(hjust = 0),
              axis.title.x.bottom = element_blank(),
              axis.title.y = element_text(hjust = 1),
              #axis.text.x.bottom = element_blank(),
              # panel.grid.minor = element_blank(),
              # panel.grid.major = element_blank(),
              panel.grid.major = element_line(color = "gray95"),
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text.x = element_text(face = "bold", hjust = 0))

# plot for total shopping time
g %+% {df_lm_results_coefs_all_toplot %>%
        filter(activity %in% "t_shop_ttl")} +
    facet_grid(lm_spc_number ~ ., switch = "y") +
    theme(text = element_text(size = 8),
          axis.title.x = element_text(hjust = +1))

p <- g %+% {df_lm_results_coefs_all_toplot %>%
                filter(parse_number(lm_spc_number) %in% c(1:5, 7:8)) %>%
                filter(activity %in% "t_shop_ttl")} +
    facet_grid(lm_spc_number ~ ., switch = "y") +
    theme(text = element_text(size = 8),
          axis.title.x = element_text(hjust = +1))
p
ggsave(filename = str_c(odir_atus, "fig_individual_coefs_t_shop_ttl_", tfst, "_", tlst, ".pdf"), plot = p, width = 3.5, height = 9)

# plot for shopping related activities
p <- g %+% {df_lm_results_coefs_all_toplot %>%
                filter(parse_number(lm_spc_number) %in% c(1:5, 7:8)) %>%
                filter(activity %in% c("t_shop_groceries", "t_shop_gas", "t_shop_food", "t_shop_other", "t_shop_travel"))} +
#    facet_grid(lm_spc_number ~ activity_label, switch = "y")
    facet_grid(lm_spc_number ~ activity_label, switch = "y", scales = "free_x")
p
ggsave(filename = str_c(odir_atus, "fig_individual_coefs_t_shop_all_", tfst, "_", tlst, ".pdf"), plot = p, width =  9, height = 9)

# plot for leisure activities
p <- g %+% {df_lm_results_coefs_all_toplot %>%
                filter(parse_number(lm_spc_number) %in% c(1:5, 7:8)) %>%
                filter(activity %in% c("t_ahk_leisure_tv", "t_ahk_leisure_oth", "t_leisure", "t_ahk_sleep"))} +
#    facet_grid(lm_spc_number ~ activity_label, switch = "y")
    facet_grid(lm_spc_number ~ activity_label, switch = "y", scales = "free_x")
p
ggsave(filename = str_c(odir_atus, "fig_individual_coefs_t_leisure_", tfst, "_", tlst, ".pdf"), plot = p, width =  7, height = 9)

# plot for work related activities
p <- g %+% {df_lm_results_coefs_all_toplot %>%
                filter(parse_number(lm_spc_number) %in% c(1:5, 7:8)) %>%
                filter(activity %in% c("t_work_ttl", "t_ahk_childcare", "t_meals", "t_ahk_homeproduction"))} +
#    facet_grid(lm_spc_number ~ activity_label, switch = "y")
    facet_grid(lm_spc_number ~ activity_label, switch = "y", scales = "free_x")
p
ggsave(filename = str_c(odir_atus, "fig_individual_coefs_t_work_", tfst, "_", tlst, ".pdf"), plot = p, width =  7, height = 9)

# plot for care related activities
p <- g %+% {df_lm_results_coefs_all_toplot %>%
                filter(parse_number(lm_spc_number) %in% c(1:5, 7:8)) %>%
                filter(activity %in% c("t_eat", "t_personal_care", "t_ahk_ownmdcare", "t_ahk_othercare"))} +
    #    facet_grid(lm_spc_number ~ activity_label, switch = "y")
    facet_grid(lm_spc_number ~ activity_label, switch = "y", scales = "free_x")
p
ggsave(filename = str_c(odir_atus, "fig_individual_coefs_t_care_", tfst, "_", tlst, ".pdf"), plot = p, width =  7, height = 9)

g %+% {df_lm_results_coefs_all_toplot %>%
        filter(parse_number(lm_spc_number) %in% c(1:5, 7:8)) %>%
        filter(activity %in% c("t_ahk_education", "t_ahk_civic", "t_other"))} +
    facet_grid(lm_spc_number ~ activity_label, switch = "y")
