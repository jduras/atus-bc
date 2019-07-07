
load(file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_individual_lm_coefs_all.Rdata"))

#### plot estimated coefficients for income group categorical variable for selected models ####

df_lm_results_coefs_all_toplot <-
    df_lm_results_coefs_all %>%
    filter(parse_number(lm_spc_number) %in% c(1,4,7,8)) %>%
    group_by(lm_spc) %>%
    mutate(activity_number = row_number(),
           activity_label  = recode(activity,
                                    "t_shop_ttl"             = "Total shopping time",
                                    "t_shop_groceries"       = "Groceries",
                                    "t_shop_gas"             = "Gas",
                                    "t_shop_food"            = "Food",
                                    # "t_shop_ggf"             = "Groceries, gas, food",
                                    "t_shop_other"           = "Other",
                                    "t_shop_travel"          = "Travel",
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
               factor(levels = c("Groceries", "Gas", "Food", "Other", "Travel",
                                 "Leisure: TV", "Leisure: Other", "Leisure: Total", "Sleeping",
                                 "Work", "Childcare", "Preparing meal", "Home production",
                                 "Eating", "Personal care", "Own medical care", "Taking care of others"),
                      ordered = TRUE)) %>%
    ungroup() %>%
    select(activity_number, activity_label, activity, lm_spc_number, lm_spc_label, lm_coefs) %>%
    unnest() %>%
    # filter(str_sub(term, 1, 6) == "faminc" | term == "(Intercept)") %>%
    # group_by(lm_spc_number, activity) %>%
    # mutate(estimate = if_else(term != "(Intercept)", estimate + estimate[term == "(Intercept)"], estimate),
    #        conf_low = conf_low + estimate[term == "(Intercept)"],
    #        conf_high = conf_high + estimate[term == "(Intercept)"],
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
    ungroup()

# chart template
g <- ggplot() +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, y = reorder(term, faminc_position)), height = 0.1, alpha = 0.3) +
    # geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, y = reorder(term, faminc_position), height = 0.1, col = "black", alpha = 0.25) +
    geom_point(aes(x = estimate, y = reorder(term, faminc_position)), size = 2, color = "red") +
    geom_vline(aes(xintercept = 0)) +
    # labs(title = "Minutes Spent Shopping per Day",
    labs(title = "Hours Spent Shopping per Week",
         subtitle = "Compared to Families with Income Under 12.5 thousands",
         x = "",
         y = "Family Income (in thousands)") +
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

# plot for total shopping time
p <- g %+% {df_lm_results_coefs_all_toplot %>%
                filter(activity ==  "t_shop_ttl")} +
            scale_x_continuous(limits = c(0, 1.5), sec.axis = dup_axis()) +
            facet_grid(~lm_spc_label, switch = "y")

p

ggsave(filename = str_c(odir_atus, "fig_individual_coefs_t_shop_ttl_slides_", tfst, "_", tlst, ".pdf"),
       plot = p, width = 10, height = 3)

# plot for shopping related activities
p <- g %+% {df_lm_results_coefs_all_toplot %>%
                filter(parse_number(lm_spc_number) %in% c(8)) %>%
                filter(activity %in%  c("t_shop_groceries", "t_shop_gas", "t_shop_food", "t_shop_other", "t_shop_travel"))} +
            facet_wrap( ~ activity_label, ncol = 3, scales = "free")
p

ggsave(filename = str_c(odir_atus, "fig_individual_coefs_t_shop_categories_slides_", tfst, "_", tlst, ".pdf"),
       plot = p, width = 10, height = 5)

# plot for main non-shopping related activities
p <- g %+% {df_lm_results_coefs_all_toplot %>%
                filter(parse_number(lm_spc_number) %in% c(8)) %>%
                filter(activity %in%  c("t_ahk_leisure_tv", "t_ahk_leisure_oth", "t_leisure", "t_ahk_sleep",
                                        "t_work_ttl", "t_ahk_childcare", "t_meals", "t_ahk_homeproduction",
                                        "t_eat", "t_personal_care", "t_ahk_ownmdcare", "t_ahk_othercare"))} +
            facet_wrap( ~ activity_label, ncol = 4, scales = "free")
p

ggsave(filename = str_c(odir_atus, "fig_individual_coefs_t_nonshoping_combined_slides_", tfst, "_", tlst, ".pdf"),
       plot = p, width =  13, height = 6.5)
