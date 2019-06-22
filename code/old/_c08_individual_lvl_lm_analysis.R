
# file to estimate individual level regressions - time spent on shopping related activities

# ATUS data: time spent on varios activities
load(file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_individual.Rdata"))
# CPS data: unemployment rate
load(file = str_c(edir_atus, "ux_", tfst, "_", tlst, "_state.Rdata"))
# NIPA data: real GDP and personal income
load(file = str_c(edir_atus, "ni_", tfst, "_", tlst, "_state.Rdata"))


df_umcsent_q <-
    tq_get("UMCSENT", get = "economic.data", from = str_c(tfst,"-01-01"), to = str_c(tlst,"-12-01")) %>%
    rename(umcsent = price) %>%
    mutate(tuyear = year(date),
           tuquarter = quarter(date)) %>%
    select(-date) %>%
    group_by(tuyear, tuquarter) %>%
    summarise_at(vars(umcsent), funs(mean)) %>%
    ungroup()

df_umcsent_a <-
    df_umcsent_q %>%
    group_by(tuyear) %>%
    summarise_at(vars(umcsent), funs(mean)) %>%
    ungroup()



# fraction of individuals with non-zero time spent on activities
df_timeuse_all %>%
    select(faminc_f_1, tudiaryday, weight, starts_with("t_")) %>%
    mutate_at(vars(starts_with("t_")), funs(as.numeric(. > 0))) %>%
    group_by(faminc_f_1) %>%
    summarise_at(vars(-weight, -faminc_f_1), funs(wtd.mean(., w = weight, na.rm = TRUE))) %>%
    gather(measure, value, -faminc_f_1) %>%
    spread(faminc_f_1, value) %>%
    print(n = 50)

# observations with extremely large amount of time spent shopping
df_timeuse_all %>%
    # select(starts_with("t_shop"), -starts_with("t_shop_other_"), -t_shop_ggf)
    select(t_ahk_sleep, t_shop_groceries, t_shop_gas, t_shop_food, t_shop_other, t_shop_travel, t_shop_ttl) %>%
    filter(t_shop_ttl > 100) %>%
    arrange(t_ahk_sleep)

df_timeuse_all %>%
    select(starts_with("t_shop_")) %>%
    # select(t_shop_ttl) %>%
    gather(activity, timespent) %>%
    ggplot(aes(x = activity, y = timespent)) +
        # geom_boxplot() +
        geom_violin(trim = FALSE) +
        # geom_jitter(alpha = .05, width = 0.1) +
        stat_summary(fun.y = "median", colour = "red", size = 2, geom = "point") +
        scale_y_log10(breaks = c(0.1, 1, 10, 100))
        # coord_flip()


### Regressions ####

# assemble dataset for OLS models
df_lm <-
    df_timeuse_all %>%
    inner_join(df_ni_states_q, by = c("tuyear", "tuquarter", "gestfips")) %>%
    inner_join(df_ux_states_q, by = c("tuyear", "tuquarter", "gestfips")) %>%
    # inner_join(df_umcsent_q, by=c("tuyear","tuquarter")) %>%
    select(-c(tuquarter, tumonth, tudiarydate, gereg, tucaseid, age, num_child, grade, spouse_workhours, udur, working, faminc,
              twoyear, threeyear, drec20082009, drec20082010, pop, rGDP, gr_rGDPpc, hp_lrGDPpc)) %>%
    # filter(tuyear > 2003) %>%
    mutate(spouse_emp = replace(spouse_emp, is.na(spouse_emp), 0)) %>%
    rename(demo_male = male,
           demo_black = black,
           demo_married = married,
           demo_hv_child = hv_child,
           demo_spouse_emp = spouse_emp,
           demo_age_f = age_f,
           demo_educ_f = educ_f,
           lfs_unemp = unemp,
           lfs_retired = retired,
           lfs_homemaker = homemaker,
           lfs_student = student,
           lfs_disabled = disabled,
           lfs_othernilf = othernilf) %>%
    mutate(demo_female = 1L - demo_male,
           demo_age_f = demo_age_f %>% relevel(ref = "(47,57]"),
           demo_educ_f = demo_educ_f %>% relevel(ref = "(11,12]"))

rm(df_rGDP_states_a_raw, df_rGDP_states_q_raw,
   df_rGDPpc_states_a_raw,
   df_ni_states_a, df_ni_states_q,
   df_ux_states_a, df_ux_states_q,
   df_timeuse_all)

# OLS model specifications (RHSs of the regressions)
lm_spc <- vector("character")
lm_spc <- c(lm_spc, "faminc_f_1")
lm_spc <- c(lm_spc, "faminc_f_1 + factor(tudiaryday)")
lm_spc <- c(lm_spc, "faminc_f_1 + demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f + factor(tudiaryday)")
lm_spc <- c(lm_spc, "faminc_f_1 + demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f + lfs_unemp+lfs_retired+lfs_homemaker+lfs_student + factor(tudiaryday)")
lm_spc <- c(lm_spc, "faminc_f_1 + demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f + lfs_unemp+lfs_retired+lfs_homemaker+lfs_student + factor(tuyear) + factor(tudiaryday)")
lm_spc <- c(lm_spc, "faminc_f_1 + demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f + lfs_unemp+lfs_retired+lfs_homemaker+lfs_student + factor(gestfips) + factor(tudiaryday)")
lm_spc <- c(lm_spc, "faminc_f_1 + demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f + lfs_unemp+lfs_retired+lfs_homemaker+lfs_student + factor(gestfips) + factor(tuyear) + factor(tudiaryday)")
lm_spc <- c(lm_spc, "faminc_f_1 + demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f + lfs_unemp+lfs_retired+lfs_homemaker+lfs_student + factor(gestfips) + factor(tuyear) + factor(tudiaryday) + u3rate")
lm_spc <- c(lm_spc, "faminc_f_1 + demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f + lfs_unemp+lfs_retired+lfs_homemaker+lfs_student + factor(gestfips) + factor(tuyear) + factor(tudiaryday) + u4rate")
lm_spc <- c(lm_spc, "faminc_f_1 + demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f + lfs_unemp+lfs_retired+lfs_homemaker+lfs_student + factor(gestfips) + factor(tuyear) + factor(tudiaryday) + u5rate")
lm_spc <- c(lm_spc, "faminc_f_1 + demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f + lfs_unemp+lfs_retired+lfs_homemaker+lfs_student + factor(gestfips) + factor(tuyear) + factor(tudiaryday) + u6rate")

# to do: try to simplify this alternative way to set up specifications
# tibble(spc1 = c("faminc_f_1",
#                 "factor(tudiaryday)",
#                 "demo_female+demo_black+demo_married+demo_hv_child+demo_spouse_emp + demo_age_f + demo_educ_f",
#                 "lfs_unemp+lfs_retired+lfs_homemaker+lfs_student",
#                 rep("", 7)),
#        spc2 = c(rep("", 4),
#                 "factor(tuyear)",
#                 "factor(gestfips)",
#                 rep("factor(gestfips) + factor(tuyear)", 5)),
#        spc3 = c(rep("", 7), "u3rate", "u4rate", "u5rate", "u6rate")) %>%
#     mutate(lm_spc = accumulate(spc1, ~if_else(.y == "", .x, str_c(.x, .y, sep = " + "))),
#            lm_spc = if_else(spc2 == "", lm_spc, str_c(lm_spc, spc2, sep = " + ")),
#            lm_spc = if_else(spc3 == "", lm_spc, str_c(lm_spc, spc3, sep = " + "))) %>%
#     select(lm_spc)

# OLS model specifications (RHSs of the regressions) and names of dependent variables (LHSs of the regressions)
df_spc <-
    crossing(tibble(lm_spc) %>%
                 mutate(lm_spc_number = paste("Model", row_number())),
             tibble(activity = c("t_shop_groceries", "t_shop_gas", "t_shop_food", "t_shop_ggf",
                                 "t_shop_other",
                                 # "t_shop_other_shop", "t_shop_other_research", "t_shop_other_wait",
                                 "t_shop_travel", "t_shop_ttl",
                                 "t_ahk_work", "t_ahk_work_search", "t_work_ttl",
                                 "t_ahk_leisure_tv", "t_ahk_leisure_oth", "t_leisure",
                                 "t_eat", "t_ahk_sleep", "t_personal_care", "t_ahk_esp",
                                 "t_ahk_ownmdcare", "t_ahk_othercare", "t_ahk_childcare", "t_childcare_play",
                                 "t_meals", "t_housework", "t_home_car_maintenance", "t_ahk_homeproduction", "t_nonmarket_work_ttl",
                                 "t_ahk_education", "t_ahk_civic", "t_other")) %>%
                 mutate(activity_number = row_number(),
                        activity_label  = recode(activity, "t_shop_groceries"       = "Groceries",
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
                                                           "t_ahk_work"             = "Work",
                                                           "t_ahk_work_search"      = "Work: Search",
                                                           "t_work_ttl"             = "Work: Total",
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
                                                           "t_ahk_education"        = "Education",
                                                           "t_ahk_civic"            = "Civic",
                                                           "t_other"                = "Other activities",
                                                           "t_ttl"                  = "Total time"),
                        activity_label = factor(activity_label, levels = activity_label))) %>%
    select(lm_spc_number, lm_spc, activity_number, activity, activity_label)


# estimate individual level regressions: time spent on various activities
df_lm_results_all <-
    df_spc %>%
    mutate(lm_model = map2(lm_spc, activity, ~lm(formula(paste(.y, "~", .x)),
                                                 weight = weight,
                                                 data = df_lm)),
           lm_coefs = map(lm_model, tidy, conf.int = TRUE),
           lm_coefs = map2(lm_model, lm_coefs,
                           ~(.y %>% mutate(std_error_clustered = coeftest(.x, clustered_se(.x, df_lm$gestfips))[,"Std. Error"],
                                           conf_low_clustered  = estimate - 2*std_error_clustered,
                                           conf_high_clustered = estimate + 2*std_error_clustered))))

save(df_lm_results_all, file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_individual_lm_all.Rdata"))
