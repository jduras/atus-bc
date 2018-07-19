
load(file = paste0(edir_atus, "atus_", tfirst, "_", tlast, "_individual.Rdata"))

# sample size (number of people interviewed) by year
df_sample_size <-
    df_timeuse_all %>%
    group_by(tuyear) %>%
    summarise(obs = length(unique(tucaseid)))

df_sample_size %>%
    ggplot(aes(x = tuyear, y = obs)) +
        geom_point() +
        geom_line()

df_sample_size %>%
    summarise(mean(obs), mean(obs[tuyear != 2003]))

# observations with more than 100 hours in week
df_timeuse_all %>%
    select(tuyear, tucaseid, faminc, weight, starts_with("t_shop")) %>%
    gather(activity, timespent, starts_with("t_shop")) %>%
    filter(timespent > 100) %>%
    mutate(tucaseid = as.character(tucaseid)) %>%
    arrange(desc(timespent)) %>%
    print(n = 50)

# plot average time spent on shopping related activities by income group
df_timeuse_all %>%
    select(faminc, weight, starts_with("t_shop")) %>%
    gather(activity, timespent, -c(faminc, weight)) %>%
    mutate(faminc = faminc %>% as.factor(),
           timespent_gt_0 = if_else(timespent > 0, timespent, NA_real_)) %>%
    group_by(activity, faminc) %>%
    summarise_at(vars(timespent, timespent_gt_0),
                 funs(wtd.mean(., w = weight, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(activity = str_replace(activity, "t_shop_", ""),
           activity_f = factor(activity,
                               levels = c("groceries", "gas", "food", "ggf", "other", "travel", "ttl",
                                          "other_shop", "other_research", "other_wait"),
                               labels = c("Groceries", "Gas", "Food (ex. groceries)", "Groceries, gas, food", "Other", "Travel", "Total",
                                          "Other: shopping", "Other: research", "Other: waiting"))) %>%
    gather(measure, value, c(timespent, timespent_gt_0)) %>%
    ggplot(aes(x = faminc, y = value, col = measure)) +
        geom_point() +
        labs(x = "Household Income Group", y = "Time (hours per week)") +
        facet_wrap(~ activity_f, nrow = 1)

# summary statistics for time spent on different shopping related activities
df_timeuse_all %>%
    select(t_shop_groceries, t_shop_gas, t_shop_food, t_shop_other, t_shop_travel, t_shop_ttl, weight) %>%
    gather(activity, timespent, starts_with("t_")) %>%
    mutate(activity = str_replace(activity, "t_shop_", "")) %>%
    group_by(activity) %>%
    summarise_at(vars(timespent),
                 funs(min,
                      q25 = wtd.quantile(., weights = weight, probs = 0.25, na.rm = TRUE),
                      q50 = wtd.quantile(., weights = weight, probs = 0.50, na.rm = TRUE),
                      q75 = wtd.quantile(., weights = weight, probs = 0.75, na.rm = TRUE),
                      max,
                      avg = questionr::wtd.mean(., weights = weight, na.rm = TRUE))) %>%
    mutate(activity_f = factor(activity,
                               levels = c("groceries", "gas", "food", "other", "travel", "ttl"),
                               labels = c("Groceries", "Gas", "Food (excluding groceries)", "Other", "Travel", "Total"))) %>%
    select(activity_f, everything()) %>%
    arrange(activity_f)

df_timeuse_all %>%
    select(t_shop_groceries, t_shop_gas, t_shop_food, t_shop_other, t_shop_travel, t_shop_ttl) %>%
    gather(activity, timespent) %>%
    ggplot(aes(x = activity, y = timespent)) +
        geom_violin() +
        scale_y_log10()

# generate aggregates (weighted averages) for 2005-2007, 2008-2010, 2011-2013, 2014-2016, 2017
df_timeuse_all %>%
    select(tuyear, starts_with("t_shop"), weight) %>%
    mutate(tuperiod = case_when(tuyear %in% c(2003:2004) ~ 1,
                                tuyear %in% c(2005:2007) ~ 2,
                                tuyear %in% c(2008:2010) ~ 3,
                                tuyear %in% c(2011:2013) ~ 4,
                                tuyear %in% c(2014:2016) ~ 5,
                                tuyear %in% c(2017)      ~ 6) %>%
               factor(labels = c("2003-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017"))) %>%
    gather(activity, timespent, starts_with("t_")) %>%
    group_by(activity, tuperiod) %>%
    summarise_at(vars(timespent), funs(questionr::wtd.mean(., weights  = weight, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(activity = str_replace(activity, "t_shop_", ""),
           activity_f = factor(activity,
                               levels = c("groceries", "gas", "food", "other", "travel", "ttl",
                                          "ggf", "other_shop", "other_research", "other_wait"),
                               labels = c("Groceries", "Gas", "Food (excluding groceries)", "Other", "Travel", "Total",
                                          "Groceries, gas, food", "Other: shopping", "Other: research", "Other: waiting"))) %>%
    spread(tuperiod, timespent) %>%
    arrange(activity_f)

# generate aggregates (weighted averages) for 2005-2007, 2008-2010, 2011-2013, 2014-2016 by state
df_agg.byperiod.bystate <-
    df_timeuse_all %>%
    select(tuyear, gestfips, starts_with("t_shop"), weight) %>%
    mutate(tuperiod = case_when(tuyear %in% c(2003:2004) ~ 1,
                                tuyear %in% c(2005:2007) ~ 2,
                                tuyear %in% c(2008:2010) ~ 3,
                                tuyear %in% c(2011:2013) ~ 4,
                                tuyear %in% c(2014:2016) ~ 5,
                                tuyear %in% c(2017)      ~ 6) %>%
               factor(labels = c("2003-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017"))) %>%
    gather(activity, timespent, starts_with("t_")) %>%
    group_by(activity, tuperiod, gestfips) %>%
    summarise_at(vars(timespent), funs(wtd.mean(., w = weight, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(activity = str_replace(activity, "t_shop_", ""),
           activity_f = factor(activity,
                               levels = c("groceries", "gas", "food", "other", "travel", "ttl",
                                          "ggf", "other_shop", "other_research", "other_wait"),
                               labels = c("Groceries", "Gas", "Food (excluding groceries)", "Other", "Travel", "Total",
                                          "Groceries, gas, food", "Other: shopping", "Other: research", "Other: waiting")))

# this is an analysis of the second approach in Petrosky-Nadeau, Wasmer and Zend (2016) EL:
# generate, tabulate and plot state level changes  2005-2007 vs 2008-2010 and 2011-2013 vs 2008-2010
# if state level 3 year averages of time use data are countercyclical or procyclical
# and if there is no strong ttrend than these changes should be either both negative or both positive
# but this does not seem to be the case, as for most state the sign flips
g <- df_agg.byperiod.bystate %>%
    filter(str_sub(activity, 1, 6) != "other_") %>%
    filter(activity != "t_shop_ggf") %>%
    filter(tuperiod %in% c("2005-2007", "2008-2010", "2011-2013")) %>%
    select(-activity) %>%
    mutate(activity_f = factor(activity_f),
           tuperiod = factor(tuperiod)) %>%
    spread(tuperiod, timespent) %>%
    mutate(d.20082010_vs_20052007 = `2008-2010` - `2005-2007`,
           d.20082010_vs_20112013 = `2008-2010` - `2011-2013`) %>%
    mutate(changes1 = case_when((d.20082010_vs_20052007 < 0  & d.20082010_vs_20112013 < 0)  ~ "nn",
                                (d.20082010_vs_20052007 < 0  & d.20082010_vs_20112013 >= 0) ~ "np",
                                (d.20082010_vs_20052007 >= 0 & d.20082010_vs_20112013 < 0)  ~ "pn",
                                (d.20082010_vs_20052007 >= 0 & d.20082010_vs_20112013 >= 0) ~ "pp"),
           changes2 = case_when((d.20082010_vs_20052007 < 0 & d.20082010_vs_20112013 < 0)  ~ "nn",
                                !(d.20082010_vs_20052007 < 0 & d.20082010_vs_20112013 < 0) ~ "non_nn"),
           changes3 = case_when(((d.20082010_vs_20052007 < 0 & d.20082010_vs_20112013 < 0) | (d.20082010_vs_20052007 >= 0 & d.20082010_vs_20112013 >= 0))~ "nn_and_pp",
                                ((d.20082010_vs_20052007 < 0 & d.20082010_vs_20112013 >= 0) | (d.20082010_vs_20052007 >= 0 & d.20082010_vs_20112013 < 0)) ~ "np_and_pn")) %T>%
    {mutate(., tmp=1) %$% print(table(activity_f, changes1))} %T>%
    {mutate(., tmp=1) %$% print(table(activity_f, changes2))} %T>%
    {mutate(., tmp=1) %$% print(table(activity_f, changes3))} %T>%
    select(gestfips, activity_f, d.20082010_vs_20052007, d.20082010_vs_20112013) %>%
    {ggplot(data = .) +
        geom_point(aes(x = d.20082010_vs_20052007, y = d.20082010_vs_20112013), alpha = 0.3) +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        geom_hline(aes(yintercept = 0), linetype = "dotted") +
        labs(x = "difference between 2008-2010 and 2005-2007", y = "difference between 2008-2010 and 2011-2013") +
        facet_wrap(~activity_f, ncol = 2, scales = "free") +
        theme_light() +
        theme(legend.position = "none",
              # strip.background = element_blank(),
              strip.text.x = element_text(color = "black"),
              strip.text.y = element_text(color = "black"))}
g

fig_file <- paste0(odir_atus, "fig_state_3year_diff_t_shop_by_categories.pdf")
# ggsave(filename = fig_file, plot = g, width = 4.25, height = 5.5)
ggsave(filename = fig_file, plot = g, width = 4.675, height = 6.05)


# next, construct the dataset to estimate simple OLS models with a single dummy variable d20082010 to construct average time on activities during
#  1) pre-recession period 2005-2010 vs recession period 2008-2010
#  2) post-recession period 2011-2013 vs recession period 2008-2010
df_timeuse_all_lm <-
    df_timeuse_all %>%
    # Petrosky-Nadeau, Wasmer and Zeng (2016) EL restict the sample to those with age 25-54
    # this causes the 2005-2007 vs 2008-2010 change in the average time spent on groceries for U to become insignificant
    filter(age >= 25 & age < 55) %>%
    mutate(d20082010 = (tuyear %in% c(2008:2010)) %>% as.integer(),
           lfs = case_when(working   == 1 ~ "E",
                           unemp     == 1 ~ "U",
                           student   == 1 ~ "I",
                           retired   == 1 ~ "I",
                           homemaker == 1 ~ "I",
                           disabled  == 1 ~ "I",
                           othernilf == 1 ~ "I",
                           TRUE           ~ NA_character_)) %>%
    select(gestfips, tuyear, d20082010, lfs, starts_with("t_"), -starts_with("t_shop_other_"), weight)

# Petrosky-Nadeau, Wasmer and Zeng (2016) EL restict the sample to those with age 25-54
# this causes the 2005-2007 vs 2008-2010 change in the average time spent on groceries for U to become insignificant
# the main reason appears to be the fact that the sample gets very small
df_timeuse_all %>%
    filter(tuyear %in% c(2005:2010)) %>%
    filter(t_shop_groceries > 0) %>%
    mutate(lfs = case_when(working   == 1 ~ "E",
                           unemp     == 1 ~ "U",
                           student   == 1 ~ "I",
                           retired   == 1 ~ "I",
                           homemaker == 1 ~ "I",
                           disabled  == 1 ~ "I",
                           othernilf == 1 ~ "I",
                           TRUE           ~ NA_character_),
           included = (age >= 25 & age < 55),
           tuperiod = case_when(tuyear %in% c(2005:2007) ~ "2005_2007",
                                tuyear %in% c(2008:2010) ~ "2008_2010")) %$%
    table(included, lfs, tuperiod)

# duplicate the dataset, to get results also for the whole sample, not just split by labor force
df_timeuse_all_lm <-
    bind_rows(df_timeuse_all_lm,
              df_timeuse_all_lm %>%
                  mutate(lfs = "all"))

# split the sample by period and estimate a simple OLS with a single dummy variable d20082010 to construct average time on activities during
df_timeuse_all_lm_results <-
    bind_rows(df_timeuse_all_lm %>%
                  filter(tuyear %in% c(2005:2010)) %>%
                  mutate(tusample = "2005_2010"),
              df_timeuse_all_lm %>%
                  filter(tuyear %in% c(2008:2013)) %>%
                  mutate(tusample = "2008_2013")) %>%
    gather(activity, timespent, starts_with("t_")) %>%
    nest(-tusample, -lfs, -activity) %>%
    mutate(lm_model = map(data, ~lm(timespent ~ d20082010, weight = weight, data = .x)),
           lm_coefs = map(lm_model, tidy, conf.int = TRUE),
           # construct clustered errors
           lm_coefs = pmap(list(lm_coefs, lm_model, data),
                                     ~(..1 %>% mutate(std_error_clustered = coeftest(..2, clustered_se(..2, ..3$gestfips))[, "Std. Error"],
                                                      conf_low_clustered  = estimate - 2*std_error_clustered,
                                                      conf_high_clustered = estimate + 2*std_error_clustered,
                                                      p_value_clustered   = 2*(1 - pnorm(abs(estimate/std_error_clustered))))))) %>%
    select(tusample, lfs, activity, lm_coefs)

# plot estimated coefficient for d20082010
df_timeuse_all_lm_results %>%
    unnest() %>%
    filter(term == "d20082010") %>%
    filter(str_sub(activity, 1, 6) == "t_shop") %>%
    filter(str_sub(activity, 1, 13) != "t_shop_other_") %>%
    mutate(activity = str_replace(activity, "t_shop_", ""),
           activity_f = factor(activity,
                               levels = c("groceries", "gas", "food", "ggf", "other", "travel", "ttl"),
                               labels = c("Groceries", "Gas", "Food (excluding groceries)", "Groceries, gas, food", "Other", "Travel", "Total"))) %>%
    {ggplot(.,aes(x = estimate, y = activity_f)) +
        geom_point() +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.2), col = "red") +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        labs(x = "", y = "") +
        facet_grid(lfs ~ tusample) +
        theme_light() +
        theme(# strip.background = element_blank(),
              strip.text.x = element_text(color = "black"),
              strip.text.y = element_text(color = "black"),
              strip.placement = "outside")}


# generate aggregates (weighted averages) for each year
df_agg <- df_timeuse_all %>%
    mutate(num_child_gt_0 = if_else(num_child > 0, num_child, NA_integer_)) %>%
    select(tuyear, weight,
           male, married, black, age, hv_child, num_child, num_child_gt_0, spouse_emp,
           working, unemp, retired, disabled, student, homemaker,
           starts_with("t_")) %>%
    # add age, education, family income dummies
    bind_cols(df_timeuse_all %>%
                  model.matrix(object =~ age_f-1) %>%
                  as_tibble(),
              df_timeuse_all %>%
                  model.matrix(object = ~educ_f-1) %>%
                  as_tibble(),
              df_timeuse_all %>%
                  model.matrix(object = ~faminc_f_1-1) %>%
                  as_tibble()) %>%
    group_by(tuyear) %>%
    summarise_at(vars(-weight, -tuyear), funs(wtd.mean(., w = weight, na.rm = TRUE)))

df_timeuse_all %>%
    group_by(faminc_f_1, tuyear) %>%
    summarise(nobs = n(), weight = sum(weight)) %>%
    summarise(sum_nobs = sum(nobs), avg_weight = mean(weight))


# generate aggregates (weighted averages) by family income for each year
df_agg_byincome <-
    inner_join(
        # variables summarised using weighted average
        df_timeuse_all %>%
            mutate(num_child_gt_0 = if_else(num_child > 0, num_child, NA_integer_)) %>%
            select(tuyear, faminc_f_1, weight,
                   male, married, black, age, hv_child, num_child, num_child_gt_0, spouse_emp,
                   working, unemp, retired, disabled, student, homemaker,
                   starts_with("t_")) %>%
            # add age, education, family income dummies
            bind_cols(df_timeuse_all %>%
                          model.matrix(object =~ age_f-1) %>%
                          as_tibble(),
                      df_timeuse_all %>%
                          model.matrix(object = ~educ_f-1) %>%
                          as_tibble()) %>%
            group_by(tuyear, faminc_f_1) %>%
            summarise_at(vars(-weight, -tuyear, -faminc_f_1), funs(wtd.mean(., w = weight, na.rm = TRUE))) %>%
            ungroup(),
        # variables summarised using sum
        df_timeuse_all %>%
            select(tuyear, faminc_f_1, weight) %>%
            group_by(tuyear, faminc_f_1) %>%
            summarise(weight = sum(weight)) %>%
            ungroup(),
        by = c("tuyear", "faminc_f_1")) %>%
    arrange(faminc_f_1)



# combine aggregate demographic statistics for whole population and by family income, calculate average for 2003-2016 period
df_agg_stats <-
    full_join(
        # aggregate demographics
        df_agg %>%
            select(-c(tuyear, starts_with("t_"), starts_with("faminc"))) %>%
            summarise_all(mean) %>%
            gather(measure, value),
        # aggregate demographics by family income
        inner_join(
            df_agg %>%
                select(tuyear, starts_with("faminc_f_1")) %>%
                gather(faminc_f_1, share, -tuyear) %>%
                mutate(faminc_f_1 = str_sub(faminc_f_1, 11, -1)),
            df_agg_byincome %>%
                mutate(faminc_f_1 = faminc_f_1 %>% as.character()),
            by = c("tuyear", "faminc_f_1")) %>%
            select(-c(tuyear, weight, starts_with("t_"))) %>%
            group_by(faminc_f_1) %>%
            summarise_all(mean) %>%
            gather(measure, value, -faminc_f_1) %>%
            spread(faminc_f_1, value),
        by = "measure") %>%
    rename(whole_population    = value,
           faminc_0_to_12.5    = `[0,12.5)`,
           faminc_12.5_to_25   = `[12.5,25)`,
           faminc_25_to_50     = `[25,50)`,
           faminc_50_to_75     = `[50,75)`,
           faminc_75_to_100    = `[75,100)`,
           faminc_100_to_150   = `[100,150)`,
           faminc_150_and_over = `[150,Inf)`) %>%
    select(measure, whole_population, faminc_0_to_12.5, faminc_12.5_to_25, faminc_25_to_50,
           faminc_50_to_75, faminc_75_to_100, faminc_100_to_150, faminc_150_and_over)

# format the above into a nicer table
df_agg_stats %>%
    mutate(row.number = case_when(measure == "share"          ~ 1,
                                  measure == "male"           ~ 2,
                                  measure == "black"          ~ 3,
                                  measure == "age"            ~ 4,
                                  measure == "age_f(0,17]"    ~ 5,
                                  measure == "age_f(17,27]"   ~ 6,
                                  measure == "age_f(27,37]"   ~ 7,
                                  measure == "age_f(37,47]"   ~ 8,
                                  measure == "age_f(47,57]"   ~ 9,
                                  measure == "age_f(57,65]"   ~ 10,
                                  measure == "age_f(65,Inf]"  ~ 11,
                                  measure == "educ_f[0,11]"   ~ 12,
                                  measure == "educ_f(11,12]"  ~ 13,
                                  measure == "educ_f(12,15]"  ~ 14,
                                  measure == "educ_f(15,Inf]" ~ 15,
                                  measure == "married"        ~ 16,
                                  measure == "spouse_emp"     ~ 17,
                                  measure == "hv_child"       ~ 18,
                                  measure == "num_child"      ~ 19,
                                  measure == "num_child_gt_0" ~ 20,
                                  measure == "working"        ~ 21,
                                  measure == "unemp"          ~ 22,
                                  measure == "student"        ~ 23,
                                  measure == "retired"        ~ 24,
                                  measure == "homemaker"      ~ 25,
                                  measure == "disabled"       ~ 26) %>% as.integer()) %>%
    arrange(row.number) %>%
    mutate_if(is.numeric, funs(sprintf(fmt = "%8.4f", .))) %>%
    select(-row.number) %>%
    filter(!(measure %in% c("age.f(0,17]", "age.f(65,Inf]"))) %>%
    print(n = 50)



# plot average time spent on various activities by year
g <- df_agg %>%
    select(tuyear, starts_with("t_shop")) %>%
    select(-starts_with("t_shop_other_")) %>%
    select(-t_shop_ggf) %>%
    # select(-t_shop_food, -t_shop_groceries) %>%
    # select(tuyear, t_shop_ttl) %>%
    gather(activity, value, -tuyear) %>%
    mutate(activity = str_replace(activity, "t_shop_", ""),
           activity_f = factor(activity,
                               levels = c("groceries", "gas", "food", "other", "travel", "ttl",
                                          "ggf", "other_shop", "other_research", "other_wait"),
                               labels = c("Groceries", "Gas", "Food (excluding groceries)", "Other", "Travel", "Total",
                                          "Groceries, gas, food", "Other: shopping", "Other: research", "Other: waiting"))) %>%
    group_by(activity) %>%
    mutate(value = 100* value/value[tuyear == "2003"]) %>%
    ungroup() %>%
    {ggplot(data = .) +
            geom_line(aes(x = tuyear, y = value), size = 1.02) +
            geom_rect(data = tibble(Start = 2008, End = 2010), aes(xmin = 2008, xmax = 2010, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
            geom_hline(aes(yintercept = 100), linetype = "dotted") +
            labs(# title = "Time Use",
                 x = "Year", y = "Index (2003 = 100)",
                 col = "Shopping Time") +
            facet_wrap(~activity_f, ncol = 2) +
            theme_light() +
            theme(legend.position = "none",
                  # strip.background = element_blank(),
                  strip.text.x = element_text(color = "black"),
                  strip.text.y = element_text(color = "black"))}
g

fig_file <- paste0(odir_atus, "fig_agg_t_shop_by_categories.pdf")
# ggsave(filename = fig_file, plot = g, width = 4.25, height = 5.5)
ggsave(filename = fig_file, plot = g, width = 4.675, height = 6.05)


# shares: groups by family income over time
df_agg %>%
    select(tuyear, starts_with("faminc")) %>%
    filter(tuyear > 2003) %>%
    gather(measure, value, -tuyear) %>%
    group_by(tuyear) %>%
    mutate(inc_grp_num = row_number()) %>%
    {ggplot(., aes(x = tuyear, y = value, col = reorder(measure, inc_grp_num))) +
        geom_line() +
        # scale_colour_discrete(labels = c("0 to 12.5", "12.5 to 25", "25 to 50", "50 to 75", "75 to 100", "100 to 150", "150 and over")) +
        scale_colour_manual(labels = c("0 to 12.5", "12.5 to 25", "25 to 50", "50 to 75", "75 to 100", "100 to 150", "150 and over"),
                            values = colorRampPalette(brewer.pal(9,"Blues"))(9)[3:9]) +
        labs(title = "Population shares by family income",
             x = "Year", y = "Population share",
             col = "Family Income")}
