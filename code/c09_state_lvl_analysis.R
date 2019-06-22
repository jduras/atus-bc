
# ATUS data: time spent on varios activities
load(file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_individual.Rdata"))
# CPS data: unemployment rate
load(file = str_c(edir_atus, "ux_", tfst, "_", tlst, "_state.Rdata"))
# NIPA data: real GDP and personal income
load(file = str_c(edir_atus, "ni_", tfst, "_", tlst, "_state.Rdata"))



#### aggregates (weighted averages) for 2005-2007, 2008-2010, 2011-2013, 2014-2016, 2017-2018 by state ####

df_agg_byperiod_bystate <-
    df_timeuse_all %>%
    select(tuyear, gestfips, starts_with("t_shop"), weight) %>%
    mutate(tuperiod = case_when(tuyear %in% 2003:2004 ~ 1,
                                tuyear %in% 2005:2007 ~ 2,
                                tuyear %in% 2008:2010 ~ 3,
                                tuyear %in% 2011:2013 ~ 4,
                                tuyear %in% 2014:2016 ~ 5,
                                tuyear %in% 2017:2018 ~ 6) %>%
               factor(labels = c("2003-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2018"))) %>%
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
# and if there is no strong trend than these changes should be either both negative or both positive
# this does not seem to be the case, as for large number of states the sign flips (especially for total time)
g <- df_agg_byperiod_bystate %>%
    filter(str_sub(activity, 1, 6) != "other_") %>%
    filter(activity != "t_shop_ggf") %>%
    filter(tuperiod %in% c("2005-2007", "2008-2010", "2011-2013")) %>%
    select(-activity) %>%
    mutate(activity_f = factor(activity_f),
           tuperiod = factor(tuperiod)) %>%
    spread(tuperiod, timespent) %>%
    mutate(d_20082010_vs_20052007 = `2008-2010` - `2005-2007`,
           d_20082010_vs_20112013 = `2008-2010` - `2011-2013`) %>%
    mutate(changes1 = case_when((d_20082010_vs_20052007 < 0  & d_20082010_vs_20112013 < 0)  ~ "nn",
                                (d_20082010_vs_20052007 < 0  & d_20082010_vs_20112013 >= 0) ~ "np",
                                (d_20082010_vs_20052007 >= 0 & d_20082010_vs_20112013 < 0)  ~ "pn",
                                (d_20082010_vs_20052007 >= 0 & d_20082010_vs_20112013 >= 0) ~ "pp"),
           changes2 = case_when((d_20082010_vs_20052007 < 0 & d_20082010_vs_20112013 < 0)  ~ "nn",
                                !(d_20082010_vs_20052007 < 0 & d_20082010_vs_20112013 < 0) ~ "non_nn"),
           changes3 = case_when(((d_20082010_vs_20052007 < 0 & d_20082010_vs_20112013 < 0) | (d_20082010_vs_20052007 >= 0 & d_20082010_vs_20112013 >= 0))~ "nn_and_pp",
                                ((d_20082010_vs_20052007 < 0 & d_20082010_vs_20112013 >= 0) | (d_20082010_vs_20052007 >= 0 & d_20082010_vs_20112013 < 0)) ~ "np_and_pn")) %T>%
    {mutate(., tmp=1) %$% print(table(activity_f, changes1))} %T>%
    {mutate(., tmp=1) %$% print(table(activity_f, changes2))} %T>%
    {mutate(., tmp=1) %$% print(table(activity_f, changes3))} %T>%
    select(gestfips, activity_f, d_20082010_vs_20052007, d_20082010_vs_20112013) %>%
    {ggplot(data = .) +
            geom_point(aes(x = d_20082010_vs_20052007, y = d_20082010_vs_20112013), alpha = 0.3) +
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

fig_file <- str_c(odir_atus, "fig_state_3year_diff_t_shop_by_categories.pdf")
# ggsave(filename = fig_file, plot = g, width = 4.25, height = 5.5)
ggsave(filename = fig_file, plot = g, width = 4.675, height = 6.05)



df_timeuse_all_a <-
    df_timeuse_all %>%
    inner_join(df_ni_states_a, by = c("tuyear", "gestfips")) %>%
    inner_join(df_ux_states_a, by = c("tuyear", "gestfips"))

df_timeuse_all_q <-
    df_timeuse_all %>%
    inner_join(df_ni_states_q, by = c("tuyear", "tuquarter", "gestfips")) %>%
    inner_join(df_ux_states_q, by = c("tuyear", "tuquarter", "gestfips"))

rm(df_timeuse_all)

# save the resulting state level dataset
save(df_timeuse_all_a, df_timeuse_all_q, file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_state.Rdata"))


# load(file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_state.Rdata"))



#### state level aggregates (weighted averages) ####

df_agg_states_a <-
    inner_join(# variables summarized using weighted average
        df_timeuse_all_a %>%
            select(gestfips, tuyear, weight,
                   male, married, black, hv_child, num_child, spouse_emp, grade,
                   working, unemp, retired, disabled, student, homemaker,
                   pop, u3rate, u4rate, u5rate, u6rate, rPIpc, rGDPpc, gr_rGDPpc, hp_lrGDPpc,
                   starts_with("t_")) %>%
            # add age and education dummies
            bind_cols(df_timeuse_all_a %>%
                          model.matrix(object = ~ age_f-1) %>%
                          as_tibble(),
                      df_timeuse_all_a %>%
                          model.matrix(object = ~ educ_f-1) %>%
                          as_tibble()) %>%
            group_by(gestfips, tuyear) %>%
            summarise_at(vars(-weight), list(~wtd.mean(., w = weight, na.rm = TRUE))) %>%
            ungroup(),
        # variables summarized using sum
        df_timeuse_all_a %>%
            select(gestfips, tuyear, weight) %>%
            group_by(gestfips, tuyear) %>%
            summarise(weight = sum(weight) / 200) %>%
            ungroup(),
        by = c("gestfips", "tuyear")) %>%
    inner_join(df_state_codes, by = c("gestfips"))

# compare total CPS weights by state and state population used in NIPA data
df_agg_states_a %>%
    select(tuyear, gestfips, state_code, pop, weight) %>%
    gather(measure, value, -tuyear, -gestfips, -state_code) %>%
    ggplot(aes(x = tuyear, y = value, col = measure)) +
        geom_line() +
        facet_wrap(~ state_code)

df_agg_states_q <-
    inner_join(# variables summarizes using weighted average
        df_timeuse_all_q %>%
            select(gestfips, tuyear, tuquarter, weight,
                   male, married, black, hv_child, num_child, spouse_emp, grade,
                   working, unemp, retired, disabled, student, homemaker,
                   pop, u3rate, u4rate, u5rate, u6rate, rGDPpc,gr_rGDPpc, hp_lrGDPpc,
                   starts_with("t.")) %>%
            # add age and education dummies
            bind_cols(df_timeuse_all_q %>%
                          model.matrix(object =~ age_f-1) %>%
                          as.tibble(),
                      df_timeuse_all_q %>%
                          model.matrix(object =~ educ_f-1) %>%
                          as.tibble()) %>%
            group_by(gestfips, tuyear, tuquarter) %>%
            summarise_at(vars(-weight), list(~wtd.mean(., w = weight, na.rm = TRUE))) %>%
            ungroup(),
        # variables summarized using sum
        df_timeuse_all_q %>%
            select(gestfips,tuyear,tuquarter,weight) %>%
            group_by(gestfips, tuyear, tuquarter) %>%
            summarise(weight = sum(weight)/100) %>%
            ungroup(),
        by = c("gestfips", "tuyear", "tuquarter")) %>%
    inner_join(df_state_codes, by=c("gestfips"))


# save the resulting state level dataset
save(df_agg_states_a, df_agg_states_q, file = str_c(edir_atus, "df_agg_by_state_", tfst, "_", tlst, ".Rdata"))


# control for demographics

# form <- paste("t_shop_ ~", paste(select_vars(names(df_agg_states_a), starts_with("age_f")), collapse="+")) %>% as.formula()

df_agg_states_a_demo <-
    df_agg_states_a %>%
    mutate_at(vars(starts_with("t_shop_")),
              funs(lm(. ~ -1 + male + black +
                          `age_f(0,17]` + `age_f(17,27]` + `age_f(27,37]` + `age_f(37,47]` + `age_f(47,57]` + `age_f(57,65]` + `age_f(65,Inf]` +
                          `educ_f[0,11]` + `educ_f(11,12]` + `educ_f(12,15]` + `educ_f(15,Inf]` +
                          married + hv_child + disabled, weights = weight) %>%
                       residuals()))

df_agg_states_q_demo <-
    df_agg_states_q %>%
    mutate_at(vars(starts_with("t_shop_")),
              funs(lm(. ~ -1 + male + black+
                          `age_f(0,17]` + `age_f(17,27]` + `age_f(27,37]` + `age_f(37,47]` + `age_f(47,57]` + `age_f(57,65]` + `age_f(65,Inf]` +
                          `educ_f[0,11]` + `educ_f(11,12]` + `educ_f(12,15]` + `educ_f(15,Inf]` +
                          married + hv_child + disabled, weights = weight) %>%
                       residuals()))

# distribution of total shopping time by state, over time
ggplot(df_agg_states_a, aes(x = factor(tuyear), y = t_shop_ttl)) +
    # geom_violin(draw_quantiles = c(0.25,0.5,0.75), trim=FALSE) +
    geom_violin(draw_quantiles = 0.5, trim = FALSE) +
    geom_jitter(width = 0.15, alpha = 0.20)



#### plots ####

# plot real GDP per capita against real personal income per capita - D.C. looks like an outlier
bind_rows(df_agg_states_a %>%
              mutate(keep_DC = 1),
          df_agg_states_a %>%
              filter(state_code != "DC") %>%
              mutate(keep_DC = 0)) %>%
    mutate(is_DC = state_code == "DC") %>%
    ggplot(aes(x = log(rGDPpc), y = log(rPIpc))) +
        geom_point(aes(size = weight, col = is_DC), alpha = 0.2) +
        geom_smooth(method = "lm", aes(weight = weight)) +
        # geom_smooth(method = "lm", col="red") +
        labs(x = "log of real GDP per capita", y = "log of real personal income per capita") +
        facet_wrap(~keep_DC, ncol = 1, labeller = label_both) +
        theme_minimal() +
        theme(strip.text.x = element_text(color = "black", face = "bold", hjust = 0))


# scatter plots for measures of business cycle
df_agg_states_a %>%
    filter(state_code != "DC") %>%
    select(t_shop_ttl, u3rate, u4rate, u5rate, u6rate, rGDPpc, pop, weight) %>%
    gather(measure, value, -c(t_shop_ttl, pop, weight)) %>%
    mutate(measure = factor(measure, levels = c("u3rate", "u4rate", "u5rate", "u6rate", "rGDPpc"))) %>%
    ggplot(aes(x = value, y = t_shop_ttl)) +
        geom_point(aes(size = weight), alpha = 0.2) +
        geom_smooth(method = "lm", aes(weight = weight)) +
        # geom_smooth(method = "lm", col = "red") +
        labs(x = "", y = "Average total shopping time") +
        facet_wrap(~ measure, scales = "free", ncol = 4) +
        theme_light() +
        theme(legend.position = "none",
              strip.text.x = element_text(color = "black"),
              strip.background = element_rect(fill = "lightgray"))



#### state level regressions - time spent on shopping related activities on business cycle measure ####

# using:
#  unemployment rate U3,U4,U5 or U6
#  real GDP per capita
#  real personal income per capita

df_agg_states_a %>%
    filter(state_code != "DC") %>%
    mutate(lrGDPpc = log(rGDPpc),
           lrPIpc = log(rPIpc)) %>%
    # select(t_shop_ttl, u3rate, u4rate, u5rate, u6rate, lrGDPpc, lrPIpc, pop, weight) %>%
    select(t_shop_ttl, u3rate, lrGDPpc, pop, weight) %>%
    gather(measure, value, -c(t_shop_ttl, pop, weight)) %>%
    ggplot(aes(x = value, y = t_shop_ttl)) +
        geom_point(alpha = 0.2, aes(size = weight)) +
        geom_smooth(method = "lm", aes(weight = weight)) +
        labs(x = "", y = "Average total shopping time") +
        facet_wrap(~ measure, scales = "free", ncol = 2) +
        theme_light() +
        theme(legend.position = "none",
              strip.text.x = element_text(color = "black"),
              strip.background = element_rect(fill = "lightgray"))

spc_lm_state <- c("log(rGDPpc)",
                  "log(rGDPpc) + tuyear",
                  "log(rGDPpc) + factor(tuyear)",
                  "log(rGDPpc) + tuyear + factor(gestfips)",
                  "log(rGDPpc) + factor(gestfips) + tuyear:factor(gestfips)")

# spc_lm_state <- c(spc_lm_state,
#                   "log(rPIpc)",
#                   "log(rPIpc) + tuyear",
#                   "log(rPIpc) + factor(tuyear)",
#                   "log(rPIpc) + tuyear + factor(gestfips)",
#                   "log(rPIpc) + factor(gestfips) + tuyear:factor(gestfips)")

spc_lm_state <- c(spc_lm_state,
                  "u3rate",
                  "u3rate + tuyear",
                  "u3rate + factor(tuyear)",
                  "u3rate + tuyear + factor(gestfips)",
                  "u3rate + factor(gestfips) + tuyear:factor(gestfips)")

spc_lm_state <- c(spc_lm_state,
                  "u4rate",
                  "u4rate + tuyear",
                  "u4rate + factor(tuyear)",
                  "u4rate + tuyear + factor(gestfips)",
                  "u4rate + factor(gestfips) + tuyear:factor(gestfips)")

spc_lm_state <- c(spc_lm_state,
                  "u5rate",
                  "u5rate + tuyear",
                  "u5rate + factor(tuyear)",
                  "u5rate + tuyear + factor(gestfips)",
                  "u5rate + factor(gestfips) + tuyear:factor(gestfips)")

spc_lm_state <- c(spc_lm_state,
                  "u6rate",
                  "u6rate + tuyear",
                  "u6rate + factor(tuyear)",
                  "u6rate + tuyear + factor(gestfips)",
                  "u6rate + factor(gestfips) + tuyear:factor(gestfips)")

# spc_lm_state <- c(spc_lm_state,
#                   "log(rGDPpc) + unrate",
#                   "log(rGDPpc) + unrate + tuyear",
#                   "log(rGDPpc) + unrate + tuyear * factor(gestfips)")

# estimate individual level regressions - time spent on shopping related activities
df_lm <-
    df_agg_states_a %>%
    # df_agg_states_q %>%
    # filter(tuyear %in% 2008:2017) %>%
    filter(state_code != "DC") %>%
    mutate(unrate = u3rate,
           tuyear = tuyear)

shopping_category <- "ttl"

# estimate individual level regressions - time spent on shopping related activities
df_lm_results <-
    spc_lm_state %>%
    tibble(spec = .) %>%
    mutate(# estimate individual level regressions - time spent on shopping related activities
        lm_model = map(spec, ~lm(formula(str_c("t_shop_", shopping_category, " ~ ", .x)),
                                  weight = weight,
                                 data = df_lm)),
        lm_coefs = map(lm_model, . %>% tidy(conf.int = TRUE) %>% clean_names()),
        # construct clustered errors
        lm_coefs = map2(lm_model, lm_coefs,
                        ~(.y %>% mutate(std_error_clustered = coeftest(.x, clustered_se(.x, df_lm$gestfips))[,"Std. Error"],
                                        conf_low_clustered = estimate - 2*std_error_clustered,
                                        conf_high_clustered = estimate + 2*std_error_clustered))))

# report results using stargazer
df_lm_results %$%
    stargazer(lm_model, type = "text", out = str_c(odir_atus, "results_state_a_", shopping_category, ".tex"),
               column.sep.width = "0pt",
               title = "State level regressions",
               se = lm_coefs %>%
                  map(~ .x %$% std_error_clustered),
               omit = c("^tuyear", "^factor\\(tuyear", "^factor\\(gestfips", "^tuyear:factor\\(gestfips"),
               omit.labels = c("common time trend", "time fixed effects", "state fixed effects", "state specific time trends"),
               omit.stat = c("ser", "f"),
               dep.var.caption = "", dep.var.labels = shopping_category,
               covariate.labels = c("constant", "log real GDP p.c."),
               intercept.bottom = FALSE, no.space = TRUE)

# plot estimated coefficients for different measures of the business cycle
df_lm_results %>%
    select(spec, lm_coefs) %>%
    unnest() %>%
    filter(term %in% c("log(rGDPpc)", "log(rPIpc)", "u3rate", "u4rate", "u5rate", "u6rate")) %>%
    mutate(spec_label = str_replace_all(spec, fixed(c("log(rGDPpc)" = "", "log(rPIpc)" = "", "u3rate" = "", "u4rate" = "", "u5rate" = "", "u6rate" = ""))),
           term_label = factor(term,
                               levels = c("u3rate", "u4rate", "u5rate", "u6rate", "log(rGDPpc)", "log(rPIpc)"),
                               labels = c("u3rate", "u4rate", "u5rate", "u6rate", "log(rGDPpc)", "log(rPIpc)"),
                               ordered = TRUE)) %T>%
    {{ggplot(., aes(x = estimate, y = spec_label)) +
        geom_point() +
        geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, height = 0.1), col = "gray50") +
        geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.1), col = "red") +
        geom_vline(xintercept = 0, color = "black") +
        labs(x = "", y = "") +
        facet_wrap(~term_label, ncol = 2, scales = "free_x") +
        theme_light() +
        theme(panel.grid = element_blank(),
              strip.text.x = element_text(color = "black", face = "bold", hjust = 0),
              strip.background = element_blank(),
              axis.text.y = element_text(hjust = 0))} %>%
      print()} %>%
    filter(p_value <= 0.1)
