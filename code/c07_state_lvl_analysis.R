
# ATUS data: time spent on varios activities
load(file = paste0(edir_atus, "atus_", tfirst, "_", tlast, "_individual.Rdata"))
# CPS data: unemployment rate
load(file = paste0(edir_atus, "ux_", tfirst, "_", tlast, "_state.Rdata"))
# NIPA data: real GDP and personal income
load(file = paste0(edir_atus, "ni_", tfirst, "_", tlast, "_state.Rdata"))

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
save(df_timeuse_all_a, df_timeuse_all_q, file = paste0(edir_atus, "atus_", tfirst, "_", tlast, "_state.Rdata"))

# save(file = paste0(edir_atus, "atus_", tfirst, "_", tlast, "_state.Rdata"))



# generate state level aggregates (weighted averages)
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
                          as.tibble(),
                      df_timeuse_all_a %>%
                          model.matrix(object = ~ educ_f-1) %>%
                          as.tibble()) %>%
            group_by(gestfips, tuyear) %>%
            summarise_at(vars(-weight, -gestfips, -tuyear), funs(wtd.mean(., w = weight, na.rm = TRUE))) %>%
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
            summarise_at(vars(-weight, -gestfips, -tuyear, -tuquarter), funs(wtd.mean(., w = weight, na.rm = TRUE))) %>%
            ungroup(),
        # variables summarized using sum
        df_timeuse_all_q %>%
            select(gestfips,tuyear,tuquarter,weight) %>%
            group_by(gestfips, tuyear, tuquarter) %>%
            summarise(weight = sum(weight)/100) %>%
            ungroup(),
        by=c("gestfips", "tuyear", "tuquarter")) %>%
    inner_join(df_state_codes, by=c("gestfips"))


# save the resulting state level dataset
save(df_agg_states_a, df_agg_states_q, file = paste0(edir_atus, "df_agg_by_state_", tfirst, "_", tlast, ".Rdata"))


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



# plot real GDP per capita against real personal income per capita - D.C. looks like an outlier
bind_rows(df_agg_states_a %>%
              mutate(keep_DC = 1),
          df_agg_states_a %>%
              filter(state_code != "DC") %>%
              mutate(keep_DC = 0)) %>%
    mutate(is_DC = if_else(state_code == "DC", 1L, 0L)) %>%
    ggplot(aes(x = log(rGDPpc), y = log(rPIpc))) +
        geom_point(alpha = 0.2, aes(size = weight, col = factor(is_DC))) +
        geom_smooth(method = "lm", aes(weight = weight)) +
        # geom_smooth(method="lm", col="red") +
        labs(x = "log of real GDP per capita", y = "log of real personal income per capita") +
        facet_grid(~ keep_DC, labeller = label_both) +
        theme_light() +
        theme(strip.text.x = element_text(color = "black"))


# scatter plots for measures of business cycle
df_agg_states_a %>%
    filter(state_code != "DC") %>%
    select(t_shop_ttl, u3rate, u4rate, u5rate, u6rate, rGDPpc, pop, weight) %>%
    gather(measure, value, -c(t_shop_ttl, pop, weight)) %>%
    mutate(measure = factor(measure, levels = c("u3rate", "u4rate", "u5rate", "u6rate", "rGDPpc"))) %>%
    {ggplot(., aes(x = value, y = t_shop_ttl)) +
            geom_point(alpha = 0.2, aes(size = weight)) +
            geom_smooth(method = "lm", aes(weight = weight)) +
            geom_smooth(method = "lm", aes(weight = weight)) +
            labs(x = "", y = "Average total shopping time") +
            facet_wrap(~ measure, scales = "free", ncol = 4) +
            theme_light() +
            theme(legend.position = "none",
                  strip.text.x = element_text(color = "black"),
                  strip.background = element_rect(fill = "lightgray"))}


#
# state level regressions - time spent on shopping related activities on business cycle measure
#

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


spc_lm_state <- vector("character")
spc_lm_state <- c(spc_lm_state, "log(rGDPpc)")
spc_lm_state <- c(spc_lm_state, "log(rGDPpc) + tuyear")
spc_lm_state <- c(spc_lm_state, "log(rGDPpc) + factor(tuyear)")
spc_lm_state <- c(spc_lm_state, "log(rGDPpc) + tuyear + factor(gestfips)")
spc_lm_state <- c(spc_lm_state, "log(rGDPpc) + factor(gestfips) + tuyear:factor(gestfips)")

spc_lm_state <- c(spc_lm_state, "u3rate")
spc_lm_state <- c(spc_lm_state, "u3rate + tuyear")
spc_lm_state <- c(spc_lm_state, "u3rate + factor(tuyear)")
spc_lm_state <- c(spc_lm_state, "u3rate + tuyear + factor(gestfips)")
spc_lm_state <- c(spc_lm_state, "u3rate + factor(gestfips) + tuyear:factor(gestfips)")

spc_lm_state <- c(spc_lm_state, "u4rate")
spc_lm_state <- c(spc_lm_state, "u4rate + tuyear")
spc_lm_state <- c(spc_lm_state, "u4rate + factor(tuyear)")
spc_lm_state <- c(spc_lm_state, "u4rate + tuyear + factor(gestfips)")
spc_lm_state <- c(spc_lm_state, "u4rate + factor(gestfips) + tuyear:factor(gestfips)")

spc_lm_state <- c(spc_lm_state, "u5rate")
spc_lm_state <- c(spc_lm_state, "u5rate + tuyear")
spc_lm_state <- c(spc_lm_state, "u5rate + factor(tuyear)")
spc_lm_state <- c(spc_lm_state, "u5rate + tuyear + factor(gestfips)")
spc_lm_state <- c(spc_lm_state, "u5rate + factor(gestfips) + tuyear:factor(gestfips)")

spc_lm_state <- c(spc_lm_state, "u6rate")
spc_lm_state <- c(spc_lm_state, "u6rate + tuyear")
spc_lm_state <- c(spc_lm_state, "u6rate + factor(tuyear)")
spc_lm_state <- c(spc_lm_state, "u6rate + tuyear + factor(gestfips)")
spc_lm_state <- c(spc_lm_state, "u6rate + factor(gestfips) + tuyear:factor(gestfips)")

# spc_lm_state <- c(spc_lm_state, "log(rGDPpc) + unrate")
# spc_lm_state <- c(spc_lm_state, "log(rGDPpc) + unrate + tuyear")
# spc_lm_state <- c(spc_lm_state, "log(rGDPpc) + unrate + tuyear + factor(gestfips)")
# spc_lm_state <- c(spc_lm_state, "log(rGDPpc) + unrate + tuyear * factor(gestfips)")

# estimate individual level regressions - time spent on shopping related activities
df_lm <-
    # df_agg_states_q %>%
    df_agg_states_a %>%
    filter(tuyear != 2017) %>%
    filter(state_code != "DC") %>%
    mutate(unrate = u3rate,
           tuyear = tuyear)

shopping_category <- "ttl"

df_lm_results <-
    spc_lm_state %>%
    tibble(spec = .) %>%
    mutate(# estimate individual level regressions - time spent on shopping related activities
        lm_model = map(spec, ~lm( formula(paste0("t_shop_", shopping_category, " ~ ", .x)), weight = weight, data = df_lm )),
        lm_coefs = map(lm_model, tidy, conf.int = TRUE),
        # construct clustered errors
        lm_coefs = map2(lm_model, lm_coefs,
                        ~(.y %>% mutate(std_error_clustered = coeftest(.x, clustered_se(.x, df_lm$gestfips))[,"Std. Error"],
                                        conf_low_clustered = estimate - 2*std_error_clustered,
                                        conf_high_clustered = estimate + 2*std_error_clustered))))

# report results using stargazer
df_lm_results %$%
    stargazer(lm_model, type = "text", out = paste0(odir_atus, "results_state_a_", shopping_category, ".tex"),
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
    filter(term %in% c("u3rate", "u4rate", "u5rate", "u6rate", "log(rGDPpc)", "hp.lrGDPpc")) %T>%
    {{ggplot(.,aes(x = estimate, y = spec)) +
        geom_point() +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.2), col = "red") +
        geom_vline(xintercept = 0, linetype = "dotted") +
        theme_light() +
        facet_grid(~term, scales = "free_x")} %>% print()} %>%
    filter(p.value <= 0.1)
