
message(str_c("Merging unemployment date BLS and CPS from ", tfst, " to ", tlst))

# BLS data constructed in c06_extract_bls_data.R
load(file = str_c(edir_atus, "bls_", tfst, "_", tlst, "_ux.Rdata"))
# CPS data constructed in c07_extract_cps_data.R
load(file = str_c(edir_atus, "cps_ux.Rdata"))


#### merge national level unemployment data from CPS and BLS ####

df_ux <-
    full_join(df_ux_bls, df_ux_cps, by = "period") %>%
    mutate(n_u6_cps = n_u3_cps + n_maw2_cps + n_pte_cps,
           n_u6_bls = n_u3_bls + n_maw_bls + n_pter_bls,
           rate_u3_cps = n_u3_cps / n_lf_cps,
           rate_u3_bls = n_u3_bls / n_lf_bls,
           rate_u4_cps = (n_u3_cps + n_dw2_cps) / (n_dw2_cps + n_lf_cps),
           rate_u4_bls = (n_u3_bls + n_dw_bls) / (n_dw_bls + n_lf_bls),
           rate_u5_cps = (n_u3_cps + n_maw2_cps) / (n_maw2_cps + n_lf_cps),
           rate_u5_bls = (n_u3_bls + n_maw_bls) / (n_maw_bls + n_lf_bls),
           rate_u6_cps = (n_u3_cps + n_maw2_cps + n_pte_cps) / (n_maw2_cps + n_lf_cps),
           rate_u6_bls = (n_u3_bls + n_maw_bls + n_pter_bls) / (n_maw_bls + n_lf_bls))

df_ux %>%
    select(period, rate_u3_cps, rate_u4_bls, rate_u4_cps, rate_u4_bls, rate_u5_cps, rate_u5_bls, rate_u6_cps, rate_u6_bls) %>%
    filter(complete.cases(.))

# compare BLS and CPS based measures
df_ux %>%
    filter(period >= 200001) %>%
    gather(measure, value, -c(date, period, yearm)) %>%
    separate(measure, into = c("type", "variable", "source"), remove = FALSE) %>%
    mutate(variable_group = str_replace_all(variable, pattern = "[1-2]", ""),
           variable_version = str_extract(variable, pattern = "[1-2]") %>%
               replace_na("")) %>%
    unite(variable_group, "type", "variable_group") %>%
    unite(source, "source", "variable_version", sep = "") %>%
    # filter(str_detect(measure, "rate_u")) %>%
    {ggplot(data = .) +
        geom_line(aes(x = yearm, y = value, col = source)) +
        geom_rect(data = (rec_dates %>% filter(Start > as.yearmon("200001", format = "%Y%m"))),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), fill = "steelblue", alpha = 0.2) +
        scale_x_yearmon() +
        labs(x = "", y = "",
             title = "BLS and CPS microdata based measures") +
        facet_wrap(~variable_group, scales = "free") +
        theme(legend.position = "top",
              legend.justification = "left")} %>%
    ggplotly()

# chosen_variable <- "n_lf"
chosen_variable <- "n_dw"
# chosen_variable <- "n_pter"
# chosen_variable <- "n_lf"
# chosen_variable <- "n_u3"
# chosen_variable <- "n_u6"
# chosen_variable <- "rate_u3"
# chosen_variable <- "rate_u5"
# chosen_variable <- "rate_u6"
df_ux %>%
    tk_xts(select = starts_with(chosen_variable), date_var = yearm) %>%
    dygraphs::dygraph() %>%
        dyRangeSelector(dateWindow = c("2000-01-01", str_c(tlst, "-12-31")))

save(df_ux, file = str_c(edir_atus, "ux_", tfst, "_", tlst, ".Rdata"))


#### merge state level unemployment data from CPS and BLS ####

df_ux_states_m <-
    inner_join(df_ux_states_bls,
               df_ux_states_cps %>%
                   mutate(yearm = period %>% as.character() %>% as.yearmon("%Y%m"),
                          n_u4_cps = n_u3_cps + n_dw2_cps,
                          n_u5_cps = n_u3_cps + n_maw2_cps,
                          n_u6_cps = n_u3_cps + n_maw2_cps + n_pte_cps,
                          rate_u3_cps_nsa = 100 * n_u3_cps / n_lf_cps,
                          rate_u4_cps_nsa = 100 * (n_u3_cps + n_dw2_cps) / (n_dw2_cps + n_lf_cps),
                          rate_u5_cps_nsa = 100 * (n_u3_cps + n_maw2_cps) / (n_maw2_cps + n_lf_cps),
                          rate_u6_cps_nsa = 100 * (n_u3_cps + n_maw2_cps + n_pte_cps) / (n_maw2_cps + n_lf_cps)),
               by = c("period", "gestfips"))

# plot BLS and CPS based unemployment rates by state
df_ux_states_m %>%
    select(yearm, gestfips, state_code, starts_with("rate_u")) %>%
    gather(measure, value, -c(yearm, gestfips, state_code)) %>%
    mutate(measure = str_to_upper(str_c(str_sub(measure, 6, 7), " ", str_sub(measure, 9, 11), " ", str_sub(measure, 13, -1)))) %>%
    {ggplot(data = .) +
        geom_line(aes(x = yearm, y = value, col = measure)) +
        geom_rect(data = (rec_dates %>% filter(Start > as.yearmon("200001", format = "%Y%m"))),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), fill = "steelblue", alpha = 0.2) +
        scale_x_yearmon() +
        labs(x = "", y = "", col = "",
             title = "Unemployment Rate by State") +
        facet_wrap( ~ state_code) +
        theme(legend.position = "top",
              legend.justification = "left")} %>%
    ggplotly()

df_ux_states_q <-
    df_ux_states_m %>%
    group_by(gestfips, tuyear, tuquarter) %>%
    summarise_at(vars(starts_with("rate_u")), funs(mean)) %>%
    ungroup() %>%
    select(tuyear, tuquarter, gestfips,
           u3rate = rate_u3_cps_nsa,
           u4rate = rate_u4_cps_nsa,
           u5rate = rate_u5_cps_nsa,
           u6rate = rate_u6_cps_nsa)

df_ux_states_a <-
    df_ux_states_m %>%
    group_by(gestfips, tuyear) %>%
    summarise_at(vars(starts_with("rate_u")), funs(mean)) %>%
    ungroup() %>%
    select(tuyear, gestfips,
           u3rate = rate_u3_cps_nsa,
           u4rate = rate_u4_cps_nsa,
           u5rate = rate_u5_cps_nsa,
           u6rate = rate_u6_cps_nsa)

save(df_ux_states_a, df_ux_states_q, df_ux_states_m, file = str_c(edir_atus, "ux_", tfst, "_", tlst, "_state.Rdata"))
