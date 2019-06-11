
#### unemployment data published by BLS ####

# Civilian Labor Force  (LNU01000000)

# unemployed, discouraged and marginally attached
# Unemployment Level  (LNU03000000)
# Not in Labor Force: Searched for Work and Available, Discouraged Reasons for Not Currently Looking  (LNU05026645)
# Not in Labor Force: Searched for Work and Available, Reasons Other Than Discouragement  (LNU05026648)
# Employment Level: Persons at Work 1 to 34 Hours, Economic Reasons, All Industries  (LNU02032194)

# U3, ..., U6
# Civilian Unemployment Rate  (UNRATENSA)
# Special Unemployment Rate: Unemployed and Discouraged Workers  (U4RATENSA)
# Special Unemployment Rate: Unemployed and Marginally Attached Workers  (U5RATENSA)
# Total unemployed, plus all marginally attached workers plus total employed part time for economic reasons  (U6RATENSA)

# Part time employed for economic reasons
# FRED: Employment Level: Persons at Work 1 to 34 Hours, Economic Reasons, All Industries  (LNU02032194)
# of that
# FRED: Employment Level: Persons at Work 1 to 34 Hours, Economic Reasons: Slack Work or Business Conditions, All Industries  (LNU02032195)
# FRED: Employment Level: Persons at Work 1 to 34 Hours, Economic Reasons: Could Only Find Part-Time Work, All Industries  (LNU02032196)

# Marginally attached - Persons not in the labor force who
#  1) were not counted as unemployed because they had not searched for work in the 4 weeks preceding the survey
#  2) want a job
#  3) are available for work (PEDWAVL == 1 | (PEDWAVR==2 & PEDWAVR==1))
#  4) have looked for a job sometime in the prior 12 months (or since the end of their last job if they held one within the past 12 months),
# FRED: Not in Labor Force: Searched for Work and Available (LNU05026642)

# Discouraged - subset of the marginally attached: Persons who
#  1) are not in the labor force (PEMLR in 5-7)
#  2) want a job (PRWNTJOB = 1)
#  3) are available for a job (PEDWAVL == 1 | (PEDWAVR==2 & PEDWAVR==1))
#  4) who have looked for work sometime in the past 12 months (PEDWLKO=1) or since the end of their last job if they held one within the past 12 months (PEDWLKWK=1)
#  5) but who are not currently looking because they believe there are no jobs available or there are none for which they would qualify (PEDWRSN in 1-5)
# FRED: Not in Labor Force: Searched for Work and Available, Discouraged Reasons for Not Currently Looking  (LNU05026645)

# Part-Time Employment for Economics Reasons
# for more details see https://www.kansascityfed.org/research/datamuseum/cps/coreinfo/pseudocode/ftpt

# Marginally Attached and Discouraged Workers
# for more details see https://www.kansascityfed.org/research/datamuseum/cps/coreinfo/pseudocode/marginal
# there is a mistake in the definition of marginally attached workers: PELKAVL is only available for unemployed person with PEMLR=4
# instead marginally attached are those with PRDISC = 1 or 2, see
# https://www.frbatlanta.org/chcs/human-capital-currents/2015/0612-measuring-labor-market-status-using-basic-data.aspx?d=1&s=blogmb

#### national level unemployment data from BLS ####

df_ux_bls_raw <-
    c("LNU01000000", "LNU03000000", "LNU05026645", "LNU05026648", "LNU02032194", "LNU02032195", "LNU02032196", "LNU05026642")%>%
    tq_get(get = "economic.data", from = "1948-01-01", to = str_c(tlst, "-12-31"))

if (nrow(df_ux_bls_raw) == 0) {
    df_ux_bls_raw <- read_csv(file = str_c(ddir_atus, "other/bls_ux_raw.csv"))
} else {
    write_csv(df_ux_bls_raw, path = str_c(ddir_atus, "other/bls_ux_raw.csv"))
}

df_ux_bls <-
    df_ux_bls_raw %>%
    rename(value = price) %>%
    filter(symbol != "LNU05026648") %>%
    # c("FRED/LNU01000000", "FRED/LNU03000000", "FRED/LNU05026645", "FRED/LNU05026648", "FRED/LNU02032194", "FRED/LNU02032195", "FRED/LNU02032196", "FRED/LNU05026642")%>%
    # tq_get(get = "quandl") %>%
    # mutate(symbol = str_sub(symbol, 6, -1)) %>%
    mutate(period = year(date)*100 + month(date),
           yearm = period %>% as.character() %>% as.yearmon("%Y%m")) %>%
    spread(symbol, value) %>%
    rename(n_lf_bls = LNU01000000,
           n_u3_bls = LNU03000000,
           n_dw_bls = LNU05026645,
           n_maw_bls = LNU05026642,
           n_pter_bls = LNU02032194,
           n_pterslack_bls = LNU02032195,
           n_pterfind_bls = LNU02032196)

#### state level unemployment data from BLS ####

df_ux_states_bls_raw <-
    crossing(
        tibble(state_code = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                              "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                              "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                              "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
                              "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
        tibble(ur_code = c("UR" ,"URN"))) %>%
    unite(col = "symbol", sep = "") %>%
    tq_get("economic.data", from = "1976-01-01", to = str_c(tlst, "-12-31"))

if (nrow(df_ux_states_bls_raw) == 0) {
    df_ux_states_bls_raw <- read_csv(file = str_c(ddir_atus, "other/bls_ux_states_raw.csv"))
} else {
    write_csv(df_ux_states_bls_raw, path = str_c(ddir_atus, "other/bls_ux_states_raw.csv"))
}

df_ux_states_bls <-
    df_ux_states_bls_raw %>%
    mutate(period = year(date)*100 + month(date),
           tuyear = year(date),
           tuquarter = quarter(date),
           tumonth = month(date),
           state_code = str_sub(symbol, 1, 2),
           measure = if_else(str_sub(symbol, 5, 5) == "N", "rate_u3_bls_nsa", "rate_u3_bls_sa")) %>%
    select(-symbol) %>%
    spread(measure, price) %>%
    inner_join(df_state_codes, by = "state_code") %>%
    arrange(gestfips, tuyear, tumonth) %>%
    select(period, tuyear, tuquarter, tumonth, gestfips, state_code, rate_u3_bls_nsa, rate_u3_bls_sa)

save(df_ux_bls, df_ux_states_bls, file = str_c(edir_atus, "bls_", tfst, "_", tlst, "_ux.Rdata"))
