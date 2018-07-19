

# this file uses CPS microdata to replicate BLS time series for
# 1) the marginally attached
# 2) discouraged workers
# 3) employed part time for economic reasons

#### unemployment data from CPS ####

ddir_cps <- "D:/Data/CPS/data/basic/unpacked/"

df_ux_cps <- NULL
df_ux_states_cps <- NULL

t <- 200301

while (t <= 201803) {

    print(paste("Getting data for", t))

    df_cpsdata <- read_fwf(file = paste0(ddir_cps, "b", t),
                           col_positions = fwf_cols(GESTFIPS = c(93, 94), PRTAGE = c(122, 123), PEMLR = c(180, 181),
                                                    PEHRRSN1 = c(229, 230), PEHRRSN3 = c(233, 234), PEHRAVL = c(250, 251), PEDWRSN = c(349, 350), PEDWLKO = c(351, 352), PEDWAVL = c(359, 360), PEDWAVR = c(361, 362), PRDISC = c(389, 390),
                                                    PRWKSTAT = c(416, 417), PRWNTJOB = c(418, 419),
                                                    PWSSWGT = c(613, 622), PWCMPWGT = c(846, 855)),
                           col_types = cols(GESTFIPS = "i", PRTAGE = "i", PEMLR = "i",
                                            PEHRRSN1 = "i", PEHRRSN3 = "i", PEHRAVL = "i", PEDWRSN = "i", PEDWLKO = "i", PEDWAVL = "i", PEDWAVR = "i", PRDISC = "i",
                                            PRWKSTAT = "i", PRWNTJOB = "i",
                                            PWSSWGT = "d", PWCMPWGT = "d")) %>%
        mutate(lf = if_else(PRTAGE >= 16 & PEMLR %in% c(1:4), 1L, 0L),
               u3 = if_else(PRTAGE >= 16 & PEMLR %in% c(3:4), 1L, 0L),
               # discouraged workers
               dw1 = if_else(PRWNTJOB == 1 & (PEDWAVL == 1 | (PEDWAVL == 2 & PEDWAVR == 1)) & PEDWRSN %in% c(1:5), 1L, 0L),
               dw2 = if_else(PRDISC == 1, 1L, 0L),
               # marginally attached workers (disouraged workers are a subset of these)
               maw1 = if_else(PRWNTJOB == 1 & (PEDWAVL == 1 | (PEDWAVL == 2 & PEDWAVR == 1)), 1L, 0L),
               maw2 = if_else(PRDISC %in% c(1:2), 1L, 0L),
               # part time employed for economic reasons
               pte = if_else(PRWKSTAT %in% c(3,6), 1L, 0L),
               pter = if_else((PEHRRSN1 %in% c(1:3) & PEHRAVL==1) | PEHRRSN3 %in% c(1:2), 1L, 0L),
               pterslack = if_else((PEHRRSN1 == 1 & PEHRAVL==1) | PEHRRSN3 == 1, 1L, 0L),
               pterfind = if_else((PEHRRSN1 == 2 & PEHRAVL==1) | PEHRRSN3 == 2, 1L, 0L))

    df_ux_states_cps <-
        c(df_ux_states_cps,
          df_cpsdata %>%
              rename(gestfips = GESTFIPS) %>%
              group_by(gestfips) %>%
              summarise(n_lf_cps = sum(PWCMPWGT * lf) / 10000000,
                        n_u3_cps = sum(PWCMPWGT * u3) / 10000000,
                        n_dw1_cps = sum(PWCMPWGT * dw1) / 10000000,
                        n_dw2_cps = sum(PWCMPWGT * dw2) / 10000000,
                        n_maw1_cps = sum(PWCMPWGT * maw1) / 10000000,
                        n_maw2_cps = sum(PWCMPWGT * maw2) / 10000000,
                        n_pte_cps = sum(PWCMPWGT * pte) / 10000000,
                        n_pter_cps = sum(PWCMPWGT * pter) / 10000000,
                        n_pterslack_cps = sum(PWCMPWGT * pterslack) / 10000000,
                        n_pterfind_cps = sum(PWCMPWGT * pterfind) / 10000000) %>%
              mutate(period = t) %>%
              list())

    rm(df_cpsdata)

    # go to next month
    t <- t + 1
    # go to next year if last two digits of t are 13
    if ((t %% 100) == 13) t <- t + 88
}

df_ux_states_cps %<>%
    bind_rows() %>%
    select(period, gestfips, everything())

df_ux_cps <-
    df_ux_states_cps %>%
    select(-gestfips) %>%
    group_by(period) %>%
    summarise_all(sum)

save(df_ux_cps, df_ux_states_cps, file = paste0(odir_atus, "df_ux_cps.Rdata"))

# load(file = paste0(odir_atus, "df_ux_cps.Rdata"))


#### unemployment data published by BLS ####

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


#### national level unemployment data from BLS ####

quandl_api_key("DLk9RQrfTVkD4UTKc7op")

df_ux_bls <-
    c("LNU01000000", "LNU03000000", "LNU05026645", "LNU05026648", "LNU02032194", "LNU02032195", "LNU02032196", "LNU05026642")%>%
    tq_get(get = "economic.data", from = "1948-01-01", to = "2018-03-31") %>%
    rename(value = price) %>%
    filter(symbol != "LNU05026648") %>%
    # c("FRED/LNU01000000", "FRED/LNU03000000", "FRED/LNU05026645", "FRED/LNU05026648", "FRED/LNU02032194", "FRED/LNU02032195", "FRED/LNU02032196", "FRED/LNU05026642")%>%
    # tq_get(get = "quandl") %>%
    # mutate(symbol = str_sub(symbol, 6, -1)) %>%
    mutate(period = year(date)*100 + month(date),
           monyear = period %>% as.character() %>% as.yearmon("%Y%m")) %>%
    spread(symbol, value) %>%
    rename(n_lf_bls = LNU01000000,
           n_u3_bls = LNU03000000,
           n_dw_bls = LNU05026645,
           n_maw_bls = LNU05026642,
           n_pter_bls = LNU02032194,
           n_pterslack_bls = LNU02032195,
           n_pterfind_bls = LNU02032196)

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
    select(period, rate_u3_cps, rate_u4_bls, rate_u3_cps, rate_u4_bls, rate_u5_cps, rate_u5_bls, rate_u6_cps, rate_u6_bls) %>%
    filter(complete.cases(.))

library(dygraphs)

df_ux %>%
    select(monyear, starts_with("n_maw")) %>%
    # select(monyear, starts_with("n_dw")) %>%
    # select(monyear, starts_with("n_pter")) %>%
    # select(monyear, starts_with("n_lf")) %>%
    # select(monyear, starts_with("n_u3")) %>%
    # select(monyear, starts_with("n_u6")) %>%
    # select(monyear, starts_with("rate_u3")) %>%
    # select(monyear, starts_with("rate_u5")) %>%
    # select(monyear, starts_with("rate_u6")) %>%
    xts(x = .[, names(.) != "monyear"], order.by=.$monyear) %>%
    dygraph() %>%
        dyRangeSelector(dateWindow = c("2000-01-01", "2018-01-01"))


#### state level unemployment data from BLS ####

df_ux_states_bls_m_raw <-
    crossing(
        tibble(state_code = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                          "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                          "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                          "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
                          "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
        tibble(ur_code = c("UR" ,"URN"))) %>%
    unite(col = "symbol", sep = "") %>%
    tq_get("economic.data", from = "1976-01-01", to = "2018-03-31")

df_ux_states_bls_m <-
    df_ux_states_bls_m_raw %>%
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

df_ux_states <-
    df_ux_states_cps %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon("%Y%m"),
           n_u4_cps = n_u3_cps + n_dw2_cps,
           n_u5_cps = n_u3_cps + n_maw2_cps,
           n_u6_cps = n_u3_cps + n_maw2_cps + n_pte_cps,
           rate_u3_cps_nsa = 100 * n_u3_cps / n_lf_cps,
           rate_u4_cps_nsa = 100 * (n_u3_cps + n_dw2_cps) / (n_dw2_cps + n_lf_cps),
           rate_u5_cps_nsa = 100 * (n_u3_cps + n_maw2_cps) / (n_maw2_cps + n_lf_cps),
           rate_u6_cps_nsa = 100 * (n_u3_cps + n_maw2_cps + n_pte_cps) / (n_maw2_cps + n_lf_cps)) %>%
    inner_join(df_ux_states_bls_m, by = c("period","gestfips"))

df_ux_states %>%
    select(monyear, gestfips, state_code, starts_with("rate_u")) %>%
    gather(measure, value, -c(monyear, gestfips, state_code)) %>%
    ggplot(aes(x = monyear, y = value, col = measure)) +
        geom_line() +
        scale_x_yearmon() +
        facet_wrap( ~ state_code)

df_ux_states_q <-
    df_ux_states %>%
    group_by(gestfips, tuyear, tuquarter) %>%
    summarise_at(vars(starts_with("rate_u")), funs(mean)) %>%
    ungroup() %>%
    select(tuyear, tuquarter, gestfips,
           u3rate = rate_u3_cps_nsa,
           u4rate = rate_u4_cps_nsa,
           u5rate = rate_u5_cps_nsa,
           u6rate = rate_u6_cps_nsa)

df_ux_states_a <-
    df_ux_states %>%
    group_by(gestfips, tuyear) %>%
    summarise_at(vars(starts_with("rate_u")), funs(mean)) %>%
    ungroup() %>%
    select(tuyear, gestfips,
           u3rate = rate_u3_cps_nsa,
           u4rate = rate_u4_cps_nsa,
           u5rate = rate_u5_cps_nsa,
           u6rate = rate_u6_cps_nsa)

save(df_ux_states_a, df_ux_states_q, file = paste0(edir_atus, "ux_", tfirst, "_", tlast, "_state.Rdata"))

# mutate(monyear = period %>% as.character() %>% as.yearmon("%Y%m"))
