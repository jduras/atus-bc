
# this file uses CPS microdata to replicate BLS time series for
# 1) the marginally attached
# 2) discouraged workers
# 3) employed part time for economic reasons

#### unemployment data from CPS ####

message(str_c("Extracting relevant data from CPS ", tfst, " to ", tlst))

tseq <- seq(ymd(str_c(tfst, "-01-01")), ymd(str_c(tlst, "-12-01")), by = "months") %>%
    enframe(name = NULL, value = "date") %>%
    mutate(year = year(date),
           month = month(date),
           period = year*100 + month) %>%
    arrange(year, month) %>%
    pull(period)

# state level data on number of people in labor force, unemployed, discouraged and marginally attached by month
df_ux_states_cps <-
    future_map_dfr(
        .progress = TRUE,
        .x = tseq,
        .f = function(t = .x) {
            # message(str_c("Getting data for ", t))

            df_cpsdata <- read_fwf(file = str_c(ddir_cps, "b", t),
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
                       pterfind = if_else((PEHRRSN1 == 2 & PEHRAVL==1) | PEHRRSN3 == 2, 1L, 0L)) %>%
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
                mutate(period = t)
            }) %>%
    select(period, gestfips, everything())

# national data on number of people in labor force, unemployed, discouraged and marginally attached by month
df_ux_cps <-
    df_ux_states_cps %>%
    select(-gestfips) %>%
    group_by(period) %>%
    summarise_all(sum)

save(df_ux_cps, df_ux_states_cps, file = str_c(edir_atus, "cps_", tfst, "_", tlst, "_ux.Rdata"))
