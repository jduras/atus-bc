
message(str_c("Extracting state level data for GDP, income, unemployment ", tfst, " to ", tlst))

#### import BEA data on real personal income and real GDP by state  ####

# real personal income and real personal income per capita, by state, annual
df_rPIpc_and_rPI_states_a_raw <-
    read_csv(str_c(ddir_atus, "other/SARPI_STATE_2008_2017.csv"))

# real GDP per capita, by state, annual
df_rGDPpc_states_a_raw <-
    read_csv(str_c(ddir_atus, "other/SAGDP10N__ALL_AREAS_1997_2018.csv"))

# real GDP, by state, annual
df_rGDP_states_a_raw <-
    read_csv(str_c(ddir_atus, "other/SAGDP9N__ALL_AREAS_1997_2018.csv"))

# real GDP, by state, quarterly
df_rGDP_states_q_raw <-
    read_csv(str_c(ddir_atus, "other/SQGDP9__ALL_AREAS_2005_2018.csv"))



#### clean BEA data on real personal income and real GDP by state  ####

# state level real personal income, annual
df_rPIpc_and_rPI_states_a <-
    df_rPIpc_and_rPI_states_a_raw %>%
    clean_names() %>%
    filter(!is.na(geo_name),  geo_fips != "00000") %>%
    select(-c(table_name, region, unit, line_code, industry_classification)) %>%
    # gather(year, value, -c(geo_fips, geo_name, description)) %>%
    gather(tuyear, value, starts_with("x")) %>%
    mutate(tuyear = str_sub(tuyear, 2, 5) %>% as.integer(),
           gestfips = as.integer(geo_fips) / 1000,
           measure = case_when(str_detect(description, "Real personal income")            ~ "rPI",
                               str_detect(description, "Real per capita personal income") ~ "rPIpc",
                               TRUE                                                       ~ NA_character_)) %>%
    select(tuyear, gestfips, measure, value)

df_rPIpc_and_rPI_states_a %>% summary()

# state level real GDP per capita, annual
df_rGDPpc_states_a <-
    df_rGDPpc_states_a_raw %>%
    clean_names() %>%
    filter(row_number() < which(str_sub(geo_fips, 1, 4) == "Note") &
               !(geo_fips %in% c("00000", "91000", "92000", "93000", "94000", "95000", "96000", "97000", "98000"))) %>%
    select(-c(table_name, region, component_name, unit, industry_id, industry_classification, description)) %>%
    gather(tuyear, value, starts_with("x")) %>%
    mutate(tuyear = str_sub(tuyear, 2, 5) %>% as.integer(),
           gestfips = as.integer(geo_fips) / 1000,
           measure = "rGDPpc") %>%
    select(tuyear, gestfips, measure, value)

df_rGDPpc_states_a %>% summary()

# state level real GDP, annual
df_rGDP_states_a <-
    df_rGDP_states_a_raw %>%
    clean_names() %>%
    filter(row_number() < which(str_sub(geo_fips, 1, 4) == "Note") &
               !(geo_fips %in% c("00000", "91000", "92000", "93000", "94000", "95000", "96000", "97000", "98000")) &
               industry_id == 1) %>%
    select(-c(table_name, region, component_name, unit, industry_id, industry_classification, description)) %>%
    gather(tuyear, value, starts_with("x")) %>%
    mutate(tuyear = str_sub(tuyear, 2, 5) %>% as.integer(),
           gestfips = as.integer(geo_fips) / 1000,
           measure = "rGDP",
           value = as.numeric(value)) %>%
    select(tuyear, gestfips, measure, value)

df_rGDP_states_a %>% summary()

# state level real GDP, quarterly
df_rGDP_states_q <-
    df_rGDP_states_q_raw %>%
    clean_names() %>%
    filter(row_number() < which(str_sub(geo_fips, 1, 4) == "Note") &
               !(geo_fips %in% c("00000", "91000", "92000", "93000", "94000", "95000", "96000", "97000", "98000")) &
               industry_id == 1) %>%
    select(-c(table_name, region, component_name, industry_id, industry_classification, description)) %>%
    gather(yearqtr, value, starts_with("x")) %>%
    mutate(tuyear = str_sub(yearqtr, 2, 5) %>% as.integer(),
           tuquarter = str_sub(yearqtr, 8, 8) %>% as.integer(),
           gestfips = as.integer(geo_fips) / 1000,
           measure = "rGDP",
           value = as.numeric(value)) %>%
    select(tuyear, tuquarter, gestfips, measure, value)

df_rGDP_states_q %>% summary()


# combine and HP filter
df_ni_states_a <-
    bind_rows(df_rPIpc_and_rPI_states_a,
              df_rGDPpc_states_a,
              df_rGDP_states_a) %>%
    spread(measure, value) %>%
    arrange(gestfips, tuyear) %>%
    group_by(gestfips) %>%
    mutate(pop = rGDP * 1000000 / rGDPpc,
           gr_rGDPpc = log(rGDPpc) - log(lag(rGDPpc)),
           hp_lrGDPpc = hpfilter(log(rGDPpc), freq = 6.25, type = "lambda")$cycle) %>%
    ungroup() %>%
    select(tuyear, gestfips, pop, rPIpc, rGDP, rGDPpc, gr_rGDPpc, hp_lrGDPpc)

df_ni_states_q <-
    left_join(df_rGDP_states_q %>%
                select(tuyear, tuquarter, gestfips, rGDP = value),
              df_ni_states_a %>%
                  select(tuyear, gestfips, pop),
              by = c("tuyear", "gestfips")) %>%
    arrange(gestfips, tuyear, tuquarter) %>%
    group_by(gestfips) %>%
    mutate(rGDPpc = 1000000 * rGDP / pop,
           gr_rGDPpc = log(rGDPpc) - log(lag(rGDPpc)),
           hp_lrGDPpc = hpfilter(log(rGDPpc), freq = 1600, type = "lambda")$cycle) %>%
    ungroup() %>%
    select(tuyear, tuquarter, gestfips, pop, rGDP, rGDPpc, gr_rGDPpc, hp_lrGDPpc)

save(df_ni_states_a, df_ni_states_q,
     file = str_c(edir_atus, "ni_", tfst, "_", tlst, "_state.Rdata"))

rm(df_rPIpc_and_rPI_states_a_raw, df_rPIpc_and_rPI_states_a,
   df_rGDPpc_states_a_raw, df_rGDPpc_states_a,
   df_rGDP_states_a_raw, df_rGDP_states_a,
   df_rGDP_states_q_raw, df_rGDP_states_q)

# plot groth rate and HP filtered per capita GDP
df_ni_states_a %>%
    gather(measure, value, -tuyear, -gestfips) %>%
    filter(measure %in% c("gr_rGDPpc", "hp_lrGDPpc")) %>%
    ggplot() +
        geom_rect(data = tibble(Start = 2008, End = 2010),
                  aes(xmin = 2008, xmax = 2010, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        geom_line(aes(x = tuyear, y = value, col = measure)) +
        facet_wrap(~gestfips)
