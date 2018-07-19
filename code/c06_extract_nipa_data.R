
print(paste("Extracting state level data for GDP, income, unemployment", tfirst, "to", tlast))

# US states codes
df_state_codes <-
    read_csv(paste0(ddir_atus, "other/state_codes_gestfips.csv")) %>%
    mutate(state = state_name %>% str_replace_all(fixed(" "), "") %>% str_to_lower())

#### import BEA data on real personal income and real GDP by state  ####

# real personal income and real personal income per capita, by state, annual
df_rPIpc_and_rPI_states_a_raw <-
    read_csv(paste0(ddir_atus, "other/rPI_a_and_rPIpc_A.csv"), skip = 4)

# real GDP per capita, by state, annual
df_rGDPpc_states_a_raw <-
    read_csv(paste0(ddir_atus, "other/gdpstate_naics_all_PC.csv"))

# real GDP, by state, annual
df_rGDP_states_a_raw <-
    read_csv(paste0(ddir_atus, "other/gdpstate_naics_all_R.csv"))

# real GDP, by state, quarterly
df_rGDP_states_q_raw <-
    read_csv(paste0(ddir_atus, "other/qgdpstate_all_R.csv"))

# state level real personal income, annual
df_rPIpc_and_rPI_states_a <-
    df_rPIpc_and_rPI_states_a_raw %>%
    filter(row_number() < which(GeoFips == "Legend / Footnotes:") &
               GeoFips != "00000") %>%
    select(-LineCode) %>%
    gather(Year, Value, -c(GeoFips, GeoName, Description)) %>%
    mutate(GeoFips = as.integer(GeoFips) / 1000) %>%
    # right_join(read_csv(paste0(ddir_atus, "state_codes_gestfips.csv")),
    #            by = c("GeoFips" = "gestfips")) %>%
    # mutate(Year = as.integer(Year),
    #        GeoName = as.factor(GeoName)) %>%
    # rename(gestfips = GeoFips,
    #        year = Year,
    #        measure = Description,
    #        value = Value) %>%
    # select(gestfips, state_code, state_name, measure, value)
    mutate(tuyear = as.integer(Year)) %>%
    select(tuyear,
           gestfips = GeoFips,
           measure = Description,
           value = Value)

df_rPIpc_and_rPI_states_a %>% summary()

df_rPI_states_a <-
    df_rPIpc_and_rPI_states_a %>%
    filter(measure == "Real personal income (thousands of chained (2009) dollars)") %>%
    select(-measure) %>%
    rename(rPI = value)

df_rPIpc_states_a <-
    df_rPIpc_and_rPI_states_a %>%
    filter(measure == "Real per capita personal income (chained (2009) dollars) 2/") %>%
    select(-measure) %>%
    rename(rPIpc = value)

# state level real GDP, per capita, annual
df_rGDPpc_states_a <-
    df_rGDPpc_states_a_raw %>%
    filter(row_number() < which(str_sub(GeoFIPS, 1, 4) == "Note") &
               !(GeoFIPS %in% c("00000", "91000", "92000", "93000", "94000", "95000", "96000", "97000", "98000"))) %>%
    select(-c(Region, ComponentId, ComponentName, IndustryId, IndustryClassification, Description)) %>%
    gather(Year, Value, -c(GeoFIPS, GeoName)) %>%
    mutate(GeoFIPS = as.integer(GeoFIPS) / 1000,
           tuyear = as.integer(Year)) %>%
    select(tuyear,
           gestfips = GeoFIPS,
           rGDPpc = Value)

df_rGDPpc_states_a %>% summary()

# state level real GDP, annual
df_rGDP_states_a <-
    df_rGDP_states_a_raw %>%
    filter(row_number() < which(str_sub(GeoFIPS, 1, 4) == "Note") &
               !(GeoFIPS %in% c("00000", "91000", "92000", "93000", "94000", "95000", "96000", "97000", "98000")) &
               IndustryId == 1) %>%
    select(-c(Region, ComponentId , ComponentName, IndustryId, IndustryClassification, Description)) %>%
    gather(Year, Value, -c(GeoFIPS, GeoName)) %>%
    mutate(GeoFIPS = as.integer(GeoFIPS)/1000,
           tuyear = as.integer(Year),
           Value = as.numeric(Value)) %>%
    select(tuyear,
           gestfips = GeoFIPS,
           rGDP =  Value)

df_rGDP_states_a %>% summary()

# state level real GDP, quarterly
df_rGDP_states_q <-
    df_rGDP_states_q_raw %>%
    filter(row_number() < which(str_sub(GeoFIPS, 1, 4) == "Note") &
               !(GeoFIPS %in% c("00000", "91000", "92000", "93000", "94000", "95000", "96000", "97000", "98000")) &
               IndustryId == 1) %>%
    select(-c(Region, ComponentId , ComponentName, IndustryId, IndustryClassification, Description)) %>%
    gather(Year, Value, -c(GeoFIPS, GeoName)) %>%
    mutate(GeoFIPS = as.integer(GeoFIPS)/1000,
           tuyear = Year %>% str_sub(1, 4) %>% as.integer(),
           tuquarter  = Year %>% str_sub(-1, -1) %>% as.integer(),
           Value = as.numeric(Value)) %>%
    select(tuyear,
           tuquarter,
           gestfips = GeoFIPS,
           rGDP =  Value)

df_rGDP_states_q %>% summary()


# combine and HP filter
df_ni_states_a <-
    df_rGDP_states_a %>%
    full_join(df_rGDPpc_states_a, by = c("tuyear", "gestfips")) %>%
    full_join(df_rPIpc_states_a, by = c("tuyear", "gestfips")) %>%
    arrange(gestfips, tuyear) %>%
    group_by(gestfips) %>%
    mutate(pop = rGDP * 1000000 / rGDPpc,
           gr_rGDPpc = log(rGDPpc) - lag(log(rGDPpc)),
           hp_lrGDPpc = hpfilter(log(rGDPpc), freq = 6.25, type = "lambda")$cycle) %>%
    ungroup() %>%
    select(tuyear, gestfips, pop, rPIpc, rGDP, rGDPpc, gr_rGDPpc, hp_lrGDPpc)

df_ni_states_q <-
    df_rGDP_states_q %>%
    left_join(df_ni_states_a %>%
                  select(tuyear, gestfips, pop),
              by = c("tuyear", "gestfips")) %>%
    arrange(gestfips, tuyear, tuquarter) %>%
    group_by(gestfips) %>%
    mutate(rGDPpc = 1000000 * rGDP / pop,
           gr_rGDPpc = log(rGDPpc) - lag(log(rGDPpc)),
           hp_lrGDPpc = hpfilter(log(rGDPpc), freq = 1600, type = "lambda")$cycle) %>%
    ungroup() %>%
    select(tuyear, tuquarter, gestfips, pop, rGDP, rGDPpc, gr_rGDPpc, hp_lrGDPpc)

save(df_rGDP_states_a_raw, df_rGDP_states_q_raw,
     df_rGDPpc_states_a_raw,
     df_ni_states_a, df_ni_states_q,
     file = paste0(edir_atus, "ni_", tfirst, "_", tlast, "_state.Rdata"))

# load(file = paste0(edir_atus, "GDP_PI_", tfirst, "_", tlast, "_state.Rdata"))

rm(df_rPIpc_and_rPI_states_a_raw,
   df_rPIpc_and_rPI_states_a, df_rPIpc_states_a,
   df_rPI_states_a,
   df_rGDP_states_a_raw, df_rGDP_states_q_raw,
   df_rGDPpc_states_a_raw,
   df_rGDP_states_a, df_rGDP_states_q,
   df_rGDPpc_states_a)

