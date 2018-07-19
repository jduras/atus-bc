
# load individual level ATUS data
load(file = paste0(edir_atus, "atus_", tfirst, "_", tlast, "_individual.Rdata"))

df_timeuse_all %>%
    select(udur, reason) %$%
    table(udur, reason)

df_timeuse_all %>%
    mutate(lfs = case_when(working == 1 ~ "E",
                           unemp  == 1  ~ "U",
                           TRUE         ~ "I") %>%
               factor(levels = c("E","U","I"), ordered = TRUE)) %$%
    table(udur, lfs)


# calculate descriptive statistics as in Krueger and Mueller (2010) JPubE
#  - average job search time (min per day)
#  - job search participation rate (% of ATUS respondents time job search time > 0)
#  - average job search for job search participants (min per day, for ATUS respondents with time job search time > 0)
df_timeuse_all %>%
    filter(tuyear %in% c(2003:2007)) %>%
    # filter(tuyear %in% c(2013:2017)) %>%
    # filter(tudiaryday %in% c(2:6)) %>%
    # filter(tudiaryday %in% c(1,7)) %>%
    filter(age %in% c(20:65)) %>%
    select(tuyear, working, unemp, t_ahk_work_search, weight) %>%
    mutate(lfs = case_when(working == 1 ~ "E",
                           unemp   == 1 ~ "U",
                           TRUE         ~ "I") %>%
               factor(levels = c("E", "U", "I"), ordered = TRUE),
           t_ahk_work_search_gt_0 = if_else(t_ahk_work_search > 0, t_ahk_work_search, NA_real_) ) %>%
    select(lfs, t_ahk_work_search, t_ahk_work_search_gt_0, weight) %>%
    group_by(lfs) %>%
    summarise(avg_t_job_search = wtd.mean(t_ahk_work_search, w = weight, na.rm = TRUE)/7*60,
              participation_job_search = wtd.mean(as.integer(t_ahk_work_search > 0), w = weight, na.rm = TRUE)*100,
              avg_t_job_search_if_gt_0 = wtd.mean(t_ahk_work_search_gt_0, w = weight, na.rm = TRUE)/7*60)

