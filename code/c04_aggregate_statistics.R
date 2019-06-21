
load(file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_individual.Rdata"))

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

# sample size by family income
df_timeuse_all %>%
    group_by(faminc_f_1, tuyear) %>%
    summarise(nobs = n(), weight = sum(weight)) %>%
    group_by(tuyear) %>%
    mutate(fraction = weight / sum(weight)) %>%
    group_by(faminc_f_1) %>%
    summarise(sum_nobs = sum(nobs), avg_weight = mean(weight), avg_fraction = mean(fraction))

# observations with more than 12 hours in day
t_cutoff <- 12
df_timeuse_all %>%
    select(tuyear, tucaseid, faminc, weight, starts_with("t_shop")) %>%
    gather(activity, timespent, starts_with("t_shop")) %>%
    filter(timespent > t_cutoff) %>%
    filter(!str_detect(activity, "t_shop_other_")) %>%
    mutate(tucaseid = as.character(tucaseid)) %>%
    arrange(tucaseid, desc(timespent)) %>%
    print(n = 100) %>%
    ggplot(aes(x = timespent, y = tucaseid, col = activity)) +
        geom_point(size = 2)

# summary statistics for time spent on different shopping related activities
df_timeuse_all %>%
    select(t_shop_groceries, t_shop_gas, t_shop_food, t_shop_other, t_shop_travel, t_shop_ttl, weight) %>%
    gather(activity, timespent, starts_with("t_")) %>%
    mutate(activity = str_replace(activity, "t_shop_", "")) %>%
    group_by(activity) %>%
    summarise_at(vars(timespent),
                 list(min,
                      q25 = ~wtd.quantile(., weights = weight, probs = 0.25, na.rm = TRUE),
                      q50 = ~wtd.quantile(., weights = weight, probs = 0.50, na.rm = TRUE),
                      q75 = ~wtd.quantile(., weights = weight, probs = 0.75, na.rm = TRUE),
                      max,
                      avg = ~questionr::wtd.mean(., weights = weight, na.rm = TRUE))) %>%
    mutate(activity_label = factor(activity,
                                   levels = c("groceries", "gas", "food", "other", "travel", "ttl"),
                                   labels = c("Groceries", "Gas", "Food (excluding groceries)", "Other", "Travel", "Total"))) %>%
    select(activity_label, everything()) %>%
    arrange(activity_label)


#### distributions of time spent on shopping related activities ####

# calculate fractions of individuals with nonzero amount of time spent
df_shopping_byactivity_byyear <-
    df_timeuse_all %>%
    select(tuyear, weight, t_shop_groceries, t_shop_gas, t_shop_food, t_shop_other, t_shop_travel, t_shop_ttl) %>%
    gather(activity, timespent, -c(weight, tuyear)) %>%
    mutate(tuyear = factor(tuyear),
           activity = str_replace(activity, "t_shop_", ""),
           activity_label = factor(activity,
                                   levels = c("groceries", "gas", "food", "other", "travel", "ttl"),
                                   labels = c("Groceries", "Gas", "Food (excl. groceries)", "Other", "Travel", "Total"), ordered = TRUE),
                                   # labels = c("Groceries", "Gas", "Food\n(excl. groceries)", "Other", "Travel", "Total"), ordered = TRUE),
           timespent = 60 * timespent)

df_timeuse_fractions_gt0 <-
    df_shopping_byactivity_byyear %>%
    group_by(activity, activity_label) %>%
    summarise(fractiongt0 = sum(weight[timespent > 0]) / sum(weight)) %>%
    ungroup()

df_timeuse_fractions_gt0_byyear <-
    df_shopping_byactivity_byyear %>%
    group_by(tuyear, activity, activity_label) %>%
    summarise(fractiongt0 = sum(weight[timespent > 0]) / sum(weight)) %>%
    ungroup()

# calculate densities - distributions of time spent on shopping related activities
df_shopping_byactivity_byyear_densities <-
    df_shopping_byactivity_byyear %>%
    filter(timespent > 0) %>%
    mutate(timespent = log(timespent)) %>%
    group_by(tuyear, activity, activity_label) %>%
    mutate(nweight = weight / sum(weight)) %>%
    group_modify(~ggplot2:::compute_density(x = .$timespent, w = .$nweight)) %>%
    ungroup() %>%
    rename(timespent = x) %>%
    mutate(timespent = exp(timespent))

# plot the distributions of time spent on shopping related activities - ridge plots
g <- ggplot() +
    scale_x_log10(expand = c(0.01, 0),
                  breaks = c(1, 2, 3, 5, 10, 15, 20, 30, 45, 60, 90, 120, 600)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    labs(x = "Time spent per day [mins]") +
    theme_ridges(font_size = 11, grid = TRUE) +
    theme(axis.title.y = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          strip.background = element_blank(),
          # strip.background = element_rect(size = 1, colour = "black", fill = NA),
          strip.text.x = element_text(face = "bold", hjust = 0))

# non-weighted densities
g %+% {df_shopping_byactivity_byyear %>%
    filter(timespent > 0)} +
        geom_density_ridges(aes(x = timespent, y = activity_label),
                            scale = 4, rel_min_height = 0.00001, alpha = 0.7) +
        geom_text(data = df_timeuse_fractions_gt0,
                  aes(x = 1.1, y = activity_label, label = str_c(percent_format(accuracy = 0.1)(fractiongt0), " non-zero")),
                  hjust = "left", vjust = -3)

# non-weighted densities by year
g %+% {df_shopping_byactivity_byyear %>%
        filter(timespent > 0)} +
    geom_density_ridges(aes(x = timespent, y = tuyear), scale = 5, alpha = 0.6, color = "white") +
    facet_wrap(~activity_label) +
    labs(title = "Time Spent on Shopping Related Activities")

# weighted densities - single plot for selected year
p <- g %+% {df_shopping_byactivity_byyear_densities %>%
        filter(tuyear == 2018)} +
    geom_density_ridges(aes(x = timespent, y = activity_label, height = density, fill = activity_label),
                        stat = "identity", scale = 5, alpha = 0.7, color = "white") +
    geom_text(data = df_timeuse_fractions_gt0_byyear %>%
                  filter(tuyear == 2018),
              aes(x = 0.25, y = activity_label, label = str_c(percent_format(accuracy = 0.1)(fractiongt0), " non-zero")),
              size = 5, hjust = "left", vjust = 0) +
    scale_fill_cyclical(values = c("gray70", "steelblue")) +
    # scale_fill_cyclical(values = c("#4040B0", "#9090F0")) +
    labs(title = "Time Spent on Shopping Related Activities in 2018") +
    theme_ridges(font_size = 20, grid = TRUE) +
    theme(axis.title.y = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          strip.background = element_blank(),
          # strip.background = element_rect(size = 1, colour = "black", fill = NA),
          strip.text.x = element_text(face = "bold", hjust = 0))
p

fig_file <- str_c(odir_atus, "fig_hist_t_shop_by_categories_2018.pdf")
ggsave(filename = fig_file, plot = p, width = 16, height = 9)

# weighted densities - facet plot
g %+% df_shopping_byactivity_byyear_densities +
    geom_density_ridges(aes(x = timespent, y = tuyear, height = density, fill = tuyear),
                        stat = "identity", scale = 5, alpha = 0.6, color = "white") +
    scale_fill_cyclical(values = c("gray70", "steelblue")) +
    # scale_fill_cyclical(values = c("#4040B0", "#9090F0")) +
    labs(title = "Time Spent on Shopping Related Activities") +
    facet_rep_wrap(~activity_label, repeat.tick.labels = TRUE)

# weighted densities - single plot for selected activity
activity_selected <- "gas"
g %+% {df_shopping_byactivity_byyear_densities %>%
        filter(activity == activity_selected)} +
    geom_density_ridges(aes(x = timespent, y = tuyear, height = density, fill = tuyear),
                        stat = "identity", scale = 5, alpha = 0.6, color = "white") +
    # scale_fill_cyclical(values = c("gray70", "steelblue")) +
    scale_fill_cyclical(values = c("#4040B0", "#9090F0")) +
    labs(title = "Time Spent on Shopping Related Activities",
         subtitle = df_shopping_byactivity_byyear_densities %>%
             filter(activity == activity_selected) %>%
             slice(1) %>%
             pull(activity_label) %>%
             as.character())



#### weighted averages for 2005-2007, 2008-2010, 2011-2013, 2014-2016, 2017-2018 ####

df_timeuse_all %>%
    select(tuyear, starts_with("t_shop"), weight) %>%
    mutate(tuperiod = case_when(tuyear %in% 2003:2004 ~ 1,
                                tuyear %in% 2005:2007 ~ 2,
                                tuyear %in% 2008:2010 ~ 3,
                                tuyear %in% 2011:2013 ~ 4,
                                tuyear %in% 2014:2016 ~ 5,
                                tuyear %in% 2017:2018 ~ 6) %>%
               factor(labels = c("2003-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2018"))) %>%
    gather(activity, timespent, starts_with("t_")) %>%
    group_by(activity, tuperiod) %>%
    summarise_at(vars(timespent), funs(questionr::wtd.mean(., weights  = weight, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(activity = str_replace(activity, "t_shop_", ""),
           activity_label = factor(activity,
                                   levels = c("groceries", "gas", "food", "other", "travel", "ttl",
                                              "ggf", "other_shop", "other_research", "other_wait"),
                                   labels = c("Groceries", "Gas", "Food (excluding groceries)", "Other", "Travel", "Total",
                                              "Groceries, gas, food", "Other: shopping", "Other: research", "Other: waiting"))) %>%
    spread(tuperiod, timespent) %>%
    arrange(activity_label)



#### average time spent on various activities by year ####

# prepare data to plot average time spent on various activities by year
df_timeuse_shop_avg <-
    df_timeuse_all %>%
    select(tuyear, weight, starts_with("t_")) %>%
    group_by(tuyear) %>%
    summarise_at(vars(-weight),
                 list(fracgt0 = ~if_else(. > 0, 1, 0) %>% wtd.mean(., w = weight, na.rm = TRUE),
                      mean = ~wtd.mean(., w = weight, na.rm = TRUE),
                      meangt0 = ~if_else(. > 0, ., NA_real_) %>% wtd.mean(., w = weight, na.rm = TRUE),
                      median = ~wtd.quantile(., w = weight, probs = 0.5, type = "quantile", na.rm = TRUE),
                      mediangt0 = ~if_else(. > 0, ., NA_real_) %>% wtd.quantile(., w = weight, probs = 0.5, type = "quantile", na.rm = TRUE))) %>%
    ungroup() %>%
    select(tuyear, starts_with("t_shop"), -starts_with("t_shop_other_")) %>%
    select(-starts_with("t_shop_ggf")) %>%
    # select(-t_shop_food, -t_shop_groceries) %>%
    # select(tuyear, t_shop_ttl) %>%
    gather(activity, value, -tuyear) %>%
    mutate(measure = str_extract(activity, "([a-z0-9]*)$"),
           measure_label = recode(measure,
                                  "fracgt0" = "Fraction of individuals with time > 0",
                                  "mean" = "Average time",
                                  "meangt0" = "Average time, only individuals with time > 0",
                                  "median" = "Median time",
                                  "mediangt0" = "Median time for individuals with time > 0") %>% as.factor(),
           activity = str_replace_all(activity, c("t_shop_" = "", "_fracgt0" = "", "_meangt0" = "", "_mean" = "", "_mediangt0" = "", "_median" = "")),
           activity_label = factor(activity,
                                   levels = c("groceries", "gas", "food", "other", "travel", "ttl",
                                              "ggf", "other_shop", "other_research", "other_wait"),
                                   labels = c("Groceries", "Gas", "Food (excl. groceries)", "Other", "Travel", "Total",
                                              "Groceries, gas, food", "Other: shopping", "Other: research", "Other: waiting")),
           activity_label = factor(activity_label)) %>%
    group_by(activity, measure) %>%
    mutate(index_2003 = 100 * value / value[tuyear == "2003"]) %>%
    ungroup()

g <- ggplot() +
    geom_rect(data = tibble(Start = 2008, End = 2010),
              aes(xmin = 2008, xmax = 2010, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
    scale_x_continuous(breaks = seq(from = 2004, to = 2016, by = 2)) +
    labs(title = "Average Hours Per Day Spent on Shopping Related Activities",
         x = "", y = "",
         col = "Shopping Activity") +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(plot.margin = unit(c(0, 3, 0, 0), "lines"),
          legend.position = "none",
          strip.text.x = element_text(face = "bold", color = "black", hjust = 0)) +
    facet_wrap(~measure_label, scales = "free")

g %+% df_timeuse_shop_avg +
    geom_line(aes(x = tuyear, y = value, col = activity_label), size = 1.02) +
    geom_point(aes(x = tuyear, y = value, col = activity_label)) +
    theme(legend.position = c(0.8, 0.25))

g %+% {df_timeuse_shop_avg %>%
    filter(measure != "median")} +
    geom_hline(aes(yintercept = 100), linetype = "dotted") +
    geom_line(aes(x = tuyear, y = index_2003, col = activity_label), size = 1.02) +
    geom_point(aes(x = tuyear, y = index_2003, col = activity_label)) +
    geom_text(data = df_timeuse_shop_avg %>% filter(tuyear == 2017, measure != "median"),
              aes(x = 2017.25, y = index_2003, label = activity_label, col = activity_label),
              hjust = "left", vjust = "center", fontface = "bold") +
    labs(subtitle = "Index (2003 = 100)")

# plot average time spent on various activities by year - all actitivies in single plot
p <- g %+% {df_timeuse_shop_avg %>%
        # filter(tuyear <= 2015) %>%
        filter(measure == "mean")} +
        geom_hline(aes(yintercept = 100), linetype = "dotted") +
        geom_line(aes(x = tuyear, y = index_2003, col = activity_label), size = 1.02) +
        geom_point(aes(x = tuyear, y = index_2003, col = activity_label)) +
        geom_text(data = df_timeuse_shop_avg %>% filter(tuyear == 2017, measure == "mean"),
                  aes(x = 2017.25, y = index_2003, label = activity_label, col = activity_label),
                  hjust = "left", vjust = "center", fontface = "bold") +
        # scale_x_date(breaks = seq(from = as.Date("2004-01-01"), to = as.Date("2016-12-31"), by = "10 years"), date_labels = "%Y") +
        labs(subtitle = "Index (2003 = 100)")
p

fig_file <- str_c(odir_atus, "fig_agg_t_shop_by_categories_single.pdf")
ggsave(filename = fig_file, plot = p, width = 5.5, height = 4.25)
# ggsave(filename = fig_file, plot = p, width = 4.675, height = 6.05)

# plot average time spent on various activities by year - facet plot
p <- g %+% {df_timeuse_shop_avg %>%
        # filter(tuyear <= 2015) %>%
        filter(measure == "mean")} +
        geom_hline(aes(yintercept = 100), linetype = "dotted") +
        geom_line(aes(x = tuyear, y = index_2003), size = 1.02) +
        labs(subtitle = "Index (2003 = 100)") +
        facet_rep_wrap(~activity_label, ncol = 2, repeat.tick.labels = TRUE) +
        theme(strip.background = element_blank())
p

fig_file <- str_c(odir_atus, "fig_agg_t_shop_by_categories.pdf")
# ggsave(filename = fig_file, plot = p, width = 4.25, height = 5.5)
ggsave(filename = fig_file, plot = p, width = 4.675, height = 6.05)



#### effect of recession in aggregate data ####

# construct the dataset to estimate simple OLS models with a single dummy variable d20082010 to construct average time on activities during
#  1) pre-recession period 2005-2010 vs recession period 2008-2010
#  2) post-recession period 2011-2013 vs recession period 2008-2010
# note: Petrosky-Nadeau, Wasmer and Zeng (2016) EL restict the sample to those with age 25-54
# this causes the 2005-2007 vs 2008-2010 change in the average time spent on groceries for U to become insignificant
# the main reason appears to be the fact that the sample gets very small
df_timeuse_all_lm_whole <-
    df_timeuse_all %>%
    mutate(tuperiod = case_when(tuyear %in% 2005:2007 ~ "2005_2007",
                                tuyear %in% 2008:2010 ~ "2008_2010",
                                TRUE                  ~ NA_character_),
           d20082010 = (tuyear %in% 2008:2010) %>% as.integer(),
           included = (age >= 25 & age < 55),
           lfs = case_when(working   == 1 ~ "E",
                           unemp     == 1 ~ "U",
                           student   == 1 ~ "I",
                           retired   == 1 ~ "I",
                           homemaker == 1 ~ "I",
                           disabled  == 1 ~ "I",
                           othernilf == 1 ~ "I",
                           TRUE           ~ NA_character_)) %>%
    select(gestfips, tuyear, tuperiod, d20082010, lfs, included, starts_with("t_"), -starts_with("t_shop_other_"), weight)

df_timeuse_all_lm_whole %>%
    filter(tuyear %in% 2005:2010) %>%
    filter(t_shop_groceries > 0) %$%
    table(lfs, included, tuperiod)

# duplicate the dataset, to get results also for the whole sample, not just split by labor force
df_timeuse_all_lm_whole <-
    bind_rows(df_timeuse_all_lm_whole,
              df_timeuse_all_lm_whole %>%
                  mutate(lfs = "all"))

# split the sample by period and estimate a simple OLS with a single dummy variable d20082010 to construct average time on activities during
df_timeuse_all_lm_results <-
    bind_rows(df_timeuse_all_lm_whole %>%
                  filter(included == TRUE,
                         tuyear %in% 2005:2010) %>%
                  mutate(tusample = "2005_2010_restricted"),
              df_timeuse_all_lm_whole %>%
                  filter(included == TRUE,
                         tuyear %in% 2008:2013) %>%
                  mutate(tusample = "2008_2013_restricted"),
              df_timeuse_all_lm_whole %>%
                  filter(tuyear %in% 2005:2010) %>%
                  mutate(tusample = "2005_2010_whole"),
              df_timeuse_all_lm_whole %>%
                  filter(tuyear %in% 2008:2013) %>%
                  mutate(tusample = "2008_2013_whole")) %>%
    select(-tuyear, -tuperiod, -included) %>%
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
           activity_label = factor(activity,
                                   levels = c("groceries", "gas", "food", "ggf", "other", "travel", "ttl"),
                                   labels = c("Groceries", "Gas", "Food (excluding groceries)", "Groceries, gas, food", "Other", "Travel", "Total"))) %>%
    ggplot(aes(x = estimate, y = activity_label)) +
        geom_point() +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_errorbarh(aes(xmin = conf_low_clustered, xmax = conf_high_clustered, height = 0.2), col = "red") +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        labs(x = "", y = "") +
        facet_grid(lfs ~ tusample) +
        # facet_rep_grid(lfs ~ tusample, repeat.tick.labels = "x") +
        theme_light() +
        theme(# strip.background = element_blank(),
            strip.text.x = element_text(color = "black"),
            strip.text.y = element_text(color = "black"),
            strip.placement = "outside")








#### aggregate statistics by income ####

# plot average time spent on shopping related activities by income group
df_avg_time_shopping_by_income <-
    df_timeuse_all %>%
    select(faminc, weight, starts_with("t_shop")) %>%
    gather(activity, timespent, -c(faminc, weight)) %>%
    mutate(faminc = faminc %>% as.factor(),
           timespentgt0 = if_else(timespent > 0, timespent, NA_real_)) %>%
    group_by(activity, faminc) %>%
    summarise_at(vars(timespent, timespentgt0),
                 funs(mean   = wtd.mean(., w = weight, na.rm = TRUE),
                      median = wtd.quantile(., w = weight, probs = 0.5, type = "quantile", na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(activity = str_replace(activity, "t_shop_", ""),
           activity_label = factor(activity,
                                   levels = c("groceries", "gas", "food", "ggf", "other", "travel", "ttl",
                                              "other_shop", "other_research", "other_wait"),
                                   labels = c("Groceries", "Gas", "Food (ex. groceries)", "Groceries, gas, food", "Other", "Travel", "Total",
                                              "Other: shopping", "Other: research", "Other: waiting"))) %>%
    gather(measure, value, starts_with("timespent")) %>%
    separate(measure, into = c("variable", "measure"))

df_avg_time_shopping_by_income %>%
    # filter(value <= 20) %>%
    filter(measure == "mean") %>%
    ggplot(aes(x = faminc, y = value, col = variable)) +
        geom_point() +
        scale_color_manual(values = c("red", "gray50"),
                           labels = c("average time spent", "average time spent conditional on non-zero time spent")) +
        labs(x = "Household Income Group", y = "Time (hours per Day)",
             col = "") +
        # ylim(c(0, 12)) +
        facet_wrap(~ activity_label, nrow = 1) +
        theme(legend.position = "top",
              legend.justification = "left")

# plot average time spent on shopping related activities by income group
df_avg_time_shopping_by_income_and_year <-
    df_timeuse_all %>%
    select(tuyear, faminc, weight, starts_with("t_shop")) %>%
    gather(activity, timespent, -c(tuyear, faminc, weight)) %>%
    mutate(faminc = faminc %>% as.factor(),
           timespentgt0 = if_else(timespent > 0, timespent, NA_real_)) %>%
    group_by(tuyear, activity, faminc) %>%
    summarise(fracgt0 = if_else(timespent > 0, 1, 0) %>% wtd.mean(w = weight, na.rm = TRUE),
              timespent_mean = wtd.mean(timespent, w = weight, na.rm = TRUE),
              timespentgt0_mean = wtd.mean(timespentgt0, w = weight, na.rm = TRUE),
              weight = sum(weight, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(activity = str_replace(activity, "t_shop_", ""),
           activity_label = factor(activity,
                                   levels = c("groceries", "gas", "food", "ggf", "other", "travel", "ttl",
                                              "other_shop", "other_research", "other_wait"),
                                   labels = c("Groceries", "Gas", "Food (ex. groceries)", "Groceries, gas, food", "Other", "Travel", "Total",
                                              "Other: shopping", "Other: research", "Other: waiting"))) %>%
    gather(measure, value, c("fracgt0", "timespent_mean", "timespentgt0_mean")) %>%
    separate(measure, into = c("variable", "measure"))

df_avg_time_shopping_by_income_and_year_relative <-
    df_avg_time_shopping_by_income_and_year %>%
    filter(activity != "ggf") %>%
    mutate(weight = weight / 1000000) %>%
    group_by(activity, variable, measure) %>%
    mutate(value_relative_to_2003 = value / value[tuyear == 2003]) %>%
    ungroup()

g <-  ggplot() +
    scale_color_manual(values = colorRampPalette(colors = brewer.pal(9, "Blues")[c(9, 3)])(16)) +
    facet_wrap(~activity, scales = "free")

# plot average timespet
p <- g %+% {df_avg_time_shopping_by_income_and_year_relative %>%
        filter(variable == "timespent")} +
            geom_point(aes(x = tuyear, y = value, col = faminc)) +
            geom_line(aes(x = tuyear, y = value, col = faminc), linetype = "dotted")
p
ggplotly(p)

# plot fraction with non-zero timespent
p <- g %+% {df_avg_time_shopping_by_income_and_year_relative %>%
        filter(variable == "fracgt0")} +
            geom_point(aes(x = tuyear, y = value, col = faminc)) +
            geom_line(aes(x = tuyear, y = value, col = faminc), linetype = "dotted")
p
ggplotly(p)

# plot weights
p <- g %+% {df_avg_time_shopping_by_income_and_year_relative %>%
        filter(variable == "timespentgt0")} +
            geom_point(aes(x = tuyear, y = weight, col = faminc)) +
            geom_line(aes(x = tuyear, y = weight, col = faminc), linetype = "dotted") +
            scale_y_log10() +
            facet_wrap(~activity, scales = "free")
p
ggplotly(p)





df_timeuse_all %>%
    filter(tuyear == 2016, faminc == 4, t_shop_food > 0) %>%
    arrange(desc(t_shop_food)) %>%
    select(-starts_with("faminc")) %>%
    select("t_shop_food", starts_with("t_"), everything(), ) %>%
    print(n = 50)

# more than one hour sent on "food"
df_timeuse_all %>%
    filter(tuyear == 2016, faminc == 4, t_shop_food > 1) %>%
    select("t_shop_food", starts_with("t_"), everything()) %>%
    glimpse()

g <- ggplot() +
    geom_line(aes(x = tuyear, y = value, col = variable), linetype = "dotted") +
    geom_point(aes(x = tuyear, y = value, col = variable)) +
    facet_wrap(~ faminc) +
    theme(legend.position = "top",
          legend.justification = "left")

g %+% {df_avg_time_shopping_by_income_and_year %>%
        filter(measure == "mean") %>%
        filter(activity == "food")} +
    scale_color_manual(values = c("red", "gray50"),
                       labels = c("average time spent", "average time spent conditional on non-zero time spent")) +
    labs(x = "", y = "Time (hours per Day)", col = "")

g %+% {df_avg_time_shopping_by_income_and_year %>%
        filter(variable == "fracgt0") %>%
        filter(activity == "food")} +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "", y = "", col = "", title = "Fraction of sample with non-zero time spent shopping for food by family income group") +
    theme(legend.position = "none")

g <- ggplot() +
        geom_point(aes(x = faminc, y = value, col = variable)) +
        scale_color_manual(values = c("red", "gray50"),
                           labels = c("average time spent", "average time spent conditional on non-zero time spent")) +
        labs(x = "Household Income Group", y = "Time (hours per Day)", col = "") +
        # facet_grid(tuyear ~ activity_label) +
        facet_wrap(~ tuyear) +
        theme(legend.position = "top",
              legend.justification = "left")

g %+% {df_avg_time_shopping_by_income_and_year %>%
        filter(measure == "mean") %>%
        filter(activity == "food") %>%
        filter(tuyear >= 2012)} +
    labs(title = "Time spent shopping for food by income group")

df_avg_time_shopping_by_income_and_year %>%
    group_by(activity, faminc, variable, measure) %>%
    mutate(value_relative_to_2003 = value / value[tuyear == 2003]) %>%
    ungroup() %>%
    filter(tuyear >= 2014, activity == "food") %>%
    arrange(desc(value_relative_to_2003)) %>%
    print(n = 130)

df_avg_time_shopping_by_income_and_year %>%
    group_by(tuyear, activity, variable, measure) %>%
    summarise(value = wtd.mean(value, w = weight, na.rm = TRUE),
              weight = sum(weight, na.rm = TRUE) / 1000000000) %>%
    ungroup() %>%
    group_by(activity, variable, measure) %>%
    mutate(value_relative_to_2003 = value / value[tuyear == 2003]) %>%
    ungroup() %>%
    filter(tuyear >= 2014, activity == "food") %>%
    arrange(desc(value_relative_to_2003)) %>%
    print(n = 130)

df_avg_time_shopping_by_income_and_year %>%
    group_by(faminc, activity, variable, measure) %>%
    mutate(value_relative_to_2003 = value / value[tuyear == 2003]) %>%
    ungroup() %>%
    filter(tuyear %in% 2015:2016, activity == "food") %>%
    arrange(faminc, variable, tuyear) %>%
    print(n = 130)

# plot average time spent on shopping related activities by income group relative to the lowest income group
df_avg_time_shopping_by_incomegrp <-
    df_timeuse_all %>%
    select(faminc = faminc_f_1,
           weight, starts_with("t_shop")) %>%
    gather(activity, timespent, -c(faminc, weight)) %>%
    filter(!str_detect(activity, "t_shop_other_")) %>%
    filter(!str_detect(activity, "t_shop_ggf")) %>%
    mutate(faminc = faminc %>% as.factor()) %>%
    group_by(activity, faminc) %>%
    summarise_at(vars(timespent),
                 funs(mean = wtd.mean(., w = weight, na.rm = TRUE),
                      sd = wtd.var(., w = weight, na.rm = TRUE)^0.5)) %>%
    group_by(activity) %>%
    mutate(mean = mean / mean[as.numeric(faminc) == 1]) %>%
    ungroup() %>%
    print(n = 150) %>%
    mutate(faminc_num = as.numeric(faminc),
           activity = str_replace(activity, "t_shop_", ""),
           activity_label = factor(activity,
                                   levels = c("groceries", "gas", "food", "other", "travel", "ttl"),
                                   labels = c("Groceries", "Gas", "Food (ex. groceries)", "Other", "Travel", "Total"))) %>%
    mutate(faminc_label = faminc %>%
               str_replace(",Inf", "+") %>%
               str_replace(",", " - ") %>%
               str_replace_all("[\\[()]", ""),
           faminc = factor(faminc, labels = c(" < 12.5", "12.5-25", "25-50", "50-75", "75-100", "100-150", " > 150")))

df_avg_time_shopping_by_incomegrp_labels <-
    df_avg_time_shopping_by_incomegrp %>%
    filter(faminc_num == max(faminc_num))

df_avg_time_shopping_by_incomegrp_total <-
    df_avg_time_shopping_by_incomegrp %>%
    filter(activity == "ttl")

p <- df_avg_time_shopping_by_incomegrp %>%
    ggplot(aes(x = faminc, y = mean, group = activity_label)) +
        geom_line(size = 1, color = "gray") +
        geom_line(data = df_avg_time_shopping_by_incomegrp_total, size = 1, color = "red") +
        geom_point(size = 3, color = "gray") +
        geom_point(data = df_avg_time_shopping_by_incomegrp_total, size = 3, color = "red") +
        geom_text(data = df_avg_time_shopping_by_incomegrp_labels,
                  aes(x = 7.2, y = (mean + c(0, 0.01, 0, -0.01, -0.01, 0.01)), label = activity_label), color = "gray",
                  hjust = "left", vjust = "center") +
        geom_text(data = df_avg_time_shopping_by_incomegrp_labels %>% filter(activity == "ttl"),
                  aes(x = 7.2, y = mean + 0.01, label = activity_label), color = "red",
                  hjust = "left", vjust = "center") +
        labs(title = "Average Time Spent on Time Related Activities",
             subtitle = "Relative to Households with Income less than 12.5 thousand dollars",
             x = "Household Income (in thousands)", y = "", col = "") +
        coord_cartesian(clip = "off") +
        theme_minimal() +
        theme(text = element_text(size = 16),
              legend.position = "none",
              plot.margin = unit(c(0, 6, 0, 0), "lines"))
p

fig_file <- str_c(odir_atus, "fig_ind_t_shop_by_categories.pdf")
# ggsave(filename = fig_file, plot = p, width = 4.25, height = 5.5)
ggsave(filename = fig_file, plot = p, width = 8, height = 5)


# generate aggregates (weighted averages) for each year
df_agg <-
    df_timeuse_all %>%
    mutate(num_child_gt_0 = if_else(num_child > 0, num_child, NA_real_)) %>%
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
    summarise_at(vars(-weight), list(~wtd.mean(., w = weight, na.rm = TRUE)))

# shares: groups by family income over time
p <- df_agg %>%
    select(tuyear, starts_with("faminc")) %>%
    filter(tuyear > 2003) %>%
    gather(measure, value, -tuyear) %>%
    group_by(tuyear) %>%
    mutate(inc_grp_num = row_number()) %>%
    ggplot(aes(x = tuyear, y = value, col = reorder(measure, inc_grp_num))) +
        geom_line() +
        # scale_colour_discrete(labels = c("0 to 12.5", "12.5 to 25", "25 to 50", "50 to 75", "75 to 100", "100 to 150", "150 and over")) +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        scale_colour_brewer(palette = "Set1") +
        # scale_colour_manual(labels = c("0 to 12.5", "12.5 to 25", "25 to 50", "50 to 75", "75 to 100", "100 to 150", "150 and over"),
        #                     values = colorRampPalette(brewer.pal(9,"Blues"))(9)[3:9]) +
        labs(title = "Sample shares by family income",
             x = "", y = "sample share",
             col = "Family Income (in thousands)") +
        theme_minimal()
p
ggplotly(p)

# generate aggregates (weighted averages) by family income for each year
df_agg_by_income <-
    inner_join(
        # variables summarised using weighted average
        df_timeuse_all %>%
            mutate(num_child_gt_0 = if_else(num_child > 0, num_child, NA_real_)) %>%
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
            summarise_at(vars(-weight), list(~wtd.mean(., w = weight, na.rm = TRUE))) %>%
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
            df_agg_by_income %>%
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
    mutate(row_number = recode(measure, "share"          = 1,
                               "male"           = 2,
                               "black"          = 3,
                               "age"            = 4,
                               "age_f(0,17]"    = 5,
                               "age_f(17,27]"   = 6,
                               "age_f(27,37]"   = 7,
                               "age_f(37,47]"   = 8,
                               "age_f(47,57]"   = 9,
                               "age_f(57,65]"   = 10,
                               "age_f(65,Inf]"  = 11,
                               "educ_f[0,11]"   = 12,
                               "educ_f(11,12]"  = 13,
                               "educ_f(12,15]"  = 14,
                               "educ_f(15,Inf]" = 15,
                               "married"        = 16,
                               "spouse_emp"     = 17,
                               "hv_child"       = 18,
                               "num_child"      = 19,
                               "num_child_gt_0" = 20,
                               "working"        = 21,
                               "unemp"          = 22,
                               "student"        = 23,
                               "retired"        = 24,
                               "homemaker"      = 25,
                               "disabled"       = 26)) %>%
    arrange(row_number) %>%
    mutate_if(is.numeric, funs(sprintf(fmt = "%8.4f", .))) %>%
    select(-row_number) %>%
    filter(!(measure %in% c("age.f(0,17]", "age.f(65,Inf]"))) %>%
    print(n = 50)

# plot average aggregate demographic statistics for whole population and by family income
df_agg_stats_to_plot <-
    df_agg_stats %>%
    gather(group, value, -measure) %>%
    filter(!(measure %in% c("age", "share", "num_child", "retired", "student"))) %>%
    filter(!str_detect(measure, "age_f")) %>%
    filter(!str_detect(measure, "educ_f")) %>%
    mutate(group = factor(group, levels = c("whole_population",
                                            "faminc_0_to_12.5",
                                            "faminc_12.5_to_25",
                                            "faminc_25_to_50",
                                            "faminc_50_to_75",
                                            "faminc_75_to_100",
                                            "faminc_100_to_150",
                                            "faminc_150_and_over"),
                          labels = c("whole_population", "0-12.5", "12.5-25", "25-50", "50-75", "75-100", "100-150", "150+"),
                          ordered = TRUE),
           measure = factor(measure,
                            levels = c("married", "spouse_emp", "hv_child", "num_child_gt_0",
                                       "working", "unemp", "homemaker", "disabled",
                                       "black", "male"),
                            ordered = TRUE))

df_agg_stats_to_plot %>%
    filter(group != "whole_population") %>%
    ggplot(aes(x = group, y = value)) +
        geom_point() +
        geom_hline(data = df_agg_stats_to_plot %>%
                       filter(group == "whole_population"),
                   aes(yintercept = value), col = "red", linetype = "dashed", alpha = 0.5) +
        # geom_text(data = df_agg_stats_to_plot %>%
        #               filter(group == "whole_population"),
        #           aes(x = 0, y = value), label = "population average", col = "red", alpha = 0.5, hjust = 0, vjust = 0) +
        labs(x = "", y = "",
             title = "Demographic statistics by family income vs population average") +
        # coord_flip() +
        facet_wrap(~measure, scales = "free")
