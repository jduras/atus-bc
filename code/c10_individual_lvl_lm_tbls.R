
# report results using stargazer: total shopping time only
df_lm_results_all %>%
    filter(activity == "t_shop_ttl") %$%
    # filter(lm_spc_number == "Model 7") %$%
    stargazer(lm_model, type = "text", out = paste0(odir_atus, "results_individual_lvl_ttl.tex"),
              title = "Models for Total Shopping Time",
              # clustered standard errors
              # se = lm_coefs %>% map(~ .x %$% std_error_clustered),
              se = map(lm_coefs, ~ .x %>% use_series(std_error_clustered)),
              # column.labels = paste("Model", c(1:7)),
              # single.row = TRUE, header = FALSE, column.sep.width = "0pt",
              omit = c("^factor\\(tudiaryday", "^demo_", "^factor\\(tuyear", "^factor\\(gestfips"),
              omit.labels = c("day of week dummies", "demographic controls", "year dummies", "state dummies"),
              # omit.stat = c("all"),
              omit.stat = c("ser","f"),
              dep.var.caption = "", dep.var.labels = "",
              covariate.labels = c("Constant",
                                   "Family Income \\\\ \\hspace*{0.125cm} \\$12,500 to \\$25,000","\\hspace*{0.125cm} \\$25,000 to \\$50,000",
                                   "\\hspace*{0.125cm} \\$50,000 to \\$75,000","\\hspace*{0.125cm} \\$75,000 to \\$100,000",
                                   "\\hspace*{0.125cm} \\$100,000 to \\$150,000", "\\hspace*{0.125cm} \\$150,000 or more",
                                   # "male", "black", "married", "have child", "spouse employed",
                                   # "age: 27-37", "age: 37-47", "age: 47-57", "age: 57-65",
                                   #"high school or less", "(some) college", "advanced degree",
                                   "Labor Force Status \\\\ \\hspace*{0.125cm} unemployed", "\\hspace*{0.125cm} retired", "\\hspace*{0.125cm} homemaker", "\\hspace*{0.125cm} student",
                                   "Unemployment Rate"
                                   #"Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
              ),
              intercept.bottom = FALSE, no.space = TRUE)


# report results using stargazer: main activities, for selected model(s)
df_lm_results_all %>%
    filter(activity %in% activity_chosen) %$%
    # filter(lm_spc_number == "Model 7") %$%
    stargazer(lm_model, type = "text", out = paste0(odir_atus, "results_individual_lvl_all.txt"),
              column.sep.width = "5pt",
              # clustered standard errors
              # se = map(lm_coefs, ~ .x %$% std_error_clustered),
              column.labels = as.character(activity_label),
              # single.row=TRUE, header=FALSE, column.sep.width="0pt",
              omit = c("^demo_", "^factor\\(tuyear", "^factor\\(gestfips", "^factor\\(tudiaryday"),
              omit.labels = c("demographic controls", "year dummies", "state dummies", "day of week dummies"),
              # omit.table.layout = "s",
              # omit.stat = c("all"),
              omit.stat = c("ser", "f"),
              # dep.var.caption = "", dep.var.labels = "",
              covariate.labels = c("Constant",
                                   "Family Income \\\\ \\hspace*{0.125cm} \\$12,500 to \\$25,000", "\\hspace*{0.125cm} \\$25,000 to \\$50,000",
                                   "\\hspace*{0.125cm} \\$50,000 to \\$75,000", "\\hspace*{0.125cm} \\$75,000 to \\$100,000",
                                   "\\hspace*{0.125cm} \\$100,000 to \\$150,000", "\\hspace*{0.125cm} \\$150,000 or more",
                                   # "male", "black", "married", "have child", "spouse employed",
                                   # "age: 27-37", "age: 37-47", "age: 47-57", "age: 57-65",
                                   # "high school or less", "(some) college", "advanced degree",
                                   "Labor Force Status \\\\ \\hspace*{0.125cm} unemployed", "\\hspace*{0.125cm} retired", "\\hspace*{0.125cm} homemaker", "\\hspace*{0.125cm} student",
                                   "Unemployment Rate"
                                   # "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
              ),
              intercept.bottom = FALSE, no.space = TRUE)
