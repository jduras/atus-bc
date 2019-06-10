
message("Constructing categorical variables for age, education, household income")

load(file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_timeuse.Rdata"))

if (workage == "1865")
    df_timeuse_all %<>%
        filter(age >= 18 & age <= 65)

if (workage == "1865") age_breaks <-c(0, 17, 27, 37, 47, 57, 65, Inf)
if (workage == "all") age_breaks <- c(0, 14, 24, 34, 44, 54, 64, Inf)

educ_breaks <- c(0, 11, 12, 15, Inf)

faminc_breaks_1 <- c(0, 4, 7, 11, 13, 14, 15, 16)
faminc_labels_1 <- c("[0,12.5)", "[12.5,25)", "[25,50)", "[50,75)", "[75,100)", "[100,150)", "[150,Inf)")

faminc_breaks_2 <- c(0, 7, 11, 13, 14, 15, 16)
faminc_labels_2 <- c("[0,25)", "[25,50)", "[50,75)", "[75,100)", "[100,150)", "[150,Inf)")

faminc_breaks_3 <- c(0, 11, 13, 14, 15, 16)
faminc_labels_3 <- c("[0,50)", "[50,75)", "[75,100)", "[100,150)", "[150,Inf)")

faminc_breaks_4 <- c(0, 11, 14, 15, 16)
faminc_labels_4 <- c("[0,50)", "[50,100)", "[100,150)", "[150,Inf)")

df_timeuse_all %<>%
    filter(faminc > 0) %>%
    mutate(age_f = cut(age, breaks = age_breaks),
           educ_f = cut(grade, breaks = educ_breaks, include.lowest = TRUE),
           faminc_f_1 = cut(faminc, breaks = faminc_breaks_1, labels = faminc_labels_1, right = TRUE),
           faminc_f_2 = cut(faminc, breaks = faminc_breaks_2, labels = faminc_labels_2, right = TRUE),
           faminc_f_3 = cut(faminc, breaks = faminc_breaks_3, labels = faminc_labels_3, right = TRUE),
           faminc_f_4 = cut(faminc, breaks = faminc_breaks_4, labels = faminc_labels_4, right = TRUE))

save(df_timeuse_all, file = str_c(edir_atus, "atus_", tfirst, "_", tlast, "_individual.Rdata"))

rm(list = ls(pattern = "^faminc_"))
rm(list = ls(pattern = "_breaks$"))


# note: faminc    "Household Income (Combined income of all family members during the last 12 months)"
# -1    "Blank"
# -2    "Don't Know"
# -3    "Refused"
#  1    "Less than 5,000"
#  2     "5,000 to 7,499"
#  3     "7,500 to 9,999"
#  4    "10,000 to 12,499"
#  5    "12,500 to 14,999"
#  6    "15,000 to 19,999"
#  7    "20,000 to 24,999"
#  8    "25,000 to 29,999"
#  9    "30,000 to 34,999"
# 10    "35,000 to 39,999"
# 11    "40,000 to 49,999"
# 12    "50,000 to 59,999"
# 13    "60,000 to 74,999"
# 14    "75,000 to 99,999"
# 15    "100,000 to 149,999"
# 16    "150,000 and over"
