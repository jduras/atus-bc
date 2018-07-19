
library(Quandl)
Quandl.api_key('DLk9RQrfTVkD4UTKc7op')

# library(tidyquant)
# quandl_api_key('DLk9RQrfTVkD4UTKc7op')

state.names <- c("UNITEDSTATES","ALABAMA","ALASKA","ARIZONA",
                 "ARKANSAS","CALIFORNIA","COLORADO","CONNECTICUT","DELAWARE","DISTRICTOFCOLUMBIA","FLORIDA","GEORGIA",
                 "HAWAII","IDAHO","ILLINOIS","INDIANA","IOWA","KANSAS","KENTUCKY","LOUISIANA",
                 "MAINE","MARYLAND","MASSACHUSETTS","MICHIGAN","MINNESOTA","MISSISSIPPI","MISSOURI","MONTANA",
                 "NEBRASKA","NEVADA","NEWHAMPSHIRE","NEWJERSEY","NEWMEXICO","NEWYORK","NORTHCAROLINA","NORTHDAKOTA",
                 "OHIO","OKLAHOMA","OREGON","PENNSYLVANIA","RHODEISLAND","SOUTHCAROLINA","SOUTHDAKOTA","TENNESSEE",
                 "TEXAS","UTAH","VERMONT","VIRGINIA","WASHINGTON","WESTVIRGINIA","WISCONSIN","WYOMING")

state.codes <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA",
                 "HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM",
                 "NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")

codes.rGDPpc.A <- paste0("BEA/GSP_NAICS_ALL_PC_",state.names)
codes.rGDP.A <- paste0("BEA/GSP_NAICS_ALL_R_",state.names)
codes.rGDP.Q <- paste0("FRED/",state.codes,"RQGSP")


df.rGDPpc.states.A.raw <- Quandl(codes.rGDPpc.A)
df.rGDP.states.A.raw <- Quandl(codes.rGDP.A)
df.rGDP.states.Q.raw <- Quandl(codes.rGDP.Q)



# real GDP per capita in chained 2009 dollars, and real GDP in millions of chained 2009 dollars

df.rPIpc.states.A <-
   read_csv(paste0(ddir.atus,"rPIpc.A.csv")) %>%
   gather(tuyear, rPIpc, -c(gestfips,state_name,state_code)) %>%
   select(-c(state_name,state_code)) %>%
   mutate_at(vars(rPIpc, tuyear), as.numeric)

# annual real GDP for Nebraska not updated on Quandl as of 2018/1/5 - get that one from BEA data downloaded manually
df.rGDP.A.NE <-
   read_csv(paste0(ddir.atus,"rGDP.A.csv")) %>%
   gather(tuyear, rGDP, -c(gestfips,state_name,state_code)) %>%
   filter(state_code=="NE") %>%
   select(-c(state_name,state_code)) %>%
   mutate_if(is.character, as.numeric)

# state level real GDP, per capita, annual
df.rGDPpc.states.A <-
    df.rGDPpc.states.A.raw  %>%
    as.tibble() %>%
    gather(state, rGDPpc, -Date) %>%
    mutate(tuyear = year(Date)) %>%
    select(-Date) %>%
    mutate(state = str_to_lower(str_extract(state, "(?<=\\_)[^\\_]*(?=\\ -)"))) %>%
    inner_join(df.state.codes, by="state") %>%
    select(-state,-state_code)

# state level real GDP, annual
df.rGDP.states.A <-
   df.rGDP.states.A.raw  %>%
   select(Date, ends_with("All industry total")) %>%
   gather(state, rGDP, -Date) %>%
   mutate(state = str_to_lower(str_extract(state, "(?<=\\_)[^\\_]*(?=\\ -)"))) %>%
   inner_join(df.state.codes, by="state") %>%
   mutate(tuyear = year(Date)) %>%
   select(-c(Date,state,state_name)) %>%
   full_join(df.rGDP.A.NE, by=c("gestfips","tuyear"), suffix=c("",".NE")) %>%
   mutate(rGDP = ifelse(state_code=="NE", rGDP.NE, rGDP)) %>%
   select(-rGDP.NE) %>%
   full_join(df.rPIpc.states.A %>% select(tuyear,gestfips,rPIpc), by=c("tuyear","gestfips")) %>%
   inner_join(df.rGDPpc.states.A %>% select(tuyear,gestfips,rGDPpc), by=c("tuyear","gestfips")) %>%
   mutate(pop = rGDP*1000000/rGDPpc) %>%
   arrange(gestfips, tuyear) %>%
   group_by(gestfips) %>%
   mutate(gr.rGDPpc = log(rGDPpc)-lag(log(rGDPpc))) %>%
   mutate(hp.lrGDPpc = hpfilter(log(rGDPpc), freq=6.25, type="lambda")$cycle) %>%
   ungroup() %>%
   select(tuyear,gestfips,pop,rPIpc,rGDP,rGDPpc,gr.rGDPpc,hp.lrGDPpc)

# state level real GDP, quarterly
df.rGDP.states.Q <-
   df.rGDP.states.Q.raw  %>%
   gather(state_code, rGDP, -Date) %>%
   mutate(state_code = str_extract(state_code, "(?<=D\\.).*(?=RQ)")) %>%
   inner_join(df.state.codes, by="state_code") %>%
   mutate(tuyear = year(Date),
          tuquarter = quarter(Date)) %>%
   select(-c(Date,state,state_name)) %>%
   inner_join(df.rGDP.states.A %>% select(tuyear,gestfips,pop), by=c("tuyear","gestfips")) %>%
   mutate(rGDPpc = 1000000*rGDP/pop) %>%
   arrange(gestfips, tuyear, tuquarter) %>%
   group_by(gestfips) %>%
   mutate(gr.rGDPpc = log(rGDPpc)-lag(log(rGDPpc))) %>%
   mutate(hp.lrGDPpc = hpfilter(log(rGDPpc), freq=1600, type="lambda")$cycle) %>%
   ungroup() %>%
   select(tuyear,tuquarter,gestfips,pop,rGDP,rGDPpc,gr.rGDPpc,hp.lrGDPpc)


# data on state level unemployment rate

codes.UR <- paste0("FRED/",state.codes,"UR")

df.UR.states.M.raw <- Quandl(codes.UR)

df.UR.states.M <-
   df.UR.states.M.raw %>%
   gather(state_code, u3rate, -Date) %>%
   mutate(tuyear = year(Date),
          tuquarter = quarter(Date),
          tumonth = month(Date)) %>%
   select(-Date) %>%
   mutate(state_code = replace(state_code, TRUE, str_sub(state_code,6,7))) %>%
   arrange(state_code, tuyear, tumonth) %>%
   inner_join(df.state.codes, by="state_code") %>%
   select(-c(state_name,state)) %>%
   select(tuyear,tumonth,tuquarter,gestfips,u3rate)

df.UR.states.Q <-
   df.UR.states.M %>%
   filter(tuyear >= 2003 & tuyear <= 2016) %>%
   group_by(gestfips, tuyear, tuquarter) %>%
   summarise_at(vars(u3rate), funs(mean)) %>%
   ungroup() %>%
   arrange(gestfips, tuyear, tuquarter) %>%
   select(tuyear,tuquarter,gestfips,u3rate)

df.UR.states.A <-
   df.UR.states.M %>%
   filter(tuyear >= 2003 & tuyear <= 2016) %>%
   group_by(gestfips, tuyear) %>%
   summarise_at(vars(u3rate), funs(mean)) %>%
   ungroup() %>%
   arrange(gestfips, tuyear) %>%
   select(tuyear,gestfips,u3rate)


save(df.rGDP.states.A.raw, df.rGDP.states.Q.raw,
     df.rGDPpc.states.A.raw,
     df.rGDP.states.A, df.rGDP.states.Q,
     df.rGDPpc.states.A,
     df.UR.states.M.raw,
     df.UR.states.A, df.UR.states.Q, df.UR.states.M,
     file=paste0(edir.atus,"URandrGDPpc_",tfirst,"_",tlast,"_state.Rdata"))

# load(file=paste0(edir.atus,"URandrGDPpc_",tfirst,"_",tlast,"_state.Rdata"))

rm(df.rGDP.states.A.raw, df.rGDP.states.Q.raw, df.rGDPpc.states.A.raw, df.UR.states.M.raw)
rm(df.rGDPpc.states.A,df.rGDP.A.NE,df.rPIpc.states.A)



load(file=paste0(edir.atus,"URX_",tfirst,"_",tlast,"_state.Rdata"))

# merge ATUS data with data on GDP and unemployment rate
load(file=paste0(edir.atus,"atus_",tfirst,"_",tlast,"_individual.Rdata"))

df.timeuse.all.A <-
    df.timeuse.all %>%
    inner_join(df.rGDP.states.A, by=c("tuyear","gestfips")) %>%
    inner_join(df.UR.states.A, by=c("tuyear","gestfips")) %>%
    inner_join(df.URX.states.A, by=c("tuyear","gestfips"))

df.timeuse.all.Q <-
    df.timeuse.all %>%
    inner_join(df.rGDP.states.Q, by=c("tuyear","tuquarter","gestfips")) %>%
    inner_join(df.UR.states.Q, by=c("tuyear","tuquarter","gestfips")) %>%
    inner_join(df.URX.states.Q, by=c("tuyear","tuquarter","gestfips"))

rm(df.timeuse.all)

# save the resulting state level dataset
save(df.timeuse.all.A, df.timeuse.all.Q, file=paste0(edir.atus,"atus_",tfirst,"_",tlast,"_state.Rdata"))

# load(file=paste0(edir.atus,"atus_",tfirst,"_",tlast,"_state.Rdata"))



# generate state level aggregates (weighted averages)
df.agg.by.state.A <-
    inner_join(# variables summarized using weighted average
              df.timeuse.all.A %>%
                  select(gestfips, tuyear, weight,
                         male, married, black, hv_child, num_child, spouse_emp, grade,
                         working, unemp, retired, disabled, student, homemaker,
                         pop, u3rate, u4rate, u5rate, u6rate, rPIpc, rGDPpc, gr.rGDPpc, hp.lrGDPpc,
                         starts_with("t.")) %>%
                  # add age and education dummies
                  bind_cols(df.timeuse.all.A %>%
                                model.matrix(object =~ age.f-1) %>%
                                as.tibble(),
                            df.timeuse.all.A %>%
                                model.matrix(object = ~educ.f-1) %>%
                                as.tibble()) %>%
                  group_by(gestfips, tuyear) %>%
                  summarise_at(vars(-weight), funs(wtd.mean(., w=weight, na.rm=TRUE))) %>%
                  ungroup(),
              # variables summarized using sum
              df.timeuse.all.A %>%
                  select(gestfips, tuyear, weight) %>%
                  group_by(gestfips, tuyear) %>%
                  summarise(weight = sum(weight)/200) %>%
                  ungroup(),
              by=c("gestfips","tuyear")) %>%
    inner_join(df.state.codes, by=c("gestfips"))



df.agg.by.state.A %>%
    select(tuyear, gestfips, state_code, pop, weight) %>%
    gather(measure, value, -tuyear, -gestfips, -state_code) %>%
    {ggplot(., aes(x=tuyear, y=value, col=measure)) +
            geom_line() +
            facet_wrap(~state_code)}

df.agg.by.state.Q <-
    inner_join(# variables summarizes using weighted average
              df.timeuse.all.Q %>%
                  select(gestfips, tuyear, tuquarter, weight,
                         male, married, black, hv_child, num_child, spouse_emp, grade,
                         working, unemp, retired, disabled, student, homemaker,
                         pop, u3rate, u4rate, u5rate, u6rate, rGDPpc,gr.rGDPpc, hp.lrGDPpc,
                         starts_with("t.")) %>%
                  # add age and education dummies
                  bind_cols(df.timeuse.all.Q %>%
                                model.matrix(object =~ age.f-1) %>%
                                as.tibble(),
                            df.timeuse.all.Q %>%
                                model.matrix(object =~ educ.f-1) %>%
                                as.tibble()) %>%
                  group_by(gestfips, tuyear, tuquarter) %>%
                  summarise_at(vars(-weight), funs(wtd.mean(., w=weight, na.rm=TRUE))) %>%
                  ungroup(),
              # variables summarized using sum
              df.timeuse.all.Q %>%
                  select(gestfips,tuyear,tuquarter,weight) %>%
                  group_by(gestfips, tuyear, tuquarter) %>%
                  summarise(weight = sum(weight)/100) %>%
                  ungroup(),
              by=c("gestfips","tuyear","tuquarter")) %>%
    inner_join(df.state.codes, by=c("gestfips"))


# save the resulting state level dataset
save(df.agg.by.state.A, df.agg.by.state.Q, file=paste0(edir.atus,"df_agg_by_state_",tfirst,"_",tlast,".Rdata"))


# control for demographics

# myformula <- paste("t.shop. ~", paste(select_vars(names(df.agg.by.state.A), starts_with("age.f")), collapse="+")) %>% as.formula()

df.agg.by.state.A.demo <-
    df.agg.by.state.A %>%
    mutate_at(vars(starts_with("t.shop.")),
              funs(lm(. ~ -1+male+black+
                          `age.f(0,17]`+ `age.f(17,27]`+ `age.f(27,37]`+ `age.f(37,47]`+ `age.f(47,57]`+ `age.f(57,65]`+ `age.f(65,Inf]`+
                          `educ.f[0,11]`+ `educ.f(11,12]`+ `educ.f(12,15]`+ `educ.f(15,Inf]`+
                          married+hv_child+disabled, weights=weight) %>%
                       residuals()))

df.agg.by.state.Q.demo <-
    df.agg.by.state.Q %>%
    mutate_at(vars(starts_with("t.shop.")),
              funs(lm(. ~ -1+male+black+
                          `age.f(0,17]`+ `age.f(17,27]`+ `age.f(27,37]`+ `age.f(37,47]`+ `age.f(47,57]`+ `age.f(57,65]`+ `age.f(65,Inf]`+
                          `educ.f[0,11]`+ `educ.f(11,12]`+ `educ.f(12,15]`+ `educ.f(15,Inf]`+
                          married+hv_child+disabled, weights=weight) %>%
                       residuals()))



# distribution of total shopping time by state, over time
ggplot(df.agg.by.state.A, aes(x=factor(tuyear), y=t.shop.ttl)) +
    # geom_violin(draw_quantiles = c(0.25,0.5,0.75), trim=FALSE) +
    geom_violin(draw_quantiles = 0.5, trim=FALSE) +
    geom_jitter(width=0.15, alpha = 0.20) +
    # geom_boxplot() +
    theme_light()




# plot real GDP per capita against real personal income per capita - D.C. looks like an outlier
bind_rows(df.agg.by.state.A %>%
              mutate(keep.DC = 1),
          df.agg.by.state.A %>%
              filter(state_code != "DC") %>%
              mutate(keep.DC = 0)) %>%
mutate(is.DC = if_else(state_code == "DC", 1L, 0L)) %>%
{ggplot(., aes(x=log(rGDPpc), y=log(rPIpc))) +
        geom_point(alpha=0.2, aes(size=weight, col=factor(is.DC))) +
        geom_smooth(method="lm", aes(weight=weight)) +
        # geom_smooth(method="lm", col="red") +
        labs(x = "log of real GDP per capita", y = "log of real personal income per capita") +
        facet_grid(~keep.DC, labeller = label_both) +
        theme_light() +
        theme(strip.text.x = element_text(color = "black"))}

#
# state level regressions - time spent on shopping related activities on business cycle measure
#

# using:
#  unemployment rate U3,U4,U5 or U6
#  real GDP per capita
#  real personal income per capita

df.agg.by.state.A %>%
    filter(state_code != "DC") %>%
    mutate(lrGDPpc = log(rGDPpc),
           lrPIpc = log(rPIpc)) %>%
    # select(t.shop.ttl, u3rate, u4rate, u5rate, u6rate, lrGDPpc, lrPIpc, pop, weight) %>%
    select(t.shop.ttl, u3rate, lrGDPpc, pop, weight) %>%
    gather(measure, value, -c(t.shop.ttl, pop, weight)) %>%
    {ggplot(., aes(x=value, y=t.shop.ttl)) +
            geom_point(alpha=0.2, aes(size=weight)) +
            geom_smooth(method="lm", aes(weight=weight)) +
            labs(x = "", y = "Average total shopping time") +
            facet_wrap(~measure, scales="free", ncol=2) +
            theme_light() +
            theme(legend.position="none",
                  strip.text.x = element_text(color = "black"),
                  strip.background = element_rect(fill="lightgray"))}


rhs.spc <- list()
rhs.spc[1] <- "log(rGDPpc)"
rhs.spc[2] <- "log(rGDPpc) + tuyear1"
rhs.spc[3] <- "log(rGDPpc) + tuyear1 + factor(gestfips)"
rhs.spc[4] <- "log(rGDPpc) + tuyear*factor(gestfips) - tuyear"

rhs.spc[5] <- "unrate"
rhs.spc[6] <- "unrate + tuyear1"
rhs.spc[7] <- "unrate + tuyear1 + factor(gestfips)"
rhs.spc[8] <- "unrate + tuyear*factor(gestfips) - tuyear"

# rhs.spc[9] <- "log(rGDPpc) + unrate"
# rhs.spc[10] <- "log(rGDPpc) + unrate + tuyear"
# rhs.spc[11] <- "log(rGDPpc) + unrate + tuyear + factor(gestfips)"
# rhs.spc[12] <- "log(rGDPpc) + unrate + tuyear*factor(gestfips)"

# estimate individual level regressions - time spent on shopping related activities
df.lm <-
    # df.agg.by.state.Q %>%
    df.agg.by.state.A %>%
    filter(state_code != "DC") %>%
    mutate(unrate = u3rate,
           tuyear1 = tuyear)

df.lm.results <-
    rhs.spc %>%
    tibble(spec = .) %>%
    mutate(# estimate individual level regressions - time spent on shopping related activities
           lm.model = map(spec, ~lm( formula(paste("t.shop.groceries ~",.x)), weight=weight, data=df.lm )),
           lm.coefs = map(lm.model, tidy, conf.int = TRUE),
           # construct clustered errors
           lm.coefs = map2(lm.model, lm.coefs,
                           ~(.y %>% mutate(std.error.clustered = coeftest(.x, clustered.se(.x, df.lm$gestfips))[,"Std. Error"],
                                           conf.low.clustered = estimate - 2*std.error.clustered,
                                           conf.high.clustered = estimate + 2*std.error.clustered))))

# report results using stargazer
df.lm.results %$%
    stargazerX(lm.model, type="text", out=paste0(odir.atus,"results.state.A.groceries.tex"),
               column.sep.width = "0pt",
               title = "State level regressions",
               se = lm.coefs %>%
                   map(~ .x %$% std.error.clustered),
               omit=c("^tuyear1","^factor\\(gestfips","^tuyear:factor\\(gestfips"),
               omit.labels=c("common time trend","state fixed effects","state specific time trends"),
               omit.stat=c("ser","f"),
               dep.var.caption="", dep.var.labels="Groceries",
               covariate.labels=c("constant","log of real GDP per capita","unemployment rate"),
               intercept.bottom = FALSE, no.space=TRUE)

# plot estimated coefficients for measures of the business cycle
df.lm.results %>%
    select(spec, lm.coefs) %>%
    unnest() %>%
    filter(term %in% c("u3rate","u4rate","u5rate","u6rate","log(rGDPpc)","hp.lrGDPpc")) %T>%
    {{ggplot(.,aes(x=estimate, y=spec)) +
            geom_point() +
            geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height=0.2)) +
            geom_errorbarh(aes(xmin=conf.low.clustered, xmax=conf.high.clustered, height=0.2), col="red") +
            geom_vline(xintercept = 0, linetype="dotted") +
            theme_light() +
            facet_grid(~term, scales="free_x")} %>% print()}




df.agg.by.state.A %>%
    filter(state_code != "DC") %>%
    select(t.shop.ttl, u3rate, u4rate, u5rate, u6rate, rGDPpc, pop, weight) %>%
    gather(measure, value, -c(t.shop.ttl, pop, weight)) %>%
    {ggplot(., aes(x=value, y=t.shop.ttl)) +
            geom_point(alpha=0.2, aes(size=weight)) +
            geom_smooth(method="lm", aes(weight=weight)) +
            geom_smooth(method="lm", aes(weight=weight)) +
            labs(x = "", y = "Average total shopping time") +
            facet_wrap(~measure, scales="free", ncol=2) +
            theme_light() +
            theme(legend.position="none",
                  strip.text.x = element_text(color = "black"),
                  strip.background = element_rect(fill="lightgray"))}


