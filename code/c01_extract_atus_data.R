
print(paste("Extracting relevant data from ATUS", tfirst, "to", tlast))

t <- tfirst

while (t <= tlast) {
    print(paste(" year", t))

    # df_atus_act data
    df_atus_act <-
        read_csv(paste0(ddir_atus, "raw/atusact_", t, ".dat")) %>%
        rename_all(funs(tolower)) %>%
        select(tucaseid, tuactivity_n, tewhere, tuactdur, tuactdur24, tutier1code, tutier2code, tutier3code) %>%
        mutate_if(is.character, as.integer)

    # atucps data (only keep respondent's info)
    df_atus_cps <-
        read_csv(paste0(ddir_atus, "raw/atuscps_", t, ".dat")) %>%
        rename_all(funs(tolower)) %>%
        filter(tulineno == 1) %>%
        select(prtage, pesex, peeduca, tucaseid, tulineno, prunedur, pruntype, ptdtrace,
               gestfips, gereg, hrhtype, ifelse(t <= 2009, "hufaminc", "hefaminc"))

    # df_atus_rost data (only keep respondent's info)
    df_atus_rost <-
        read_csv(paste0(ddir_atus, "raw/atusrost_", t, ".dat")) %>%
        rename_all(funs(tolower)) %>%
        filter(tulineno == 1) %>%
        select(teage, tesex, tucaseid, tulineno)

    # df_atus_resp data
    df_atus_resp <-
        read_csv(paste0(ddir_atus, "raw/atusresp_", t, ".dat")) %>%
        rename_all(tolower) %>%
        select(tulk, tufwk, tulay, tudis, tuabsot, teret1, telfs, trchildnum, trsppres, tryhhchild,
               tucaseid, tulineno, tudiaryday, tudiarydate, tumonth, tuyear, turetot, trernwa,
               teschft, tehruslt, trernhly, tespempnot, tespuhrs,
               ifelse(t <= 2005, c("tu06fwgt", "tufinlwgt"), "tufinlwgt"))

    # df_atus <- inner_join(df_atus_cps, df_atus_rost, by=c("tucaseid","tulineno"))
    df_atus <- df_atus_cps %>%
        inner_join(df_atus_rost, by = "tucaseid") %>%
        inner_join(df_atus_resp, by = "tucaseid") %>%
        inner_join(df_atus_act, by = "tucaseid")

    rm(df_atus_act, df_atus_resp, df_atus_cps, df_atus_rost)

    vtravel <- ifelse(t < 2005, 17, 18)

    df_atus %<>%
        mutate(
            # interview date
            tudiarydate = as.Date(as.character(tudiarydate), format = "%Y%m%d"),
            tuquarter = quarter(tudiarydate),

            # demographics
            age = teage,                                                                # discrepancies with prtage ???
            male = as.integer(tesex == 1),
            black = as.integer(ptdtrace == 2),                                          # black only
            married = as.integer(trsppres == 1),                                        # does not include unmarried partner
            num_child = trchildnum,
            hv_child = as.integer(num_child > 0),                                       # family with children
            spouse_emp = as.integer(case_when(tespempnot == 1 ~ 1,
                                              tespempnot == 2 ~ 0)),
            spouse_workhours = case_when(!is.na(tespuhrs) & tespuhrs >= 0 ~ tespuhrs),  # usual hours of work of spouse or unmarried partner

            # education recode
            grade = case_when(peeduca == 31 ~ 0,
                              peeduca == 32 ~ 1,
                              peeduca == 33 ~ 5,
                              peeduca == 34 ~ 7,
                              peeduca == 35 ~ 9,
                              peeduca == 36 ~ 10,
                             (peeduca == 37 | peeduca == 38) ~ 11,
                              peeduca == 39 ~ 12,
                             (peeduca >= 40 & peeduca <= 42) ~ 13,
                              peeduca == 43 ~ 16,
                              peeduca >= 44 ~ 17),

            # labor force status recode
            working   = as.integer(telfs == 1 | telfs == 2),
            unemp     = as.integer(telfs == 3 | telfs == 4),
            retired   = as.integer(telfs == 5 & (turetot == 1 | tulk == 3 | tufwk == 3 | tulay == 3 | tuabsot == 3) & teret1 == 2),
            disabled  = as.integer(telfs == 5 & (tuabsot == 4 | tufwk == 4 | tulay == 4 | tulk == 4 | tudis == 1)),
            student   = as.integer(teschft == 1 & working == 0 & unemp == 0),
            homemaker = as.integer(working == 0 & retired == 0 & student == 0 & disabled == 0 & unemp == 0),
            othernilf = as.integer(working == 0 & retired == 0 & student == 0 & homemaker == 0 & unemp == 0),

            # reason for unemployment
            reason  = case_when(pruntype == 1        ~ 0L,      # on layoff
                                pruntype %in% c(2,3) ~ 1L,      # other job loser
                                pruntype %in% c(5,6) ~ 2L,      # re-entrants and new entrants
                                pruntype == 4        ~ 3L),     # job leaver

            # unemployment duration in weeks
            udur = prunedur,

            # family income
            faminc = ifelse(tuyear <= 2009, hufaminc, hefaminc),
            # sample weight
            weight = ifelse(tuyear <= 2005, tu06fwgt, tufinlwgt),

            # recode time use activities

            # market work

            t_work = replace(tuactdur24,
                             !( (tutier1code == 5 & tutier2code == 1) |                                                             #  Working
                                (tutier1code == vtravel & tutier2code == 5 & tutier3code != 3 & tutier3code != 4) |                 #  Travel related to work
                                (tutier1code == 5 & tutier2code == 2) |                                                             #  Work-related activities
                                (tutier1code == 5 & tutier2code == 99) ), 0),                                                       #  Work and Work-Related Activities, n.e.c.

            t_work_act = replace(tuactdur24,
                             !( (tutier1code == 5 & tutier2code == 3) |                                                             #  Other Income-generating Activities
                                (tutier1code == vtravel & tutier2code == 5 & tutier3code == 3) ), 0),

            t_work_search = replace(tuactdur24,
                             !( (tutier1code == 5 & tutier2code == 4) |                                                             #  Job Search and Interviewing
                                (tutier1code == vtravel & tutier2code == 5 & tutier3code == 4) ), 0),

            # home work

            t_meals = replace(tuactdur24,
                             !( (tutier1code == 2 & tutier2code == 2) ), 0),                                                       # Food and Drink Preparation, Presentation, and Clean-up.

            t_housework = replace(tuactdur24,
                             !( (tutier1code == 2 & tutier2code == 1) ), 0),                                                       # Housework

            t_home_car_maintenance = replace(tuactdur24,
                             !( (tutier1code == 2 & tutier2code == 3) |                                                            # Interior Maintenance, Repair, and Decoration
                                (tutier1code == 2 & tutier2code == 4) |                                                            # Exterior Maintenance, Repair, and Decoration
                                (tutier1code == 2 & tutier2code == 7) |                                                            # Vehicles
                                (tutier1code == 2 & tutier2code == 8) |                                                            # Appliances and Tools
                                (tutier1code == vtravel & tutier2code == 2 & tutier3code == 4) ), 0),

            t_homeproduction = replace(tuactdur24,
                             !( (tutier1code == 2 & tutier2code == 2) |                                                             #  Food and Drink Preparation, Presentation, and Clean-up
                                (tutier1code == 2 & tutier2code == 1) |                                                             #  Housework
                                (tutier1code == 2 & tutier2code == 3) |                                                             #  Interior Maintenance, Repair, and Decoration
                                (tutier1code == 2 & tutier2code == 4) |                                                             #  Exterior Maintenance, Repair, and Decoration
                                (tutier1code == 2 & tutier2code == 7) |                                                             #  Vehicles
                                (tutier1code == 2 & tutier2code == 8) |                                                             #  Appliances and Tools
                                (tutier1code == vtravel & tutier2code == 2 & tutier3code == 4)  |
                                (tutier1code == 2 & tutier2code == 9 & tutier3code != 3 & tutier3code != 4) |                       #  Household Management except mail and email
                                (tutier1code == 2 & tutier2code == 99) |                                                            #  Household Activities, nec
                                (tutier1code == vtravel & tutier2code == 2 & tutier3code != 4 & tutier3code != 5 & tutier3code != 6) ), 0),#  Travel related to household activities

            t_garden = replace(tuactdur24,
                             !( (tutier1code == 2 & tutier2code == 5) |                                                             #  Lawn, Garden, and Houseplants
                                (tutier1code == vtravel & tutier2code == 2 & tutier3code == 5) ), 0),

            t_pet = replace(tuactdur24,
                             !( (tutier1code == 2 & tutier2code == 6) |                                                             #  Animals and Pets
                                (tutier1code == vtravel & tutier2code == 2 & tutier3code == 6) ), 0),

            # shopping

            t_shop = replace(tuactdur24,
                             !( (tutier1code == 7) |                                                                              #  Consumer Purchases
                                (tutier1code == 8 & tutier2code != 4) |                                                           #  Professional and Personal Care Services excluding medical
                                (tutier1code == 9) |                                                                              #  Household Services
                                (tutier1code == 10) |                                                                             #  Goverment Services and Civic Obligations
                                (tutier1code == vtravel & tutier2code == 7) |                                                     #  Travel related to consumer purchases
                                (tutier1code == vtravel & tutier2code == 8 & tutier3code != 4) |                                  #  Travel related to Professional and Personal Care Services except medical
                                (tutier1code == vtravel & tutier2code == 9) |                                                     #  Travel related to household services
                                (tutier1code == vtravel & tutier2code == 10) ), 0),                                               #  Travel Related to Using Government Services and Civic Obligations

            # education
            t_education = replace(tuactdur24,
                             !( (tutier1code == 6) |                                                                              #  Education
                                (tutier1code == vtravel & tutier2code == 6) ), 0),                                                #  Travel Related to Education

            # civic
            t_civic = replace(tuactdur24,
                             !( (tutier1code == 14) |                                                                             #  Religious and Spiritual Activities
                                (tutier1code == 15) |                                                                             #  Volunteer Activities
                                (tutier1code == vtravel & tutier2code == 14) |                                                    #  Travel Related to Religious/Spiritual Activities
                                (tutier1code == vtravel & tutier2code == 15) ), 0),                                               #  Travel Related to Volunteer Activities

            # leisure
            t_tv = replace(tuactdur24,
                             !( (tutier1code == 12 & tutier2code == 3 & tutier3code == 3) |                                       #  Television and movies (not religious)
                                (tutier1code == 12 & tutier2code == 3 & tutier3code == 4) ), 0),                                  #  Television (religious)

            t_leisure = replace(tuactdur24,
                             !( tutier1code == 12 |                                                                               #  Socializing, Relaxing, and Leisure
                                tutier1code == 13        |                                                                        #  Sports, Exercise, and Recreation
                                tutier1code == 16     |                                                                           #  Telephone Calls
                                (tutier1code == vtravel & tutier2code == 12) |                                                    #  Travel related to socializing, relaxing, and leisure
                                (tutier1code == vtravel & tutier2code == 13) |                                                    #  Travel related to sports, exercise, and recreation
                                (tutier1code == vtravel & tutier2code == 16) |                                                    #  Travel related to telephone calls
                                (tutier1code == 2 & tutier2code == 9 & tutier3code == 3 ) |                                       #  Household and personal mail
                                (tutier1code == 2 & tutier2code == 9 & tutier3code == 4 ) ), 0),                                  #  Household and personal email

            t_other = replace(tuactdur24,
                             !( (tutier1code == 50) |                                                                             #  Unable to Code
                                (tutier1code == vtravel & tutier2code == vtravel ) |                                              #  Security procedures related to travelling
                                (tutier1code == vtravel & tutier2code == 99) ), 0),                                               #  Traveling, n.e.c.

            # eating, sleeping, personal care

            t_eat = replace(tuactdur24,
                             !( tutier1code == 11 |                                                                               #  Eating and Drinking
                               (tutier1code == vtravel & tutier2code == 11) ), 0),                                                #  Travel Related to Eating and Drinking

            t_sleep = replace(tuactdur24,
                              !( tutier1code ==  1 & tutier2code == 1 ), 0),                                                      #  Sleeping

            t_personal_care = replace(tuactdur24,
                              !( (tutier1code == 1 & tutier2code != 1 & tutier2code != 3)  |                                      #  Personal Care minus sleeping and health
                                 (tutier1code == vtravel & tutier2code == 1) ), 0),                                               #  Travel Related to Personal Care

            t_own_medical_care = replace(tuactdur24,
                              !( (tutier1code == 1 & tutier2code == 3) |                                                 #  Health-related Self Care
                                 (tutier1code == 8 & tutier2code == 4) |                                                 #  Medical Care Services
                                 (tutier1code == vtravel & tutier2code == 8 & tutier3code == 4) ), 0),                   #  Travel related to medical services

            t_other_care = ifelse(tuyear < 2005,
                                  replace(tuactdur24,
                                          !( (tutier1code == 3 & tutier2code == 4) |                                #  Caring For Household Adults
                                             (tutier1code == 3 & tutier2code == 5) |                                #  Helping Household Adults
                                             (tutier1code == 3 & tutier2code == 99)|                                #  Caring For and Helping household Members, n.e.c.
                                             (tutier1code == 4 & tutier2code == 4) |                                #  Caring For Nonhousehold Adults
                                             (tutier1code == 4 & tutier2code == 5) |                                #  Helping Nonhousehold Adults
                                             (tutier1code == 4 & tutier2code == 99)|                                #  Caring For and Helping nonhousehold Members, n.e.c.
                                             (tutier1code == vtravel & tutier2code == 3 & tutier3code == 2) |       #  Travel Related to Caring For and Helping household adults
                                             (tutier1code == vtravel & tutier2code == 3 & tutier3code == 99) |      #  Travel Related to Caring For and Helping household members, nec
                                             (tutier1code == vtravel & tutier2code == 4 & tutier3code == 2) |       #  Travel Related to Caring For and Helping Nonhousehold adults
                                             (tutier1code == vtravel & tutier2code == 4 & tutier3code == 99) ), 0), #  Travel Related to Caring For and Helping nonhousehold members, nec
                                  replace(tuactdur24,
                                          !( (tutier1code == 3 & tutier2code == 4) |                                #  Caring For Household Adults
                                             (tutier1code == 3 & tutier2code == 5) |                                #  Helping Household Adults
                                             (tutier1code == 3 & tutier2code == 99) |                               #  Caring For and Helping household Members, n.e.c.
                                             (tutier1code == 4 & tutier2code == 4) |                                #  Caring For Nonhousehold Adults
                                             (tutier1code == 4 & tutier2code == 5) |                                #  Helping Nonhousehold Adults
                                             (tutier1code == 4 & tutier2code == 99) |                               #  Caring For and Helping nonhousehold Members, n.e.c.
                                             (tutier1code == vtravel & tutier2code == 3 & tutier3code == 4) |       #  Travel Related to Caring For household adults
                                             (tutier1code == vtravel & tutier2code == 3 & tutier3code == 5) |       #  Travel Related to Helping household members
                                             (tutier1code == vtravel & tutier2code == 4 & tutier3code == 4) |       #  Travel Related to Caring For nonhousehold adults
                                             (tutier1code == vtravel & tutier2code == 4 & tutier3code == 5) |       #  Travel Related to Helping nonhousehold members, nec
                                             (tutier1code == vtravel & tutier2code == 3 & tutier3code == 99) |      #  Travel Related to Caring For and Helping household members, nec
                                             (tutier1code == vtravel & tutier2code == 4 & tutier3code == 99) ), 0) ),     #  Travel Related to Caring For and Helping nonhousehold members, nec

            # child care

            t_childcare_basic = ifelse(tuyear < 2005,
                                  replace(tuactdur24,
                                          !( (tutier1code == 3 & tutier2code == 1 & tutier3code == 1) |            #  Physical care for household children
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 8) |            #  Organization and planning for household children
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 9) |            #  Looking after household children (as a primary activity)
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 10)|            #  Attending household children's events
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 11)|            #  Waiting for/with household children
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 12)|            #  Picking up/dropping off household children
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 99)|            #  Caring for and helping hosuehold children, nec
                                             (tutier1code == 3 & tutier2code == 3) |                               #  Activities Related to household Children's Health
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 1) |            #  Physical care for nonhousehold children
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 8) |            #  Organization and planning for nonhousehold children
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 9) |            #  Looking after nonhousehold children (as a primary activity)
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 10)|            #  Attending nonhousehold children's events
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 11)|            #  Waiting for/with nonhousehold children
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 12)|            #  Picking up/dropping off nonhousehold children
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 99)|            #  Caring for and helping nonhosuehold children, nec
                                             (tutier1code == 4 & tutier2code == 3) |                               #  Activities Related to nonhousehold Children's Health
                                             (tutier1code == vtravel & tutier2code == 3 & tutier3code == 1) |      #  Travel Related to Caring For and Helping household children
                                             (tutier1code == vtravel & tutier2code == 4 & tutier3code == 1) ), 0), #  Travel Related to Caring For and Helping nonhousehold children
                                  replace(tuactdur24,
                                          !( (tutier1code == 3 & tutier2code == 1 & tutier3code == 1) |             #  Physical care for household children
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 8) |             #  Organization and planning for household children
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 9) |             #  Looking after household children (as a primary activity)
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 10)|             #  Attending household children's events
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 11)|             #  Waiting for/with household children
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 12)|             #  Picking up/dropping off household children
                                             (tutier1code == 3 & tutier2code == 1 & tutier3code == 99)|             #  Caring for and helping hosuehold children, nec
                                             (tutier1code == 3 & tutier2code == 3) |                                #  Activities Related to household Children's Health
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 1) |             #  Physical care for nonhousehold children
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 8) |             #  Organization and planning for nonhousehold children
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 9) |             #  Looking after nonhousehold children (as a primary activity)
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 10) |            #  Attending nonhousehold children's events
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 11) |            #  Waiting for/with nonhousehold children
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 12) |            #  Picking up/dropping off nonhousehold children
                                             (tutier1code == 4 & tutier2code == 1 & tutier3code == 99) |            #  Caring for and helping nonhosuehold children, nec
                                             (tutier1code == 4 & tutier2code == 3) |                                #  Activities Related to nonhousehold Children's Health
                                             (tutier1code == vtravel & tutier2code == 3 & tutier3code == 1) |       #  Travel Related to Caring For and Helping household children
                                             (tutier1code == vtravel & tutier2code == 4 & tutier3code == 1) |       #  Travel Related to Caring For and Helping nonhousehold children
                                             (tutier1code == vtravel & tutier2code == 3 & tutier3code == 3) |       #  Travel Related to household children Health
                                             (tutier1code == vtravel & tutier2code == 4 & tutier3code == 3) ), 0)), #  Travel Related to nonhousehold children Health

           t_childcare_teach = ifelse(tuyear < 2005,
                                     replace(tuactdur24,
                                             !( (tutier1code == 3 & tutier2code == 1 & tutier3code == 2) |            #  Reading to/with household children
                                                (tutier1code == 3 & tutier2code == 1 & tutier3code == 6) |            #  Talking with/listening to household children
                                                (tutier1code == 3 & tutier2code == 1 & tutier3code == 7) |            #  Helping/teaching household children (not related to education)
                                                (tutier1code == 3 & tutier2code == 2) |                               #  Activities Related to household Children's Education
                                                (tutier1code == 4 & tutier2code == 1 & tutier3code == 2) |            #  Reading to/with nonhoushold children
                                                (tutier1code == 4 & tutier2code == 1 & tutier3code == 6) |            #  Talking with/listening to nonhousehold children
                                                (tutier1code == 4 & tutier2code == 1 & tutier3code == 7) |            #  Helping/teaching nonhousehold children (not related to education)
                                                (tutier1code == 4 & tutier2code == 2) ), 0),                          #  Activities Related to Nonhousehold Children's Education
                                     replace(tuactdur24,
                                             !( (tutier1code == 3 & tutier2code == 1 & tutier3code == 2) |            #  Reading to/with household children
                                                (tutier1code == 3 & tutier2code == 1 & tutier3code == 6) |            #  Talking with/listening to household children
                                                (tutier1code == 3 & tutier2code == 1 & tutier3code == 7) |            #  Helping/teaching household children (not related to education)
                                                (tutier1code == 3 & tutier2code == 2) |                               #  Activities Related to household Children's Education
                                                (tutier1code == 4 & tutier2code == 1 & tutier3code == 2) |            #  Reading to/with nonhoushold children
                                                (tutier1code == 4 & tutier2code == 1 & tutier3code == 6) |            #  Talking with/listening to nonhousehold children
                                                (tutier1code == 4 & tutier2code == 1 & tutier3code == 7) |            #  Helping/teaching nonhousehold children (not related to education)
                                                (tutier1code == 4 & tutier2code == 2) |                               #  Activities Related to Nonhousehold Children's Education
                                                (tutier1code == 18 & tutier2code == 3 & tutier3code == 2) |           #  Travel Related to household children education
                                                (tutier1code == 18 & tutier2code == 4 & tutier3code == 2) ), 0) ),    #  Travel Related to nonhousehold children education

           t_childcare_play = replace(tuactdur24,
                                             !( (tutier1code == 3 & tutier2code == 1 & tutier3code == 3) |            #  Playing with household children, not sports
                                                (tutier1code == 3 & tutier2code == 1 & tutier3code == 4) |            #  Arts and crafts with household children
                                                (tutier1code == 3 & tutier2code == 1 & tutier3code == 5) |            #  Playing sports with household children
                                                (tutier1code == 4 & tutier2code == 1 & tutier3code == 3) |            #  Playing with nonhousehold children, not sports
                                                (tutier1code == 4 & tutier2code == 1 & tutier3code == 4) |            #  Arts and crafts with nonhousehold children
                                                (tutier1code == 4 & tutier2code == 1 & tutier3code == 5) ), 0),       #  Playing sports with nonhousehold children


           # categories considered in Aguiar, Hurst & Karabarbounis (2013) AER

           t_ahk_work          = t_work,
           t_ahk_work_act      = t_work_act,
           t_ahk_work_search   = t_work_search,

           t_ahk_sleep         = t_sleep,
           t_ahk_ep            = t_eat + t_personal_care,
           t_ahk_esp           = t_eat + t_sleep + t_personal_care,
           t_ahk_espleisure    = t_eat + t_sleep + t_personal_care + t_leisure + t_pet,
           t_ahk_leisure_tv    = t_tv,
           t_ahk_leisure_oth   = t_leisure + t_pet - t_tv,

           t_ahk_homeproduction= t_homeproduction + t_garden,
           t_ahk_shop          = t_shop,
           # t_ahk_home          = t_homeproduction + t_garden + t_shop + t_other.care,

           t_ahk_childcare     = t_childcare_basic + t_childcare_teach + t_childcare_play,
           t_ahk_othercare     = t_other_care,
           t_ahk_ownmdcare     = t_own_medical_care,

           t_ahk_education     = t_education,
           t_ahk_civic         = t_civic,

           t_work_ttl          = t_ahk_work + t_ahk_work_act + t_ahk_work_search,
           t_nonmarket_work_ttl= t_ahk_homeproduction + t_ahk_shop + t_ahk_childcare + t_ahk_othercare,

           t_ttl =
               t_ahk_work + t_ahk_work_act + t_ahk_work_search +
               t_ahk_homeproduction + t_ahk_shop +
               t_ahk_childcare + t_ahk_othercare +
               t_ahk_ownmdcare + t_eat + t_sleep + t_personal_care + t_leisure + t_pet +
               t_ahk_education + t_ahk_civic + t_other,

           # recode shopping related activities

           t_shop_groceries = replace(tuactdur24,
                                 !( (tutier1code == 7 & tutier2code == 1 & tutier3code == 1) ), 0),                # Consumer Purchases - Grocery shopping (070101)

           t_shop_gas = replace(tuactdur24,
                                 !( (tutier1code == 7 & tutier2code == 1 & tutier3code == 2) ), 0),                # Consumer Purchases - Purchasing gas (070102)

           t_shop_food = replace(tuactdur24,
                                 !( (tutier1code == 7 & tutier2code == 1 & tutier3code == 3) ), 0),                # Consumer Purchases - Purchasing food (not groceries) (070103)

           t_shop_other = replace(tuactdur24,
                                 !( (tutier1code == 7 & tutier2code == 1 & tutier3code == 4) |                     # Shopping, except groceries, food, and gas (070104)
                                    (tutier1code == 7 & tutier2code == 1 & tutier3code == 5) |                     # Waiting associated with shopping (070105)
                                    (tutier1code == 7 & tutier2code == 1 & tutier3code == 99) |                    # Shopping, n.e.c. (070199)
                                    (tutier1code == 7 & tutier2code == 2) |                                        # Researching Purchases (070200)
                                    (tutier1code == 7 & tutier2code == 99) |                                       # Consumer purchases, n.e.c. (079900)
                                    (tutier1code == 8 & tutier2code == 1 & tutier3code == 2) |                     # Waiting associated w/purchasing childcare svcs
                                    (tutier1code == 8 & tutier2code == 2 & tutier3code == 3) |                     # Waiting associated w/banking/financial services
                                    (tutier1code == 8 & tutier2code == 3 & tutier3code == 2) |                     # Waiting associated with legal services
                                    (tutier1code == 8 & tutier2code == 4 & tutier3code == 3) |                     # Waiting associated with medical services
                                    (tutier1code == 8 & tutier2code == 5 & tutier3code == 2) |                     # Waiting associated w/personal care services
                                    (tutier1code == 8 & tutier2code == 6 & tutier3code == 2) |                     # Waiting associated w/purchasing/selling real estate
                                    (tutier1code == 8 & tutier2code == 7 & tutier3code == 2) |                     # Waiting associated with veterinary services
                                    (tutier1code == 9 & tutier2code == 1 & tutier3code == 4) |                     # Waiting associated with using household services
                                    (tutier1code == 9 & tutier2code == 2 & tutier3code == 2) |                     # Waiting associated w/ home main/repair/d?cor/constr
                                    (tutier1code == 9 & tutier2code == 3 & tutier3code == 2) |                     # Waiting associated with pet services
                                    (tutier1code == 9 & tutier2code == 4 & tutier3code == 2) |                     # Waiting associated with using lawn & garden services
                                    (tutier1code == 9 & tutier2code == 5 & tutier3code == 2) |                     # Waiting associated with vehicle main. or repair svcs
                                    (tutier1code == 12 & tutier2code == 5 & tutier3code == 4) ), 0),               # Waiting associated with arts & entertainment

           t_shop_other_shop = replace(tuactdur24,
                                 !( (tutier1code == 7 & tutier2code == 1 & tutier3code == 4) |                     # Shopping, except groceries, food, and gas (070104)
                                    (tutier1code == 7 & tutier2code == 1 & tutier3code == 99) |                    # Shopping, n.e.c. (070199)
                                    (tutier1code == 7 & tutier2code == 99) ), 0),                                  # Consumer purchases, n.e.c. (079900)


           t_shop_other_research = replace(tuactdur24,
                                 !( (tutier1code == 7 & tutier2code == 2) ), 0),                                 # Researching Purchases (070200)


           t_shop_other_wait = replace(tuactdur24,
                                 !( (tutier1code == 7 & tutier2code == 1 & tutier3code == 5) |                     # Waiting associated with shopping (070105)
                                    (tutier1code == 8 & tutier2code == 1 & tutier3code == 2) |                     # Waiting associated w/purchasing childcare svcs
                                    (tutier1code == 8 & tutier2code == 2 & tutier3code == 3) |                     # Waiting associated w/banking/financial services
                                    (tutier1code == 8 & tutier2code == 3 & tutier3code == 2) |                     # Waiting associated with legal services
                                    (tutier1code == 8 & tutier2code == 4 & tutier3code == 3) |                     # Waiting associated with medical services
                                    (tutier1code == 8 & tutier2code == 5 & tutier3code == 2) |                     # Waiting associated w/personal care services
                                    (tutier1code == 8 & tutier2code == 6 & tutier3code == 2) |                     # Waiting associated w/purchasing/selling real estate
                                    (tutier1code == 8 & tutier2code == 7 & tutier3code == 2) |                     # Waiting associated with veterinary services
                                    (tutier1code == 9 & tutier2code == 1 & tutier3code == 4) |                     # Waiting associated with using household services
                                    (tutier1code == 9 & tutier2code == 2 & tutier3code == 2) |                     # Waiting associated w/ home main/repair/d?cor/constr
                                    (tutier1code == 9 & tutier2code == 3 & tutier3code == 2) |                     # Waiting associated with pet services
                                    (tutier1code == 9 & tutier2code == 4 & tutier3code == 2) |                     # Waiting associated with using lawn & garden services
                                    (tutier1code == 9 & tutier2code == 5 & tutier3code == 2) |                     # Waiting associated with vehicle main. or repair svcs
                                    (tutier1code == 12 & tutier2code == 5 & tutier3code == 4) ), 0),               # Waiting associated with arts & entertainment

           # (tutier1code == 7 & tutier2code == 3) |                                                               # Security Procedures Related to Consumer Purchases (070300)
           # (tutier1code == 8 & tutier2code == 8) |                                                               # Security Procedures Rel. to Professional/Personal Svcs
           # (tutier1code == 8 & tutier2code == 6 & tutier3code == 1) |                                            # Activities rel. to purchasing/selling real estate

           t_shop_travel = replace(tuactdur24,
                                 !( (tutier1code == vtravel & tutier2code == 7) |                                # Travel related to consumer purchases
                                    (tutier1code == vtravel & tutier2code == 8) |                                # Travel related to Professional and Personal Care Services
                                    (tutier1code == vtravel & tutier2code == 9) |                                # Travel related to household services
                                    (tutier1code == vtravel & tutier2code == 12 & tutier3code == 4) ), 0),         # Travel related to arts and entertainment (181204)

           t_shop_ggf = t_shop_groceries + t_shop_gas + t_shop_food,
           t_shop_ttl = t_shop_groceries + t_shop_gas + t_shop_food + t_shop_other + t_shop_travel
	)

    # aggregate by activity and location
    df_location <- df_atus %>%
        mutate(loc = case_when(tewhere %in% c(1, 3)                                   ~ "home",
                               tewhere == 2                                           ~ "work",
                               tewhere == 4                                           ~ "restaurant",
                               tewhere %in% c(6, 7)                                   ~ "store",
                               tewhere == 8                                           ~ "school",
                               tewhere == 9                                           ~ "outside",
                               tewhere %in% c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21) ~ "vehicle/walking",
                               tewhere %in% c(5, 10, 30, 31, 32)                      ~ "church, library, bank, gym, post office",
                               TRUE                                                   ~ "other")) %>%
        select(tuyear, starts_with("t_shop_"), loc, weight) %>%
        gather(activity, timespent, -c(tuyear, loc, weight)) %>%
        filter(timespent >0) %>%
        group_by(activity, loc) %>%
        summarise(tuyear = first(tuyear),
                  nobs = n(),
                  ttl_weight = sum(weight),
                  ttl_timespent = wtd.mean(timespent, weight = weight)) %>%
        ungroup()

    # df_location %$%
    #   wtd.table(x=activity, y=loc, weight=ttl.timespent)

    df_atus %>%
        mutate(loc = case_when(tewhere %in% c(1, 3)                                   ~ "home",
                               tewhere == 2                                           ~ "work",
                               tewhere == 4                                           ~ "restaurant",
                               tewhere %in% c(6, 7)                                   ~ "store",
                               tewhere == 8                                           ~ "school",
                               tewhere == 9                                           ~ "outside",
                               tewhere %in% c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21) ~ "vehicle/walking",
                               tewhere %in% c(5, 10, 30, 31, 32)                      ~ "church, library, bank, gym, post office",
                               TRUE                                                   ~ "other")) %>%
        select(loc, starts_with("t_shop_")) %>%
        gather(activity, timespent, -loc) %>%
        filter(timespent >0) %$%
        table(activity, loc)


    # aggregate by tucaseid
    df_timeuse <-
        inner_join(# variables for which first observation is taken
                   df_atus %>%
                       select(tuyear, tuquarter, tumonth, tudiarydate, tudiaryday, gestfips, gereg,
                              tucaseid, age, male, black, married, num_child, hv_child, grade,
                              spouse_emp, spouse_workhours, working, unemp, retired, disabled, student, homemaker, othernilf, reason, udur,
                              faminc, weight) %>%
                       group_by(tucaseid) %>%
                       slice(1) %>%
                       ungroup(),
                   # variables for which average is taken
                   df_atus %>%
                       select(tucaseid,
                              t_shop_groceries, t_shop_gas, t_shop_food, t_shop_other, t_shop_travel, t_shop_ggf, t_shop_ttl,
                              t_shop_other_shop, t_shop_other_research, t_shop_other_wait,
                              t_ahk_work, t_ahk_work_search, t_work_ttl,
                              t_ahk_leisure_tv, t_ahk_leisure_oth, t_leisure,
                              t_eat, t_personal_care, t_ahk_sleep, t_ahk_esp, t_ahk_espleisure,
                              t_ahk_othercare, t_ahk_ownmdcare, t_ahk_childcare, t_childcare_play,
                              t_meals, t_housework, t_home_car_maintenance, t_ahk_homeproduction, t_ahk_shop, t_nonmarket_work_ttl,
                              t_ahk_education, t_ahk_civic, t_other, t_ttl) %>%
                       group_by(tucaseid) %>%
                       summarise_all(sum) %>%
                       ungroup(),
                   by = "tucaseid")

    rm(df_atus)

    # check for missing activities
    nobs_not_full_day <- df_timeuse %>%
        filter(t_ttl != 1440) %>%
        nrow() > 0

    if (nobs_not_full_day) stop("missing activities, total time < 24hr")

    df_timeuse %<>%
    mutate(twoyear = (tuyear - 2003) %/% 2 + 1,
           threeyear = (tuyear - 2002) %/% 3 + 1,
           drec20082009 = (tuyear %in% c(2008:2009)) %>% as.integer(),
           drec20082010 = (tuyear %in% c(2008:2010)) %>% as.integer())

    # # days of week are not equally represented in weight, ATUS adjusts so that days of week reflect frequency within the month of interview
    # # tab interview_day [aw=weight], matcell(freq)
    # freq <- table(interview_day)
    # freq <- freq/sum(freq)
    # dywt <- NA
    # for (day in seq(1,7)) {
    #  dywt[interview_day==day] = 1/7 * 1/freq[day]
    # }
    # daywt <- weight*dywt
    # weight_adj <- daywt/sum(daywt) # adjusted so sum of weights is one to conform with other years

    # convert to hours per week
    df_timeuse %<>%
        mutate_at(vars(starts_with("t_")), funs(./60*7))

    save(df_timeuse, df_location, file = paste0(edir_atus, "subset_", t, ".Rdata"))

    rm(df_timeuse, df_location)

    # go to next year
    t <- t + 1

}
