######################################################################
### 1667 - Research and Policy Seminar - Economics of Distribution ###
###                       Austrian Microcensus                     ###
###                         Alexander Huber                        ###
######################################################################

# install packages
#install.packages("tidyverse") # for datawrangling
#install.packages("haven") # for loading .dta-Files
#install.packages("dineq") # for distributional measures
#install.packages("Hmisc) # for distributional measures

library(tidyverse)
library(haven)
library(dineq)
library(Hmisc)

# Mikrozensus 2021 
#----------------------------------------------------------------------------------------------------------------------------------#

# 1) Load Data ####

# main sample
mz21_full <- haven::read_dta("C:/Users/alhuber/Documents/MZ/2021/10752_da10_de_v4_0.dta") # 174000 obs, 253 variables

# income data (ONLY AVAILABLE UPON REQUEST)
mz21_inc <- haven::read_sav("C:/Users/alhuber/Documents/MZ/EK und AMS/ek_2021.sav")

mz21_full <- right_join(mz21_full, mz21_inc, by = c("asbper","ajahr","aquartal"))

# stuff runs faster when we turn it into a dataframe
#mz21_full <- as.data.frame(mz21_full)

# asbper   : personal ID
# ajahr    : year of the survey
# aquartal : quarter

# overview
View(mz21_full)

mz21_full %>% group_by(ajahr,aquartal) %>% summarise(n=n()) # approx. 43,500 obs. per quarter


# sum of weights 8.8 Mio. (inhabitants of Austria)
sum(mz21_full$gewjahr)
# gewjahr : survey weight 

# persons
mz21_full %>% ungroup() %>% summarise(persons = sum(gewjahr))

# !!! IMPORTANT: !!! 
# even though a person or HH can be part of the survey in up to 5 quarters, it is treated as a "new person" in each quarter (except we are interested in the panel structure)

# households
mz21_full %>% filter(xhrp==1) %>% # xhrp is the household reference person
  ungroup() %>% 
  summarise(households = sum(gewjahr))

# or:
mz21_full %>% group_by(aquartal,asbhh) %>% # first group by quarter and household
  slice(1) %>% # we only want one entry by household and quarter (otherwise we remain on the personal level)
  ungroup() %>% 
  summarise(households = sum(gewjahr)) 
# asbhh : household ID


#----------------------------------------------------------------------------------------------------------------------------------#
# 2) create and adapt variables ####


## 2.A) age group ####
mz21_full <- mz21_full %>% mutate(agegroup = case_when(xbalt5 == 0 ~ "child",                               
                                                       xbalt5 %in% c(1,2,3,4) ~ "under 35",                
                                                       xbalt5 %in% c(5,6,7) ~ "35 - 49",
                                                       xbalt5 %in% c(8,9,10) ~ "50 - 64",
                                                       xbalt5 %in% c(11,12,13,14,15) ~ "65 and older"))
mz21_full %>% group_by(agegroup) %>% summarise(n = n()) # alphabetical order does not fit
mz21_full$agegroup <- factor(mz21_full$agegroup, levels = c("child","under 35","35 - 49","50 - 64","65 and older"))
# xbalt5 : age group (5-year cohorts)

## 2.B) Bundesland/region ####
mz21_full <- mz21_full %>% mutate(bundesland = case_when(xnuts2==11 ~ "Burgenland",
                                                         xnuts2==12 ~ "Lower Austria",
                                                         xnuts2==13 ~ "Vienna",
                                                         xnuts2==21 ~ "Carinthia",
                                                         xnuts2==22 ~ "Styria",
                                                         xnuts2==31 ~ "Upper Austria",
                                                         xnuts2==32 ~ "Salzburg",
                                                         xnuts2==33 ~ "Tyrol",
                                                         xnuts2==34 ~ "Vorarlberg"),
                                  urbanisation = case_when(xurb==1 ~ "high",
                                                           xurb==2 ~ "medium",
                                                           xurb==3 ~ "low"))

## 2.C) household type ####
mz21_full %>% group_by(xhhtyp1,xhhtyp2,xhhtyp3,xhhtyp4) %>% summarise(n=n())
mz21_full <- mz21_full %>% mutate(HHTYP = case_when(xhhtyp3==1 ~ "couple no child",
                                                    xhhtyp3==2 ~ "couple with child",
                                                    xhhtyp3 %in% c(3,4) ~ "single parent",
                                                    xhhtyp3 %in% c(5,7) ~ "other",
                                                    xhhtyp3==6 ~ "single"))

## 2.D) employment status (in reference week) ####
mz21_full <- mz21_full %>% mutate(empstatus = case_when(xerwstat==1 ~ "employed",
                                                        xerwstat==2 ~ "unemployed",
                                                        xerwstat==3 ~ "non working",
                                                        xerwstat==4 ~ "military or civil service",
                                                        xerwstat==5 ~ "under 15"),
                                  hrs_contract = ifelse(dvers>=0,dvers,NA),             # hrs/week according to contract
                                  hrs_normal = ifelse(dstd>=0,dstd,NA),                 # hrs/week normally worked
                                  hrs_contract_cat = case_when(xdvers5==0 ~ "less than 12",
                                                               xdvers5==1 ~ "12 - 24",
                                                               xdvers5==2 ~ "25 - 35",
                                                               xdvers5==3 ~ "36 - 40",
                                                               xdvers5==4 ~ "41 - 59",
                                                               xdvers5==5 ~ "60 and more",
                                                               TRUE ~ NA),
                                  hrs_normal_cat = case_when(xdstd5==0 ~ "less than 12",
                                                             xdstd5==1 ~ "12 - 24",
                                                             xdstd5==2 ~ "25 - 35",
                                                             xdstd5==3 ~ "36 - 40",
                                                             xdstd5==4 ~ "41 - 59",
                                                             xdstd5==5 ~ "60 and more",
                                                             TRUE ~ NA),
                                  parttime = ifelse(xdvers==1,"part-time","full-time"), # up to 35 hrs/week --> part-time
                                  parttime_reason = case_when(dteiljngr==0 ~ "fulltime",
                                                              dteiljngr==1 ~ "care",
                                                              dteiljngr==2 ~ "education",
                                                              dteiljngr==3 ~ "health",
                                                              dteiljngr==4 ~ "no fulltime found",
                                                              dteiljngr==5 ~ "voluntary",
                                                              dteiljngr==6 ~ "family reasons",
                                                              dteiljngr==7 ~ "personal reasons",
                                                              dteiljngr==8 ~ "other reason",
                                                              TRUE ~ NA))
mz21_full$hrs_contract_cat <- factor(mz21_full$hrs_contract_cat, levels = c("less than 12","12 - 24","25 - 35","36 - 40","41 - 59","60 and more"))
mz21_full$hrs_normal_cat <- factor(mz21_full$hrs_normal_cat, levels = c("less than 12","12 - 24","25 - 35","36 - 40","41 - 59","60 and more"))



## 2.E) equivalence weight ####
# compute an equivalence weight for each household (OECD-modified scale https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Equivalised_income )
mz21_full <- mz21_full %>% mutate(eq_person = case_when(xhrp==1 ~ 1,
                                                        xhrp!=1 & balt>=14 ~ 0.5,                
                                                        xhrp!=1 & balt<14 ~ 0.3))
mz21_full <- mz21_full %>% group_by(ajahr,aquartal,asbhh) %>% mutate(eq_wgt = sum(eq_person)) # HH-level equivalence weight is the sum of its members' weights
# balt : age in years

## 2.F) personal variables
mz21_full <- mz21_full %>% ungroup() %>% mutate(sex = ifelse(bsex==1,"male","female"),
                                                parent = ifelse(xeltern1==1,1,0),             # person is a parent
                                                age_youngest = ifelse(xminalt>=0,xminalt,NA), # age of youngest child in HH
                                                age_oldest = ifelse(xmaxalt>=0,xmaxalt,NA),   # age of oldest child in HH
                                                children = ifelse(xanzkind>=0,xanzkind,NA))   # number of children in HH


# create a household file
mz21_hh <- mz21_full %>% filter(xhrp==1) # filter by reference person


#----------------------------------------------------------------------------------------------------------------------------------------------#
# 3) INCOME and WORKING HOURS ####

# income in the Microcensus comes with one big advantage and one big disadvantage
# bad: only employee income --> we cannot compute distributions for the whole population, no disposable household income, etc.
# good: great and reliable (tax data) measure for wages, hourly wages, gross-net differences etc. --> we need to focus on these aspects

mz21_full <- mz21_full %>% mutate(incnet = ifelse(rincmon>=0,rincmon,NA),
                                  incgross = ifelse(rincmob>=0,rincmob,NA))

summary(mz21_full$incnet)
summary(mz21_full$incgross)

# we have 100,000 NA's --> income is available only for 42% of the sample

mz21_full %>% group_by(empstatus,is.na(incnet)) %>% summarise(n=n())
mz21_full %>% group_by(empstatus,dberst,is.na(incnet)) %>% summarise(n=n())
# also within employed persons, we have NA's for income --> be aware

mz21_full %>% group_by(empstatus,is.na(hrs_contract)) %>% summarise(n=n())
mz21_full %>% group_by(empstatus,is.na(hrs_normal)) %>% summarise(n=n())


## 3.A) hourly wages ####
mz21_full <- mz21_full %>% mutate(hrly_wage_contract_gross = ifelse(incgross>=0 & hrs_contract>=0, incgross/(hrs_contract*(13/3)), NA),
                                  hrly_wage_contract_net = ifelse(incnet>=0 & hrs_contract>=0, incgross/(hrs_contract*(13/3)), NA),
                                  hrly_wage_normal_gross = ifelse(incgross>=0 & hrs_normal>=0, incnet/(hrs_normal*(13/3)), NA),
                                  hrly_wage_normal_net = ifelse(incnet>=0 & hrs_normal>=0, incnet/(hrs_normal*(13/3)), NA))


## 3.B) plots ####

plotdata <- mz21_full %>% filter(empstatus=="employed",hrly_wage_contract_gross>=0) %>% group_by(sex,agegroup) %>% summarise(mean = sum(hrly_wage_contract_gross*gewjahr)/sum(gewjahr))
ggplot(plotdata, aes(x=agegroup, y=mean, fill=sex)) + geom_bar(stat="identity",position="dodge") +
  labs(title = "Men have higher hourly wages than women",
       x="Agegroup",
       y="Mean hourly wage") +
  theme_light() + theme(legend.title = element_blank())

plotdata <- mz21_full %>% filter(empstatus=="employed",hrly_wage_contract_gross>=0) %>% group_by(sex,agegroup,parent) %>% summarise(mean = sum(hrly_wage_contract_gross*gewjahr)/sum(gewjahr))
ggplot(plotdata, aes(x=agegroup, y=mean, fill=factor(parent))) + geom_bar(stat="identity",position="dodge") + 
  labs(title = "Mothers earn less than women without child, but fathers earn more",
       x="Agegroup",
       y="Mean hourly wage") +
  theme_light() + theme(legend.title = element_text("fam")) +  facet_wrap(~sex)

# hrs normally worked is available for all employed persons, hrs_contract shows NA's

# let's look at employment shares by age group
mz21_full %>% group_by(agegroup,empstatus) %>% summarise(sum = sum(gewjahr))

plotdata <- mz21_full %>% group_by(agegroup,empstatus) %>% summarise(people = sum(gewjahr)) %>% group_by(agegroup) %>% mutate(share = people/sum(people,na.rm=TRUE))
ggplot(plotdata, aes(x=agegroup,y=share,fill=empstatus)) + 
  geom_bar(stat="identity",position="stack") + theme_light() + theme(axis.text.x = element_text(angle=45,hjust=1))

plotdata <- mz21_full %>% group_by(sex,agegroup,empstatus) %>% summarise(people = sum(gewjahr)) %>% group_by(sex,agegroup) %>% mutate(share = people/sum(people,na.rm=TRUE))
ggplot(plotdata, aes(x=agegroup,y=share,fill=empstatus)) + 
  geom_bar(stat="identity",position="stack") + theme_light() + theme(axis.text.x = element_text(angle=45,hjust=1)) + facet_wrap(~sex)

plotdata <- mz21_full %>% filter(empstatus=="employed") %>% group_by(sex,agegroup,parttime) %>% summarise(people = sum(gewjahr)) %>% group_by(sex,agegroup) %>% mutate(share = people/sum(people))
ggplot(plotdata, aes(x=agegroup,y=share,fill=parttime)) +
  geom_bar(stat="identity",position="stack") + theme_light() + theme(axis.text.x = element_text(angle=45,hjust=1)) + facet_wrap(~sex)



#----------------------------------------------------------------------------------------------------------------------------------------------#
# 4) HOUSING ####

# who lives where?
mz21_hh %>% group_by(wrechtk) %>% summarise(n=n()) 
mz21_hh %>% group_by(wrechtk,wrechtl) %>% summarise(n=n())
mz21_hh %>% group_by(wrechtk,wrechtl,wrechthm) %>% summarise(n=n())


# let's make new variables
mz21_hh <- mz21_hh %>% mutate(owner = case_when(wrechtk==2~"owner",
                                                TRUE ~ "no owner"),
                              tenure_compact = case_when(wrechtk==1 ~ "renter",
                                                         wrechtk==2 ~ "owner",
                                                         wrechtk==3 ~ "rent-free"),
                              tenure_detailed = case_when(wrechtk==2 & wrechtl==1 ~ "owner (house)",
                                                          wrechtk==2 & wrechtl==2 ~ "owner (apartment)",
                                                          wrechtk==1 & wrechtl==3 & wrechthm==1 ~ "renter (public)",
                                                          wrechtk==1 & wrechtl==3 & wrechthm==2 ~ "renter (association)",
                                                          wrechtk==1 & wrechtl==3 & wrechthm==3 ~ "renter (private)",
                                                          TRUE ~ "other"))
mz21_hh %>% group_by(wrechtk,wrechtl,wrechthm, tenure_compact, tenure_detailed) %>% summarise(n=n())
# CAUTION: for simplicity, we count company apartments (Dienstwohnung) and subleases (Untermiete) as "other"

# owner share by Bundesland
mz21_hh %>% group_by(bundesland, owner) %>% summarise(hh = sum(gewjahr))

mz21_hh %>% group_by(bundesland,owner) %>% summarise(hh = sum(gewjahr)) %>% group_by(bundesland) %>% mutate(share = hh/sum(hh)*100)

# make a plot
plotdata <- mz21_hh %>% group_by(bundesland,owner) %>% summarise(hh = sum(gewjahr)) %>% group_by(bundesland) %>% mutate(share = hh/sum(hh)*100)
ggplot(plotdata, aes(x=bundesland, y=share, fill=owner)) + geom_bar(stat = "identity", position = "stack") 
# make a nicer plot
ggplot(plotdata, aes(x=bundesland, y=share, fill=owner)) + geom_bar(stat = "identity", position = "stack") +
  labs(x="Bundesland",y="Owner share in %",title="Owner share by Bundesland",caption="Source: Microcensus 2021") +
  theme_light() + theme(axis.text.x = element_text(angle = 45, hjust=1), legend.title = element_blank())


# the average monthly total housing costs by tenure status
mz21_hh %>% group_by(tenure_compact) %>% summarise(wk_mean = mean(wkges),
                                                   wk_wtd_mean = weighted.mean(wkges,gewjahr))
# wkges : total housing costs (excluding energy, mortgage repayment and interest payments)

# how are housing costs distributed?
summary(mz21_hh$wkges) # seems like there are lots of NA's (-3)

mz21_hh %>% group_by(tenure_compact) %>% summarise(wk_min = min(wkges),
                                                   wk_max = max(wkges))

# How many observations without housing costs?
mz21_hh %>% mutate(costs_given = ifelse(wkges==-3,"no","yes")) %>% group_by(costs_given) %>% summarise(n=n())
mz21_hh %>% mutate(costs_given = ifelse(wkges==-3,"no","yes")) %>% group_by(tenure_compact, costs_given) %>% summarise(n=n())
mz21_hh %>% mutate(costs_given = ifelse(wkges==-3,"no","yes")) %>% group_by(tenure_detailed, costs_given) %>% summarise(n=n())
# all main tenants (except official residences and subtenants) have housing costs given


## 4.A) Rental Market ####

# create a renter file 
mz21_rent <- mz21_hh %>% filter(tenure_detailed %in% c("renter (association)","renter (private)","renter (public)"))

# Recode NA's to zero (not 100% correct, but that is how Statistics Austria computes their net-rent variable)
mz21_rent$wkges[mz21_rent$wkges==-3] <- 0
mz21_rent$wkbetr[mz21_rent$wkbetr==-3] <- 0
mz21_rent$wkheizmu[mz21_rent$wkheizmu==-3] <- 0
mz21_rent$wkwwmu[mz21_rent$wkwwmu==-3] <- 0
mz21_rent$wkheizwwou[mz21_rent$wkheizwwou==-3] <- 0
mz21_rent$wkgar[mz21_rent$wkgar==-3] <- 0
mz21_rent$wkgarxtra[mz21_rent$wkgarxtra==-3] <- 0

# new variables
mz21_rent <- mz21_rent %>% mutate(hc_m2 = wkges/wm2,
                                  rent = case_when(wkgarent == 1 ~ wkges - wkbetr - wkheizmu - wkwwmu - wkheizwwou - wkgar,
                                                   wkgarent != 1 ~ wkges - wkbetr - wkheizmu - wkwwmu - wkheizwwou),
                                  rent_m2 = rent/wm2)


# let's take another look at the distribution
summary(mz21_rent$hc_m2)
ggplot(mz21_rent, aes(x=hc_m2)) + geom_density()

# rent over different tenure forms
mz21_rent %>% group_by(tenure_detailed) %>% summarise(rent = weighted.mean(rent,gewjahr),
                                                      rent_m2 = weighted.mean(rent_m2,gewjahr))
# regional differences
mz21_rent %>% group_by(tenure_detailed, bundesland) %>% summarise(rent_m2 = weighted.mean(rent_m2,gewjahr))

# hard to read --> WIDE-FORMAT
mz21_rent_wide <- mz21_rent %>% group_by(tenure_detailed, bundesland) %>% summarise(rent_m2 = weighted.mean(rent_m2,gewjahr))
mz21_rent_wide <- mz21_rent_wide %>% 
  pivot_wider(id_cols = bundesland,
              names_from = tenure_detailed,
              values_from = rent_m2)
mz21_rent_wide

# back to LONG-FORMAT
mz21_rent_long <- mz21_rent_wide %>%
  pivot_longer(cols = c(`renter (association)`,`renter (private)`,`renter (public)`),
               names_to = "tenure_detailed",
               values_to = "rent_m2")
mz21_rent_long

# another plot
ggplot(mz21_rent_long, aes(x=bundesland, y=rent_m2, fill=tenure_detailed)) + geom_bar(stat="identity",position = "dodge") 
ggplot(mz21_rent_long, aes(x=bundesland, y=rent_m2, fill=tenure_detailed)) + geom_bar(stat="identity",position = "dodge") + facet_wrap(~tenure_detailed, ncol=1)
ggplot(mz21_rent_long, aes(x=bundesland, y=rent_m2, fill=tenure_detailed)) + geom_bar(stat="identity",position = "dodge") + 
  labs(x="",y="Average rent per m²",title="Rent per m² by tenure status and Bundesland",caption="Source: Microcensus 2021") +
  geom_text(aes(label = round(rent_m2,1)),
            vjust = -0.2) +
  theme_light() + theme(axis.text.x = element_text(angle=45,hjust=1),
                        legend.title = element_blank()) + 
  facet_wrap(~tenure_detailed, ncol=1)



#----------------------------------------------------------------------------------------------------------------------------------------------#

