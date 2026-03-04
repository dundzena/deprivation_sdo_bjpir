
library(haven)
library(tidyverse)
library(scales)


# BES data

# load data
bes_full <- read_dta("data_uncleaned/BES2019_W15_v25.0.dta")

# select variables that I need
vars <- c("id",
  "sdoantiegal1", # SDO
  "sdoantiegal2",
  "sdoantiegal3",
  "sdoantiegal4",
  "sdodominance1",
  "sdodominance2",
  "sdodominance3",
  "sdodominance4", 
  "selfEcon", # deprivation - self
  "emEcon", # deprivation - ethnic minorities
  "wbEcon", # deprivation - white Britons
  "p_ethnicity", # ethnicity
  "p_education", # education
  "gender", # gender
  "immigCultural", #immigration positively contributes to culture
  "immigEcon", #immigration positively contributes to economy
  "p_gross_household", #Household income
  "age", # age in years
  "generalElectionVote", # which party respondent would vote if there were a general election tomorrow
  "country", # which uk country is R a part of
  "oslaua"
)

# select all variables from the big dataset

###SCOTLAND AND NOTHERN IRELAND EXCLUDED DUE TO NO SDO AND RD###

bes <- bes_full[, vars]

# Including rural/urban divide

lads <- read.csv("rural_urban.csv")

bes$oslaua <- as_factor(bes$oslaua, levels = "labels")

lads$oslaua <- lads$LAD21NM

bes <- merge(bes, lads, by = "oslaua")

bes$urban <- ifelse(bes$Urban_rural_flag == "Urban", 1, 0)

# 9999 is non-response

bes[bes == 9999] <- NA

# don't know/prefer not to answer categories

bes$p_gross_household[bes$p_gross_household == 16] <- NA
bes$p_gross_household[bes$p_gross_household == 17] <- NA

bes <- na.omit(bes)

# recode variable function

recode_sdo_bes <- function(sdo){
  sdo <-
    dplyr::recode(
      sdo,
      "1" = 7,
      "2" = 6,
      "3" = 5,
      "4" = 4,
      "5" = 3,
      "6" = 2,
      "7" = 1
    )
}

rev_var <- function(variable) {
  
  new_var <- (variable * -1) + max(variable, na.rm = T)
  
  return(new_var)
  
}

# recoding the reverse coded vairables

bes$sdoantiegal3 <- recode_sdo_bes(as.numeric(bes$sdoantiegal3))

bes$sdoantiegal4 <- recode_sdo_bes(as.numeric(bes$sdoantiegal4))

bes$sdodominance3   <- recode_sdo_bes(as.numeric(bes$sdodominance3))

bes$sdodominance4  <- recode_sdo_bes(as.numeric(bes$sdodominance4))


psych::alpha(
  data.frame(
    bes$sdoantiegal1,
    bes$sdoantiegal2,
    bes$sdoantiegal3,
    bes$sdoantiegal4,
    bes$sdodominance1,
    bes$sdodominance2,
    bes$sdodominance3,
    bes$sdodominance4
  )
)

# merging them into one scale

bes$sdo <-
  as.numeric(bes$sdoantiegal1) + as.numeric(bes$sdoantiegal2) + as.numeric(bes$sdoantiegal3) + as.numeric(bes$sdoantiegal4) + as.numeric(bes$sdodominance1) + as.numeric(bes$sdodominance2) + as.numeric(bes$sdodominance3) + as.numeric(bes$sdodominance4)




#IRD

bes$ird <- bes$wbEcon - bes$selfEcon  

#GRD

bes$grd <- bes$wbEcon - bes$emEcon


# recoding IRD

bes$ird <-
  dplyr::recode(
    bes$ird,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# Recoding GRD

bes$grd <-
  dplyr::recode(
    bes$grd,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# building a composite immigrant attitude measure

bes$immig_att <- (bes$immigCultural + bes$immigEcon) - 2

# reversing it. higher values = more dislike

bes$immig_att <- scales::rescale(bes$immig_att, to = c(0, 1))
bes$immig_att <- rev_var(bes$immig_att)


bes$sdo <- scales::rescale(bes$sdo, to = c(0, 1))
bes$grd <- scales::rescale(bes$grd, to = c(0, 1))
bes$grd <- rev_var(bes$grd)

bes$ird <- scales::rescale(bes$ird, to = c(0, 1))
bes$ird <- rev_var(bes$ird)

# 1 is white British, so we make everyone else a 0

bes$p_ethnicity[bes$p_ethnicity != 1] <- 0


# education

bes$p_education <- dplyr::recode(as.numeric(bes$p_education), 
                              "1" = 1, # no qualification
                              "2" = 2, # below university
                              "3" = 2,
                              "4" = 2,
                              "5" = 2,
                              "6" = 2,
                              "7" = 2,
                              "8" = 2,
                              "9" = 2,
                              "10" = 2,
                              "11" = 2,
                              "12" = 2,
                              "13" = 2,
                              "14" = 2,
                              "15" = 3, # university
                              "16" = 3,
                              "17" = 3,
                              "18" = 4, # unspecified
                              "19" = 4,
                              "20" = 4
                              
                              
)


# wales dummy

bes$wales <- ifelse(bes$country == 3, 1, 0)

bes <- na.omit(bes)


# saving the dataframe for analysis

saveRDS(bes, file = "besW15.rds")


### I only want income and ethnicity now

# load data
bes_full <- read_dta("data_uncleaned/BES2019_W15_v25.0.dta")

# select variables that I need
vars <- c("id",
          "p_ethnicity", # ethnicity
          "p_gross_household", #Household income,
          "p_education", # education
          "gender", # gender
          "age"
)


# select all variables from the big dataset

bes <- bes_full[, vars]

bes$p_gross_household[bes$p_gross_household == 16] <- NA
bes$p_gross_household[bes$p_gross_household == 17] <- NA

bes$p_ethnicity[bes$p_ethnicity != 1] <- 0

bes$p_education <- dplyr::recode(as.numeric(bes$p_education), 
                                 "1" = 1, # no qualification
                                 "2" = 2, # below university
                                 "3" = 2,
                                 "4" = 2,
                                 "5" = 2,
                                 "6" = 2,
                                 "7" = 2,
                                 "8" = 2,
                                 "9" = 2,
                                 "10" = 2,
                                 "11" = 2,
                                 "12" = 2,
                                 "13" = 2,
                                 "14" = 2,
                                 "15" = 3, # university
                                 "16" = 3,
                                 "17" = 3,
                                 "18" = 4, # unspecified
                                 "19" = 4,
                                 "20" = 4
                                 
                                 
)


bes$p_gross_household <- dplyr::recode(as.numeric(bes$p_gross_household), 
                                 "1" = 1, # 1st quartile
                                 "2" = 1, 
                                 "3" = 1,
                                 "4" = 1,
                                 "5" = 2, # 2nd quartile
                                 "6" = 2,
                                 "7" = 3, # 3rd quartile
                                 "8" = 3, 
                                 "9" = 3,
                                 "10" = 4, # 4th quartile
                                 "11" = 4,
                                 "12" = 4,
                                 "13" = 4,
                                 "14" = 4,
                                 "15" = 4 

                                 
)

bes <- na.omit(bes)

saveRDS(bes, file = "besW15_income.rds")



### Doing the same for wave 11

# load data
bes_full <- read_dta("data_uncleaned/BES2015_W11_v25.0.dta")

# select variables that I need
vars <- c("id",
  "selfEcon", # deprivation - self
  "emEcon", # deprivation - ethnic minorities
  "wbEcon", # deprivation - white Britons
  "p_ethnicity", # ethnicity
  "p_education", # education
  "gender", # gender
  "immigCultural", #immigration positively contributes to culture
  "immigEcon", #immigration positively contributes to economy
  "p_gross_household", #Household income
  "p_education", #education
  "age", # age in years
  "country" # which uk country is R a part of
)

# select all variables from the big dataset

bes <- bes_full[, vars]

bes <- as.matrix(bes)

bes <- as.data.frame(bes)

# 9999 is non-response

bes[bes == 9999] <- NA

# don't know/prefer not to answer categories

bes$p_gross_household[bes$p_gross_household == 16] <- NA
bes$p_gross_household[bes$p_gross_household == 17] <- NA

bes <- na.omit(bes)


rev_var <- function(variable) {
  
  new_var <- (variable * -1) + max(variable, na.rm = T)
  
  return(new_var)
  
}


#IRD

bes$ird <- bes$wbEcon - bes$selfEcon  

#GRD

bes$grd <- bes$wbEcon - bes$emEcon


# recoding IRD

bes$ird <-
  dplyr::recode(
    bes$ird,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# Recoding GRD

bes$grd <-
  dplyr::recode(
    bes$grd,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# building a composite immigrant attitude measure

bes$immig_att <- (bes$immigCultural + bes$immigEcon) - 2

# reversing it. higher values = more dislike

bes$immig_att <- rescale(bes$immig_att, to = c(0, 1))
bes$immig_att <- rev_var(bes$immig_att)



bes$sdo <- rescale(bes$sdo, to = c(0, 1))
bes$grd <- rescale(bes$grd, to = c(0, 1))
bes$grd <- rev_var(bes$grd)

bes$ird <- rescale(bes$ird, to = c(0, 1))
bes$ird <- rev_var(bes$ird)

# 1 is whites, so we keep those

bes$p_ethnicity[bes$p_ethnicity != 1] <- NA


# education

bes$p_education <- dplyr::recode(as.numeric(bes$p_education), 
                                 "1" = 1, # no qualification
                                 "2" = 2, # below university
                                 "3" = 2,
                                 "4" = 2,
                                 "5" = 2,
                                 "6" = 2,
                                 "7" = 2,
                                 "8" = 2,
                                 "9" = 2,
                                 "10" = 2,
                                 "11" = 2,
                                 "12" = 2,
                                 "13" = 2,
                                 "14" = 2,
                                 "15" = 3, # university
                                 "16" = 3,
                                 "17" = 3,
                                 "18" = 4, # unspecified
                                 "19" = 4,
                                 "20" = 4
                                 
                                 
)


# wales dummy

bes$wales <- ifelse(bes$country == 3, 1, 0)

bes <- na.omit(bes)


# saving the dataframe for analysis

saveRDS(bes, file = "besW11.rds")


# Now onto the full panel

bes_full <- read_dta("data_uncleaned/BES2019_W25_Panel_v25.1.dta")

# select variables that I need
vars <- c("id",
          "selfEconW11", # deprivation - self
          "selfEconW15",
          "emEconW11", # deprivation - ethnic minorities
          "emEconW15",
          "wbEconW11", # deprivation - white Britons
          "wbEconW15",
          "p_ethnicityW11", # ethnicity
          "p_ethnicityW15", 
          "p_educationW11", # education
          "p_educationW15",
          "gender", # gender
          "immigCulturalW11", #immigration positively contributes to culture
          "immigCulturalW15",
          "immigEconW11", #immigration positively contributes to economy
          "immigEconW15",
          "p_gross_householdW11", #Household income
          "p_gross_householdW15",
          "p_educationW11", #education
          "p_educationW15",
          "ageW11", # age in years
          "ageW15",
          "countryW11", # which uk country is R a part of
          "countryW15"
)

# select all variables from the big dataset

bes <- BES2019_W25_Panel_v25_1[, vars]

bes <- as.matrix(bes)

bes <- as.data.frame(bes)

# 9999 is non-response

bes[bes == 9999] <- NA

# don't know/prefer not to answer categories

bes$p_gross_householdW11[bes$p_gross_householdW11 == 16] <- NA
bes$p_gross_householdW11[bes$p_gross_householdW11 == 17] <- NA

bes$p_gross_householdW15[bes$p_gross_householdW15 == 16] <- NA
bes$p_gross_householdW15[bes$p_gross_householdW15 == 17] <- NA

bes <- na.omit(bes)


rev_var <- function(variable) {
  
  new_var <- (variable * -1) + max(variable, na.rm = T)
  
  return(new_var)
  
}


#IRD

bes$irdW11 <- bes$wbEconW11 - bes$selfEconW11 
bes$irdW15 <- bes$wbEconW15 - bes$selfEconW15  

#GRD

bes$grdW11 <- bes$wbEconW11 - bes$emEconW11
bes$grdW15 <- bes$wbEconW15 - bes$emEconW15



# recoding IRD

bes$irdW11 <-
  dplyr::recode(
    bes$irdW11,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )


bes$irdW15 <-
  dplyr::recode(
    bes$irdW15,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )


# Recoding GRD

bes$grdW11 <-
  dplyr::recode(
    bes$grdW11,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

bes$grdW15 <-
  dplyr::recode(
    bes$grdW15,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )


# building a composite immigrant attitude measure

bes$immig_attW11 <- (bes$immigCulturalW11 + bes$immigEconW11) - 2
bes$immig_attW15 <- (bes$immigCulturalW15 + bes$immigEconW15) - 2

# reversing it. higher values = more dislike

bes$immig_attW11 <- rescale(bes$immig_attW11, to = c(0, 1))
bes$immig_attW11 <- rev_var(bes$immig_attW11)

bes$immig_attW15 <- rescale(bes$immig_attW15, to = c(0, 1))
bes$immig_attW15 <- rev_var(bes$immig_attW15)


bes$grdW11 <- rescale(bes$grdW11, to = c(0, 1))
bes$grdW11 <- rev_var(bes$grdW11)

bes$grdW15 <- rescale(bes$grdW15, to = c(0, 1))
bes$grdW15 <- rev_var(bes$grdW15)

bes$irdW11 <- rescale(bes$irdW11, to = c(0, 1))
bes$irdW11 <- rev_var(bes$irdW11)

bes$irdW15 <- rescale(bes$irdW15, to = c(0, 1))
bes$irdW15 <- rev_var(bes$irdW15)

# 1 is whites, so we keep those

bes$p_ethnicityW11[bes$p_ethnicityW11 != 1] <- NA


# education

bes$p_educationW11 <- dplyr::recode(as.numeric(bes$p_educationW11), 
                                 "1" = 1, # no qualification
                                 "2" = 2, # below university
                                 "3" = 2,
                                 "4" = 2,
                                 "5" = 2,
                                 "6" = 2,
                                 "7" = 2,
                                 "8" = 2,
                                 "9" = 2,
                                 "10" = 2,
                                 "11" = 2,
                                 "12" = 2,
                                 "13" = 2,
                                 "14" = 2,
                                 "15" = 3, # university
                                 "16" = 3,
                                 "17" = 3,
                                 "18" = 4, # unspecified
                                 "19" = 4,
                                 "20" = 4
                                 
                                 
)

bes$p_educationW15 <- dplyr::recode(as.numeric(bes$p_educationW15), 
                                    "1" = 1, # no qualification
                                    "2" = 2, # below university
                                    "3" = 2,
                                    "4" = 2,
                                    "5" = 2,
                                    "6" = 2,
                                    "7" = 2,
                                    "8" = 2,
                                    "9" = 2,
                                    "10" = 2,
                                    "11" = 2,
                                    "12" = 2,
                                    "13" = 2,
                                    "14" = 2,
                                    "15" = 3, # university
                                    "16" = 3,
                                    "17" = 3,
                                    "18" = 4, # unspecified
                                    "19" = 4,
                                    "20" = 4
                                    
                                    
)



bes <- na.omit(bes)


# saving the dataframe for analysis

saveRDS(bes, file = "besW11_w15_panel.rds")



### BES with authoritarianism

# BES data

# load data
bes_full <- read_dta("data_uncleaned/BES2019_W15_v25.0.dta")

# select variables that I need
vars <- c("id",
          "sdoantiegal1", # SDO
          "sdoantiegal2",
          "sdoantiegal3",
          "sdoantiegal4",
          "sdodominance1",
          "sdodominance2",
          "sdodominance3",
          "sdodominance4", 
          "selfEcon", # deprivation - self
          "emEcon", # deprivation - ethnic minorities
          "wbEcon", # deprivation - white Britons
          "p_ethnicity", # ethnicity
          "p_education", # education
          "gender", # gender
          "immigCultural", #immigration positively contributes to culture
          "immigEcon", #immigration positively contributes to economy
          "p_gross_household", #Household income
          "age", # age in years
          "generalElectionVote", # which party respondent would vote if there were a general election tomorrow
          "country", # which uk country is R a part of
          "oslaua",
          "al_scaleW14" # authoritarianism
)

# select all variables from the big dataset

###SCOTLAND AND NOTHERN IRELAND EXCLUDED DUE TO NO SDO AND RD###

bes <- bes_full[, vars]

# Including rural/urban divide

lads <- read.csv("rural_urban.csv")

bes$oslaua <- as_factor(bes$oslaua, levels = "labels")

lads$oslaua <- lads$LAD21NM

bes <- merge(bes, lads, by = "oslaua")

bes$urban <- ifelse(bes$Urban_rural_flag == "Urban", 1, 0)

# 9999 is non-response

bes[bes == 9999] <- NA

# don't know/prefer not to answer categories

bes$p_gross_household[bes$p_gross_household == 16] <- NA
bes$p_gross_household[bes$p_gross_household == 17] <- NA

bes <- na.omit(bes)

# recode variable function

recode_sdo_bes <- function(sdo){
  sdo <-
    dplyr::recode(
      sdo,
      "1" = 7,
      "2" = 6,
      "3" = 5,
      "4" = 4,
      "5" = 3,
      "6" = 2,
      "7" = 1
    )
}

rev_var <- function(variable) {
  
  new_var <- (variable * -1) + max(variable, na.rm = T)
  
  return(new_var)
  
}

# recoding the reverse coded vairables

bes$sdoantiegal3 <- recode_sdo_bes(as.numeric(bes$sdoantiegal3))

bes$sdoantiegal4 <- recode_sdo_bes(as.numeric(bes$sdoantiegal4))

bes$sdodominance3   <- recode_sdo_bes(as.numeric(bes$sdodominance3))

bes$sdodominance4  <- recode_sdo_bes(as.numeric(bes$sdodominance4))


psych::alpha(
  data.frame(
    bes$sdoantiegal1,
    bes$sdoantiegal2,
    bes$sdoantiegal3,
    bes$sdoantiegal4,
    bes$sdodominance1,
    bes$sdodominance2,
    bes$sdodominance3,
    bes$sdodominance4
  )
)

# merging them into one scale

bes$sdo <-
  as.numeric(bes$sdoantiegal1) + as.numeric(bes$sdoantiegal2) + as.numeric(bes$sdoantiegal3) + as.numeric(bes$sdoantiegal4) + as.numeric(bes$sdodominance1) + as.numeric(bes$sdodominance2) + as.numeric(bes$sdodominance3) + as.numeric(bes$sdodominance4)




#IRD

bes$ird <- bes$wbEcon - bes$selfEcon  

#GRD

bes$grd <- bes$wbEcon - bes$emEcon


# recoding IRD

bes$ird <-
  dplyr::recode(
    bes$ird,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# Recoding GRD

bes$grd <-
  dplyr::recode(
    bes$grd,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# building a composite immigrant attitude measure

bes$immig_att <- (bes$immigCultural + bes$immigEcon) - 2

# reversing it. higher values = more dislike

bes$immig_att <- scales::rescale(bes$immig_att, to = c(0, 1))
bes$immig_att <- rev_var(bes$immig_att)



bes$sdo <- scales::rescale(bes$sdo, to = c(0, 1))
bes$auth <- scales::rescale(as.numeric(bes$al_scaleW14), to = c(0, 1))
bes$grd <- scales::rescale(bes$grd, to = c(0, 1))
bes$grd <- rev_var(bes$grd)

bes$ird <- scales::rescale(bes$ird, to = c(0, 1))
bes$ird <- rev_var(bes$ird)

# 1 is white British, so we make everyone else a 0

bes$p_ethnicity[bes$p_ethnicity != 1] <- 0


# education

bes$p_education <- dplyr::recode(as.numeric(bes$p_education), 
                                 "1" = 1, # no qualification
                                 "2" = 2, # below university
                                 "3" = 2,
                                 "4" = 2,
                                 "5" = 2,
                                 "6" = 2,
                                 "7" = 2,
                                 "8" = 2,
                                 "9" = 2,
                                 "10" = 2,
                                 "11" = 2,
                                 "12" = 2,
                                 "13" = 2,
                                 "14" = 2,
                                 "15" = 3, # university
                                 "16" = 3,
                                 "17" = 3,
                                 "18" = 4, # unspecified
                                 "19" = 4,
                                 "20" = 4
                                 
                                 
)


# wales dummy

bes$wales <- ifelse(bes$country == 3, 1, 0)

bes <- na.omit(bes)


# saving the dataframe for analysis

saveRDS(bes, file = "besW15_auth.rds")

### Wave 21 with RWA and GRD

# BES data

# load data
bes_full <- read_dta("data_uncleaned/BES2019_W21_v25.1.dta")

# select variables that I need
vars <- c("id",
          "al_scale", # RWA
          "selfEcon", # deprivation - self
          "emEcon", # deprivation - ethnic minorities
          "wbEcon", # deprivation - white Britons
          "p_ethnicity", # ethnicity
          "p_education", # education
          "gender", # gender
          "immigEcon", #immigration positively contributes to economy
          "p_gross_household", #Household income
          "age", # age in years
          "generalElectionVote", # which party respondent would vote if there were a general election tomorrow
          "country", # which uk country is R a part of
          "oslaua"
)

# select all variables from the big dataset

###SCOTLAND AND NOTHERN IRELAND EXCLUDED DUE TO NO SDO AND RD###

bes <- bes_full[, vars]

# Including rural/urban divide

lads <- read.csv("rural_urban.csv")

bes$oslaua <- as_factor(bes$oslaua, levels = "labels")

lads$oslaua <- lads$LAD21NM

bes <- merge(bes, lads, by = "oslaua")

bes$urban <- ifelse(bes$Urban_rural_flag == "Urban", 1, 0)

# 9999 is non-response

bes[bes == 9999] <- NA

# don't know/prefer not to answer categories

bes$p_gross_household[bes$p_gross_household == 16] <- NA
bes$p_gross_household[bes$p_gross_household == 17] <- NA

bes <- na.omit(bes)


rev_var <- function(variable) {
  
  new_var <- (variable * -1) + max(variable, na.rm = T)
  
  return(new_var)
  
}


#IRD

bes$ird <- bes$wbEcon - bes$selfEcon  

#GRD

bes$grd <- bes$wbEcon - bes$emEcon


# recoding IRD

bes$ird <-
  dplyr::recode(
    bes$ird,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# Recoding GRD

bes$grd <-
  dplyr::recode(
    bes$grd,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# building a composite immigrant attitude measure

bes$immig_att <- (bes$immigEcon) - 1

# reversing it. higher values = more dislike

bes$immig_att <- scales::rescale(bes$immig_att, to = c(0, 1))
bes$immig_att <- rev_var(bes$immig_att)



bes$auth <- scales::rescale(as.numeric(bes$al_scale), to = c(0, 1))
bes$grd <- scales::rescale(bes$grd, to = c(0, 1))
bes$grd <- rev_var(bes$grd)

bes$ird <- scales::rescale(bes$ird, to = c(0, 1))
bes$ird <- rev_var(bes$ird)

# 1 is white British, so we make everyone else a 0

bes$p_ethnicity[bes$p_ethnicity != 1] <- 0


# education

bes$p_education <- dplyr::recode(as.numeric(bes$p_education), 
                                 "1" = 1, # no qualification
                                 "2" = 2, # below university
                                 "3" = 2,
                                 "4" = 2,
                                 "5" = 2,
                                 "6" = 2,
                                 "7" = 2,
                                 "8" = 2,
                                 "9" = 2,
                                 "10" = 2,
                                 "11" = 2,
                                 "12" = 2,
                                 "13" = 2,
                                 "14" = 2,
                                 "15" = 3, # university
                                 "16" = 3,
                                 "17" = 3,
                                 "18" = 4, # unspecified
                                 "19" = 4,
                                 "20" = 4
                                 
                                 
)


# wales dummy

bes$wales <- ifelse(bes$country == 3, 1, 0)

bes <- na.omit(bes)


# saving the dataframe for analysis

saveRDS(bes, file = "besW21_auth.rds")




# BES with the left-right variable

# load data
bes_full <- read_dta("data_uncleaned/BES2019_W15_v25.0.dta")

# select variables that I need
vars <- c("id",
          "sdoantiegal1", # SDO
          "sdoantiegal2",
          "sdoantiegal3",
          "sdoantiegal4",
          "sdodominance1",
          "sdodominance2",
          "sdodominance3",
          "sdodominance4", 
          "selfEcon", # deprivation - self
          "emEcon", # deprivation - ethnic minorities
          "wbEcon", # deprivation - white Britons
          "p_ethnicity", # ethnicity
          "p_education", # education
          "gender", # gender
          "immigCultural", #immigration positively contributes to culture
          "immigEcon", #immigration positively contributes to economy
          "p_gross_household", #Household income
          "age", # age in years
          "generalElectionVote", # which party respondent would vote if there were a general election tomorrow
          "country", # which uk country is R a part of
          "oslaua", # local authority name
          "leftRight" # left-right orientation
)

# select all variables from the big dataset

###SCOTLAND AND NOTHERN IRELAND EXCLUDED DUE TO NO SDO AND RD###

bes <- bes_full[, vars]

# Including rural/urban divide

lads <- read.csv("rural_urban.csv")

bes$oslaua <- as_factor(bes$oslaua, levels = "labels")

lads$oslaua <- lads$LAD21NM

bes <- merge(bes, lads, by = "oslaua")

bes$urban <- ifelse(bes$Urban_rural_flag == "Urban", 1, 0)

# 9999 is non-response

bes[bes == 9999] <- NA

# don't know/prefer not to answer categories

bes$p_gross_household[bes$p_gross_household == 16] <- NA
bes$p_gross_household[bes$p_gross_household == 17] <- NA

bes <- na.omit(bes)

# recode variable function

recode_sdo_bes <- function(sdo){
  sdo <-
    dplyr::recode(
      sdo,
      "1" = 7,
      "2" = 6,
      "3" = 5,
      "4" = 4,
      "5" = 3,
      "6" = 2,
      "7" = 1
    )
}

rev_var <- function(variable) {
  
  new_var <- (variable * -1) + max(variable, na.rm = T)
  
  return(new_var)
  
}

# recoding the reverse coded vairables

bes$sdoantiegal3 <- recode_sdo_bes(as.numeric(bes$sdoantiegal3))

bes$sdoantiegal4 <- recode_sdo_bes(as.numeric(bes$sdoantiegal4))

bes$sdodominance3   <- recode_sdo_bes(as.numeric(bes$sdodominance3))

bes$sdodominance4  <- recode_sdo_bes(as.numeric(bes$sdodominance4))


psych::alpha(
  data.frame(
    bes$sdoantiegal1,
    bes$sdoantiegal2,
    bes$sdoantiegal3,
    bes$sdoantiegal4,
    bes$sdodominance1,
    bes$sdodominance2,
    bes$sdodominance3,
    bes$sdodominance4
  )
)

# merging them into one scale

bes$sdo <-
  as.numeric(bes$sdoantiegal1) + as.numeric(bes$sdoantiegal2) + as.numeric(bes$sdoantiegal3) + as.numeric(bes$sdoantiegal4) + as.numeric(bes$sdodominance1) + as.numeric(bes$sdodominance2) + as.numeric(bes$sdodominance3) + as.numeric(bes$sdodominance4)




#IRD

bes$ird <- bes$wbEcon - bes$selfEcon  

#GRD

bes$grd <- bes$wbEcon - bes$emEcon


# recoding IRD

bes$ird <-
  dplyr::recode(
    bes$ird,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# Recoding GRD

bes$grd <-
  dplyr::recode(
    bes$grd,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

# building a composite immigrant attitude measure

bes$immig_att <- (bes$immigCultural + bes$immigEcon) - 2

# reversing it. higher values = more dislike

bes$immig_att <- scales::rescale(bes$immig_att, to = c(0, 1))
bes$immig_att <- rev_var(bes$immig_att)



bes$sdo <- scales::rescale(bes$sdo, to = c(0, 1))
bes$grd <- scales::rescale(bes$grd, to = c(0, 1))
bes$grd <- rev_var(bes$grd)

bes$ird <- scales::rescale(bes$ird, to = c(0, 1))
bes$ird <- rev_var(bes$ird)

# 1 is white British, so we make everyone else a 0

bes$p_ethnicity[bes$p_ethnicity != 1] <- 0


# education

bes$p_education <- dplyr::recode(as.numeric(bes$p_education), 
                                 "1" = 1, # no qualification
                                 "2" = 2, # below university
                                 "3" = 2,
                                 "4" = 2,
                                 "5" = 2,
                                 "6" = 2,
                                 "7" = 2,
                                 "8" = 2,
                                 "9" = 2,
                                 "10" = 2,
                                 "11" = 2,
                                 "12" = 2,
                                 "13" = 2,
                                 "14" = 2,
                                 "15" = 3, # university
                                 "16" = 3,
                                 "17" = 3,
                                 "18" = 4, # unspecified
                                 "19" = 4,
                                 "20" = 4
                                 
                                 
)


# wales dummy

bes$wales <- ifelse(bes$country == 3, 1, 0)

bes <- na.omit(bes)


# saving the dataframe for analysis

saveRDS(bes, file = "besW15_lr.rds")

