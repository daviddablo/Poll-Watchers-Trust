####################################
###   Race and Trust Data Clean  ###
###         Laura Uribe          ###
###     Created: 11/07/2024      ###
###     Updated: 12/10/2024      ###
####################################

library(readr)
library(tidyverse)
library(dplyr)
library(sjmisc)
library(readxl)
library(beepr)

# Header --------------------------------------------------------------------

# if(Sys.info()[7] == "laura"){
#   setwd("/Users/laura/Dropbox/RA/MIT Evolving Election Administration Landscape/Determinants of Ethnoracial Electoral Trust/")
# }

if(Sys.info()[7] == "daviddablo"){
  setwd("/Users/daviddablo/Desktop/For Replicatability/step1_cleanup")
}

# df_raw <- read.csv("2_data/df_raw.csv", header=TRUE)
df_raw <- read.csv("df_raw.csv", header=TRUE)
df_raw <- df_raw[-1,]
df_raw <- df_raw[-1,]

df_pass <- subset(df_raw, X3_Poll.Confidence.Nat != "")
df_pass <- subset(df_pass, soft2 == 1)
remove(df_raw)
# DV: Trust Own/Other State --------------------------------------------------------------
table(df_pass$Trust_Own_Vote)
table(df_pass$Trust_Local_Vote)
table(df_pass$Trust_State_Vote.)
table(df_pass$Trust_Country_Vote)

table(df_pass$Trust_Own_Vote1)
table(df_pass$Trust_Local_Vote1)
table(df_pass$Trust_State_Vote1)
table(df_pass$Trust_Country_Vote1)

# Checking Variables -----------------------------------------------------------
table(df_pass$age)
table(df_pass$gender)
table(df_pass$ethnicity)
table(df_pass$hispanic)
table(df_pass$hhi)
table(df_pass$zip)
table(df_pass$education)
table(df_pass$political_party)

table(df_pass$age == "")
table(df_pass$gender == "")
table(df_pass$ethnicity == "")
table(df_pass$hispanic == "")
table(df_pass$hhi == "")
table(df_pass$zip == "")
table(df_pass$education == "")
table(df_pass$political_party == "")

df_pass <- df_pass %>%
  mutate(across(c(age, gender, ethnicity, hispanic, hhi, education, political_party), as.numeric))

hist(df_pass$age)
hist(df_pass$gender)
hist(df_pass$ethnicity)
hist(df_pass$hispanic)
hist(df_pass$hhi)
hist(df_pass$education)
hist(df_pass$political_party)

# Checking Manipulation --------------------------------------------------------
table(df_pass$Manipulation)

# Checking Randomization --------------------------------------------------------

table(df_pass$FL_37_DO_FL_39, df_pass$FL_37_DO_FL_40)
table(df_pass$FL_37_DO_FL_41, df_pass$FL_37_DO_Control.Honda)

# table(df_pass$T1.Inst.ST.Time_First.Click)
# table(df_pass$T2.Belong.ST.Time_First.Click)
# table(df_pass$T3.Security.ST.Time_First.Click)
# table(df_pass$T4.Inst.JK.Time_First.Click)
# table(df_pass$T4.Belong.JK.Time_First.Click)
# table(df_pass$T5.Security.JK.Time_First.Click)
# table(df_pass$Control.Honda.Time_First.Click)

df_pass <- df_pass %>%
  mutate(
    treatment = case_when(
      T1.Inst.ST.Time_First.Click != "" & T4.Inst.JK.Time_First.Click == "" ~ "Inst ST",
      T2.Belong.ST.Time_First.Click != "" & T4.Belong.JK.Time_First.Click == "" ~ "Belong ST",
      T3.Security.ST.Time_First.Click != "" & T5.Security.JK.Time_First.Click == "" ~ "Security ST",
      T4.Inst.JK.Time_First.Click != "" & T1.Inst.ST.Time_First.Click == "" ~ "Inst JK",
      T4.Belong.JK.Time_First.Click != "" & T2.Belong.ST.Time_First.Click == "" ~ "Belong JK",
      T5.Security.JK.Time_First.Click != "" & T3.Security.ST.Time_First.Click == "" ~ "Security JK",
      Control.Honda.Time_First.Click != "" ~ "Control",
      TRUE ~ NA_character_),
    treatment_dummy = case_when(
      T1.Inst.ST.Time_First.Click != "" & T4.Inst.JK.Time_First.Click == "" ~ "Treatment",
      T2.Belong.ST.Time_First.Click != "" & T4.Belong.JK.Time_First.Click == "" ~ "Treatment",
      T3.Security.ST.Time_First.Click != "" & T5.Security.JK.Time_First.Click == "" ~ "Treatment",
      T4.Inst.JK.Time_First.Click != "" & T1.Inst.ST.Time_First.Click == "" ~ "Treatment",
      T4.Belong.JK.Time_First.Click != "" & T2.Belong.ST.Time_First.Click == "" ~ "Treatment",
      T5.Security.JK.Time_First.Click != "" & T3.Security.ST.Time_First.Click == "" ~ "Treatment",
      Control.Honda.Time_First.Click != "" ~ "Control",
      TRUE ~ NA_character_),
    treatment_messenger = case_when(
      T1.Inst.ST.Time_First.Click != "" & T4.Inst.JK.Time_First.Click == "" ~ "ST",
      T2.Belong.ST.Time_First.Click != "" & T4.Belong.JK.Time_First.Click == "" ~ "ST",
      T3.Security.ST.Time_First.Click != "" & T5.Security.JK.Time_First.Click == "" ~ "ST",
      T4.Inst.JK.Time_First.Click != "" & T1.Inst.ST.Time_First.Click == "" ~ "JK",
      T4.Belong.JK.Time_First.Click != "" & T2.Belong.ST.Time_First.Click == "" ~ "JK",
      T5.Security.JK.Time_First.Click != "" & T3.Security.ST.Time_First.Click == "" ~ "JK",
      Control.Honda.Time_First.Click != "" ~ "Control",
      TRUE ~ NA_character_),
    treatment_message = case_when(
      T1.Inst.ST.Time_First.Click != "" & T4.Inst.JK.Time_First.Click == "" ~ "Institutional",
      T2.Belong.ST.Time_First.Click != "" & T4.Belong.JK.Time_First.Click == "" ~ "Belonging",
      T3.Security.ST.Time_First.Click != "" & T5.Security.JK.Time_First.Click == "" ~ "Security",
      T4.Inst.JK.Time_First.Click != "" & T1.Inst.ST.Time_First.Click == "" ~ "Institutional",
      T4.Belong.JK.Time_First.Click != "" & T2.Belong.ST.Time_First.Click == "" ~ "Belonging",
      T5.Security.JK.Time_First.Click != "" & T3.Security.ST.Time_First.Click == "" ~ "Security",
      Control.Honda.Time_First.Click != "" ~ "Control",
      TRUE ~ NA_character_),
  )

table(df_pass$treatment)

# Demographics -----------------------------------------------------------------
df <- df_pass
remove(df_pass)

df <- df %>%
  mutate(party_short = recode(political_party,
                              "1" = "Democrat",
                              "8" = "Democrat",
                              "2" = "Republican",
                              "5" = "Republican",
                              "3" = "Independent",
                              "6" = "Independent",
                              "7" = "Independent",
                              "4" = "Prefer not to say"),
         party_full = recode(political_party,
                             "1" = "Democrat",
                             "8" = "Strong Democrat",
                             "2" = "Republican",
                             "5" = "Strong Republican",
                             "3" = "Independent",
                             "6" = "Independent who leans Republican",
                             "7" = "Independent who leans Democrat",
                             "4" = "Prefer not to say"),
         race_full=recode(ethnicity,
                          "1" = "White",
                          "2" = "Black, or African American",
                          "3" = "American Indian or Alaska Native",
                          "4" = "Asian *** Asian Indian",
                          "5" = "Asian *** Chinese",
                          "6" = "Asian *** Filipino",
                          "7" = "Asian *** Japanese",
                          "8" = "Asian *** Korean",
                          "9" = "Asian *** Vietnamese",
                          "10" = "Asian *** Other",
                          "11" = "Pacific Islander *** Native Hawaiian",
                          "12" = "Pacific Islander *** Guamanian",
                          "13" = "Pacific Islander *** Samoan",
                          "14" = "Pacific Islander *** Other Pacific Islander",
                          "15" = "Some other race",
                          "16" = "Prefer not to answer"),
         race_short=recode(ethnicity,
                           "1" = "white",
                           "2" = "black",
                           "3" = "native",
                           "4" = "aapi",
                           "5" = "aapi",
                           "6" = "aapi",
                           "7" = "aapi",
                           "8" = "aapi",
                           "9" = "aapi",
                           "10" = "aapi",
                           "11" = "aapi",
                           "12" = "aapi",
                           "13" = "aapi",
                           "14" = "aapi",
                           "15" = "other",
                           "16" = "other"),
         ethnicity_full=recode(hispanic,
                               "1" = "Not Hispanic",
                               "2" = "Hispanic - Mexican, Mexican American, Chicano",
                               "3" = "Hispanic - Cuban",
                               "4" = "Hispanic - Argentina",
                               "5" = "Hispanic - Colombia",
                               "6" = "Hispanic - Ecuador",
                               "7" = "Hispanic - El Salvadore",
                               "8" = "Hispanic - Guatemala",
                               "9" = "Hispanic - Nicaragua",
                               "10" = "Hispanic - Panama",
                               "11" = "Hispanic - Peru",
                               "12" = "Hispanic - Spain",
                               "13" = "Hispanic - Venezuela",
                               "14" = "Hispanic - Other Country",
                               "15" = "Prefer not to answer",
                               "16" = "Hispanic - Puerto Rican"),
         ethnicity_short=recode(hispanic,
                                "1" = "Not Hispanic",
                                "2" = "Hispanic",
                                "3" = "Hispanic",
                                "4" = "Hispanic",
                                "5" = "Hispanic",
                                "6" = "Hispanic",
                                "7" = "Hispanic",
                                "8" = "Hispanic",
                                "9" = "Hispanic",
                                "10" = "Hispanic",
                                "11" = "Hispanic",
                                "12" = "Hispanic",
                                "13" = "Hispanic",
                                "14" = "Hispanic",
                                "15" = "Prefer not to answer",
                                "16" = "Hispanic"),
         income_full = recode(hhi,
                              "1" = "Less than $14,999",
                              "2" = "$15,000 to $19,999",
                              "3" = "$20,000 to $24,999",
                              "4" = "$25,000 to $29,999",
                              "5" = "$30,000 to $34,999",
                              "6" = "$35,000 to $39,999",
                              "7" = "$40,000 to $44,999",
                              "8" = "$45,000 to $49,999",
                              "9" = "$50,000 to $54,999",
                              "10" = "$55,000 to $59,999",
                              "11" = "$60,000 to $64,999",
                              "12" = "$65,000 to $69,999",
                              "13" = "$70,000 to $74,999",
                              "14" = "$75,000 to $79,999",
                              "15" = "$80,000 to $84,999",
                              "16" = "$85,000 to $89,999",
                              "17" = "$90,000 to $94,999",
                              "18" = "$95,000 to $99,999",
                              "19" = "$100,000 to $124,999",
                              "20" = "$125,000 to $149,999",
                              "21" = "$150,000 to $174,999",
                              "22" = "$175,000 to $199,999",
                              "23" = "$200,000 to $249,999",
                              "24" = "$250,000 and above",
                              "25" = "Prefer not to answer"),
         education_full = recode(education,
                                 "1" = "Some high school or less",
                                 "2" = "High school graduate",
                                 "3" = "Other post high school vocational training",
                                 "4" = "Completed some college, but no degree",
                                 "5" = "Associate's degree",
                                 "6" = "Bachelor's degree",
                                 "7" = "Master's or professional degree",
                                 "8" = "Doctorate degree"),
         education_short = recode(education,
                                  "1" = "High school or less",
                                  "2" = "High school or less",
                                  "3" = "Some college",
                                  "4" = "Some college",
                                  "5" = "Some college",
                                  "6" = "College graduate",
                                  "7" = "Graduate degree",
                                  "8" = "Graduate degree"),
         gender_txt = recode(gender,
                             "1" = "Male",
                             "2" = "Female"),
         gender_binary = recode(gender,
                                "1" = 0,
                                "2" = 1)
  )

table(df$Race_Primer)
table(df$gender_txt)
table(df$ethnicity_short)
table(df$race_short)
table(df$income_full)
table(df$education_short)
table(df$party_short)

df <- df %>%
  mutate(
    ethnorace = ifelse(ethnicity_short == "Hispanic", "latine", race_short),
    non_white = ifelse(ethnorace != "white", 1, 0),
    black = ifelse(ethnorace == "black", 1, 0),
    native = ifelse(ethnorace == "native", 1, 0),
    white = ifelse(ethnorace == "white", 1, 0),
    latine = ifelse(ethnorace == "latine", 1, 0),
    aapi = ifelse(ethnorace == "aapi", 1, 0),
    blacknative = ifelse(ethnorace == "black" | ethnorace == "native", 1, 0),
    latineaapi = ifelse(ethnorace == "latine" | ethnorace == "aapi", 1, 0)
  )

table(df$ethnorace)
table(df$non_white)
table(df$black)

# df <- df %>%
#   mutate(
#     ethnorace_aapi = as.integer(ethnorace == "aapi"),
#     ethnorace_black = as.integer(ethnorace == "black"),
#     ethnorace_latine = as.integer(ethnorace == "latine"),
#     ethnorace_native = as.integer(ethnorace == "native"),
#     ethnorace_other = as.integer(ethnorace == "other"),
#     ethnorace_white = as.integer(ethnorace == "white")
#   )


# Moderators and Mediators -----------------------------------------------------

# Moderators
df <- df %>%
  mutate(lf = case_when(
    Linked.Fate == "No" ~ 1,
    Linked.Fate == "Yes, but not too much" ~ 2,
    Linked.Fate == "Yes, some" ~ 3,
    Linked.Fate == "Yes, a lot" ~ 4),
    worktogether = case_when(
      Race.Work.Together == "Not at all important" ~ 1,
      Race.Work.Together == "Somewhat important" ~ 2,
      Race.Work.Together == "Very important" ~ 3),
    discrim = case_when(
      Discrimination. == "No" ~ 0,
      Discrimination. == "Yes" ~ 1),
    elect_discrim = case_when(
      Election.Discrim == "None at all" ~ 1,
      Election.Discrim == "A little" ~ 2,
      Election.Discrim == "A moderate amount" ~ 3,
      Election.Discrim == "A lot" ~ 4,
      Election.Discrim == "A great deal" ~ 5),
    efficacy.infl = case_when(
      Efficacy.Influence == "Disagree strongly" ~ 1,
      Efficacy.Influence == "Disagree somewhat" ~ 2,
      Efficacy.Influence == "Neither agree nor disagree" ~ 3,
      Efficacy.Influence == "Agree somewhat" ~ 4,
      Efficacy.Influence == "Agree strongly" ~ 5),
    efficacy.und = case_when(
      Efficacy.Understand == "Disagree strongly" ~ 1,
      Efficacy.Understand == "Disagree somewhat" ~ 2,
      Efficacy.Understand == "Neither agree nor disagree" ~ 3,
      Efficacy.Understand == "Agree somewhat" ~ 4,
      Efficacy.Understand == "Agree strongly" ~ 5),
  )

# Mediators
df <- df %>%
  mutate(belonging_society = case_when(
  Belong.Society == "Not at all" ~ 1,
  Belong.Society == "Not much" ~ 2,
  Belong.Society == "Some" ~ 3,
  Belong.Society == "A lot" ~ 4),
  belonging_inout = case_when(
    Belong.InOutsider == "Not at all" ~ 1,
    Belong.InOutsider == "Not much" ~ 2,
    Belong.InOutsider == "Some" ~ 3,
    Belong.InOutsider == "A lot" ~ 4),
  belonging_respect = case_when(
    Belong.Respect == "Not at all" ~ 1,
    Belong.Respect == "Not much" ~ 2,
    Belong.Respect == "Some" ~ 3,
    Belong.Respect == "A lot" ~ 4),
  belonging_include = case_when(
    Belong.Include == "Not at all" ~ 1,
    Belong.Include == "Not much" ~ 2,
    Belong.Include == "Some" ~ 3,
    Belong.Include == "A lot" ~ 4),
  full_citizen = case_when(
    Full.Citizen == "Strongly disagree" ~ 1,
    Full.Citizen == "Somewhat disagree" ~ 2,
    Full.Citizen == "Neither agree nor disagree" ~ 3,
    Full.Citizen == "Somewhat agree" ~ 4,
    Full.Citizen == "Strongly agree" ~ 5)
  )

df <- df %>%
  mutate(
    belonging = rowMeans(across(c(belonging_include, belonging_inout, belonging_society, belonging_respect)), na.rm = TRUE),  # Calculate the average
    belonging = (belonging - min(belonging, na.rm = TRUE)) / (max(belonging, na.rm = TRUE) - min(belonging, na.rm = TRUE))  # Standardize between 0 and 1
  )

# Political Variables ----------------------------------------------------------

df <- df %>%
  mutate(ownvote_pre = case_when(
    Trust_Own_Vote == "Not at all confident" ~ 1,
    Trust_Own_Vote == "Not too confident" ~ 2,
    Trust_Own_Vote == "Somewhat confident" ~ 3,
    Trust_Own_Vote == "Very confident" ~ 4),
    ownvote_pre5 = case_when(
      Trust_Own_Vote == "Not at all confident" ~ 1,
      Trust_Own_Vote == "Not too confident" ~ 2,
      Trust_Own_Vote == "I don't know" ~ 3,
      Trust_Own_Vote == "Somewhat confident" ~ 4,
      Trust_Own_Vote == "Very confident" ~ 5),
    localvote_pre = case_when(
      Trust_Local_Vote == "Not at all confident" ~ 1,
      Trust_Local_Vote == "Not too confident" ~ 2,
      Trust_Local_Vote == "Somewhat confident" ~ 3,
      Trust_Local_Vote == "Very confident" ~ 4),
    localvote_pre5 = case_when(
      Trust_Local_Vote == "Not at all confident" ~ 1,
      Trust_Local_Vote == "Not too confident" ~ 2,
      Trust_Local_Vote == "I don't know" ~ 3,
      Trust_Local_Vote == "Somewhat confident" ~ 4,
      Trust_Local_Vote == "Very confident" ~ 5),
    statevote_pre = case_when(
      Trust_State_Vote. == "Not at all confident" ~ 1,
      Trust_State_Vote. == "Not too confident" ~ 2,
      Trust_State_Vote. == "Somewhat confident" ~ 3,
      Trust_State_Vote. == "Very confident" ~ 4),
    statevote_pre5 = case_when(
      Trust_State_Vote. == "Not at all confident" ~ 1,
      Trust_State_Vote. == "Not too confident" ~ 2,
      Trust_State_Vote. == "I don't know" ~ 3,
      Trust_State_Vote. == "Somewhat confident" ~ 4,
      Trust_State_Vote. == "Very confident" ~ 5),
    usavote_pre = case_when(
      Trust_Country_Vote == "Not at all confident" ~ 1,
      Trust_Country_Vote == "Not too confident" ~ 2,
      Trust_Country_Vote == "Somewhat confident" ~ 3,
      Trust_Country_Vote == "Very confident" ~ 4),
    usavote_pre5 = case_when(
      Trust_Country_Vote == "Not at all confident" ~ 1,
      Trust_Country_Vote == "Not too confident" ~ 2,
      Trust_Country_Vote == "I don't know" ~ 3,
      Trust_Country_Vote == "Somewhat confident" ~ 4,
      Trust_Country_Vote == "Very confident" ~ 5),
    ownvote_post = case_when(
      Trust_Own_Vote1 == "Not at all confident" ~ 1,
      Trust_Own_Vote1 == "Not too confident" ~ 2,
      Trust_Own_Vote1 == "Somewhat confident" ~ 3,
      Trust_Own_Vote1 == "Very confident" ~ 4),
    ownvote_post5 = case_when(
      Trust_Own_Vote1 == "Not at all confident" ~ 1,
      Trust_Own_Vote1 == "Not too confident" ~ 2,
      Trust_Own_Vote1 == "I don't know" ~ 3,
      Trust_Own_Vote1 == "Somewhat confident" ~ 4,
      Trust_Own_Vote1 == "Very confident" ~ 5),
    localvote_post = case_when(
      Trust_Local_Vote1 == "Not at all confident" ~ 1,
      Trust_Local_Vote1 == "Not too confident" ~ 2,
      Trust_Local_Vote1 == "Somewhat confident" ~ 3,
      Trust_Local_Vote1 == "Very confident" ~ 4),
    localvote_post5 = case_when(
      Trust_Local_Vote1 == "Not at all confident" ~ 1,
      Trust_Local_Vote1 == "Not too confident" ~ 2,
      Trust_Local_Vote1 == "I don't know" ~ 3,
      Trust_Local_Vote1 == "Somewhat confident" ~ 4,
      Trust_Local_Vote1 == "Very confident" ~ 5),
    statevote_post = case_when(
      Trust_State_Vote1 == "Not at all confident" ~ 1,
      Trust_State_Vote1 == "Not too confident" ~ 2,
      Trust_State_Vote1 == "Somewhat confident" ~ 3,
      Trust_State_Vote1 == "Very confident" ~ 4),
    statevote_post5 = case_when(
      Trust_State_Vote1 == "Not at all confident" ~ 1,
      Trust_State_Vote1 == "Not too confident" ~ 2,
      Trust_State_Vote1 == "I don't know" ~ 3,
      Trust_State_Vote1 == "Somewhat confident" ~ 4,
      Trust_State_Vote1 == "Very confident" ~ 5),
    usavote_post = case_when(
      Trust_Country_Vote1 == "Not at all confident" ~ 1,
      Trust_Country_Vote1 == "Not too confident" ~ 2,
      Trust_Country_Vote1 == "Somewhat confident" ~ 3,
      Trust_Country_Vote1 == "Very confident" ~ 4),
    usavote_post5 = case_when(
      Trust_Country_Vote1 == "Not at all confident" ~ 1,
      Trust_Country_Vote1 == "Not too confident" ~ 2,
      Trust_Country_Vote1 == "I don't know" ~ 3,
      Trust_Country_Vote1 == "Somewhat confident" ~ 4,
      Trust_Country_Vote1 == "Very confident" ~ 5),
    usavote_post5 = case_when(
      Trust_Country_Vote1 == "Not at all confident" ~ 1,
      Trust_Country_Vote1 == "Not too confident" ~ 2,
      Trust_Country_Vote1 == "I don't know" ~ 3,
      Trust_Country_Vote1 == "Somewhat confident" ~ 4,
      Trust_Country_Vote1 == "Very confident" ~ 5),
    voted24 = case_when(
      X2024.Vote. == "Yes" ~ 1,
      X2024.Vote. == "No" ~ 0,
      X2024.Vote. == "I don't remember" ~ 0),
    generaltrust = case_when(
      General.Trust == "Distrust a lot" ~ 1,
      General.Trust == "Distrust some" ~ 2,
      General.Trust == "Trust some" ~ 3,
      General.Trust == "Trust a lot" ~ 4),
    generaltrust5 = case_when(
      General.Trust == "Distrust a lot" ~ 1,
      General.Trust == "Distrust some" ~ 2,
      General.Trust == "Don't know/no opinion" ~ 3,
      General.Trust == "Trust some" ~ 4,
      General.Trust == "Trust a lot" ~ 5),
    info_post = case_when(
      Post.Information == "True" ~ 1,
      Post.Information == "False" ~ 0)
  )

df <- df %>%
  mutate(trust_others = case_when(
    Trust.Others == "Never" ~ 1,
    Trust.Others == "Some of the time" ~ 2,
    Trust.Others == "About half the time" ~ 3,
    Trust.Others == "Most of the time" ~ 4,
    Trust.Others == "Always" ~ 5),
    trust_others_post = case_when(
      Trust.Others.1 == "Never" ~ 1,
      Trust.Others.1 == "Some of the time" ~ 2,
      Trust.Others.1 == "About half the time" ~ 3,
      Trust.Others.1 == "Most of the time" ~ 4,
      Trust.Others.1 == "Always" ~ 5),
    trust_natgov = case_when(
      Trust.National.Gov == "Never" ~ 1,
      Trust.National.Gov == "Some of the time" ~ 2,
      Trust.National.Gov == "About half the time" ~ 3,
      Trust.National.Gov == "Most of the time" ~ 4,
      Trust.National.Gov == "Always" ~ 5),
    trust_natgov_post = case_when(
      Trust.National.Gov1. == "Never" ~ 1,
      Trust.National.Gov1. == "Some of the time" ~ 2,
      Trust.National.Gov1. == "About half the time" ~ 3,
      Trust.National.Gov1. == "Most of the time" ~ 4,
      Trust.National.Gov1. == "Always" ~ 5),
    trust_stgov = case_when(
      Trust.State.Gov == "Never" ~ 1,
      Trust.State.Gov == "Some of the time" ~ 2,
      Trust.State.Gov == "About half the time" ~ 3,
      Trust.State.Gov == "Most of the time" ~ 4,
      Trust.State.Gov == "Always" ~ 5),
    trust_stgov_post = case_when(
      Trust.State.Gov1 == "Never" ~ 1,
      Trust.State.Gov1 == "Some of the time" ~ 2,
      Trust.State.Gov1 == "About half the time" ~ 3,
      Trust.State.Gov1 == "Most of the time" ~ 4,
      Trust.State.Gov1 == "Always" ~ 5),
    trust_women = case_when(
      Trust.Gov.Groups_1 == "Never" ~ 1,
      Trust.Gov.Groups_1 == "Some of the time" ~ 2,
      Trust.Gov.Groups_1 == "About half the time" ~ 3,
      Trust.Gov.Groups_1 == "Most of the time" ~ 4,
      Trust.Gov.Groups_1 == "Always" ~ 5),
    trust_men = case_when(
      Trust.Gov.Groups_2 == "Never" ~ 1,
      Trust.Gov.Groups_2 == "Some of the time" ~ 2,
      Trust.Gov.Groups_2 == "About half the time" ~ 3,
      Trust.Gov.Groups_2 == "Most of the time" ~ 4,
      Trust.Gov.Groups_2 == "Always" ~ 5),
    trust_rural = case_when(
      Trust.Gov.Groups_3 == "Never" ~ 1,
      Trust.Gov.Groups_3 == "Some of the time" ~ 2,
      Trust.Gov.Groups_3 == "About half the time" ~ 3,
      Trust.Gov.Groups_3 == "Most of the time" ~ 4,
      Trust.Gov.Groups_3 == "Always" ~ 5),
    trust_city = case_when(
      Trust.Gov.Groups_4 == "Never" ~ 1,
      Trust.Gov.Groups_4 == "Some of the time" ~ 2,
      Trust.Gov.Groups_4 == "About half the time" ~ 3,
      Trust.Gov.Groups_4 == "Most of the time" ~ 4,
      Trust.Gov.Groups_4 == "Always" ~ 5),
    trust_black = case_when(
      Trust.Gov.Groups_5 == "Never" ~ 1,
      Trust.Gov.Groups_5 == "Some of the time" ~ 2,
      Trust.Gov.Groups_5 == "About half the time" ~ 3,
      Trust.Gov.Groups_5 == "Most of the time" ~ 4,
      Trust.Gov.Groups_5 == "Always" ~ 5),
    trust_white = case_when(
      Trust.Gov.Groups_6 == "Never" ~ 1,
      Trust.Gov.Groups_6 == "Some of the time" ~ 2,
      Trust.Gov.Groups_6 == "About half the time" ~ 3,
      Trust.Gov.Groups_6 == "Most of the time" ~ 4,
      Trust.Gov.Groups_6 == "Always" ~ 5),
    trust_latine = case_when(
      Trust.Gov.Groups_7 == "Never" ~ 1,
      Trust.Gov.Groups_7 == "Some of the time" ~ 2,
      Trust.Gov.Groups_7 == "About half the time" ~ 3,
      Trust.Gov.Groups_7 == "Most of the time" ~ 4,
      Trust.Gov.Groups_7 == "Always" ~ 5),
    trust_aapi = case_when(
      Trust.Gov.Groups_8 == "Never" ~ 1,
      Trust.Gov.Groups_8 == "Some of the time" ~ 2,
      Trust.Gov.Groups_8 == "About half the time" ~ 3,
      Trust.Gov.Groups_8 == "Most of the time" ~ 4,
      Trust.Gov.Groups_8 == "Always" ~ 5),
    trust_native = case_when(
      Trust.Gov.Groups_9 == "Never" ~ 1,
      Trust.Gov.Groups_9 == "Some of the time" ~ 2,
      Trust.Gov.Groups_9 == "About half the time" ~ 3,
      Trust.Gov.Groups_9 == "Most of the time" ~ 4,
      Trust.Gov.Groups_9 == "Always" ~ 5),
    trust_republicans = case_when(
      Trust.Gov.Groups_10 == "Never" ~ 1,
      Trust.Gov.Groups_10 == "Some of the time" ~ 2,
      Trust.Gov.Groups_10 == "About half the time" ~ 3,
      Trust.Gov.Groups_10 == "Most of the time" ~ 4,
      Trust.Gov.Groups_10 == "Always" ~ 5),
    trust_democrats = case_when(
      Trust.Gov.Groups_11 == "Never" ~ 1,
      Trust.Gov.Groups_11 == "Some of the time" ~ 2,
      Trust.Gov.Groups_11 == "About half the time" ~ 3,
      Trust.Gov.Groups_11 == "Most of the time" ~ 4,
      Trust.Gov.Groups_11 == "Always" ~ 5),
    vote_harris = case_when(
      Vote.Candidate == "Kamala Harris and Tim Walz" ~ 1,
      Vote.Candidate != "Kamala Harris and Tim Walz" ~ 0)
  )

df <- df %>%
  mutate(ownvote = ownvote_post - ownvote_pre,
         localvote = localvote_post - localvote_pre,
         statevote = statevote_post - statevote_pre,
         usavote = usavote_post - usavote_pre,
         society = trust_others_post - trust_others,
         stgov = trust_stgov_post - trust_stgov,
         natgov = trust_natgov_post - trust_natgov,
         )

df <- df %>%
  mutate(avgvote_post = rowMeans(cbind(ownvote_post, localvote_post, statevote_post, usavote_post), na.rm = TRUE),
         avggen_post = rowMeans(cbind(trust_others_post, trust_stgov_post, trust_natgov_post), na.rm = TRUE),
         avgvote_pre = rowMeans(cbind(ownvote_pre, localvote_pre, statevote_pre, usavote_pre), na.rm = TRUE),
         avggen_pre = rowMeans(cbind(trust_others, trust_stgov, trust_natgov), na.rm = TRUE)
         )

df$messenger_ST <- ifelse(df$treatment_messenger == "ST", 1, 0)

# Controls ---------------------------------------------------------------------

df <- df %>%
  mutate(talkpolitics = case_when(
    Talk.Politics == "Never" ~ 1,
    Talk.Politics == "A few times a year" ~ 2,
    Talk.Politics == "Once a month" ~ 3,
    Talk.Politics == "Once a week or more" ~ 4),
    talkelections = case_when(
      Talk.Elections == "Never" ~ 1,
      Talk.Elections == "A few times a year" ~ 2,
      Talk.Elections == "Once a month" ~ 3,
      Talk.Elections == "Once a week or more" ~ 4),
  )


# Saving Dataset ---------------------------------------------------------------
write.csv(df,"df_clean.csv")

beep()
rm()
remove(df)
