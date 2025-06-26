rm(list = ls())

library(ggplot2)
library(stargazer)
library(sandwich)
library(lmtest)
library(here)
library(tidyverse)
library(clubSandwich)
library(data.table)
library(estimatr)
library(survey)
library(marginaleffects)

# Replace with your working directory
setwd("/Users/daviddablo/Desktop/For Replicatability")

# Read in cleaned Qualtrics data
DF <- read.csv("step1_cleanup/df_clean.csv")

DF <- DF %>%
  mutate(
    age_cat = case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 44 ~ "30-44",
      age >= 45 & age <= 64 ~ "45-64",
      age >= 65 ~ "65+"
    ),
    gender_cat = case_when(
      gender == 1 ~ "male",
      gender == 2 ~ "female"
    ),
    ethnic_cat = case_when(
      ethnicity_short == "Hispanic" ~ "hisp",
      ethnicity_short == "Not Hispanic" ~ "not hisp"    
    ),
    educ_cat = case_when(
      education_short == "High school or less" ~ "high school",
      education_short == "Some college" ~ "some college",
      education_short == "College graduate" ~ "college",
      education_short == "Graduate degree" ~ "grad"
    ),
    race_cat = case_when(
      race_short == "white" ~ "white",
      race_short == "aapi" ~ "asian",
      race_short == "black" ~ "black",
      race_short == "native" ~ "native",
      race_short == "other" ~ "other"
    )
  )

DF_short <- DF %>%
  select(
    age_cat, gender_cat, ethnic_cat, educ_cat, race_cat, party_short
  )

stargazer(DF_short)


DT <- data.table(DF)
DT$n <- 1:nrow(DT)



pop.targs <- list(
  # age
  data.frame(age_cat=c('18-29', '30-44', '45-64', '65+'),
             Freq=nrow(DT)*c(0.204, 0.247, 0.323, 0.226)),
  #race
  data.frame(race_cat=c('white', 'black', 'asian', 'native', 'other'), 
             Freq=nrow(DT)*c(0.753, 0.137, 0.067, 0.013, 0.03)),
  #gender
  data.frame(gender_cat=c('male','female'), Freq=nrow(DT)*c(0.488, 0.512)),
  #ethnicity
  data.frame(ethnic_cat=c('hisp','not hisp'), Freq=nrow(DT)*c(0.195, 0.805)),
  #educ
  data.frame(educ_cat=c('high school', 'some college', 'college', 'grad'), 
             Freq=nrow(DT)*c(0.364, 0.306, 0.206, 0.124)))


# Create survey design object for Lucid data.
dt.svy <- svydesign(ids=~1,data=DT,weights=NULL)


# Rake to each population margin.
dt.svy.rk <- rake(dt.svy, sample.margins=list(~age_cat, ~race_cat, ~gender_cat, ~ethnic_cat, ~educ_cat), population.margins=pop.targs)


# Present individuals with largest and smallest weights.
cat("Summary of distribution of weights before trimming:\n")
the.wts <- weights(dt.svy.rk)
print(summary(the.wts))
print(quantile(the.wts,probs=seq(.1,.9,.1)))
#cat("Observations with max and min weights assigned. \nMax:\n")
#print(DT[the.wts == max(the.wts),the.vars,with=F])
#cat("Min:\n")
#print(DT[the.wts == min(the.wts),the.vars,with=F])


# Trim the weights?
if (max(the.wts) > 8) {
  DF.svy.rk2 <- trimWeights(dt.svy.rk,lower=1/8,upper=8,strict=T)
  cat("Summary of distribution of weights after trimming:\n")
  print(summary(the.wts <- weights(DF.svy.rk2)))
  cat("Observations with max and min weights aDFgned.\nMax:\n")
  # print(DF[the.wts == max(the.wts),the.vars,with=F])
  cat("Min:\n")
  # print(DF[the.wts == min(the.wts),the.vars,with=F])
} else {
  dt.svy.rk2 <- dt.svy.rk
}


DF<-cbind(DF, the.wts)
DF$weights <- DF$the.wts
rm(DT, dt.svy, dt.svy.rk,  pop.targs, the.wts)
rm(DF_short, DF.svy.rk2)