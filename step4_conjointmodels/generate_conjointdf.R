# Figure 11:
# Overall model of average marginal component effects of conjoint attributes on a respondent’s confidence in their vote being counted.

library(tidyverse)
library(cjoint)
library(cowplot)
library(modelsummary)
library(car)
library(weights)
library(kableExtra)
library(usmap)
library(likert)
library(stargazer)
library(lmtest)
library(sandwich)
library(coefplot)
library(lfe)

# Select key variables
conjoint <- DF %>%
  select(ResponseId,
         starts_with("X1_Poll.Confidence"), 
         starts_with("X2_Poll.Confidence"), 
         starts_with("X3_Poll.Confidence"),
         starts_with("choice1_"), 
         starts_with("choice2_"), 
         starts_with("choice3_"),
         State,
         party_short,
         ethnorace,
         weights
  )

# Melt the data to long format for analysis
conjoint <- conjoint %>%
  pivot_longer(
    cols = -c(ResponseId, State, party_short, ethnorace, weights),
    names_to = c("VariableGroup", "Attribute"),
    names_pattern = "(X\\d+|choice\\d+)_(.*)"
  ) %>%
  mutate(
    ChoiceSet = case_when(
      VariableGroup == "X1" | VariableGroup == "choice1" ~ "1",
      VariableGroup == "X2" | VariableGroup == "choice2" ~ "2",
      VariableGroup == "X3" | VariableGroup == "choice3" ~ "3",
      TRUE ~ NA_character_
    )
  ) %>%
  pivot_wider(
    id_cols = c(ResponseId, State, party_short, weights, ethnorace, ChoiceSet),
    names_from = Attribute,
    values_from = value
  )

# Create factor 
confidence_levels <- c("Not at all confident", "Not too confident", 
                       "Somewhat confident", "Very confident")

# Apply levels to the confidence variables
conjoint <- conjoint %>%
  mutate(
    Poll.Confidence.Own = factor(Poll.Confidence.Own, levels = confidence_levels, ordered = T),
    Poll.Confidence.Nat = factor(Poll.Confidence.Nat, levels = confidence_levels, ordered = T),
    Poll.Confidence.Race = factor(Poll.Confidence.Race, levels = confidence_levels, ordered = T)
  ) 

conjoint <- conjoint %>%
  mutate(
    partisanship = factor(partisanship, levels = c("Wearing badge with no party affiliation",
                                                   "Wearing Republican Party badge", 
                                                   "Wearing Democratic Party badge")),
    method = factor(method, levels = c("Touchscreen electronic machines with audit",
                                       "Touchscreen electronic machines", 
                                       "Machine-counted paper ballots", 
                                       "Hand-counted paper ballots")),
    voterID = factor(voterID, levels = c("None required", 
                                         "State name on sign in",
                                         "State name and give signature on sign in",
                                         "Show photo ID on sign in",
                                         "Show photo ID and give signature on sign in")),
    registration = factor(registration, levels = c("Same day registration not permitted",
                                                   "Same day registration during early voting",
                                                   "Same day registration up to Election Day")),
    closing = factor(closing, levels = c("6 PM", 
                                         "7 PM",
                                         "8 PM",
                                         "9 PM",
                                         "10 PM")),
    inGroupStatus = factor(inGroupStatus, levels = c("out-group", "in-group")),
    pollWatcher = factor(pollWatcher),
  )

conjoint <- conjoint %>%
  mutate(PCO = as.numeric(Poll.Confidence.Own),
         PCN = as.numeric(Poll.Confidence.Nat),
         PCR = as.numeric(Poll.Confidence.Race)
  ) %>%
  mutate(PCO_binary = ifelse(PCO > 2, 1, 0),
         PCN_binary = ifelse(PCN > 2, 1, 0),
         PCR_binary = ifelse(PCR > 2, 1, 0)
  )


conjoint <- conjoint %>%
  mutate(
    pollWatcherRace = case_when(
      pollWatcher %in% c("Cooper Smith", "Michael Anderson", "Allison Miller", "Susan Moss") ~ "White",
      pollWatcher %in% c("Devante Thomas", "Tyrese Howard", "Tanya Jackson", "Imani Hall") ~ "Black",
      pollWatcher %in% c("Andrew Chen", "Eric Kim", "Christine Zhang", "Mayu Kobayashi") ~ "Asian",
      pollWatcher %in% c("Jose Rodriguez", "Miguel Castillo", "Ana Rosales", "Gloria Medina") ~ "Hispanic",
      pollWatcher %in% c("Benjamin Longfish", "Nelson Todacheene", "Winona Laughing", "GloJean Gorman") ~ "Native",
      TRUE ~ NA_character_
    ),
    pollWatcherGender = case_when(
      pollWatcher %in% c("Cooper Smith", "Michael Anderson", "Devante Thomas", "Tyrese Howard", 
                         "Andrew Chen", "Eric Kim", "Jose Rodriguez", "Miguel Castillo",
                         "Benjamin Longfish", "Nelson Todacheene") ~ "Male",
      pollWatcher %in% c("Allison Miller", "Susan Moss", "Tanya Jackson", "Imani Hall",
                         "Christine Zhang", "Mayu Kobayashi", "Ana Rosales", "Gloria Medina",
                         "Winona Laughing", "GloJean Gorman") ~ "Female",
      TRUE ~ NA_character_
    )
  )

conjoint <- conjoint %>%
  mutate(
    nonwhite = case_when(ethnorace != "white" ~ 1, TRUE ~ 0),
    white = case_when(ethnorace == "white" ~ 1, TRUE ~ 0),
    democrat = case_when(party_short == "Democrat" ~ 1, TRUE ~ 0),
    republican = case_when(party_short == "Republican" ~ 1, TRUE ~ 0),
    independent = case_when(party_short == "Independent" ~ 1, TRUE ~ 0),
    pw_party = case_when(partisanship == "Wearing Republican Party badge" ~ "Republican",
                         partisanship == "Wearing Democratic Party badge" ~ "Democrat",
                         partisanship == "Wearing badge with no party affiliation" ~ 
                           "Independent"),
    coPartisanStatus = case_when(
      party_short == "Democrat" & partisanship == "Wearing Democratic Party badge" ~ 1,
      party_short == "Republican" & partisanship == "Wearing Republican Party badge" ~ 1,
      TRUE ~ 0),
    coPartisanStatus_windep = case_when(
      party_short == "Democrat" & partisanship == "Wearing Democratic Party badge" ~ 1,
      party_short == "Republican" & partisanship == "Wearing Republican Party badge" ~ 1,
      party_short == "Independent" & partisanship == "Wearing badge with no party affiliation" ~ 1,
      TRUE ~ 0),
    outPartisanStatus = case_when(
      party_short == "Democrat" & partisanship == "Wearing Republican Party badge" ~ 1,
      party_short == "Republican" & partisanship == "Wearing Democratic Party badge" ~ 1,
      TRUE ~ 0),
    nonwhite_pw = case_when(pollWatcherRace != "White" ~ 1, TRUE ~ 0),
    white_pw = case_when(pollWatcherRace == "White" ~ 1, TRUE ~ 0),
  )

conjoint <- conjoint %>%
  mutate(inGroupStatus = case_when(inGroupStatus == "in-group" ~ 1, 
                                   inGroupStatus == "out-group" ~ 0, 
                                   TRUE ~ NA))

# Need to factor party
conjoint$party_short <- factor(conjoint$party_short, levels = c("Independent", "Democrat", "Republican"))
conjoint$pw_party <- factor(conjoint$pw_party, levels = c("Independent", "Democrat", "Republican"))
conjoint$pollWatcherGender <- factor(conjoint$pollWatcherGender, levels = c("Male", "Female"))
conjoint$pollWatcherRace <- factor(conjoint$pollWatcherRace, levels = c("White", "Black", "Asian", "Hispanic", "Native"))


conjoint <- conjoint %>%
  mutate(ethnorace = factor(ethnorace, levels = c("white", "black", "aapi", "latine", "native","other")),
         pollWatcherRace = factor(pollWatcherRace, levels = c("White", "Black", "Asian", "Hispanic", "Native")),
         southernstate = case_when (State %in% c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Tennessee", "Texas", "Virginia") ~ 1, TRUE ~ 0))

conjoint <- conjoint %>%
  mutate(
    partisanship_rel = case_when(
      coPartisanStatus == 1 ~ "Co",
      outPartisanStatus == 1 ~ "Out",
      TRUE ~ "Non"
    )
  )

conjoint$partisanship_rel <- factor(conjoint$partisanship_rel, levels = c("Non", "Co", "Out"))