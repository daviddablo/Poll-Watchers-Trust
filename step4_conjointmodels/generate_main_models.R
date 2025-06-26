# Main models
main_model <- felm(PCO_binary ~ pollWatcherRace + pollWatcherGender + pw_party + method + voterID + registration + closing | State + ChoiceSet + ResponseId, data = conjoint, weights = conjoint$weights)

# Compute clustered standard errors
cluster_se <- summary(main_model)$coefficients[, "Std. Error"]

# Create dataframe with coefficients and clustered standard errors
coef_df <- data.frame(
  Term = names(coef(main_model)),
  Estimate = coef(main_model),
  Clustered_SE = cluster_se
)

#meant to be descriptive based on the samples , understanding voters of color

coef_df <- coef_df %>%
  mutate(Attribute = case_when(
    Term == "(Intercept)" ~ "Intercept",
    Term %in% c("pollWatcherRaceBlack", "pollWatcherRaceAsian", "pollWatcherRaceHispanic", "pollWatcherRaceNative") ~ "Poll Watcher Race",
    Term %in% c("pollWatcherGenderFemale") ~ "Poll Watcher Gender",
    Term %in% c("pw_partyDemocrat", "pw_partyRepublican") ~ "Poll Watcher Party",
    Term %in% c("methodTouchscreen electronic machines", "methodMachine-counted paper ballots", "methodHand-counted paper ballots") ~ "Voting Method",
    Term %in% c("voterIDState name on sign in", "voterIDState name and give signature on sign in", "voterIDShow photo ID on sign in", "voterIDShow photo ID and give signature on sign in") ~ "ID Requirements",
    Term %in% c("registrationSame day registration during early voting", "registrationSame day registration up to Election Day") ~ "Same Day Registration",
    Term %in% c("closing7 PM", "closing8 PM", "closing9 PM", "closing10 PM") ~ "Poll Closing Time"
  )
  ) %>%
  mutate(
    Level = case_when(
      Term == "(Intercept)" ~ "Intercept",
      Term == "pollWatcherRaceBlack" ~ "Black",
      Term == "pollWatcherRaceAsian" ~ "Asian",
      Term == "pollWatcherRaceHispanic" ~ "Latine",
      Term == "pollWatcherRaceNative" ~ "Native",
      Term == "pollWatcherGenderFemale" ~ "Female",
      Term == "pw_partyDemocrat" ~ "Democrat",
      Term == "pw_partyRepublican" ~ "Republican",
      Term == "methodTouchscreen electronic machines" ~ "Touchscreen electronic machines",
      Term == "methodMachine-counted paper ballots" ~ "Machine-counted paper ballots",
      Term == "methodHand-counted paper ballots" ~ "Hand-counted paper ballots",
      Term == "voterIDState name on sign in" ~ "State name on sign in",
      Term == "voterIDState name and give signature on sign in" ~ "State name and give signature on sign in",
      Term == "voterIDShow photo ID on sign in" ~ "Show photo ID on sign in",
      Term == "voterIDShow photo ID and give signature on sign in" ~ "Show photo ID and give signature on sign in",
      Term == "registrationSame day registration during early voting" ~ "During early voting",
      Term == "registrationSame day registration up to Election Day" ~ "Up to Election Day",
      Term == "closing7 PM" ~ "7 PM",
      Term == "closing8 PM" ~ "8 PM",
      Term == "closing9 PM" ~ "9 PM",
      Term == "closing10 PM" ~ "10 PM"
    )
  )

# Insert reference categories

reference_categories <- tibble(
  Term = c("pollWatcherRaceWhite", "pw_partyIndependent", "methodTouchscreen electronic machines with audit",
           "voterIDNone required", "registrationNot permitted", "closing6 PM", "pollWatcherGenderMale"),
  Estimate = 0,
  Clustered_SE = 0,
  Attribute = c("Poll Watcher Race", "Poll Watcher Party", "Voting Method",
                "ID Requirements", "Same Day Registration", "Poll Closing Time", "Poll Watcher Gender"),
  Level = c("White", "Independent", "Touchscreen electronic machines with audit",
            "None required", "Not permitted", "6 PM", "Male")
)

# Append reference categories
coef_df <- bind_rows(coef_df, reference_categories) %>% filter(Term != "(Intercept)")

# Define the desired order for Attributes
attribute_order <- c(
  "Poll Watcher Race", "Poll Watcher Party", "Poll Watcher Gender",
  "ID Requirements", "Same Day Registration", "Poll Closing Time", "Voting Method"
)

# Define the desired order for Levels within each Attribute
level_order <- list(
  "Poll Watcher Race" = c("Native", "Latine", "Asian", "Black", "White"),
  "Poll Watcher Party" = c("Democrat", "Republican", "Independent"),
  "Poll Watcher Gender" = c("Female","Male"),
  "ID Requirements" = c("Show photo ID and give signature on sign in", "Show photo ID on sign in", "State name and give signature on sign in", "State name on sign in", "None required"),
  "Same Day Registration" = c("Up to Election Day", "During early voting", "Not permitted"),
  "Poll Closing Time" = c("10 PM", "9 PM", "8 PM", "7 PM", "6 PM"),
  "Voting Method" = c("Hand-counted paper ballots",
                      "Machine-counted paper ballots",
                      "Touchscreen electronic machines",
                      "Touchscreen electronic machines with audit")
)

# Apply the ordering to the dataframe
coef_df <- coef_df %>%
  mutate(
    Attribute = factor(Attribute, levels = attribute_order),
    Level = factor(Level, levels = unlist(level_order))
  ) %>%
  arrange(Attribute, Level)



modelplot <- ggplot(coef_df, aes(x = Estimate, y = Level)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = Estimate - Clustered_SE * 1.96, xmax = Estimate + Clustered_SE * 1.96), linewidth = 1, width = .2) +
 geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.6) + # Defined zero line
  geom_text(aes(label = ifelse(Estimate == 0, "", sprintf("%.3f", Estimate))), vjust = -1, size = 3, fontface = "bold") +
  labs(
    x = "AMCE (Own Vote)",
    y = "Attribute Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    strip.text.y = element_text(angle = 0),  # Makes facet labels horizontal
    legend.position = "none",  # Removes the color legend
    element_text(color = "black")
  ) +
  facet_grid(rows = vars(Attribute), scales = "free_y", space = "free")

ggsave("step6_outputs/main_model.jpg", modelplot, width = 9, height = 12, dpi = 300)

rm(main_model, modelplot, coef_df, level_order, reference_categories)

main_model <- felm(PCO_binary ~ inGroupStatus + partisanship_rel + pollWatcherGender + method + voterID + registration + closing | State + ChoiceSet + ResponseId, data = conjoint, weights = conjoint$weights)

# Compute clustered standard errors
cluster_se <- summary(main_model)$coefficients[, "Std. Error"]

# Create dataframe with coefficients and clustered standard errors
coef_df <- data.frame(
  Term = names(coef(main_model)),
  Estimate = coef(main_model),
  Clustered_SE = cluster_se
)

#meant to be descriptive based on the samples , understanding voters of color

coef_df <- coef_df %>%
  mutate(Attribute = case_when(
    Term == "(Intercept)" ~ "Intercept",
    Term %in% c("inGroupStatus") ~ "Poll Watcher Race",
    Term %in% c("partisanship_relCo", "partisanship_relOut") ~ "Poll Watcher Party",
    Term %in% c("pollWatcherGenderFemale") ~ "Poll Watcher Gender",
    Term %in% c("pw_partyDemocrat", "pw_partyRepublican") ~ "Poll Watcher Party",
    Term %in% c("methodTouchscreen electronic machines", "methodMachine-counted paper ballots", "methodHand-counted paper ballots") ~ "Voting Method",
    Term %in% c("voterIDState name on sign in", "voterIDState name and give signature on sign in", "voterIDShow photo ID on sign in", "voterIDShow photo ID and give signature on sign in") ~ "ID Requirements",
    Term %in% c("registrationSame day registration during early voting", "registrationSame day registration up to Election Day") ~ "Same Day Registration",
    Term %in% c("closing7 PM", "closing8 PM", "closing9 PM", "closing10 PM") ~ "Poll Closing Time"
  )
  ) %>%
  mutate(
    Level = case_when(
      Term == "(Intercept)" ~ "Intercept",
      Term == "inGroupStatus" ~ "Co-Racial",
      Term == "partisanship_relCo" ~ "Co-Partisan",
      Term == "partisanship_relOut" ~ "Out-Partisan",
      Term == "pollWatcherGenderFemale" ~ "Female",
      Term == "pw_partyDemocrat" ~ "Democrat",
      Term == "pw_partyRepublican" ~ "Republican",
      Term == "methodTouchscreen electronic machines" ~ "Touchscreen electronic machines",
      Term == "methodMachine-counted paper ballots" ~ "Machine-counted paper ballots",
      Term == "methodHand-counted paper ballots" ~ "Hand-counted paper ballots",
      Term == "voterIDState name on sign in" ~ "State name on sign in",
      Term == "voterIDState name and give signature on sign in" ~ "State name and give signature on sign in",
      Term == "voterIDShow photo ID on sign in" ~ "Show photo ID on sign in",
      Term == "voterIDShow photo ID and give signature on sign in" ~ "Show photo ID and give signature on sign in",
      Term == "registrationSame day registration during early voting" ~ "During early voting",
      Term == "registrationSame day registration up to Election Day" ~ "Up to Election Day",
      Term == "closing7 PM" ~ "7 PM",
      Term == "closing8 PM" ~ "8 PM",
      Term == "closing9 PM" ~ "9 PM",
      Term == "closing10 PM" ~ "10 PM"
    )
  )

# Insert reference categories

reference_categories <- tibble(
  Term = c("Out-Racial", "Nonpartisan", "methodTouchscreen electronic machines with audit",
           "voterIDNone required", "registrationNot permitted", "closing6 PM", "pollWatcherGenderMale"),
  Estimate = 0,
  Clustered_SE = 0,
  Attribute = c("Poll Watcher Race", "Poll Watcher Party", "Voting Method",
                "ID Requirements", "Same Day Registration", "Poll Closing Time", "Poll Watcher Gender"),
  Level = c("Out-Racial", "No Party", "Touchscreen electronic machines with audit",
            "None required", "Not permitted", "6 PM", "Male")
)

# Append reference categories
coef_df <- bind_rows(coef_df, reference_categories) %>% filter(Term != "(Intercept)")

# Define the desired order for Attributes
attribute_order <- c(
  "Poll Watcher Race", "Poll Watcher Party", "Poll Watcher Gender",
  "ID Requirements", "Same Day Registration", "Poll Closing Time", "Voting Method"
)

# Define the desired order for Levels within each Attribute
level_order <- list(
  "Poll Watcher Race" = c("Co-Racial", "Out-Racial"),
  "Poll Watcher Party" = c("Co-Partisan", "Out-Partisan", "No Party"),
  "Poll Watcher Gender" = c("Female","Male"),
  "ID Requirements" = c("Show photo ID and give signature on sign in", "Show photo ID on sign in", "State name and give signature on sign in", "State name on sign in", "None required"),
  "Same Day Registration" = c("Up to Election Day", "During early voting", "Not permitted"),
  "Poll Closing Time" = c("10 PM", "9 PM", "8 PM", "7 PM", "6 PM"),
  "Voting Method" = c("Hand-counted paper ballots",
                      "Machine-counted paper ballots",
                      "Touchscreen electronic machines",
                      "Touchscreen electronic machines with audit")
)

# Apply the ordering to the dataframe
coef_df <- coef_df %>%
  mutate(
    Attribute = factor(Attribute, levels = attribute_order),
    Level = factor(Level, levels = unlist(level_order))
  ) %>%
  arrange(Attribute, Level)



modelplot <- ggplot(coef_df, aes(x = Estimate, y = Level)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = Estimate - Clustered_SE * 1.96, xmax = Estimate + Clustered_SE * 1.96), linewidth = 1, width = .2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.6) + # Defined zero line
  geom_text(aes(label = ifelse(Estimate == 0, "", sprintf("%.3f", Estimate))), vjust = -1, size = 3, fontface = "bold") +
  labs(
    x = "AMCE (Own Vote)",
    y = "Attribute Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    strip.text.y = element_text(angle = 0),  # Makes facet labels horizontal
    legend.position = "none",  # Removes the color legend
    element_text(color = "black")
  ) +
  facet_grid(rows = vars(Attribute), scales = "free_y", space = "free")

ggsave("step6_outputs/main_model_conditional.jpg", modelplot, width = 9, height = 12, dpi = 300)

rm(main_model, modelplot, coef_df, level_order, reference_categories)