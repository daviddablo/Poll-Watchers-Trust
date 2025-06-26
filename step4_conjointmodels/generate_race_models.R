#race hypotheses
race_hyp <- felm(PCO_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                   method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                 data = conjoint, weights = conjoint$weights)
race_hyp_w <- felm(PCO_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId +  State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "white",], weights = conjoint[conjoint$ethnorace == "white",]$weights)
race_hyp_b <- felm(PCO_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "black",], weights = conjoint[conjoint$ethnorace == "black",]$weights)
race_hyp_a <- felm(PCO_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId +  State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "aapi",], weights = conjoint[conjoint$ethnorace == "aapi",]$weights)
race_hyp_l <- felm(PCO_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "latine",], weights = conjoint[conjoint$ethnorace == "latine",]$weights)
race_hyp_n <- felm(PCO_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "native",], weights = conjoint[conjoint$ethnorace == "native",]$weights)
race_hyp_o <- felm(PCO_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "other",], weights = conjoint[conjoint$ethnorace == "other",]$weights)

extract_ci_single <- function(model, var) {
  # Extract coefficients and standard errors
  coefs <- summary(model)$coefficients
  if (!(var %in% rownames(coefs))) stop(paste("Variable", var, "not found in model"))
  
  estimate <- coefs[var, "Estimate"]
  std_error <- coefs[var, "Std. Error"]
  
  # Compute 95% confidence interval
  ci_lower <- estimate - 1.96 * std_error
  ci_upper <- estimate + 1.96 * std_error
  
  # Return as a named vector
  c(Estimate = estimate, CI_Lower = ci_lower, CI_Upper = ci_upper)
}


# Define the variable to extract
target_var <- "inGroupStatus"

# Extract values for each model
ci_all <- extract_ci_single(race_hyp, target_var)
ci_white <- extract_ci_single(race_hyp_w, target_var)
ci_black <- extract_ci_single(race_hyp_b, target_var)
ci_aapi <- extract_ci_single(race_hyp_a, target_var)
ci_latine <- extract_ci_single(race_hyp_l, target_var)
ci_native <- extract_ci_single(race_hyp_n, target_var)
ci_other <- extract_ci_single(race_hyp_o, target_var)

# Create dataframe for plotting
ci_data <- data.frame(
  Group = c("All", "White", "Black", "AAPI", "Latine", "Native", "Other"),
  Estimate = c(ci_all["Estimate"], ci_white["Estimate"], ci_black["Estimate"], 
               ci_aapi["Estimate"], ci_latine["Estimate"], ci_native["Estimate"], ci_other["Estimate"]),
  CI_Lower = c(ci_all["CI_Lower"], ci_white["CI_Lower"], ci_black["CI_Lower"], 
               ci_aapi["CI_Lower"], ci_latine["CI_Lower"], ci_native["CI_Lower"], ci_other["CI_Lower"]),
  CI_Upper = c(ci_all["CI_Upper"], ci_white["CI_Upper"], ci_black["CI_Upper"], 
               ci_aapi["CI_Upper"], ci_latine["CI_Upper"], ci_native["CI_Upper"], ci_other["CI_Upper"])
) 

ci_data$Group <- factor(ci_data$Group, levels = c("All", "White", "Black", "AAPI", "Latine", "Native", "Other"))


hypothesesA1 <- ggplot(ci_data, aes(x = Group, y = Estimate)) +
  geom_point(size = 5) +  # Effect estimate points
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, size = 2) + # Thick bars for confidence intervals
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) + # Defined zero line
  geom_text(aes(label = sprintf("%.3f", Estimate)), hjust = -0.3, size = 6, fontface = "bold") +  # Move text right
  labs(
    y = "AMCE (Own Vote)",  # Label for y-axis
    x = "Ethnoracial Group"  # Label for x-axis
  ) +
  theme_minimal(base_size = 20) + # Use a minimal theme with larger font size
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),  # Add black panel borders
    strip.text = element_text(size = 22, face = "bold"),  # Title above each panel
    axis.text = element_text(size = 18),  # Larger x and y axis labels
    panel.grid = element_blank()  # Remove background gridlines
  )

# Save the plot
ggsave("step6_outputs/hypothesis_A1_PCO.jpg", hypothesesA1, width = 12, height = 7, dpi = 300)


#race hypotheses
race_hyp <- felm(PCR_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                   method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                 data = conjoint, weights = conjoint$weights)
race_hyp_w <- felm(PCR_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId +  State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "white",], weights = conjoint[conjoint$ethnorace == "white",]$weights)
race_hyp_b <- felm(PCO_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "black",], weights = conjoint[conjoint$ethnorace == "black",]$weights)
race_hyp_a <- felm(PCR_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId +  State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "aapi",], weights = conjoint[conjoint$ethnorace == "aapi",]$weights)
race_hyp_l <- felm(PCR_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "latine",], weights = conjoint[conjoint$ethnorace == "latine",]$weights)
race_hyp_n <- felm(PCR_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "native",], weights = conjoint[conjoint$ethnorace == "native",]$weights)
race_hyp_o <- felm(PCR_binary ~ inGroupStatus + pw_party + pollWatcherGender + 
                     method + voterID + registration + closing | ResponseId + State + ChoiceSet, 
                   data = conjoint[conjoint$ethnorace == "other",], weights = conjoint[conjoint$ethnorace == "other",]$weights)

# Define the variable to extract
target_var <- "inGroupStatus"

# Extract values for each model
ci_all <- extract_ci_single(race_hyp, target_var)
ci_white <- extract_ci_single(race_hyp_w, target_var)
ci_black <- extract_ci_single(race_hyp_b, target_var)
ci_aapi <- extract_ci_single(race_hyp_a, target_var)
ci_latine <- extract_ci_single(race_hyp_l, target_var)
ci_native <- extract_ci_single(race_hyp_n, target_var)
ci_other <- extract_ci_single(race_hyp_o, target_var)

# Create dataframe for plotting
ci_data <- data.frame(
  Group = c("All", "White", "Black", "AAPI", "Latine", "Native", "Other"),
  Estimate = c(ci_all["Estimate"], ci_white["Estimate"], ci_black["Estimate"], 
               ci_aapi["Estimate"], ci_latine["Estimate"], ci_native["Estimate"], ci_other["Estimate"]),
  CI_Lower = c(ci_all["CI_Lower"], ci_white["CI_Lower"], ci_black["CI_Lower"], 
               ci_aapi["CI_Lower"], ci_latine["CI_Lower"], ci_native["CI_Lower"], ci_other["CI_Lower"]),
  CI_Upper = c(ci_all["CI_Upper"], ci_white["CI_Upper"], ci_black["CI_Upper"], 
               ci_aapi["CI_Upper"], ci_latine["CI_Upper"], ci_native["CI_Upper"], ci_other["CI_Upper"])
) 

ci_data$Group <- factor(ci_data$Group, levels = c("All", "White", "Black", "AAPI", "Latine", "Native", "Other"))


hypothesesA1 <- ggplot(ci_data, aes(x = Group, y = Estimate)) +
  geom_point(size = 5) +  # Effect estimate points
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, size = 2) + # Thick bars for confidence intervals
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) + # Defined zero line
  geom_text(aes(label = sprintf("%.3f", Estimate)), hjust = -0.3, size = 6, fontface = "bold") +  # Move text right
  labs(
    y = "AMCE (Racial Group's Votes)",  # Label for y-axis
    x = "Ethnoracial Group"  # Label for x-axis
  ) +
  theme_minimal(base_size = 20) + # Use a minimal theme with larger font size
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),  # Add black panel borders
    strip.text = element_text(size = 22, face = "bold"),  # Title above each panel
    axis.text = element_text(size = 18),  # Larger x and y axis labels
    panel.grid = element_blank()  # Remove background gridlines
  )

# Save the plot
ggsave("step6_outputs/hypothesis_A1_PCR.jpg", hypothesesA1, width = 12, height = 7, dpi = 300)

rm(ci_data, hypothesesA1, race_hyp, race_hyp_a, race_hyp_b, race_hyp_l, race_hyp_n, race_hyp_o, race_hyp_w)


#A2A3 PCO

model1_a2 <- felm(PCO_binary ~ nonwhite_pw + pw_party + pollWatcherGender + 
                    method + voterID + registration + closing | ResponseId + State + ChoiceSet, data = conjoint, weights = conjoint$weights)

model2_a2 <- felm(PCO_binary ~ nonwhite_pw * nonwhite + pw_party + pollWatcherGender + 
                    method + voterID + registration + closing | State + ChoiceSet, data = conjoint, weights = conjoint$weights)

ci_all <- extract_ci_single(model1_a2, "nonwhite_pw")
ci_white <- extract_ci_single(model2_a2, "nonwhite_pw")
nonwhite_est <- summary(model2_a2)$coefficients["nonwhite_pw", "Estimate"] + summary(model2_a2)$coefficients["nonwhite_pw:nonwhite", "Estimate"]
nonwhite_std <- sqrt((summary(model2_a2)$coefficients["nonwhite_pw", "Std. Error"])^2 + (summary(model2_a2)$coefficients["nonwhite_pw:nonwhite", "Std. Error"])^2)
ci_nonwhite <- data.frame(
  Estimate = nonwhite_est,
  CI_Lower = nonwhite_est - 1.96 * nonwhite_std,
  CI_Upper = nonwhite_est + 1.96 * nonwhite_std
)

ci_data_a2a3 <- data.frame(
  Group = c("All Respondents", "All Respondents", "White Respondents", "White Respondents", "Nonwhite Respondents", "Nonwhite Respondents"),
  PollWatcher = c("White", "Nonwhite", "White", "Nonwhite", "White", "Nonwhite"),
  Estimate = c(0, as.numeric(ci_all["Estimate"]), 0, as.numeric(ci_white["Estimate"]), 0, as.numeric(ci_nonwhite["Estimate"])),
  CI_Lower = c(0, as.numeric(ci_all["CI_Lower"]), 0, as.numeric(ci_white["CI_Lower"]), 0, as.numeric(ci_nonwhite["CI_Lower"])),
  CI_Upper = c(0, as.numeric(ci_all["CI_Upper"]), 0, as.numeric(ci_white["CI_Upper"]), 0, as.numeric(ci_nonwhite["CI_Upper"]))
) 

ci_data_a2a3$PollWatcher <- factor(ci_data_a2a3$PollWatcher, levels = c("White", "Nonwhite"))

ci_data_a2a3$Group <- factor(ci_data_a2a3$Group, levels = c("All Respondents", "White Respondents", "Nonwhite Respondents"))

hypothesesA2_A3_2 <- ggplot(ci_data_a2a3, aes(x = PollWatcher, y = Estimate)) +
  geom_point(size = 5) +  # Effect estimate points
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, size = 2) + # Thick bars for confidence intervals
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) + # Defined zero line
  geom_text(aes(label = ifelse(Estimate == 0, "", sprintf("%.3f", Estimate))), hjust = -0.3, size = 6, fontface = "bold")  +
  facet_grid(. ~ Group) +  # Keep clear panel separation
  labs(
    y = "AMCE (Own Vote)",  # Label for y-axis
    x = "Poll Watcher"  # Label for x-axis
  ) +
  theme_minimal(base_size = 20) + # Use a minimal theme with larger font size
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),  # Add black panel borders
    strip.text = element_text(size = 22, face = "bold"),  # Title above each panel
    axis.text = element_text(size = 18),  # Larger x and y axis labels
    panel.grid = element_blank()  # Remove background gridlines
  )

ggsave("step6_outputs/hypothesis_A2A3_PCO.jpg", hypothesesA2_A3_2, width = 12, height = 7, dpi = 300)

#A2A3 PCR

model1_a2 <- felm(PCR_binary ~ nonwhite_pw + pw_party + pollWatcherGender + 
                    method + voterID + registration + closing | ResponseId + State + ChoiceSet, data = conjoint, weights = conjoint$weights)

model2_a2 <- felm(PCR_binary ~ nonwhite_pw * nonwhite + pw_party + pollWatcherGender + 
                    method + voterID + registration + closing | State + ChoiceSet, data = conjoint, weights = conjoint$weights)

# modelsummary(list(model1_a2, model2_a2), stars = T)

ci_all <- extract_ci_single(model1_a2, "nonwhite_pw")
ci_white <- extract_ci_single(model2_a2, "nonwhite_pw")
nonwhite_est <- summary(model2_a2)$coefficients["nonwhite_pw", "Estimate"] + summary(model2_a2)$coefficients["nonwhite_pw:nonwhite", "Estimate"]
nonwhite_std <- sqrt((summary(model2_a2)$coefficients["nonwhite_pw", "Std. Error"])^2 + (summary(model2_a2)$coefficients["nonwhite_pw:nonwhite", "Std. Error"])^2)
ci_nonwhite <- data.frame(
  Estimate = nonwhite_est,
  CI_Lower = nonwhite_est - 1.96 * nonwhite_std,
  CI_Upper = nonwhite_est + 1.96 * nonwhite_std
)


ci_data_a2a3 <- data.frame(
  Group = c("All Respondents", "All Respondents", "White Respondents", "White Respondents", "Nonwhite Respondents", "Nonwhite Respondents"),
  PollWatcher = c("White", "Nonwhite", "White", "Nonwhite", "White", "Nonwhite"),
  Estimate = c(0, as.numeric(ci_all["Estimate"]), 0, as.numeric(ci_white["Estimate"]), 0, as.numeric(ci_nonwhite["Estimate"])),
  CI_Lower = c(0, as.numeric(ci_all["CI_Lower"]), 0, as.numeric(ci_white["CI_Lower"]), 0, as.numeric(ci_nonwhite["CI_Lower"])),
  CI_Upper = c(0, as.numeric(ci_all["CI_Upper"]), 0, as.numeric(ci_white["CI_Upper"]), 0, as.numeric(ci_nonwhite["CI_Upper"]))
) 

ci_data_a2a3$PollWatcher <- factor(ci_data_a2a3$PollWatcher, levels = c("White", "Nonwhite"))

ci_data_a2a3$Group <- factor(ci_data_a2a3$Group, levels = c("All Respondents", "White Respondents", "Nonwhite Respondents"))

hypothesesA2_A3_2 <- ggplot(ci_data_a2a3, aes(x = PollWatcher, y = Estimate)) +
  geom_point(size = 5) +  # Effect estimate points
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, size = 2) + # Thick bars for confidence intervals
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) + # Defined zero line 
  geom_text(aes(label = ifelse(Estimate == 0, "", sprintf("%.3f", Estimate))), hjust = -0.3, size = 6, fontface = "bold") +  # Move text right +
  facet_grid(. ~ Group) +  # Keep clear panel separation
  labs(
    y = "AMCE (Racial Group's Votes)",  # Label for y-axis
    x = "Poll Watcher"  # Label for x-axis
  ) +
  theme_minimal(base_size = 20) + # Use a minimal theme with larger font size
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),  # Add black panel borders
    strip.text = element_text(size = 22, face = "bold"),  # Title above each panel
    axis.text = element_text(size = 18),  # Larger x and y axis labels
    panel.grid = element_blank()  # Remove background gridlines
  )

ggsave("step6_outputs/hypothesis_A2A3_PCR.jpg", hypothesesA2_A3_2, width = 12, height = 7, dpi = 300)