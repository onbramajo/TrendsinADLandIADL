
library(dplyr)
library(tidyr)
library(broom)
library(kableExtra)
library(writexl)



# Function to extract and format model results
extract_model_results <- function(model, model_name) {
  # Get tidy results
  results <- broom::tidy(model) %>%
    mutate(
      model = model_name,
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3),
      p.value = round(p.value, 4),
      # Add significance stars
      significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.10 ~ "†",
        TRUE ~ ""
      ),
      # Format p-value display
      p_display = case_when(
        p.value < 0.001 ~ "<0.001",
        TRUE ~ as.character(p.value)
      )
    ) %>%
    select(model, term, estimate, std.error, statistic, p_display, significance)
  
  return(results)
}

# Extract results from all 12 models
all_models <- list(
  "Female 50-64 ADL" = model1_svy,
  "Female 50-64 IADL" = model2_svy,
  "Female 65-74 ADL" = model3_svy,
  "Female 65-74 IADL" = model4_svy,
  "Female 75-84 ADL" = model5_svy,
  "Female 75-84 IADL" = model6_svy,
  "Male 50-64 ADL" = model7_svy,
  "Male 50-64 IADL" = model8_svy,
  "Male 65-74 ADL" = model9_svy,
  "Male 65-74 IADL" = model10_svy,
  "Male 75-84 ADL" = model11_svy,
  "Male 75-84 IADL" = model12_svy
)

# Extract all results
all_results <- map2_dfr(
  all_models, 
  names(all_models), 
  extract_model_results
)

# View the combined results
print(all_results, n = 100)

# Save as Excel file with all models
write_xlsx(all_results, "model_results_all.xlsx")

# Save as CSV
write.csv(all_results, "model_results_all.csv", row.names = FALSE)

# ===== OPTION 1: Create separate tables for each model =====

create_individual_model_table <- function(model, model_name) {
  results <- extract_model_results(model, model_name)
  
  table <- results %>%
    select(-model) %>%
    kbl(
      caption = model_name,
      col.names = c("Term", "Coefficient", "Std. Error", "Z-statistic", "P-value", "Sig."),
      align = c("l", "r", "r", "r", "r", "c"),
      booktabs = TRUE
    ) %>%
    kable_styling(
      latex_options = c("striped", "hold_position"),
      full_width = FALSE
    ) %>%
    footnote(
      general = "Significance codes: *** p<0.001, ** p<0.01, * p<0.05, † p<0.10",
      general_title = "Note:",
      footnote_as_chunk = TRUE
    )
  
  return(table)
}

# Create individual tables
for (i in seq_along(all_models)) {
  cat("\n\n")
  print(create_individual_model_table(all_models[[i]], names(all_models)[i]))
}

# ===== OPTION 2: Create one large combined table =====


combined_table <- all_results %>%
  select(-model) %>%
  kbl(
    caption = "Logistic Regression Results for All Models",
    col.names = c("Term", "Coefficient", "Std. Error", "Z-statistic", "P-value", "Sig."),
    align = c("l", "l", "r", "r", "r", "r", "c"),
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    full_width = FALSE,
    font_size = 9
  ) %>%
  pack_rows(index = table(all_results$model)) %>%
  footnote(
    general = "Significance codes: *** p<0.001, ** p<0.01, * p<0.05, † p<0.10",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

print(combined_table)
# 
# # Save as HTML
# save_kable(combined_table, "model_results_combined.html")


# Create descriptive statistics by race/ethnicity, decade, and age group
descriptives_by_race <- NHIS_complete %>%
  mutate(
    decade = case_when(
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 & year <= 2018 ~ "2010-2018"
    )
  ) %>%
  filter(!is.na(decade)) %>%
  group_by(RaceEthnicity2, age_category, decade) %>%
  summarise(
    n = n(),
    female_pct = round(mean(Sex == "Female") * 100, 2),
    female_se = round(sd(Sex == "Female") / sqrt(n()) * 100, 2),
    mean_age = round(mean(age), 2),
    age_se = round(sd(age) / sqrt(n()), 2),
    adl_pct = round(mean(ladl) * 100, 2),
    adl_se = round(sd(ladl) / sqrt(n()) * 100, 2),
    iadl_pct = round(mean(laiadl) * 100, 2),
    iadl_se = round(sd(laiadl) / sqrt(n()) * 100, 2),
    ba_pct = round(mean(EducRecoded == "BA and Beyond") * 100, 2),
    ba_se = round(sd(EducRecoded == "BA and Beyond") / sqrt(n()) * 100, 2),
    usborn_pct = round(mean(USBorn == "US Born") * 100, 2),
    usborn_se = round(sd(USBorn == "US Born") / sqrt(n()) * 100, 2),
    .groups = 'drop'
  ) %>%
  arrange(RaceEthnicity2, age_category, decade)

# View the data
print(descriptives_by_race, n = 100)

# Create formatted table with SE in parentheses
descriptives_formatted <- descriptives_by_race %>%
  mutate(
    `Females (%)` = paste0(female_pct, " (", female_se, ")"),
    `Age (mean, years)` = paste0(mean_age, " (", age_se, ")"),
    `ADL Limitation (%)` = paste0(adl_pct, " (", adl_se, ")"),
    `IADL Limitation (%)` = paste0(iadl_pct, " (", iadl_se, ")"),
    `BA or Higher (%)` = paste0(ba_pct, " (", ba_se, ")"),
    `US-Born (%)` = paste0(usborn_pct, " (", usborn_se, ")")
  ) %>%
  select(
    `Race/Ethnicity` = RaceEthnicity2,
    `Age Group` = age_category,
    Period = decade,
    Observations = n,
    `Females (%)`,
    `Age (mean, years)`,
    `ADL Limitation (%)`,
    `IADL Limitation (%)`,
    `BA or Higher (%)`,
    `US-Born (%)`
  )

# Save as CSV
write.csv(descriptives_formatted, "descriptives_by_race_ethnicity.csv", row.names = FALSE)

# Create publication-ready table
table_formatted <- descriptives_formatted %>%
  kbl(
    caption = "Descriptive characteristics by race/ethnicity, decade, and age group, ages 50-84",
    booktabs = TRUE,
    align = c("l", "l", "l", "r", "r", "r", "r", "r", "r", "r")
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    full_width = FALSE,
    font_size = 9
  ) %>%
  pack_rows(index = table(descriptives_formatted$`Race/Ethnicity`)) %>%
  footnote(
    general = "Data are from the National Health Interview Survey, 2000-2018. Standard errors in parentheses. Estimates reflect sample weighting where applicable.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

print(table_formatted)

# Save as HTML
save_kable(table_formatted, "descriptives_by_race_ethnicity.html")

# Alternative: Create separate tables for each age group (cleaner presentation)

# Create descriptive statistics by race/ethnicity, decade, and age group
descriptives_by_race <- NHIS_complete %>%
  mutate(
    decade = case_when(
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 & year <= 2018 ~ "2010-2018"
    )
  ) %>%
  filter(!is.na(decade)) %>%
  group_by(RaceEthnicity2, age_category, decade) %>%
  summarise(
    n = n(),
    female_pct = round(mean(Sex == "Female") * 100, 2),
    female_se = round(sd(Sex == "Female") / sqrt(n()) * 100, 2),
    mean_age = round(mean(age), 2),
    age_se = round(sd(age) / sqrt(n()), 2),
    adl_pct = round(mean(ladl) * 100, 2),
    adl_se = round(sd(ladl) / sqrt(n()) * 100, 2),
    iadl_pct = round(mean(laiadl) * 100, 2),
    iadl_se = round(sd(laiadl) / sqrt(n()) * 100, 2),
    ba_pct = round(mean(EducRecoded == "BA and Beyond") * 100, 2),
    ba_se = round(sd(EducRecoded == "BA and Beyond") / sqrt(n()) * 100, 2),
    .groups = 'drop'
  ) %>%
  arrange(RaceEthnicity2, age_category, decade)

# View the data
print(descriptives_by_race, n = 100)

# Create formatted table with SE in parentheses
descriptives_formatted <- descriptives_by_race %>%
  mutate(
    `Females (%)` = paste0(female_pct, " (", female_se, ")"),
    `Age (mean, years)` = paste0(mean_age, " (", age_se, ")"),
    `ADL Limitation (%)` = paste0(adl_pct, " (", adl_se, ")"),
    `IADL Limitation (%)` = paste0(iadl_pct, " (", iadl_se, ")"),
    `BA or Higher (%)` = paste0(ba_pct, " (", ba_se, ")")
  ) %>%
  select(
    `Race/Ethnicity` = RaceEthnicity2,
    `Age Group` = age_category,
    Period = decade,
    Observations = n,
    `Females (%)`,
    `Age (mean, years)`,
    `ADL Limitation (%)`,
    `IADL Limitation (%)`,
    `BA or Higher (%)`
  )
# 
# # Save as CSV
# write.csv(descriptives_formatted, "descriptives_by_race_ethnicity.csv", row.names = FALSE)


# Create formatted table with SE in parentheses (without race/ethnicity column)
descriptives_formatted <- descriptives_by_race %>%
  mutate(
    `Females (%)` = paste0(female_pct, " (", female_se, ")"),
    `Age (mean, years)` = paste0(mean_age, " (", age_se, ")"),
    `ADL Limitation (%)` = paste0(adl_pct, " (", adl_se, ")"),
    `IADL Limitation (%)` = paste0(iadl_pct, " (", iadl_se, ")"),
    `BA or Higher (%)` = paste0(ba_pct, " (", ba_se, ")")
  ) %>%
  select(
    race_ethnicity = RaceEthnicity2,  # Keep for grouping but will remove from display
    `Age Group` = age_category,
    Period = decade,
    Observations = n,
    `Females (%)`,
    `Age (mean, years)`,
    `ADL Limitation (%)`,
    `IADL Limitation (%)`,
    `BA or Higher (%)`
  )


# Create formatted table with SE in parentheses
descriptives_formatted <- descriptives_by_race %>%
  mutate(
    `Females (%)` = paste0(female_pct, " (", female_se, ")"),
    `Age (mean, years)` = paste0(mean_age, " (", age_se, ")"),
    `ADL Limitation (%)` = paste0(adl_pct, " (", adl_se, ")"),
    `IADL Limitation (%)` = paste0(iadl_pct, " (", iadl_se, ")"),
    `BA or Higher (%)` = paste0(ba_pct, " (", ba_se, ")")
  ) %>%
  select(
    race_ethnicity = RaceEthnicity2,  # Rename for easier reference
    `Age Group` = age_category,
    Period = decade,
    Observations = n,
    `Females (%)`,
    `Age (mean, years)`,
    `ADL Limitation (%)`,
    `IADL Limitation (%)`,
    `BA or Higher (%)`
  )

# Create publication-ready table WITHOUT race/ethnicity column
table_formatted <- descriptives_formatted %>%
  select(-race_ethnicity) %>%  # Remove race/ethnicity from table body
  kbl(
    caption = "Table 1. Descriptive characteristics by race/ethnicity, decade, and age group, ages 50-84",
    booktabs = TRUE,
    align = c("l", "l", "r", "r", "r", "r", "r", "r")
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    full_width = FALSE,
    font_size = 9
  ) %>%
  pack_rows(index = table(descriptives_formatted$race_ethnicity)) %>%  # Race/ethnicity as section headers
  footnote(
    general = "Data are from the National Health Interview Survey, 2000-2018. Standard errors in parentheses. Estimates reflect sample weighting where applicable.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

print(table_formatted)

# Save as HTML
save_kable(table_formatted, "descriptives_by_race_ethnicity.html")