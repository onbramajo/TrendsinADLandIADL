
library(haven)
library(tidyverse)
library(survey)

# 1. LOAD RAW DATA

NHIS_raw <- read_dta("workingdirectory/nhis_00006.dta.gz")

cat("Step 1: Initial sample size:", nrow(NHIS_raw), "\n\n")

# 2. BASIC FILTERING AND VARIABLE CREATION
# =========================================


NHIS <- NHIS_raw %>%
  filter(
    age > 49 & age < 85,    # Age 50-84
    year < 2019,             # Years 2000-2018
    ladl < 6,                # Valid ADL responses
    laiadl < 6               # Valid IADL responses
  ) %>%
  mutate(
    # Age categories
    age_category = case_when(
      between(age, 50, 64) ~ "50-64",
      between(age, 65, 74) ~ "65-74",
      between(age, 75, 84) ~ "75-84"
    ),
    
    # Sex recoding
    Sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    ),
    
    # Race/Ethnicity
    RaceEthnicity = case_when(
      racenew == 100 & hispyn == 1 ~ "NH-White",
      racenew == 200 & hispyn == 1 ~ "NH-Black",
      hispyn == 2 ~ "Hispanic",
      TRUE ~ "Other"  # All other combinations
    ),
      
    # Nativity
    nativity_proper = case_when(
      usborn %in% c(10, 12) ~ "Foreign Born",
      usborn %in% c(11, 20) ~ "US Born"
    ),
    
    # Education
    EducRecoded = case_when(
      educ < 200 ~ "Less than GED or High School Diploma",
      educ >= 200 & educ <= 303 ~ "GED to Some College",
      educ >= 400 | educ %in% c(996, 999) ~ "BA and Beyond"
    ),
    
    # Outcomes (convert 1=no, 2=yes to 0=no, 1=yes)
    ladl = ladl - 1,
    laiadl = laiadl - 1,
    
    # Time variables
    year_centered = year - 2010,
    year_after_2010 = pmax(0, year - 2010)
  )

cat("After basic filtering:", nrow(NHIS), "observations\n")
# 3. IDENTIFY INCOMPLETE CASES
# =============================

# Check missingness in each variable
missing_check <- NHIS %>%
  summarise(
    total = n(),
    missing_sex = sum(is.na(Sex)),
    missing_age_cat = sum(is.na(age_category)),
    missing_race = sum(is.na(RaceEthnicity)),
    missing_nativity = sum(is.na(nativity_proper)),
    missing_education = sum(is.na(EducRecoded)),
    missing_ladl = sum(is.na(ladl)),
    missing_laiadl = sum(is.na(laiadl))
  )

cat("Missing values by variable:\n")
print(t(missing_check))
cat("\n")

# Breakdown by race/ethnicity
cat("Missing nativity by race/ethnicity:\n")
nativity_by_race <- NHIS %>%
  group_by(RaceEthnicity) %>%
  summarise(
    total = n(),
    missing_nativity = sum(is.na(nativity_proper)),
    pct_missing = round(100 * missing_nativity / total, 2)
  )
print(nativity_by_race)
cat("\n")

# 4. FILTER TO COMPLETE CASES
# ============================

NHIS_complete <- NHIS %>%
  filter(
    !is.na(Sex),
    !is.na(age_category),
    !is.na(RaceEthnicity),
    !is.na(nativity_proper),
    !is.na(EducRecoded),
    !is.na(ladl),
    !is.na(laiadl)
  )

NHIS_complete <- NHIS_complete %>% 
  filter(RaceEthnicity!="Other")

cat("Before complete case filtering:", nrow(NHIS), "\n")
cat("After complete case filtering:", nrow(NHIS_complete), "\n")
cat("Rows removed:", nrow(NHIS) - nrow(NHIS_complete), "\n")
cat("Percent removed:", round(100 * (nrow(NHIS) - nrow(NHIS_complete)) / nrow(NHIS), 2), "%\n\n")

# 5. CREATE RACE-ETHNICITY-NATIVITY VARIABLE AND FACTORS
# ===================================================


NHIS_complete <- NHIS_complete %>%
  mutate(
    RaceEthnicity2 = case_when(
      RaceEthnicity == "NH-White" ~ "NH-White",
      RaceEthnicity == "NH-Black" ~ "NH-Black",
      RaceEthnicity == "Hispanic" & nativity_proper == "US Born" ~ "Hispanic US Born",
      RaceEthnicity == "Hispanic" & nativity_proper == "Foreign Born" ~ "Hispanic Foreign Born"
    )
  )

NHIS_complete <- NHIS_complete %>%
  mutate(
    Sex = factor(Sex, levels = c("Male", "Female")),
    age_category = factor(age_category, levels = c("50-64", "65-74", "75-84")),
    RaceEthnicity = factor(RaceEthnicity, levels = c("NH-White", "NH-Black", "Hispanic")),
    nativity_proper = factor(nativity_proper, levels = c("Foreign Born", "US Born")),
    RaceEthnicity2 = factor(RaceEthnicity2, 
                                   levels = c("NH-White", "NH-Black", 
                                              "Hispanic US Born", "Hispanic Foreign Born")),
    EducRecoded = factor(EducRecoded, 
                         levels = c("Less than GED or High School Diploma",
                                    "GED to Some College",
                                    "BA and Beyond"))
  )

# 7. FINAL SAMPLE DISTRIBUTION
# =============================

cat("Total observations:", nrow(NHIS_complete), "\n")
cat("Years:", min(NHIS_complete$year), "-", max(NHIS_complete$year), "\n")
cat("Age range:", min(NHIS_complete$age), "-", max(NHIS_complete$age), "\n\n")

cat("Distribution by Sex:\n")
print(table(NHIS_complete$Sex))
cat("\n")

cat("Distribution by Age Category:\n")
print(table(NHIS_complete$age_category))
cat("\n")

cat("Distribution by Race/Ethnicity:\n")
print(table(NHIS_complete$RaceEthnicity))
cat("\n")

cat("Distribution by Nativity:\n")
print(table(NHIS_complete$nativity_proper))
cat("\n")

cat("Distribution by Race/Ethnicity:\n")
print(table(NHIS_complete$RaceEthnicity2))
cat("\n")

cat("Cross-tab: Race/Ethnicity x Nativity:\n")
print(addmargins(table(NHIS_complete$RaceEthnicity, NHIS_complete$nativity_proper)))
cat("\n")

cat("Distribution by Education:\n")
print(table(NHIS_complete$EducRecoded))
cat("\n")

cat("ADL Limitation Prevalence:\n")
cat("Overall:", round(mean(NHIS_complete$ladl) * 100, 2), "%\n\n")

cat("IADL Limitation Prevalence:\n")
cat("Overall:", round(mean(NHIS_complete$laiadl) * 100, 2), "%\n\n")

# 7. DESCRIPTIVES
# ====================================


descriptives <- NHIS_complete %>%
  group_by(RaceEthnicity2, Sex, age_category) %>%
  summarise(
    n = n(),
    mean_age = round(mean(age), 1),
    adl_prev = round(mean(ladl) * 100, 2),
    iadl_prev = round(mean(laiadl) * 100, 2),
    .groups = 'drop'
  ) %>%
  arrange(Sex, age_category, RaceEthnicity2)

print(descriptives, n = 24)



# First, create decade variable and calculate descriptives by decade
descriptives_by_decade <- NHIS_complete %>%
  mutate(
    decade = case_when(
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 & year <= 2018 ~ "2010-2018",
      TRUE ~ "Other"
    )
  ) %>%
  filter(decade != "Other") %>%
  group_by(decade, RaceEthnicity2, Sex, age_category) %>%
  summarise(
    n = n(),
    mean_age = round(mean(age, na.rm = TRUE), 1),
    sd_age = round(sd(age, na.rm = TRUE), 1),
    adl_prev = round(mean(ladl, na.rm = TRUE) * 100, 2),
    adl_sd = round(sd(ladl, na.rm = TRUE) * 100, 2),
    iadl_prev = round(mean(laiadl, na.rm = TRUE) * 100, 2),
    iadl_sd = round(sd(laiadl, na.rm = TRUE) * 100, 2),
    .groups = 'drop'
  ) %>%
  arrange(decade, Sex, age_category, RaceEthnicity2)

# Overall descriptives (across all years)
descriptives_overall <- NHIS_complete %>%
  group_by(RaceEthnicity2, Sex, age_category) %>%
  summarise(
    n = n(),
    mean_age = round(mean(age, na.rm = TRUE), 1),
    sd_age = round(sd(age, na.rm = TRUE), 1),
    adl_prev = round(mean(ladl, na.rm = TRUE) * 100, 2),
    adl_sd = round(sd(ladl, na.rm = TRUE) * 100, 2),
    iadl_prev = round(mean(laiadl, na.rm = TRUE) * 100, 2),
    iadl_sd = round(sd(laiadl, na.rm = TRUE) * 100, 2),
    .groups = 'drop'
  ) %>%
  mutate(decade = "Overall") %>%
  arrange(Sex, age_category, RaceEthnicity2)

# Combine
descriptives_combined <- bind_rows(descriptives_by_decade, descriptives_overall) %>%
  mutate(
    decade = factor(decade, levels = c("2000-2009", "2010-2018", "Overall")),
    age_category = factor(age_category, levels = c("50-64", "65-74", "75-84")),
    RaceEthnicity2 = factor(RaceEthnicity2, 
                                   levels = c("NH-White", "NH-Black", 
                                              "Hispanic US Born", "Hispanic Foreign Born"))
  )

# View the data
print(descriptives_combined, n = 72)

# 1. Sample Size Plot by Decade
plot_sample_size <- ggplot(descriptives_by_decade, 
                           aes(x = age_category, y = n, fill = decade)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = scales::comma(n)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 5) +
  facet_grid(Sex ~ RaceEthnicity2) +
  scale_fill_manual(values = c("2000-2009" = "#3498db", "2010-2018" = "#e74c3c")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Sample Size by Decade, Age Group, and Race/Ethnicity",
    x = "Age Group",
    y = "Sample Size (N)",
    fill = "Decade"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(0.5, "lines")
  )

print(plot_sample_size)

# 5. Create summary table for publication
summary_table <- descriptives_combined %>%
  mutate(
    ADL = sprintf("%.2f (%.2f)", adl_prev, adl_sd),
    IADL = sprintf("%.2f (%.2f)", iadl_prev, iadl_sd),
    N = scales::comma(n)
  ) %>%
  select(Decade = decade, Sex, Age = age_category, 
         `Race/Ethnicity` = RaceEthnicity2, 
         N, ADL, IADL) %>%
  arrange(Decade, Sex, Age, `Race/Ethnicity`)

print(summary_table, n = 100)

# # Save table as CSV
# write.csv(summary_table, "descriptive_statistics_by_decade.csv", row.names = FALSE)





# 9. CREATE SURVEY DESIGN OBJECT
# ===============================

nhis_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~perweight,
  data = NHIS_complete,
  nest = TRUE
)

NHIS <- NHIS_complete


# Quick verification
cat("  Any missing in key variables:", any(is.na(NHIS$Sex), is.na(NHIS$RaceEthnicity2), 
                                           is.na(NHIS$nativity_proper), is.na(NHIS$EducRecoded)), "\n")
cat("  Sample size:", nrow(NHIS), "\n")


#now use the design with psu and strata

model1_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "50-64"))


model2_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "65-74"))

model3_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "75-84"))

model4_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "50-64"))

model5_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "65-74"))

model6_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "75-84"))

model7_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "50-64"))

model8_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "65-74"))

model9_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "75-84"))

model10_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "50-64"))

model11_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "65-74"))

model12_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded ,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "75-84"))


# summary of all models 
summary(model1_svy )
summary(model2_svy )
summary(model3_svy )
summary(model4_svy )
summary(model5_svy )
summary(model6_svy )
summary(model7_svy )
summary(model8_svy )
summary(model9_svy )
summary(model10_svy )
summary(model11_svy )
summary(model12_svy )

# estimate expits
exp(coef(model1_svy ))
exp(coef(model2_svy ))
exp(coef(model3_svy ))
exp(coef(model4_svy ))
exp(coef(model5_svy ))
exp(coef(model6_svy ))
exp(coef(model7_svy ))
exp(coef(model8_svy ))
exp(coef(model9_svy ))
exp(coef(model10_svy ))
exp(coef(model11_svy ))
exp(coef(model12_svy ))



# Fixed prediction function with proper column references
calculate_predictions_with_hispanic_nativity <- function(svy_model, model_name, age_val, outcome_type = "ADL") {
  
  # Create prediction data
  new_data <- expand.grid(
    year = 2000:2018,
    RaceEthnicity2 = c("NH-White", "NH-Black", "Hispanic US Born", "Hispanic Foreign Born")
  ) %>%
    mutate(
      age = age_val,
      EducRecoded = "GED to Some College",
      year_centered = year - 2010,
      year_after_2010 = pmax(0, year - 2010),
      # # Add nativity_proper based on RaceEthnicity2
      # nativity_proper = case_when(
      #   RaceEthnicity2 == "Hispanic US Born" ~ "US Born",
      #   RaceEthnicity2 == "Hispanic Foreign Born" ~ "Foreign Born",
      #   RaceEthnicity2 == "NH-White" ~ "US Born",  # Assuming NH-White are US born
      #   RaceEthnicity2 == "NH-Black" ~ "US Born"   # Assuming NH-Black are US born
      # ),
      # Create plot_group for visualization
      plot_group = RaceEthnicity2
    )
  
  # Get coefficients and vcov matrix
  coefs <- coef(svy_model)
  vcov_mat <- vcov(svy_model)
  
  # Create design matrix manually
  n_pred <- nrow(new_data)
  X_new <- matrix(0, nrow = n_pred, ncol = length(coefs))
  colnames(X_new) <- names(coefs)
  
  # Fill design matrix for each prediction
  for(i in 1:n_pred) {
    row_data <- new_data[i, ]
    
    # Intercept
    X_new[i, "(Intercept)"] <- 1
    
    # Continuous variables
    X_new[i, "year_centered"] <- row_data$year_centered
    X_new[i, "year_after_2010"] <- row_data$year_after_2010
    X_new[i, "age"] <- row_data$age
    
    # Race/Ethnicity (NH-White is reference)
    if(row_data$RaceEthnicity2 == "NH-Black") {
      X_new[i, "RaceEthnicity2NH-Black"] <- 1
    }
    if(row_data$RaceEthnicity2 == "Hispanic US Born") {
      X_new[i, "RaceEthnicity2Hispanic US Born"] <- 1
    }
    if(row_data$RaceEthnicity2 == "Hispanic Foreign Born") {
      X_new[i, "RaceEthnicity2Hispanic Foreign Born"] <- 1
    }
    
    # Education (reference category gets 0 for both)
    X_new[i, "EducRecodedGED to Some College"] <- 1
    
    # # Nativity (Foreign Born is reference, so US Born gets 1)
    # if(row_data$nativity_proper == "US Born") {
    #   X_new[i, "nativity_properUS Born"] <- 1
    # }
    
    # Race-year interactions
    if(row_data$RaceEthnicity2 == "NH-Black") {
      X_new[i, "year_centered:RaceEthnicity2NH-Black"] <- row_data$year_centered
      X_new[i, "year_after_2010:RaceEthnicity2NH-Black"] <- row_data$year_after_2010
    }
    
    if(row_data$RaceEthnicity2 == "Hispanic US Born") {
      X_new[i, "year_centered:RaceEthnicity2Hispanic US Born"] <- row_data$year_centered
      X_new[i, "year_after_2010:RaceEthnicity2Hispanic US Born"] <- row_data$year_after_2010
    }
    
    if(row_data$RaceEthnicity2 == "Hispanic Foreign Born") {
      X_new[i, "year_centered:RaceEthnicity2Hispanic Foreign Born"] <- row_data$year_centered
      X_new[i, "year_after_2010:RaceEthnicity2Hispanic Foreign Born"] <- row_data$year_after_2010
    }
  }
  
  # Calculate linear predictions and SEs
  linear_preds <- X_new %*% coefs
  linear_se <- sqrt(diag(X_new %*% vcov_mat %*% t(X_new)))
  
  # Convert to probability scale
  new_data$predicted <- as.numeric(plogis(linear_preds))
  new_data$se <- abs(new_data$predicted * (1 - new_data$predicted) * linear_se)
  new_data$model <- model_name
  new_data$outcome <- outcome_type
  
  return(new_data)
}

# Generate predictions for all 12 models
pred1_hispanic <- calculate_predictions_with_hispanic_nativity(model1_svy, "Male 50-64", 57, "ADL")
pred2_hispanic <- calculate_predictions_with_hispanic_nativity(model2_svy, "Male 65-74", 69, "ADL") 
pred3_hispanic <- calculate_predictions_with_hispanic_nativity(model3_svy, "Male 75-84", 79, "ADL")
pred4_hispanic <- calculate_predictions_with_hispanic_nativity(model4_svy, "Female 50-64", 57, "ADL")
pred5_hispanic <- calculate_predictions_with_hispanic_nativity(model5_svy, "Female 65-74", 69, "ADL")
pred6_hispanic <- calculate_predictions_with_hispanic_nativity(model6_svy, "Female 75-84", 79, "ADL")

pred7_hispanic <- calculate_predictions_with_hispanic_nativity(model7_svy, "Male 50-64", 57, "IADL")
pred8_hispanic <- calculate_predictions_with_hispanic_nativity(model8_svy, "Male 65-74", 69, "IADL")
pred9_hispanic <- calculate_predictions_with_hispanic_nativity(model9_svy, "Male 75-84", 79, "IADL")
pred10_hispanic <- calculate_predictions_with_hispanic_nativity(model10_svy, "Female 50-64", 57, "IADL")
pred11_hispanic <- calculate_predictions_with_hispanic_nativity(model11_svy, "Female 65-74", 69, "IADL")
pred12_hispanic <- calculate_predictions_with_hispanic_nativity(model12_svy, "Female 75-84", 79, "IADL")

# Combine all predictions
all_preds_hispanic <- bind_rows(
  pred1_hispanic, pred2_hispanic, pred3_hispanic, pred4_hispanic, pred5_hispanic, pred6_hispanic,
  pred7_hispanic, pred8_hispanic, pred9_hispanic, pred10_hispanic, pred11_hispanic, pred12_hispanic
)

# Plotting function
create_age_group_plot_hispanic <- function(age_group_name) {
  
  data_subset <- all_preds_hispanic %>% 
    filter(grepl(age_group_name, model)) %>%
    mutate(
      age_group = age_group_name,
      sex = ifelse(grepl("Male", model), "Male", "Female")
    )
  
  color_palette <- c(
    "NH-White" = "blue",
    "NH-Black" = "red", 
    "Hispanic US Born" = "darkgreen",
    "Hispanic Foreign Born" = "orange"
  )
  
  ggplot(data_subset, aes(x = year, y = predicted * 100, color = RaceEthnicity2)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = (predicted - 1.96*se) * 100, 
                    ymax = (predicted + 1.96*se) * 100, 
                    fill = RaceEthnicity2), alpha = 0.2, color = NA) +
    facet_grid(outcome ~ sex, scales = "free_y") +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    labs(title = paste("ADL/IADL prevalence by year: Ages", age_group_name, "(2000-2018)"),
         x = "Year",
         y = "Predicted Probability (%)",
         color = "Race/Ethnicity",
         fill = "Race/Ethnicity") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 14),        # X-axis tick labels
      axis.text.y = element_text(size = 14),        # Y-axis tick labels
      axis.title.x = element_text(size = 16),       # X-axis title
      axis.title.y = element_text(size = 16),       # Y-axis title
      legend.text = element_text(size = 14),        # Legend text
      legend.title = element_text(size = 16),       # Legend title
      plot.title = element_text(size = 18),         # Plot title
      strip.text = element_text(size = 14)          # Facet labels
    ) +
    geom_vline(xintercept = 2010, linetype = "dashed", color = "gray50", alpha = 0.8, linewidth = 0.9)
}
  
# Generate the three plots
plot_50_64_hispanic <- create_age_group_plot_hispanic("50-64")
plot_65_74_hispanic <- create_age_group_plot_hispanic("65-74") 
plot_75_84_hispanic <- create_age_group_plot_hispanic("75-84")

# Display plots
print(plot_50_64_hispanic)
print(plot_65_74_hispanic)
print(plot_75_84_hispanic)

# Calculate trends with confidence intervals
calculate_trends_hispanic <- function(pred_data) {
  
  trend_data <- pred_data %>%
    filter(year %in% c(2000, 2010, 2018)) %>%
    group_by(model, outcome, plot_group) %>%
    arrange(year) %>%
    summarise(
      pred_2000 = (predicted * 100)[year == 2000],
      pred_2010 = (predicted * 100)[year == 2010], 
      pred_2018 = (predicted * 100)[year == 2018],
      se_2000 = (se * 100)[year == 2000],
      se_2010 = (se * 100)[year == 2010],
      se_2018 = (se * 100)[year == 2018],
      .groups = 'drop'
    ) %>%
    mutate(
      # Prevalence confidence intervals
      pred_2000_lower = pred_2000 - 1.96 * se_2000,
      pred_2000_upper = pred_2000 + 1.96 * se_2000,
      pred_2010_lower = pred_2010 - 1.96 * se_2010,
      pred_2010_upper = pred_2010 + 1.96 * se_2010,
      pred_2018_lower = pred_2018 - 1.96 * se_2018,
      pred_2018_upper = pred_2018 + 1.96 * se_2018,
      
      # Absolute changes
      change_2000_2010 = pred_2010 - pred_2000,
      change_2010_2018 = pred_2018 - pred_2010, 
      change_2000_2018 = pred_2018 - pred_2000,
      
      # Change confidence intervals (using propagation of error)
      change_2000_2010_se = sqrt(se_2000^2 + se_2010^2),
      change_2010_2018_se = sqrt(se_2010^2 + se_2018^2),
      change_2000_2018_se = sqrt(se_2000^2 + se_2018^2),
      
      change_2000_2010_lower = change_2000_2010 - 1.96 * change_2000_2010_se,
      change_2000_2010_upper = change_2000_2010 + 1.96 * change_2000_2010_se,
      change_2010_2018_lower = change_2010_2018 - 1.96 * change_2010_2018_se,
      change_2010_2018_upper = change_2010_2018 + 1.96 * change_2010_2018_se,
      change_2000_2018_lower = change_2000_2018 - 1.96 * change_2000_2018_se,
      change_2000_2018_upper = change_2000_2018 + 1.96 * change_2000_2018_se,
      
      # Percent changes
      pct_change_2000_2010 = ((pred_2010 - pred_2000) / pred_2000) * 100,
      pct_change_2010_2018 = ((pred_2018 - pred_2010) / pred_2010) * 100,
      pct_change_2000_2018 = ((pred_2018 - pred_2000) / pred_2000) * 100,
      
      # Percent change confidence intervals (delta method approximation)
      pct_change_2000_2010_se = abs(pct_change_2000_2010) * sqrt((se_2000/pred_2000)^2 + (se_2010/pred_2010)^2),
      pct_change_2010_2018_se = abs(pct_change_2010_2018) * sqrt((se_2010/pred_2010)^2 + (se_2018/pred_2018)^2),
      pct_change_2000_2018_se = abs(pct_change_2000_2018) * sqrt((se_2000/pred_2000)^2 + (se_2018/pred_2018)^2),
      
      pct_change_2000_2010_lower = pct_change_2000_2010 - 1.96 * pct_change_2000_2010_se,
      pct_change_2000_2010_upper = pct_change_2000_2010 + 1.96 * pct_change_2000_2010_se,
      pct_change_2010_2018_lower = pct_change_2010_2018 - 1.96 * pct_change_2010_2018_se,
      pct_change_2010_2018_upper = pct_change_2010_2018 + 1.96 * pct_change_2010_2018_se,
      pct_change_2000_2018_lower = pct_change_2000_2018 - 1.96 * pct_change_2000_2018_se,
      pct_change_2000_2018_upper = pct_change_2000_2018 + 1.96 * pct_change_2000_2018_se,
      
      # Risk ratios
      rr_2010_vs_2000 = pred_2010 / pred_2000,
      rr_2018_vs_2010 = pred_2018 / pred_2010,
      rr_2018_vs_2000 = pred_2018 / pred_2000,
      
      # Risk ratio confidence intervals (delta method)
      rr_2010_vs_2000_log_se = sqrt((se_2000/pred_2000)^2 + (se_2010/pred_2010)^2),
      rr_2018_vs_2010_log_se = sqrt((se_2010/pred_2010)^2 + (se_2018/pred_2018)^2),
      rr_2018_vs_2000_log_se = sqrt((se_2000/pred_2000)^2 + (se_2018/pred_2018)^2),
      
      rr_2010_vs_2000_lower = exp(log(rr_2010_vs_2000) - 1.96 * rr_2010_vs_2000_log_se),
      rr_2010_vs_2000_upper = exp(log(rr_2010_vs_2000) + 1.96 * rr_2010_vs_2000_log_se),
      rr_2018_vs_2010_lower = exp(log(rr_2018_vs_2010) - 1.96 * rr_2018_vs_2010_log_se),
      rr_2018_vs_2010_upper = exp(log(rr_2018_vs_2010) + 1.96 * rr_2018_vs_2010_log_se),
      rr_2018_vs_2000_lower = exp(log(rr_2018_vs_2000) - 1.96 * rr_2018_vs_2000_log_se),
      rr_2018_vs_2000_upper = exp(log(rr_2018_vs_2000) + 1.96 * rr_2018_vs_2000_log_se)
    )
  
  return(trend_data)
}

# Calculate comprehensive trends
trends_hispanic <- calculate_trends_hispanic(all_preds_hispanic)

# Create summary tables with confidence intervals
point_estimates_hispanic <- trends_hispanic %>%
  select(model, outcome, plot_group, 
         pred_2000, pred_2000_lower, pred_2000_upper,
         pred_2010, pred_2010_lower, pred_2010_upper,
         pred_2018, pred_2018_lower, pred_2018_upper) %>%
  mutate(
    pred_2000_ci = paste0(round(pred_2000, 2), " (", round(pred_2000_lower, 2), "-", round(pred_2000_upper, 2), ")"),
    pred_2010_ci = paste0(round(pred_2010, 2), " (", round(pred_2010_lower, 2), "-", round(pred_2010_upper, 2), ")"),
    pred_2018_ci = paste0(round(pred_2018, 2), " (", round(pred_2018_lower, 2), "-", round(pred_2018_upper, 2), ")")
  ) %>%
  select(model, outcome, plot_group, pred_2000_ci, pred_2010_ci, pred_2018_ci)



# Fixed forest plot function with geom_text for risk ratios and changes in prevalence

# Fixed forest plot function with geom_text and bold for significant results
library(gridExtra)
library(grid)

# Fixed forest plot function with geom_text and bold for significant results
create_risk_ratio_forest_plot_fixed <- function(rr_data, age_group_filter = NULL) {
  
  # Apply age filter if specified
  if (!is.null(age_group_filter)) {
    rr_data <- rr_data %>% filter(grepl(age_group_filter, model))
  }
  
  # Reshape data for forest plot
  forest_data <- rr_data %>%
    select(model, outcome, plot_group, 
           rr_2010_vs_2000, rr_2010_vs_2000_lower, rr_2010_vs_2000_upper,
           rr_2018_vs_2010, rr_2018_vs_2010_lower, rr_2018_vs_2010_upper,
           rr_2018_vs_2000, rr_2018_vs_2000_lower, rr_2018_vs_2000_upper) %>%
    # Add sex variable
    mutate(
      sex = ifelse(grepl("Male", model), "Male", "Female")
    ) %>%
    # Create long format
    bind_rows(
      # 2010 vs 2000
      select(., model, outcome, plot_group, sex,
             rr = rr_2010_vs_2000, lower_ci = rr_2010_vs_2000_lower, upper_ci = rr_2010_vs_2000_upper) %>%
        mutate(comparison = "2010 vs 2000"),
      
      # 2018 vs 2010  
      select(., model, outcome, plot_group, sex,
             rr = rr_2018_vs_2010, lower_ci = rr_2018_vs_2010_lower, upper_ci = rr_2018_vs_2010_upper) %>%
        mutate(comparison = "2018 vs 2010"),
      
      # 2018 vs 2000
      select(., model, outcome, plot_group, sex,
             rr = rr_2018_vs_2000, lower_ci = rr_2018_vs_2000_lower, upper_ci = rr_2018_vs_2000_upper) %>%
        mutate(comparison = "2018 vs 2000")
    ) %>%
    # Remove rows with missing or invalid values
    filter(!is.na(rr), !is.na(lower_ci), !is.na(upper_ci), 
           rr > 0, lower_ci > 0, upper_ci > 0,
           is.finite(rr), is.finite(lower_ci), is.finite(upper_ci)) %>%
    # Create proper ordering with time comparisons in correct order
    mutate(
      comparison = factor(comparison, levels = c("2010 vs 2000", "2018 vs 2010", "2018 vs 2000"))
    ) %>%
    # Create ordered factors: Sex > Ethnicity > Year (in correct order)
    arrange(sex, plot_group, comparison) %>%
    mutate(
      group_comparison_ordered = paste(sex, plot_group, comparison, sep=" | "),
      y_factor = factor(group_comparison_ordered, levels = unique(group_comparison_ordered)),
      # Determine if CI excludes 1 (significant)
      is_significant = (lower_ci > 1 | upper_ci < 1),
      # Create label text with RR and CI on single line
      value_label = sprintf("%.2f (%.2f-%.2f)", rr, lower_ci, upper_ci),
      # Set font face based on significance
      font_face = ifelse(is_significant, "bold", "plain")
    )
  
  # Calculate max x value for fixed text position BY OUTCOME
  max_x_by_outcome <- forest_data %>%
    group_by(outcome) %>%
    summarise(max_x = max(upper_ci, na.rm = TRUE) * 1.05, .groups = "drop")
  
  # Join back to forest_data
  forest_data <- forest_data %>%
    left_join(max_x_by_outcome, by = "outcome")
  
  # Color palette by ethnicity (matching your Hispanic plots)
  color_palette <- c(
    "NH-White" = "blue",
    "NH-Black" = "red", 
    "Hispanic US Born" = "darkgreen",
    "Hispanic Foreign Born" = "orange"
  )
  
  # Get unique outcomes for faceting
  outcomes <- unique(forest_data$outcome)
  
  # Create separate plots for each outcome
  plot_list <- lapply(outcomes, function(out) {
    data_subset <- forest_data %>% filter(outcome == out)
    
    p <- ggplot(data_subset, aes(y = y_factor, x = rr, 
                                 color = plot_group)) +
      
      # Reference line at RR = 1
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", size = 0.8) +
      
      # Confidence intervals
      geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), 
                     height = 0.3, size = 0.8, alpha = 0.8) +
      
      # Point estimates - make significant ones larger, no shapes
      geom_point(aes(size = is_significant), alpha = 0.9) +
      scale_size_manual(values = c("TRUE" = 4, "FALSE" = 3), guide = "none") +
      
      # Add text labels at fixed position - bold for significant
      geom_text(aes(label = value_label, x = max_x, fontface = font_face),
                hjust = 0,
                size = 5,
                show.legend = FALSE,
                color = "black") +
      
      # Styling
      scale_color_manual(values = color_palette, name = "Race/Ethnicity") +
      scale_x_log10(breaks = c(0.3, 0.5, 0.7, 1.0, 1.4, 2.0, 3.0, 5.0), 
                    limits = c(0.3, 5.0),
                    expand = expansion(mult = c(0.05, 0.35))) +
      
      labs(
        title = out,
        x = NULL,
        y = NULL
      ) +
      
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),           # Increased from 10.5
        axis.title.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        panel.grid.major.x = element_line(color = "gray95", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      
      coord_cartesian(clip = "off")
    
    return(p)
  })
  
  # Remove y-axis text from right column and adjust margins for alignment
  for(i in seq_along(plot_list)) {
    if(i %% 2 == 0) {  # Right column
      plot_list[[i]] <- plot_list[[i]] + 
        theme(axis.text.y = element_blank(),
              plot.margin = margin(12, 10, 10, 10))  # Add 2mm top margin to right plots
    }
  }
  
  # Create layout matrix for proper alignment
  n_plots <- length(plot_list)
  n_rows <- ceiling(n_plots / 2)
  
  # Combine plots - no legend, x-axis label centered at bottom
  final_plot <- grid.arrange(
    grobs = plot_list,
    ncol = 2,
    heights = rep(1, n_rows),
    top = textGrob(
      paste("Risk Ratios with 95% Confidence Intervals", 
            if(!is.null(age_group_filter)) paste("- Ages", age_group_filter) else ""),
      gp = gpar(fontsize = 18, fontface = "bold")
    ),
    bottom = textGrob(
      paste("Risk Ratio (log scale)\n", "Bold values indicate 95% CI excludes 1.0"),
      gp = gpar(fontsize = c(16, 12), fontface = c("plain", "italic"))
    ),
    left = textGrob("Sex | Race/Ethnicity | Time Comparison", rot = 90, 
                    gp = gpar(fontsize = 16))
  )
  
  return(final_plot)
}

# Fixed prevalence change plot with bold for significant results
create_prevalence_change_plot_fixed <- function(comprehensive_data, age_group_filter = NULL) {
  
  # Apply filter if specified
  if (!is.null(age_group_filter)) {
    comprehensive_data <- comprehensive_data %>% filter(grepl(age_group_filter, model))
  }
  
  # Prepare data for plotting
  plot_data <- comprehensive_data %>%
    mutate(
      sex = ifelse(grepl("Male", model), "Male", "Female")
    ) %>%
    select(model, outcome, plot_group, sex,
           change_2000_2010, change_2000_2010_lower, change_2000_2010_upper,
           change_2010_2018, change_2010_2018_lower, change_2010_2018_upper,
           change_2000_2018, change_2000_2018_lower, change_2000_2018_upper) %>%
    # Create long format
    bind_rows(
      select(., model, outcome, plot_group, sex,
             change = change_2000_2010, lower_ci = change_2000_2010_lower, upper_ci = change_2000_2010_upper) %>%
        mutate(period = "2000-2010"),
      
      select(., model, outcome, plot_group, sex,
             change = change_2010_2018, lower_ci = change_2010_2018_lower, upper_ci = change_2010_2018_upper) %>%
        mutate(period = "2010-2018"),
      
      select(., model, outcome, plot_group, sex,
             change = change_2000_2018, lower_ci = change_2000_2018_lower, upper_ci = change_2000_2018_upper) %>%
        mutate(period = "2000-2018")
    ) %>%
    # Remove invalid values
    filter(!is.na(change), !is.na(lower_ci), !is.na(upper_ci),
           is.finite(change), is.finite(lower_ci), is.finite(upper_ci)) %>%
    # Create proper ordering for periods
    mutate(
      period = factor(period, levels = c("2000-2010", "2010-2018", "2000-2018"))
    ) %>%
    # Order by Sex > Ethnicity > Period
    arrange(sex, plot_group, period) %>%
    mutate(
      group_period_ordered = paste(sex, plot_group, period, sep = " | "),
      y_factor = factor(group_period_ordered, levels = unique(group_period_ordered)),
      # Determine if CI excludes 0 (significant)
      is_significant = (lower_ci > 0 | upper_ci < 0),
      # Create label text with change and CI on single line
      value_label = sprintf("%.2f (%.2f-%.2f)", change, lower_ci, upper_ci),
      # Set font face based on significance
      font_face = ifelse(is_significant, "bold", "plain")
    )
  
  # Calculate max x value for fixed text position BY OUTCOME
  max_x_by_outcome <- plot_data %>%
    group_by(outcome) %>%
    summarise(max_x = max(upper_ci, na.rm = TRUE) * 1.05, .groups = "drop")
  
  # Join back to plot_data
  plot_data <- plot_data %>%
    left_join(max_x_by_outcome, by = "outcome")
  
  # Color palette by ethnicity (same as risk ratio plot)
  color_palette <- c(
    "NH-White" = "blue",
    "NH-Black" = "red", 
    "Hispanic US Born" = "darkgreen",
    "Hispanic Foreign Born" = "orange"
  )
  
  # Get unique outcomes for faceting
  outcomes <- unique(plot_data$outcome)
  
  # Create separate plots for each outcome
  plot_list <- lapply(outcomes, function(out) {
    data_subset <- plot_data %>% filter(outcome == out)
    
    p <- ggplot(data_subset, aes(y = y_factor, x = change, 
                                 color = plot_group)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
      geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), 
                     height = 0.3, size = 0.8, alpha = 0.8) +
      
      # Point estimates - make significant ones larger, no shapes
      geom_point(aes(size = is_significant), alpha = 0.9) +
      scale_size_manual(values = c("TRUE" = 4, "FALSE" = 3), guide = "none") +
      
      # Add text labels at fixed position - bold for significant
      geom_text(aes(label = value_label, x = max_x, fontface = font_face),
                hjust = 0,
                size = 5,
                show.legend = FALSE,
                color = "black") +
      
      scale_color_manual(values = color_palette, name = "Race/Ethnicity") +
      scale_x_continuous(expand = expansion(mult = c(0.05, 0.35))) +
      
      labs(
        title = out,
        x = NULL,
        y = NULL
      ) +
      
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),           # Increased from 10.5
        axis.title.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        panel.grid.major.x = element_line(color = "gray95", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      
      coord_cartesian(clip = "off")
    
    return(p)
  })
  
  # Remove y-axis text from right column and adjust margins for alignment
  for(i in seq_along(plot_list)) {
    if(i %% 2 == 0) {  # Right column
      plot_list[[i]] <- plot_list[[i]] + 
        theme(axis.text.y = element_blank(),
              plot.margin = margin(12, 10, 10, 10))  # Add 2mm top margin to right plots
    }
  }
  
  # Create layout matrix for proper alignment
  n_plots <- length(plot_list)
  n_rows <- ceiling(n_plots / 2)
  
  # Combine plots - no legend, x-axis label centered at bottom
  final_plot <- grid.arrange(
    grobs = plot_list,
    ncol = 2,
    heights = rep(1, n_rows),
    top = textGrob(
      paste("Prevalence Changes with 95% Confidence Intervals", 
            if(!is.null(age_group_filter)) paste("- Ages", age_group_filter) else ""),
      gp = gpar(fontsize = 18, fontface = "bold")
    ),
    bottom = textGrob(
      paste("Change in Prevalence (percentage points)\n", "Bold values indicate 95% CI excludes 0"),
      gp = gpar(fontsize = c(16, 12), fontface = c("plain", "italic"))
    ),
    left = textGrob("Sex | Race/Ethnicity | Time Period", rot = 90, 
                    gp = gpar(fontsize = 16))
  )
  
  return(final_plot)
}


# Generate the plots with bold significant results
rr_forest_50_64 <- create_risk_ratio_forest_plot_fixed(trends_hispanic, "50-64")
rr_forest_65_74 <- create_risk_ratio_forest_plot_fixed(trends_hispanic, "65-74") 
rr_forest_75_84 <- create_risk_ratio_forest_plot_fixed(trends_hispanic, "75-84")

prev_change_50_64 <- create_prevalence_change_plot_fixed(trends_hispanic, "50-64")
prev_change_65_74 <- create_prevalence_change_plot_fixed(trends_hispanic, "65-74")
prev_change_75_84 <- create_prevalence_change_plot_fixed(trends_hispanic, "75-84")


# # 
# # # Save plots
#  ggsave("rr_forest_50_64plot2.png", rr_forest_50_64, width = 16, height = 10, dpi = 300)
#  ggsave("rr_forest_65_74plot3.png", rr_forest_65_74, width = 16, height = 10, dpi = 300)
#  ggsave("rr_forest_75_84plot4.png", rr_forest_75_84, width = 16, height = 10, dpi = 300)
# 
# ggsave("prev_change_50_64.png", prev_change_50_64, width = 14, height = 10, dpi = 300)
# ggsave("prev_change_65_74.png", prev_change_65_74, width = 14, height = 10, dpi = 300)
# ggsave("prev_change_75_84.png", prev_change_75_84, width = 14, height = 10, dpi = 300)



#### do sex- specific prevalence plots 

create_prevalence_plot_by_sex <- function(pred_data, sex_filter, age_group_filter = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  
  # Filter by sex
  pred_data <- pred_data %>% filter(str_detect(model, sex_filter))
  
  # Optional age group filter
  if (!is.null(age_group_filter)) {
    pred_data <- pred_data %>% filter(str_detect(model, age_group_filter))
  }
  
  # Pivot to long
  preds_long <- pred_data %>%
    pivot_longer(
      cols = starts_with("pred_"),
      names_to = "year",
      names_prefix = "pred_",
      values_to = "prevalence"
    ) %>%
    filter(year %in% c("2000", "2010", "2018"))
  
  ses_long <- pred_data %>%
    pivot_longer(
      cols = starts_with("se_"),
      names_to = "year",
      names_prefix = "se_",
      values_to = "se"
    ) %>%
    filter(year %in% c("2000", "2010", "2018"))
  
  plot_data <- preds_long %>%
    left_join(ses_long, by = c("model", "outcome", "plot_group", "year")) %>%
    mutate(
      year = factor(year, levels = c("2000", "2010", "2018")),
      age_group = str_extract(model, "\\d{2}-\\d{2}"),
      lower_ci = pmax(0, prevalence - 1.96 * se),
      upper_ci = pmin(100, prevalence + 1.96 * se)
    ) %>%
    filter(!is.na(prevalence), !is.na(se), is.finite(prevalence), is.finite(se))
  
  # Create ordering factors
  plot_data <- plot_data %>%
    mutate(
      # Order for age groups
      age_order = factor(age_group, levels = c("50-64", "65-74", "75-84")),
      # Order for race/ethnicity groups
      race_order = factor(plot_group, 
                          levels = c("NH-White", "NH-Black", 
                                     "Hispanic US Born", "Hispanic Foreign Born")),
      # Create a combined label for y-axis: Age Group | Race/Ethnicity | Year
      group_label = paste(age_group, plot_group, year, sep = " | ")
    ) %>%
    # Sort by age, race, then year
    arrange(age_order, race_order, year) %>%
    mutate(
      group_label = factor(group_label, levels = unique(group_label)),
      # Create label text with prevalence and CI on single line
      value_label = sprintf("%.2f (%.2f-%.2f)", prevalence, lower_ci, upper_ci)
    )
  
  # Calculate max x value for fixed text position BY OUTCOME
  max_x_by_outcome <- plot_data %>%
    group_by(outcome) %>%
    summarise(max_x = max(upper_ci, na.rm = TRUE) * 1.01, .groups = "drop")
  
  # Join back to plot_data
  plot_data <- plot_data %>%
    left_join(max_x_by_outcome, by = "outcome")
  
  # Define colors and shapes for years
  year_colors <- c("2000" = "#1f77b4", "2010" = "#ff7f0e", "2018" = "#2ca02c")
  year_shapes <- c("2000" = 16, "2010" = 17, "2018" = 15)
  
  # Create the plot - IMPORTANT: layer order matters!
  # geom_text should come AFTER other geoms to be on top
  p <- ggplot(plot_data, aes(x = prevalence, y = group_label, 
                             color = year, shape = year)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), 
                   height = 0.25, linewidth = 0.7, alpha = 0.7) +
    geom_point(size = 3, alpha = 0.9) +
    # Text should be last to appear on top - now using max_x per outcome
    geom_text(aes(label = value_label, x = max_x),  # Use calculated max_x
              hjust = 0,
              size = 4.5,
              show.legend = FALSE, 
              color = "black",
              fontface = "plain") +
    scale_color_manual(values = year_colors, name = "Year") +
    scale_shape_manual(values = year_shapes, name = "Year") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.35))) +  # Extra space on right
    facet_wrap(~ outcome, scales = "free_x", ncol = 2) +
    labs(
      title = paste("Predicted ADL and IADL Prevalence -", sex_filter),
      x = "Predicted Prevalence (%)",
      y = "Age Group | Race/Ethnicity | Year"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 16, hjust = 1),
      axis.text.x = element_text(size = 16, hjust = 1),
      strip.text = element_text(size = 16, face = "bold"),
      panel.spacing = unit(1, "lines"),
      # Make grid lines more subtle
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.major.x = element_line(color = "gray95", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    coord_cartesian(clip = "off")  # Allow text outside plot area
  
  return(p)
}

# Create separate plots for males and females
prev_females <- create_prevalence_plot_by_sex(trends_hispanic, "Female")
prev_males <- create_prevalence_plot_by_sex(trends_hispanic, "Male")

# Display plots
print(prev_females)
print(prev_males)

### extra code for tests
#### extra code for Power analysis, multicollinearity

library(dplyr)
library(broom)
library(car)
library(pwr)
library(kableExtra)



# ============================================================================
# 1. MULTICOLLINEARITY (VIF) - Already done but let's compile results
# ============================================================================


car::vif(model1_svy )
car::vif(model2_svy )
car::vif(model3_svy )
car::vif(model4_svy )
car::vif(model5_svy )
car::vif(model6_svy )
car::vif(model7_svy )
car::vif(model8_svy )
car::vif(model9_svy )
car::vif(model10_svy )
car::vif(model11_svy )
car::vif(model12_svy )


# Extract VIF for all models
vif_results <- data.frame(
  Model = c("Male 50-64 ADL", "Male 65-74 ADL", "Male 75-84 ADL",
            "Female 50-64 ADL", "Female 65-74 ADL", "Female 75-84 ADL",
            "Male 50-64 IADL", "Male 65-74 IADL", "Male 75-84 IADL",
            "Female 50-64 IADL", "Female 65-74 IADL", "Female 75-84 IADL")
)

# Get VIF for each model
all_models <- list(model1_svy, model2_svy, model3_svy, model4_svy, 
                   model5_svy, model6_svy, model7_svy, model8_svy,
                   model9_svy, model10_svy, model11_svy, model12_svy)

# Function to get max VIF from a model
get_max_vif <- function(model) {
  vif_vals <- car::vif(model)
  # Handle GVIF for categorical variables
  if(is.matrix(vif_vals)) {
    max_vif <- max(vif_vals[, "GVIF^(1/(2*Df))"])
  } else {
    max_vif <- max(vif_vals)
  }
  return(round(max_vif, 2))
}

vif_results$Max_VIF <- sapply(all_models, get_max_vif)

kable(vif_results)



# ============================================================================
# 2. POWER ANALYSIS - For detecting changes in prevalence
# ============================================================================


# Extract sample sizes and prevalences from descriptive table
power_analysis_data <- data.frame(
  Group = c("NH-White 50-64", "NH-Black 50-64", "Hispanic US Born 50-64", 
            "Hispanic Foreign Born 50-64", "NH-White 65-74", "NH-Black 65-74",
            "Hispanic US Born 65-74", "Hispanic Foreign Born 65-74",
            "NH-White 75-84", "NH-Black 75-84", "Hispanic US Born 75-84",
            "Hispanic Foreign Born 75-84"),
  
  # ADL prevalences
  ADL_2000_2009 = c(1.51, 3.26, 2.73, 1.50, 2.85, 5.95, 5.58, 4.31, 6.42, 12.27, 12.10, 11.51),
  ADL_2010_2018 = c(1.93, 3.86, 3.75, 1.58, 2.99, 6.50, 5.74, 4.79, 6.43, 13.18, 12.02, 13.58),
  
  # IADL prevalences
  IADL_2000_2009 = c(3.42, 6.53, 5.63, 2.57, 5.83, 11.10, 9.96, 7.09, 13.61, 23.38, 20.91, 17.24),
  IADL_2010_2018 = c(3.95, 6.99, 6.61, 2.82, 5.50, 10.56, 10.56, 7.89, 12.09, 20.50, 18.28, 18.60),
  
  # Sample sizes
  n_2000_2009 = c(96347, 18738, 8064, 12665, 38764, 6655, 2941, 3852, 26397, 3520, 1521, 1868),
  n_2010_2018 = c(109240, 21806, 8473, 14468, 52115, 7980, 2964, 4028, 27677, 3757, 1439, 1892)
)

# Function to calculate power for two proportions test
calculate_power_proportions <- function(p1, p2, n1, n2, alpha = 0.05) {
  # Convert percentages to proportions
  p1 <- p1 / 100
  p2 <- p2 / 100
  
  # Effect size (h)
  h <- 2 * (asin(sqrt(p2)) - asin(sqrt(p1)))
  
  # Use pwr.2p2n.test for unequal sample sizes
  result <- pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = alpha, 
                          alternative = "two.sided")
  
  return(result$power)
}

# Calculate power for ADL
power_analysis_data <- power_analysis_data %>%
  mutate(
    Power_ADL = mapply(calculate_power_proportions, 
                       ADL_2000_2009, ADL_2010_2018, 
                       n_2000_2009, n_2010_2018),
    Power_IADL = mapply(calculate_power_proportions,
                        IADL_2000_2009, IADL_2010_2018,
                        n_2000_2009, n_2010_2018),
    Power_ADL_pct = round(Power_ADL * 100, 1),
    Power_IADL_pct = round(Power_IADL * 100, 1)
  )

# Display power analysis results
power_summary <- power_analysis_data %>%
  select(Group, 
         `ADL 2000-09` = ADL_2000_2009,
         `ADL 2010-18` = ADL_2010_2018,
         `ADL Power (%)` = Power_ADL_pct,
         `IADL 2000-09` = IADL_2000_2009,
         `IADL 2010-18` = IADL_2010_2018,
         `IADL Power (%)` = Power_IADL_pct)

print(power_summary)

# Highlight high and low power cases
cat("\n\n========== HIGH POWER CASES (>80%) ==========\n\n")
high_power_adl <- power_analysis_data %>%
  filter(Power_ADL_pct > 80) %>%
  select(Group, ADL_2000_2009, ADL_2010_2018, n_2000_2009, n_2010_2018, Power_ADL_pct) %>%
  arrange(desc(Power_ADL_pct))

print(high_power_adl)

cat("\n\n========== LOW POWER CASES (<50%) ==========\n\n")
low_power_adl <- power_analysis_data %>%
  filter(Power_ADL_pct < 50) %>%
  select(Group, ADL_2000_2009, ADL_2010_2018, n_2000_2009, n_2010_2018, Power_ADL_pct) %>%
  arrange(Power_ADL_pct)

print(low_power_adl)


# 3. GOODNESS OF FIT
# r square of all models  

# Method 1: McFadden's Pseudo R-squared (most common for logistic regression)
mcfadden_r2 <- function(svy_model) {
  # Get log-likelihood of full model
  full_loglik <- logLik(svy_model)
  
  # Create null model (intercept only)
  null_formula <- update(formula(svy_model), . ~ 1)
  null_model <- svyglm(null_formula, design = svy_model$survey.design, family = binomial)
  null_loglik <- logLik(null_model)
  
  # Calculate McFadden's R-squared
  r2 <- 1 - (full_loglik / null_loglik)
  return(as.numeric(r2))
}

# Method 2: Nagelkerke R-squared (normalized version)
nagelkerke_r2 <- function(svy_model) {
  full_loglik <- logLik(svy_model)
  null_formula <- update(formula(svy_model), . ~ 1)
  null_model <- svyglm(null_formula, design = svy_model$survey.design, family = binomial)
  null_loglik <- logLik(null_model)
  
  n <- nrow(svy_model$survey.design$variables)
  
  # Cox & Snell R-squared
  cox_snell <- 1 - exp((2/n) * (null_loglik - full_loglik))
  
  # Nagelkerke R-squared (normalized)
  max_r2 <- 1 - exp((2/n) * null_loglik)
  nagelkerke <- cox_snell / max_r2
  
  return(as.numeric(nagelkerke))
}

# Method 3: Simple approach using deviance
simple_pseudo_r2 <- function(svy_model) {
  # Get residual and null deviance
  res_dev <- svy_model$deviance
  null_dev <- svy_model$null.deviance
  
  # Pseudo R-squared
  r2 <- 1 - (res_dev / null_dev)
  return(r2)
}

# Method 4: Calculate for all 12 models
calculate_r2_all_models <- function(model_list, model_names) {
  results <- data.frame(
    model = model_names,
    mcfadden_r2 = numeric(length(model_list)),
    nagelkerke_r2 = numeric(length(model_list)),
    simple_pseudo_r2 = numeric(length(model_list)),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:length(model_list)) {
    cat("Calculating R-squared for model", i, ":", model_names[i], "\n")
    
    try({
      results$mcfadden_r2[i] <- mcfadden_r2(model_list[[i]])
    }, silent = TRUE)
    
    try({
      results$nagelkerke_r2[i] <- nagelkerke_r2(model_list[[i]])
    }, silent = TRUE)
    
    try({
      results$simple_pseudo_r2[i] <- simple_pseudo_r2(model_list[[i]])
    }, silent = TRUE)
  }
  
  return(results)
}

# Apply to your 12 survey models
model_names <- c("Male 50-64 ADL", "Male 65-74 ADL", "Male 75-84 ADL", 
                 "Female 50-64 ADL", "Female 65-74 ADL", "Female 75-84 ADL",
                 "Male 50-64 IADL", "Male 65-74 IADL", "Male 75-84 IADL",
                 "Female 50-64 IADL", "Female 65-74 IADL", "Female 75-84 IADL")

svy_models <- list(model1_svy, model2_svy, model3_svy, model4_svy, model5_svy, model6_svy,
                   model7_svy, model8_svy, model9_svy, model10_svy, model11_svy, model12_svy)

# Calculate R-squared values
r2_results <- calculate_r2_all_models(svy_models, model_names)

print("R-squared values for all survey models:")
print(r2_results)

# Summary statistics
cat("\nSummary of model fit across all models:\n")
cat("McFadden R-squared range:", round(min(r2_results$mcfadden_r2, na.rm = TRUE), 4), 
    "to", round(max(r2_results$mcfadden_r2, na.rm = TRUE), 4), "\n")
cat("Average McFadden R-squared:", round(mean(r2_results$mcfadden_r2, na.rm = TRUE), 4), "\n")
# 
# # Create visualization of model fit




#### extra code for FDR correction

library(dplyr)
library(broom)
library(purrr)

# Extract all model results with original p-values
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

# Extract results from all models
extract_model_results <- function(model, model_name) {
  broom::tidy(model) %>%
    mutate(
      model = model_name,
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3),
      p.value.original = p.value
    ) %>%
    select(model, term, estimate, std.error, statistic, p.value.original)
}

# Combine all results
all_results <- map2_dfr(
  all_models, 
  names(all_models), 
  extract_model_results
)

# Apply FDR correction (Benjamini-Hochberg)
all_results <- all_results %>%
  mutate(
    p.value.fdr = p.adjust(p.value.original, method = "BH"),
    # Original significance
    sig_original = case_when(
      p.value.original < 0.001 ~ "***",
      p.value.original < 0.01 ~ "**",
      p.value.original < 0.05 ~ "*",
      p.value.original < 0.10 ~ "†",
      TRUE ~ ""
    ),
    # FDR-corrected significance
    sig_fdr = case_when(
      p.value.fdr < 0.001 ~ "***",
      p.value.fdr < 0.01 ~ "**",
      p.value.fdr < 0.05 ~ "*",
      p.value.fdr < 0.10 ~ "†",
      TRUE ~ ""
    ),
    # Flag if significance changed
    sig_changed = (sig_original != "" & sig_fdr == ""),
    # Round p-values
    p.value.original = round(p.value.original, 4),
    p.value.fdr = round(p.value.fdr, 4)
  )

# Summary statistics for FDR correction

# Total coefficients
total_coefs <- nrow(all_results)
cat("Total coefficients tested:", total_coefs, "\n\n")

# Originally significant at different levels
orig_sig_001 <- sum(all_results$p.value.original < 0.001)
orig_sig_01 <- sum(all_results$p.value.original < 0.01)
orig_sig_05 <- sum(all_results$p.value.original < 0.05)
orig_sig_10 <- sum(all_results$p.value.original < 0.10)

cat("Originally significant coefficients:\n")
cat("  p < 0.001:", orig_sig_001, "\n")
cat("  p < 0.01:", orig_sig_01, "\n")
cat("  p < 0.05:", orig_sig_05, "\n")
cat("  p < 0.10:", orig_sig_10, "\n\n")

# After FDR correction
fdr_sig_001 <- sum(all_results$p.value.fdr < 0.001)
fdr_sig_01 <- sum(all_results$p.value.fdr < 0.01)
fdr_sig_05 <- sum(all_results$p.value.fdr < 0.05)
fdr_sig_10 <- sum(all_results$p.value.fdr < 0.10)

cat("After FDR correction (Benjamini-Hochberg):\n")
cat("  FDR < 0.001:", fdr_sig_001, "\n")
cat("  FDR < 0.01:", fdr_sig_01, "\n")
cat("  FDR < 0.05:", fdr_sig_05, "\n")
cat("  FDR < 0.10:", fdr_sig_10, "\n\n")

# Coefficients that lost significance
lost_sig <- all_results %>% 
  filter(sig_changed) %>%
  arrange(model, term)

cat("Coefficients that LOST significance after FDR correction:\n")
cat("  Total:", nrow(lost_sig), "\n\n")

# Show which ones lost significance
if (nrow(lost_sig) > 0) {
  cat("========== COEFFICIENTS LOSING SIGNIFICANCE ==========\n\n")
  
  lost_sig_display <- lost_sig %>%
    select(Model = model, Term = term, Estimate = estimate, 
           `P-value (original)` = p.value.original, 
           `Sig. (original)` = sig_original,
           `P-value (FDR)` = p.value.fdr,
           `Sig. (FDR)` = sig_fdr)
  
  print(lost_sig_display, n = Inf)
}

# Show coefficients by category
cat("\n\n========== BREAKDOWN BY COEFFICIENT TYPE ==========\n\n")

# Categorize coefficients
all_results_categorized <- all_results %>%
  mutate(
    coef_type = case_when(
      term == "(Intercept)" ~ "Intercept",
      grepl("^year_centered$|^year_after_2010$", term) ~ "Time trends",
      grepl("RaceEthnicity2", term) & !grepl(":", term) ~ "Race/ethnicity (main)",
      grepl("Educ", term) ~ "Education",
      term == "age" ~ "Age",
      grepl(":", term) ~ "Interactions",
      TRUE ~ "Other"
    )
  )

# Summary by coefficient type
coef_summary <- all_results_categorized %>%
  group_by(coef_type) %>%
  summarise(
    Total = n(),
    `Orig sig (p<0.05)` = sum(p.value.original < 0.05),
    `FDR sig (q<0.05)` = sum(p.value.fdr < 0.05),
    `Lost sig` = sum(sig_changed),
    .groups = 'drop'
  )

print(coef_summary)

# Detailed table of all results with FDR
cat("\n\n========== SAVING DETAILED RESULTS ==========\n\n")

# Create comprehensive table
results_table <- all_results %>%
  select(
    Model = model,
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `Z-statistic` = statistic,
    `P-value` = p.value.original,
    `P-value (FDR)` = p.value.fdr,
    `Sig. (original)` = sig_original,
    `Sig. (FDR)` = sig_fdr
  )


# Identify coefficients that lost significance
lost_sig_detailed <- all_results %>%
  filter(sig_changed) %>%  # Only those that lost significance
  arrange(model, term) %>%
  select(
    Model = model,
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `Z-statistic` = statistic,
    `P-value (original)` = p.value.original,
    `Original Sig.` = sig_original,
    `P-value (FDR)` = p.value.fdr,
    `FDR Sig.` = sig_fdr
  )

# Print the table
print(lost_sig_detailed, n = Inf)

# Create a kable formatted table
library(kableExtra)

lost_sig_table <- lost_sig_detailed %>%
  kbl(
    caption = "Coefficients that Lost Significance After FDR Correction",
    booktabs = TRUE,
    align = c("l", "l", "r", "r", "r", "r", "c", "r", "c")
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    full_width = FALSE,
    font_size = 9
  ) %>%
  row_spec(0, bold = TRUE) %>%
  pack_rows(index = table(lost_sig_detailed$Model)) %>%
  footnote(
    general = "These coefficients were significant before FDR correction but not after. FDR correction using Benjamini-Hochberg method.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

print(lost_sig_table)
save_kable(lost_sig_table, "lost_significance_fdr.html")
