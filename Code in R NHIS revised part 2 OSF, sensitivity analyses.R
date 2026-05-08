
library(haven)
library(tidyverse)
library(survey)

# 1. LOAD RAW DATA
# ================

NHIS_raw <- read_dta("workingdirectorynhis_00006.dta.gz")

cat("Step 1: Initial sample size:", nrow(NHIS_raw), "\n\n")

# 2. BASIC FILTERING AND VARIABLE CREATION


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
      hispyn == 2 ~ "Hispanic"
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
cat("Removed:", nrow(NHIS_raw) - nrow(NHIS), "observations\n\n")

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

print(t(missing_check))


nativity_by_race <- NHIS %>%
  group_by(RaceEthnicity) %>%
  summarise(
    total = n(),
    missing_nativity = sum(is.na(nativity_proper)),
    pct_missing = round(100 * missing_nativity / total, 2)
  )
print(nativity_by_race)


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


# 8. CREATE BINOMIAL VALUES FOR SENSITIVITY ANALYSIS OF DIFFICULTY WALKING AND MEMORY PROBLEMS
# ===============================

table(NHIS$lawalk)

NHIS_complete <- NHIS_complete %>% 
  mutate(
    lamemry = case_when(
      lamemry == 1 ~ 0L,  # No difficulty = 0
      lamemry == 2 ~ 1L,  # Difficulty = 1
      TRUE ~ 2L           # Other values = 2 (filtered out)
    ),
    
    lawalkbin = case_when(
      lawalk == 1 ~ 0L,   # No difficulty = 0
      lawalk == 2 ~ 1L,   # Difficulty = 1
      TRUE ~ 2L           # Other values = 2 (filtered out)
    )
  ) %>% 
  filter(lawalkbin < 2 & lamemry < 2)



# 9. CREATE SURVEY DESIGN OBJECT
# ===============================
nhis_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~perweight,
  data = NHIS_complete,
  nest = TRUE
)
nhis_design2 <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~perweight,
  data = NHIS_complete,
  nest = TRUE
)


#now use the design with psu and strata

model13_svy <- svyglm(
  lawalkbin ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Male" & age_category == "50-64"))


model14_svy <- svyglm(
  lawalkbin ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Male" & age_category == "65-74"))

model15_svy <- svyglm(
  lawalkbin ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Male" & age_category == "75-84"))

model16_svy <- svyglm(
  lawalkbin ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Female" & age_category == "50-64"))

model17_svy <- svyglm(
  lawalkbin ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Female" & age_category == "65-74"))

model18_svy <- svyglm(
  lawalkbin ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Female" & age_category == "75-84"))

model19_svy <- svyglm(
  lamemry ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Male" & age_category == "50-64"))

model20_svy <- svyglm(
  lamemry~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Male" & age_category == "65-74"))

model21_svy <- svyglm(
  lamemry ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Male" & age_category == "75-84"))

model22_svy <- svyglm(
  lamemry  ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Female" & age_category == "50-64"))

model23_svy <- svyglm(
  lamemry ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Female" & age_category == "65-74"))

model24_svy <- svyglm(
  lamemry ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded + nativity_proper,
  family = binomial,
  design = subset(nhis_design2, Sex == "Female" & age_category == "75-84"))



# summary of all models 
summary(model13_svy )
summary(model14_svy )
summary(model15_svy )
summary(model16_svy )
summary(model17_svy )
summary(model18_svy )
summary(model19_svy )
summary(model20_svy )
summary(model21_svy )
summary(model22_svy )
summary(model23_svy )
summary(model24_svy )


# 
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
model_names2 <- c("Male 50-64 WALK", "Male 65-74 WALK", "Male 75-84 WALK", 
                 "Female 50-64 WALK", "Female 65-74 WALK", "Female 75-84 WALK",
                 "Male 50-64 MEMORY", "Male 65-74 MEMORY", "Male 75-84 MEMORY",
                 "Female 50-64 MEMORY", "Female 65-74 MEMORY", "Female 75-84 MEMORY")

svy_models2 <- list(model13_svy, model14_svy, model15_svy, model16_svy, model17_svy, model18_svy,
                   model19_svy, model20_svy, model21_svy, model22_svy, model23_svy, model24_svy)

# Calculate R-squared values
r2_results2 <- calculate_r2_all_models(svy_models2, model_names2)

print(r2_results2)

# 
# # Create visualization of model fit for figures 4S to 6S


# Fixed prediction function with proper column references
calculate_predictions_with_hispanic_nativity <- function(svy_model, model_name, age_val, outcome_type = "ADL") {
  
  # Create prediction data
  new_data <- expand.grid(
    year = 2000:2018,
    RaceEthnicity2= c("NH-White", "NH-Black", "Hispanic US Born", "Hispanic Foreign Born")
  ) %>%
    mutate(
      age = age_val,
      EducRecoded = "GED to Some College",
      year_centered = year - 2010,
      year_after_2010 = pmax(0, year - 2010),
      # Add nativity_proper based on RaceEthnicity2
      nativity_proper = case_when(
        RaceEthnicity2== "Hispanic US Born" ~ "US Born",
        RaceEthnicity2== "Hispanic Foreign Born" ~ "Foreign Born",
        RaceEthnicity2== "NH-White" ~ "US Born",  # Assuming NH-White are US born
        RaceEthnicity2== "NH-Black" ~ "US Born"   # Assuming NH-Black are US born
      ),
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
    if(row_data$RaceEthnicity2== "NH-Black") {
      X_new[i, "RaceEthnicity2NH-Black"] <- 1
    }
    if(row_data$RaceEthnicity2== "Hispanic US Born") {
      X_new[i, "RaceEthnicity2Hispanic US Born"] <- 1
    }
    if(row_data$RaceEthnicity2== "Hispanic Foreign Born") {
      X_new[i, "RaceEthnicity2Hispanic Foreign Born"] <- 1
    }
    
    # Education (reference category gets 0 for both)
    X_new[i, "EducRecodedGED to Some College"] <- 1
    
    # Nativity (Foreign Born is reference, so US Born gets 1)
    if(row_data$nativity_proper == "US Born") {
      X_new[i, "nativity_properUS Born"] <- 1
    }
    
    # Race-year interactions
    if(row_data$RaceEthnicity2== "NH-Black") {
      X_new[i, "year_centered:RaceEthnicity2NH-Black"] <- row_data$year_centered
      X_new[i, "year_after_2010:RaceEthnicity2NH-Black"] <- row_data$year_after_2010
    }
    
    if(row_data$RaceEthnicity2== "Hispanic US Born") {
      X_new[i, "year_centered:RaceEthnicity2Hispanic US Born"] <- row_data$year_centered
      X_new[i, "year_after_2010:RaceEthnicity2Hispanic US Born"] <- row_data$year_after_2010
    }
    
    if(row_data$RaceEthnicity2== "Hispanic Foreign Born") {
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
pred13_hispanic <- calculate_predictions_with_hispanic_nativity(model13_svy, "Male 50-64", 57, "WALK")
pred14_hispanic <- calculate_predictions_with_hispanic_nativity(model14_svy, "Male 65-74", 69, "WALK") 
pred15_hispanic <- calculate_predictions_with_hispanic_nativity(model15_svy, "Male 75-84", 79, "WALK")
pred16_hispanic <- calculate_predictions_with_hispanic_nativity(model16_svy, "Female 50-64", 57, "WALK")
pred17_hispanic <- calculate_predictions_with_hispanic_nativity(model17_svy, "Female 65-74", 69, "WALK")
pred18_hispanic <- calculate_predictions_with_hispanic_nativity(model18_svy, "Female 75-84", 79, "WALK")

pred19_hispanic <- calculate_predictions_with_hispanic_nativity(model19_svy, "Male 50-64", 57, "MEMORY")
pred20_hispanic <- calculate_predictions_with_hispanic_nativity(model20_svy, "Male 65-74", 69, "MEMORY")
pred21_hispanic <- calculate_predictions_with_hispanic_nativity(model21_svy, "Male 75-84", 79, "MEMORY")
pred22_hispanic <- calculate_predictions_with_hispanic_nativity(model22_svy, "Female 50-64", 57, "MEMORY")
pred23_hispanic <- calculate_predictions_with_hispanic_nativity(model23_svy, "Female 65-74", 69, "MEMORY")
pred24_hispanic <- calculate_predictions_with_hispanic_nativity(model24_svy, "Female 75-84", 79, "MEMORY")


car::vif(model13_svy)

# Combine all predictions
all_preds_hispanic <- bind_rows(
  pred13_hispanic, pred14_hispanic, pred15_hispanic, pred16_hispanic, pred17_hispanic, pred18_hispanic,
  pred19_hispanic, pred20_hispanic, pred21_hispanic, pred22_hispanic, pred23_hispanic, pred24_hispanic
)

# Simple plotting function



create_age_group_plot_hispanic <- function(age_group_name) {
  
  data_subset <- all_preds_hispanic %>% 
    filter(grepl(age_group_name, model)) %>%
    mutate(
      age_group = age_group_name,
      sex = ifelse(grepl("Male", model), "Male", "Female"),
      # Reorder outcomes so WALK appears above MEMORY
      outcome = factor(outcome, levels = c("WALK", "MEMORY"))
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
    labs(title = paste("Reported difficulty in walking without help and in doing activities due to memory problems by year: Ages", age_group_name, "(2000–2018)"),
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
      plot.title = element_text(size = 14),         # Plot title
      strip.text = element_text(size = 14)          # Facet labels
    ) +
    geom_vline(xintercept = 2010, linetype = "dashed", color = "gray50", alpha = 0.8, linewidth = 0.9)
}
# Generate the three plots
plot_50_64_hispanicWM <- create_age_group_plot_hispanic("50-64")
plot_65_74_hispanicWM <- create_age_group_plot_hispanic("65-74") 
plot_75_84_hispanicWM <- create_age_group_plot_hispanic("75-84")

# Display plots
print(plot_50_64_hispanicWM)
print(plot_65_74_hispanicWM)
print(plot_75_84_hispanicWM)



### repeat with removing education (models 25 to 37, figures 7S to 9S)


model25_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 ,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "50-64"))


model26_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "65-74"))

model27_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "75-84"))

model28_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "50-64"))

model29_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "65-74"))

model30_svy <- svyglm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "75-84"))

model31_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "50-64"))

model32_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "65-74"))

model33_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Male" & age_category == "75-84"))

model34_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "50-64"))

model35_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "65-74"))

model36_svy <- svyglm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2+ age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010,
  family = binomial,
  design = subset(nhis_design, Sex == "Female" & age_category == "75-84"))



# summary of all models 
summary(model25_svy )
summary(model26_svy )
summary(model27_svy )
summary(model28_svy )
summary(model29_svy )
summary(model30_svy )
summary(model31_svy )
summary(model32_svy )
summary(model33_svy )
summary(model34_svy )
summary(model35_svy )
summary(model36_svy )
# 
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

svy_models3 <- list(model25_svy, model26_svy, model27_svy, model28_svy, model29_svy, model30_svy,
                   model31_svy, model32_svy, model33_svy, model34_svy, model35_svy, model36_svy)

# Calculate R-squared values
r2_results3 <- calculate_r2_all_models(svy_models3, model_names)

print("R-squared values for all survey models:")
print(r2_results3)


# Fixed prediction function with proper column references
calculate_predictions_with_hispanic_nativity <- function(svy_model, model_name, age_val, outcome_type = "ADL") {
  
  # Create prediction data
  new_data <- expand.grid(
    year = 2000:2018,
    RaceEthnicity2= c("NH-White", "NH-Black", "Hispanic US Born", "Hispanic Foreign Born")
  ) %>%
    mutate(
      age = age_val,
      EducRecoded = "GED to Some College",
      year_centered = year - 2010,
      year_after_2010 = pmax(0, year - 2010),
      # Add nativity_proper based on RaceEthnicity2
      nativity_proper = case_when(
        RaceEthnicity2== "Hispanic US Born" ~ "US Born",
        RaceEthnicity2== "Hispanic Foreign Born" ~ "Foreign Born",
        RaceEthnicity2== "NH-White" ~ "US Born",  # Assuming NH-White are US born
        RaceEthnicity2== "NH-Black" ~ "US Born"   # Assuming NH-Black are US born
      ),
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
    if(row_data$RaceEthnicity2== "NH-Black") {
      X_new[i, "RaceEthnicity2NH-Black"] <- 1
    }
    if(row_data$RaceEthnicity2== "Hispanic US Born") {
      X_new[i, "RaceEthnicity2Hispanic US Born"] <- 1
    }
    if(row_data$RaceEthnicity2== "Hispanic Foreign Born") {
      X_new[i, "RaceEthnicity2Hispanic Foreign Born"] <- 1
    }
    # 
    # # Education (reference category gets 0 for both)
    # X_new[i, "EducRecodedGED to Some College"] <- 1
    # 
    # # Nativity (Foreign Born is reference, so US Born gets 1)
    # if(row_data$nativity_proper == "US Born") {
    #   X_new[i, "nativity_properUS Born"] <- 1
    # }
    
    # Race-year interactions
    if(row_data$RaceEthnicity2== "NH-Black") {
      X_new[i, "year_centered:RaceEthnicity2NH-Black"] <- row_data$year_centered
      X_new[i, "year_after_2010:RaceEthnicity2NH-Black"] <- row_data$year_after_2010
    }
    
    if(row_data$RaceEthnicity2== "Hispanic US Born") {
      X_new[i, "year_centered:RaceEthnicity2Hispanic US Born"] <- row_data$year_centered
      X_new[i, "year_after_2010:RaceEthnicity2Hispanic US Born"] <- row_data$year_after_2010
    }
    
    if(row_data$RaceEthnicity2== "Hispanic Foreign Born") {
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
pred25_hispanic <- calculate_predictions_with_hispanic_nativity(model25_svy, "Male 50-64", 57, "ADL")
pred26_hispanic <- calculate_predictions_with_hispanic_nativity(model26_svy, "Male 65-74", 69, "ADL") 
pred27_hispanic <- calculate_predictions_with_hispanic_nativity(model27_svy, "Male 75-84", 79, "ADL")
pred28_hispanic <- calculate_predictions_with_hispanic_nativity(model28_svy, "Female 50-64", 57, "ADL")
pred29_hispanic <- calculate_predictions_with_hispanic_nativity(model29_svy, "Female 65-74", 69, "ADL")
pred30_hispanic <- calculate_predictions_with_hispanic_nativity(model30_svy, "Female 75-84", 79, "ADL")

pred31_hispanic <- calculate_predictions_with_hispanic_nativity(model31_svy, "Male 50-64", 57, "IADL")
pred32_hispanic <- calculate_predictions_with_hispanic_nativity(model32_svy, "Male 65-74", 69, "IADL")
pred33_hispanic <- calculate_predictions_with_hispanic_nativity(model33_svy, "Male 75-84", 79, "IADL")
pred34_hispanic <- calculate_predictions_with_hispanic_nativity(model34_svy, "Female 50-64", 57, "IADL")
pred35_hispanic <- calculate_predictions_with_hispanic_nativity(model35_svy, "Female 65-74", 69, "IADL")
pred36_hispanic <- calculate_predictions_with_hispanic_nativity(model36_svy, "Female 75-84", 79, "IADL")

# Combine all predictions
all_preds_hispanic <- bind_rows(
  pred25_hispanic, pred26_hispanic, pred27_hispanic, pred28_hispanic, pred29_hispanic, pred30_hispanic,
  pred31_hispanic, pred32_hispanic, pred33_hispanic, pred34_hispanic, pred35_hispanic, pred36_hispanic
)

# Simple plotting function
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
    labs(title = paste("ADL/IADL prevalence by year (not controlled by education): Ages", age_group_name, "(2000-2018)"),
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
      plot.title = element_text(size = 14),         # Plot title
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

# Calculate comprehensive trends with confidence intervals
calculate_comprehensive_trends_hispanic <- function(pred_data) {
  
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
comprehensive_trends_hispanic <- calculate_comprehensive_trends_hispanic(all_preds_hispanic)

# Create summary tables with confidence intervals
point_estimates_hispanic <- comprehensive_trends_hispanic %>%
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


# Fixed forest plot function with proper ordering and color scheme
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
      y_factor = factor(group_comparison_ordered, levels = unique(group_comparison_ordered))
    )
  
  # Color palette by ethnicity (matching your Hispanic plots)
  color_palette <- c(
    "NH-White" = "blue",
    "NH-Black" = "red", 
    "Hispanic US Born" = "darkgreen",
    "Hispanic Foreign Born" = "orange"
  )
  
  # Shape palette for time comparisons
  shape_palette <- c(
    "2010 vs 2000" = 16,
    "2018 vs 2010" = 17, 
    "2018 vs 2000" = 18
  )
  
  # Create the plot
  p <- ggplot(forest_data, aes(y = y_factor, x = rr, 
                               color = plot_group,     # Color by ethnicity
                               shape = comparison)) +  # Shape by time comparison
    
    # Reference line at RR = 1
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", size = 0.8) +
    
    # Confidence intervals
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), 
                   height = 0.3, size = 0.8, alpha = 0.8) +
    
    # Point estimates
    geom_point(size = 3, alpha = 0.9) +
    
    # Faceting by outcome only
    facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
    
    # Styling
    scale_color_manual(values = color_palette, name = "Race/Ethnicity-Nativity") +
    scale_shape_manual(values = shape_palette, name = "Time Comparison") +
    scale_x_log10(breaks = c(0.3, 0.5, 0.7, 1.0, 1.4, 2.0, 3.0, 5.0), 
                  limits = c(0.3, 5.0)) +
    
    labs(
      title = paste("Risk Ratios with 95% Confidence Intervals", 
                    if(!is.null(age_group_filter)) paste("- Ages", age_group_filter) else ""),
      x = "Risk Ratio (log scale)",
      y = "Sex | Race/Ethnicity-Nativity | Time Comparison"
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 8),
      strip.text = element_text(size = 12, face = "bold")
    ) +
    
    guides(
      color = guide_legend(title = "Race/Ethnicity-Nativity", override.aes = list(size = 4)),
      shape = guide_legend(title = "Time Comparison", override.aes = list(size = 4))
    )
  
  return(p)
}

# Fixed prevalence change plot with same color scheme


# Generate the plots with corrected aesthetics
rr_forest_50_64 <- create_risk_ratio_forest_plot_fixed(comprehensive_trends_hispanic, "50-64")
rr_forest_65_74 <- create_risk_ratio_forest_plot_fixed(comprehensive_trends_hispanic, "65-74") 
rr_forest_75_84 <- create_risk_ratio_forest_plot_fixed(comprehensive_trends_hispanic, "75-84")

prev_change_50_64 <- create_prevalence_change_plot_fixed(comprehensive_trends_hispanic, "50-64")
prev_change_65_74 <- create_prevalence_change_plot_fixed(comprehensive_trends_hispanic, "65-74")
prev_change_75_84 <- create_prevalence_change_plot_fixed(comprehensive_trends_hispanic, "75-84")

# Display the plots
print(rr_forest_50_64)
print(rr_forest_65_74)
print(rr_forest_75_84)

print(prev_change_50_64)
print(prev_change_65_74)
print(prev_change_75_84)

#### removing sampling 

#now use the design without psu and strata
# Unweighted logistic regression models (37-49), figures S10 to S12
# Note: Using glm() instead of svyglm(), no survey design needed

model37 <- glm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Male" & age_category == "50-64"))

model38 <- glm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Male" & age_category == "65-74"))

model39 <- glm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Male" & age_category == "75-84"))

model40 <- glm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Female" & age_category == "50-64"))

model41 <- glm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Female" & age_category == "65-74"))

model42 <- glm(
  ladl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Female" & age_category == "75-84"))

model43 <- glm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Male" & age_category == "50-64"))

model44 <- glm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Male" & age_category == "65-74"))

model45 <- glm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Male" & age_category == "75-84"))

model46 <- glm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Female" & age_category == "50-64"))

model47 <- glm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Female" & age_category == "65-74"))

model48 <- glm(
  laiadl ~ year_centered + year_after_2010 + 
    RaceEthnicity2 + age + 
    RaceEthnicity2:year_centered + 
    RaceEthnicity2:year_after_2010 + 
    EducRecoded,
  family = binomial,
  data = subset(NHIS_complete, Sex == "Female" & age_category == "75-84"))

# Model summaries
summary(model37)
summary(model38)
summary(model39)
summary(model40)
summary(model41)
summary(model42)
summary(model43)
summary(model44)
summary(model45)
summary(model46)
summary(model47)
summary(model48)

# Odds ratios
exp(coef(model37))
exp(coef(model38))
exp(coef(model39))
exp(coef(model40))
exp(coef(model41))
exp(coef(model42))
exp(coef(model43))
exp(coef(model44))
exp(coef(model45))
exp(coef(model46))
exp(coef(model47))
exp(coef(model48))

# VIF checks
car::vif(model37)
car::vif(model38)
car::vif(model39)
car::vif(model40)
car::vif(model41)
car::vif(model42)
car::vif(model43)
car::vif(model44)
car::vif(model45)
car::vif(model46)
car::vif(model47)
car::vif(model48)

# R-squared functions for unweighted models
mcfadden_r2_unweighted <- function(model) {
  full_loglik <- logLik(model)
  null_model <- glm(update(formula(model), . ~ 1), 
                    data = model$data, 
                    family = binomial)
  null_loglik <- logLik(null_model)
  r2 <- 1 - (full_loglik / null_loglik)
  return(as.numeric(r2))
}

nagelkerke_r2_unweighted <- function(model) {
  full_loglik <- logLik(model)
  null_model <- glm(update(formula(model), . ~ 1), 
                    data = model$data, 
                    family = binomial)
  null_loglik <- logLik(null_model)
  
  n <- nrow(model$data)
  cox_snell <- 1 - exp((2/n) * (null_loglik - full_loglik))
  max_r2 <- 1 - exp((2/n) * null_loglik)
  nagelkerke <- cox_snell / max_r2
  
  return(as.numeric(nagelkerke))
}

# Calculate R-squared for all unweighted models
model_names_unweighted <- c("Male 50-64 ADL", "Male 65-74 ADL", "Male 75-84 ADL", 
                            "Female 50-64 ADL", "Female 65-74 ADL", "Female 75-84 ADL",
                            "Male 50-64 IADL", "Male 65-74 IADL", "Male 75-84 IADL",
                            "Female 50-64 IADL", "Female 65-74 IADL", "Female 75-84 IADL")

unweighted_models <- list(model37, model38, model39, model40, model41, model42,
                          model43, model44, model45, model46, model47, model48)

r2_results_unweighted <- data.frame(
  model = model_names_unweighted,
  mcfadden_r2 = sapply(unweighted_models, mcfadden_r2_unweighted),
  nagelkerke_r2 = sapply(unweighted_models, nagelkerke_r2_unweighted)
)

print("R-squared values for all unweighted models:")
print(r2_results_unweighted)

# Prediction function for unweighted models
calculate_predictions_unweighted <- function(model, model_name, age_val, outcome_type = "ADL") {
  
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
      plot_group = RaceEthnicity2
    )
  
  # Get predictions using predict() for standard glm
  preds <- predict(model, newdata = new_data, type = "link", se.fit = TRUE)
  
  # Convert to probability scale
  new_data$predicted <- plogis(preds$fit)
  new_data$se <- abs(new_data$predicted * (1 - new_data$predicted) * preds$se.fit)
  new_data$model <- model_name
  new_data$outcome <- outcome_type
  
  return(new_data)
}

# Generate predictions for all 12 unweighted models
pred37_unweighted <- calculate_predictions_unweighted(model37, "Male 50-64", 57, "ADL")
pred38_unweighted <- calculate_predictions_unweighted(model38, "Male 65-74", 69, "ADL")
pred39_unweighted <- calculate_predictions_unweighted(model39, "Male 75-84", 79, "ADL")
pred40_unweighted <- calculate_predictions_unweighted(model40, "Female 50-64", 57, "ADL")
pred41_unweighted <- calculate_predictions_unweighted(model41, "Female 65-74", 69, "ADL")
pred42_unweighted <- calculate_predictions_unweighted(model42, "Female 75-84", 79, "ADL")

pred43_unweighted <- calculate_predictions_unweighted(model43, "Male 50-64", 57, "IADL")
pred44_unweighted <- calculate_predictions_unweighted(model44, "Male 65-74", 69, "IADL")
pred45_unweighted <- calculate_predictions_unweighted(model45, "Male 75-84", 79, "IADL")
pred46_unweighted <- calculate_predictions_unweighted(model46, "Female 50-64", 57, "IADL")
pred47_unweighted <- calculate_predictions_unweighted(model47, "Female 65-74", 69, "IADL")
pred48_unweighted <- calculate_predictions_unweighted(model48, "Female 75-84", 79, "IADL")

# Combine all predictions
all_preds_unweighted <- bind_rows(
  pred37_unweighted, pred38_unweighted, pred39_unweighted, 
  pred40_unweighted, pred41_unweighted, pred42_unweighted,
  pred43_unweighted, pred44_unweighted, pred45_unweighted, 
  pred46_unweighted, pred47_unweighted, pred48_unweighted
)

# Plotting function for unweighted models
create_age_group_plot_unweighted <- function(age_group_name) {
  
  data_subset <- all_preds_unweighted %>% 
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
    labs(title = paste("ADL/IADL prevalence by year (Unweighted): Ages", age_group_name, "(2000-2018)"),
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
plot_50_64_unweighted <- create_age_group_plot_unweighted("50-64")
plot_65_74_unweighted <- create_age_group_plot_unweighted("65-74") 
plot_75_84_unweighted <- create_age_group_plot_unweighted("75-84")

# Display plots
print(plot_50_64_unweighted)
print(plot_65_74_unweighted)
print(plot_75_84_unweighted)
# 
# # Save plots
# ggsave("plot_50_64_unweighted.png", plot_50_64_unweighted, width = 12, height = 8, dpi = 300)
# ggsave("plot_65_74_unweighted.png", plot_65_74_unweighted, width = 12, height = 8, dpi = 300)
# ggsave("plot_75_84_unweighted.png", plot_75_84_unweighted, width = 12, height = 8, dpi = 300)
# 
# # Compare weighted vs unweighted predictions for one example
# comparison_data <- bind_rows(
#   all_preds_hispanic %>% mutate(type = "Weighted (Survey)"),
#   all_preds_unweighted %>% mutate(type = "Unweighted")
# ) %>%
#   filter(model == "Female 50-64")
# 
# ggplot(comparison_data, aes(x = year, y = predicted * 100, 
#                             color = RaceEthnicity2, linetype = type)) +
#   geom_line(size = 1) +
#   facet_wrap(~ outcome, scales = "free_y") +
#   scale_color_manual(values = c(
#     "NH-White" = "blue",
#     "NH-Black" = "red", 
#     "Hispanic US Born" = "darkgreen",
#     "Hispanic Foreign Born" = "orange"
#   )) +
#   labs(title = "Comparison: Weighted vs Unweighted Models (Female 50-64)",
#        x = "Year",
#        y = "Predicted Probability (%)",
#        color = "Race/Ethnicity",
#        linetype = "Model Type") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# ggsave("comparison_weighted_unweighted.png", width = 12, height = 6, dpi = 300)
# 
# cat("\n\nAll unweighted models completed!\n")
# cat("Plots saved for ages 50-64, 65-74, and 75-84\n")
# cat("Comparison plot saved showing weighted vs unweighted estimates\n")
# 
# 
# 
