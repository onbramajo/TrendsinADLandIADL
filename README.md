Age-specific trends in ADL and IADL limitations among US adults aged 50-84, 2000-2018

R code for:

Bramajo O, O'Grady B, Chakraborty M, Mehta NK (2026). Age-specific trends in limitations of daily activities in American adults aged 50-84 by race and ethnicity, 2000-2018. PLOS ONE 21(2): e0340694. https://doi.org/10.1371/journal.pone.0340694

This repository replaces the original OSF repository (https://osf.io/6pfth).

What the analysis does

Using the National Health Interview Survey (NHIS) 2000-2018, we fit survey-weighted logistic regressions of ADL and IADL limitation, stratified by sex and age group (50-64, 65-74, 75-84). That gives 12 main models. Each model includes year centered at 2010, a post-2010 slope term (year_after_2010), age, a race/ethnicity-nativity variable (NH-White, NH-Black, US-born Hispanic, foreign-born Hispanic), its interactions with both time terms, and education (3 categories). From the models we compute predicted prevalence in 2000, 2010 and 2018, absolute and relative changes, and risk ratios with 95% confidence intervals.

Files
File	Content
Code_in_R_NHIS_part_1_main_code_for_OSF.R	Main analysis. Data cleaning, sample selection, descriptives, models 1-12, predicted probabilities, Figs 1-8, S1-S3 Figs, VIF (S5 Table), power analysis (S6 Table), pseudo R-squared (S3 Table), FDR correction (S4 Table).
Code_in_R_NHIS_revised_13_10_25_extra_code_table_for_exported_models.R	Exports model coefficients (S2 Table) to .xlsx/.csv and builds the descriptive table (Table 1).
Code_in_R_NHIS_revised_part_2_OSF__sensitivity_analyses.R	Sensitivity analyses: difficulty walking and memory problems as outcomes (models 13-24, S4-S6 Figs), models without education (models 25-36, S7-S9 Figs), unweighted models (models 37-48, S10-S12 Figs).
Basic_Codebook_NHIS_16_10_25.txt	IPUMS NHIS codebook for the data extract (variables, years, value labels).
Data

The data are not included. IPUMS terms of use do not allow redistribution.

Register at IPUMS NHIS: https://nhis.ipums.org/nhis/
Create an extract with samples 2000-2019 and the variables listed in Basic_Codebook_NHIS_16_10_25.txt.
Download it in Stata format (.dta.gz). The scripts expect a file called nhis_00006.dta.gz; rename yours or edit the path at the top of each script.

The 2019 sample is in the extract but is dropped in the code, because the 2019 redesign changed the limitation questions.

How to run

The scripts were written to run in one R session, in this order, because later scripts use objects created earlier:

Code_in_R_NHIS_part_1_main_code_for_OSF.R
Code_in_R_NHIS_revised_13_10_25_extra_code_table_for_exported_models.R (needs model1_svy to model12_svy and NHIS_complete from step 1)
Code_in_R_NHIS_revised_part_2_OSF__sensitivity_analyses.R (reloads the data, but uses create_prevalence_change_plot_fixed() defined in step 1)

Before running, set the data path in line 8 of part 1 and line 9 of part 2.

Most ggsave() and write.csv() calls are commented out. Uncomment them to save figures and tables.

Software

Analyses were run in R 4.3.1. Packages used:

haven, tidyverse, survey, broom, car, pwr, gridExtra, kableExtra, writexl

r
install.packages(c("haven", "tidyverse", "survey", "broom", "car",
                   "pwr", "gridExtra", "kableExtra", "writexl"))
Key variable definitions
Variable in code	Definition
ladl	Needs help with ADL (IPUMS LADL), recoded 0 = no, 1 = yes
laiadl	Needs help with IADL (IPUMS LAIADL), recoded 0 = no, 1 = yes
RaceEthnicity2	NH-White, NH-Black, Hispanic US Born, Hispanic Foreign Born (from RACENEW, HISPYN, USBORN)
EducRecoded	Less than high school / GED to some college / BA and beyond (from EDUC)
year_centered	Survey year minus 2010
year_after_2010	max(0, year - 2010)
Survey design	psu, strata, perweight, with nest = TRUE

Final analytic sample: 477,171 complete cases (see paper, Methods).

Contact

Octavio Bramajo, octavio.bramajo@iem.uzh.ch
