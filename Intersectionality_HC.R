# Project Title - Understanding Intersectional Barriers to Healthcare Utilization among Young Women in the United States
# Check the working Directory
getwd()

# Load necessary packages
library(haven)
library(dplyr)
library(tidyr)
library(survey)
library(lme4)
library(gtsummary)
library(forcats)
library(tidyverse)
library(labelled)
library(janitor)

# Import Data
brfss <- read_dta("C:\\Users\\alawo\\Desktop\\R_Data Analytics\\R_Analytics\\BRFSS2022.dta")

head(brfss)

# Just to do a quick tab of the needed variables for the analysis
brfss %>%
  count(sexvar)

# A quick frequency of some variables with proportions
brfss %>%
  count(medcost1, genhlth, sexvar, persdoc3, x_age80) %>%
  mutate(prop = n/sum(n))


# Survey Weight Setup
brfss <- brfss %>%
  mutate(
    strata = x_strwt,
    psu = x_psu,
    wt = x_llcpwt / 1e6
  )

survey_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~wt,
  data = brfss,
  nest = TRUE
)

# Create Study Population
brfss <- brfss %>%
  mutate(
    pop = if_else(x_age80 < 25 & sexvar == 2, 1, 0)
  )

# Check the frequency
brfss %>% 
  count(pop)

# Develop the Study Outcome Variable

# Frequency of medost1
brfss %>%
  count(medcost1)

# Recode to create "barrier" variable
brfss <- brfss %>%
  mutate(
    barrier = case_when(
      medcost1 == 1 ~ 1, # Yes
      medcost1 == 0 ~ 2, # No
      medcost1 %in% 7:9 ~ NA_real_ # Missing
    )
  )

# View frequency barrier
brfss %>%
  count(barrier)

## Create the independent variables
## Age of the respondents
brfss %>%
  count(x_age80)

brfss <- brfss %>%
  rename(Age = x_age80)

brfss %>%
  count(Age)

# Marital Status of the respondents
length(brfss$marital)
nrow(brfss)

table(brfss$marital, useNA = "ifany")

brfss$Mar_Stat <- case_when(
  brfss$marital %in% c(1, 6) ~ 0, # Patnered
  brfss$marital %in% c(2, 3, 4, 5) ~ 1, # Single
  brfss$marital == 9 ~ NA_real_ # Missing
)

table(brfss$Mar_Stat, useNA = "ifany")


#Race and Ethnicity
brfss %>%
  count(x_imprace)

library(labelled)     # For variable labels and value labels
library(sjlabelled)   # For frequency tables with labels

# Frequency of original variable
table(brfss$x_imprace) 

class(brfss)

# Recode and label
brfss <- brfss %>%
  mutate(Race = case_when(
    x_imprace == 1 ~ 1,
    x_imprace == 2 ~ 2,
    x_imprace == 3 ~ 3,
    x_imprace %in% c(4, 6) ~ 4,
    x_imprace == 5 ~ 5,
    TRUE ~ NA_real_
  )) %>%
  mutate(Race = labelled(
    Race,
    labels = c(
      NH_White = 1,
      NH_Black = 2,
      NH_Asian = 3,
      NH_AIAN = 4,
      Hispanic = 5
    )
  ))

# Frequency of new Race variable
table(brfss$Race)

# Income Level
table(brfss$x_incomg1, useNA = "ifany")

# Recode x_incomg into Income_level
brfss <- brfss %>%
  mutate(
    Income_level = case_when(
      x_incomg1 %in% 1:2 ~ 0, # "<$$25k"
      x_incomg1 %in% 3:7 ~ 1, # "$25k+"
      x_incomg1 == 9 ~ NA_real_ # Missing
    )
  )

# View frequency of Income_level
table(brfss$Income_level, useNA = "ifany")


# Highest Level of Education
table(brfss$x_educag, useNA = "ifany")

# Recode x_educag into educ_level
brfss <- brfss %>%
  mutate(
    educ_level = case_when(
      x_educag == 1 ~ 1,
      x_educag == 2 ~ 2,
      x_educag == 3 ~ 3,
      x_educag == 4 ~ 4,
      x_educag == 9 ~ NA_real_
    )
  )

# View the frequency of educ_level
table(brfss$educ_level, useNA = "ifany")

# Health Insurance Status
table(brfss$x_hlthpln, useNA = "ifany")

brfss %>%
  count(x_hlthpln)

# Recode x_hlthpln to Health_cov
brfss <- brfss %>%
  mutate(
    health_cov = case_when(
      x_hlthpln == 2 ~ 0, # "Yes"
      x_hlthpln == 1 ~ 1, # "No"
      x_hlthpln %in% 7:9 ~ NA_real_ # Missing
    )
  )

# View the new variable
brfss %>%
  count(health_cov)

# Type of Place of residence
brfss %>%
  count(x_urbstat)

brfss <- brfss %>%
  rename(residence = x_urbstat)

brfss %>%
  count(residence)

# Employment Status
brfss %>%
  count(employ1)

brfss <- brfss %>%
  mutate(
    employment = case_when(
      employ1 %in% 1:2 ~ 1, # "Employed"
      employ1 %in% 3:8 ~ 2, # "Unemployed"
      employ1 == 9~ NA_real_ # Missing
    )
  )

# View the new employment variable
brfss %>%
  count(employment)

# Health Status of the respondents 
brfss %>%
  count(genhlth)

brfss <- brfss %>%
  mutate(
    SRH = case_when(
      genhlth %in% 1:3 ~ 1, # "Ex/VGood/Good"
      genhlth %in% 4:5 ~ 2, # "Fair/Poor"
      genhlth %in% 7:9 ~ NA_real_ # Missing
    )
  )

# View the SRH variable
brfss %>%
  count(SRH)

# Frequency of all Study Variables
install.packages("srvyr")
library(srvyr)

# Convert weights if needed (DHS weights are often scaled)
brfss <- brfss %>%
  mutate(wt = wt / 1e6)  # Adjust if needed

# Filter to population of interest
brfss <- brfss %>%
  filter(pop == 1) %>%
  as_survey_design(weights = wt)

# Frequency of all variables of interest

vars <- c("Age","Mar_Stat", "Race", "Income_level", 
          "educ_level", "health_cov", "residence", 
          "employment", "SRH", "barrier")

# Loop through each variable and print frequency tables
for (var in vars) {
  cat("Variable:", var, "\n\n")
  
  # Use quo_name and tidy eval to work with column names as strings
  brfss %>%
    group_by(.data[[var]]) %>%
    summarise(
      freq = survey_total(vartype = NULL),
      percent = survey_mean(vartype = NULL)
    ) %>%
    print()
  
  cat("\n---\n\n")
}


library(tidyverse)

table(brfss$barrier, useNA = "ifany")

# Keep only the needed rows and columns
brfss_clean <- brfss %>%
  filter(pop == 1) %>%
  select(wt, x_strwt, x_llcpwt, x_psu, x_state, barrier, Age, Race, educ_level, Mar_Stat,
         Income_level, health_cov, residence, employment, SRH)

summary(brfss_clean)

# Multiple Imputation
install.packages("mice")

library(mice)

# Check missing data pattern
brfss_clean <- as.data.frame(brfss_clean)

md.pattern(brfss_clean)

brfss_clean <- brfss_clean %>%
  mutate(across(c(educ_level, Mar_Stat, Income_level, 
                  health_cov, residence, employment, SRH, Race), as.factor))


# Impute data (10 imputations, random seed set for reproducibility)
imp <- mice(brfss_clean, m = 10, seed = 53421, method = c(
  "", "", "", "",  # wt, x_strwt, x_llcpwt, x_psu: no imputation
  "",              # barrier: dependent variable
  "",              # Age: predictor only
  "",              # Race: predictor only
  "polyreg",       # educ_level: multinomial logistic
  "logreg",        # Mar_Stat
  "logreg",        # Income_level
  "logreg",        # health_cov
  "logreg",        # residence
  "logreg",        # employment
  "logreg"         # SRH
))

summary(imp)

#Model 1 - educ * Race

model1a <- with(imp, glm(barrier ~ educ_level * Race, family = binomial))
summary(pool(model1a))  # pooled results

model1b <- with(imp, glm(barrier ~ educ_level * Race + Age + Mar_Stat + Income_level + 
                           health_cov + residence + employment + SRH, family = binomial))
summary(pool(model1b))

# Model 2 - Income_level * Race

model2a <- with(imp, glm(barrier ~ Income_level * Race, family = binomial))
summary(pool(model2a))

model2b <- with(imp, glm(barrier ~ Income_level * Race + Age + Mar_Stat + educ_level + 
                           health_cov + residence + employment + SRH, family = binomial))
summary(pool(model2b))

# Model 3 - Health_Cov * Race
model3a <- with(imp, glm(barrier ~ health_cov * Race, family = binomial))
summary(pool(model3a))

model3b <- with(imp, glm(barrier ~ health_cov * Race + Age + Mar_Stat + educ_level +
                           Income_level + residence + employment + SRH, family = binomial))
summary(pool(model3b))

# Model 4 - Residence * Race
model4a <- with(imp, glm(barrier ~ residence * Race, family = binomial))
summary(pool(model4a))

model4b <- with(imp, glm(barrier ~ residence * Race + Age + Mar_Stat + educ_level +
                           Income_level + health_cov + employment + SRH, family = binomial))
summary(pool(model4b))



