# Make Example Dataset 1
# 
# Christian Testa
# October 30 2020
# 
# For use as an example in a talk on visualizing longitudinal data analysis.


###########################
# Dataset Characteristics # 
###########################

# Total Population Size 
N <- 400

# We'll have four cohorts, observed at 4 different times, 
# with a quantitative outcome.
cohorts <- c('A', 'B', 'C', 'D')
n_cohorts <- length(cohorts)
years <- c(2005, 2010, 2015, 2020)



############
# Cohort 1 # 
############

# Cohort 1 will be a group of individuals with roughly increasing outcomes

# Start participants off log normally distributed 
cohort_1_2005 <- 
  rlnorm(n = N / n_cohorts, meanlog = 1, sdlog = 1)


# Roughly increase by adding a random value (normally distributed, mean = 1) to
# the previous values but don't allow the value to go below zero.

roughly_increase <- function(previous_values) {
  sapply(previous_values + 
    rnorm(n = N / n_cohorts, mean = 3, sd = 1), max, 0)
}

# Increase each observation set based on the previous observation
cohort_1_2010 <- roughly_increase(cohort_1_2005)
cohort_1_2015 <- roughly_increase(cohort_1_2010)
cohort_1_2020 <- roughly_increase(cohort_1_2015)



############
# Cohort 2 #
############

# Cohort 1 will be roughly constant but start at a slightly higher value

# Start participants off log normally distributed 
cohort_2_2005 <- 
  rlnorm(n = N / n_cohorts, meanlog = 1.1, sdlog = 1)

# Keep roughly steady by adding a normally distributed value 
# (mean = 0, sd = 1), but don't let it go below zero.

keep_roughly_steady <- function(previous_values) {
  sapply(previous_values + rnorm(n = N / n_cohorts, mean = 0, sd = 1), max, 0)
}

# Define each observation set based on the previous observation
cohort_2_2010 <- keep_roughly_steady(cohort_2_2005)
cohort_2_2015 <- keep_roughly_steady(cohort_2_2010)
cohort_2_2020 <- keep_roughly_steady(cohort_2_2015)



############
# Cohort 3 # 
############

# Let's have cohort 3 be a more eratic group which 
# has a low first value, high second value, low third value,
# and high fourth value.

# Start participants off log normally distributed 
cohort_3_2005 <- 
  rlnorm(n = N / n_cohorts, meanlog = 1, sdlog = 1)

# increase by roughly 5, but don't let it go below zero
cohort_3_2010 <- 
  sapply(cohort_3_2005 + rnorm(n = N / n_cohorts, mean = 5, sd = 1), max, 0)

# decrease by roughly 4, but don't let it go below zero
cohort_3_2015 <- 
  sapply(cohort_3_2010 - rnorm(n = N / n_cohorts, mean = 4, sd = 1), max, 0)

# increase by roughly 6, but don't let it go below zero
cohort_3_2020 <- 
  sapply(cohort_3_2015 + rnorm(n = N / n_cohorts, mean = 6, sd = 1), max, 0)



############
# Cohort 4 #
############

# Cohort 4 will start from a slightly high value, but fairly steadily will 
# decline over the observations.

# Start log normally distributed
cohort_4_2005 <- 
  rlnorm(n = N / n_cohorts, meanlog = 1.125, sdlog = 1)

# Steadily decline by ~1.5 each iteration fairly consistently (sd=.25)

roughly_decline <- function(previous_values) {
  sapply(previous_values - rnorm(n = N / n_cohorts, 1.5, sd = .25), max, 0)
}

cohort_4_2010 <- roughly_decline(cohort_4_2005)
cohort_4_2015 <- roughly_decline(cohort_4_2010)
cohort_4_2020 <- roughly_decline(cohort_4_2015)



################
# Make Dataset #
################

df <- data.frame(
  group = rep(cohorts, each = N / n_cohorts),
  gender = c('M', 'F')[rbinom(n = N, size = 1, prob = 0.5)+1],
  `2005` = c(cohort_1_2005, cohort_2_2005, cohort_3_2005, cohort_4_2005),
  `2010` = c(cohort_1_2010, cohort_2_2010, cohort_3_2010, cohort_4_2010),
  `2015` = c(cohort_1_2015, cohort_2_2015, cohort_3_2015, cohort_4_2015),
  `2020` = c(cohort_1_2020, cohort_2_2020, cohort_3_2020, cohort_4_2020))

# increase values by 5 for women
library(dplyr)
df <- df %>% 
  mutate_if(is.numeric, ~ ifelse(gender == 'M', 
    sapply(. + 5 + rnorm(n = N, mean = 4, sd = 2), max, 0), 
    sapply(. + rnorm(n = N, mean = 2, sd = 2), max, 0)))

write.csv(df, "example_data/example_dataset_1.csv", row.names=F)