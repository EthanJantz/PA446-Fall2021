# HW3
# your final answers and your final R script can be uploaded via the link below:
# 
# FROM HERE ON OUT, MAKE SURE YOU USE THE FULL SALARIES DATASET, DETAILS BELOW
# Blackboard > PA 446 > Data files > salary_data_full
#

#---PROBLEM 1---
"Now that you are done with data wrangling, it is time to
synthesize everything you learned. Use the original data files: 
IL.TXT, census_2010_race.csv, and pa446_chicago_full.csv.
Clean all of them and create 1 master table.
The master table need to have the following columns:
last_name, first_name, job_titles, department, 
annual_salary (for both salaried and hourly employees),
race, gender.
SHOW YOUR WORK.
"

#if you write any code for the problem, please include your code/work here

library(here)
library(tidyverse)

options(scipen = 9999999999)

# Use copy of HW 2 script to avoid re-writing work already done
# Changes have been made to HW 2 since original submission
# See them at: 
# https://github.com/EthanJantz/PA446-Fall2021/blob/main/HW%202/hw2.R
tictoc::tic()
source(here("scripts", "hw2.R"))
tictoc::toc()
# Remove unnecessary environment objects
rm(list = c("data_raw", "ssa", "ssa_raw"))

data <- data %>%
  select(
    LAST_NAME,
    FIRST_NAME,
    JOB_TITLES,
    DEPARTMENT,
    ANNUAL_SALARY,
    GENDER = "NEW_GENDER"
  )

print(
  paste0("Column names: ", 
         paste(colnames(data), collapse = ", "))
) # "Column names: LAST_NAME, FIRST_NAME, JOB_TITLES, DEPARTMENT, ANNUAL_SALARY, GENDER"


# Ingest, clean, join race data -------------------------------------------

census_raw <- read_csv(here("data", "census_2010_race.csv"))

# Clean + lengthen raw data
census <- census_raw %>%
  pivot_longer(cols = starts_with("pct"),
               names_to = "RACE",
               names_prefix = "pct",
               values_to = "PCT") %>%
  # (S) values probably mean "Suppressed", but 
  # Census API documentation doesn't seem to 
  # provide clarification
  filter(PCT != "(S)") %>%
  mutate(
    RACE = as.factor(RACE),
    PCT = as.numeric(PCT)
  )

# Filter out highest race value
# This is the same method used for imputing gender
# We're assuming that the decisions based on this method
# will even out across various names. Better methods
# do exist, see my answer to problem 4 in scripts/hw2.R
census <- census %>%
  # Filter out all but the highest PCT value for each name
  group_by(name) %>%
  filter(PCT == max(PCT)) %>%
  select(
    LAST_NAME = "name",
    RACE,
    PCT
  )

summary(census)

# Convert RACE values from census codes to readable text
# Note, this process may take a moment
# tictoc::tic() # 65.25 sec elapsed
census <- census %>%
  mutate(
    # Using variable descriptions from here:
    # https://api.census.gov/data/2010/surname/variables.html
    RACE = as.character(RACE), # Temporary conversion to call case_when
    RACE = case_when(
      RACE == "2prace" ~ "TWO OR MORE RACES",
      RACE == "aian" ~ "AMERICAN INDIAN OR ALASKAN NATIVE",
      RACE == "api" ~ "ASIAN OR NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      RACE == "black" ~ "BLACK OR AFRICAN AMERICAN",
      RACE == "hispanic" ~ "HISPANIC OR LATINO ORIGIN",
      RACE == "white" ~ "WHITE",
      RACE == TRUE ~ "ERROR" # To ensure all values are coded with this function call, though it shouldn't be necessary
    ) %>%
      as.factor()
  )
# tictoc::toc() 

summary(census)

# Testing to ensure no duplicate names
# census %>%
#   count(LAST_NAME) %>%
#   arrange(desc(n)) # No duplicates found

# Join to data
data <- data %>%
  left_join(census, by = c("LAST_NAME")) %>%
  mutate(DEPARTMENT = as.factor(DEPARTMENT)) %>%
  select(LAST_NAME,
         FIRST_NAME,
         JOB_TITLES,
         DEPARTMENT,
         ANNUAL_SALARY,
         GENDER,
         RACE)

summary(data) # 3,585 NA RACE values, or ~ 11% of observations

#---PROBLEM 2---
"
As you already know, one of the mayor of Chicago's priority this year is equity in pay for city employees,
especially in some of the city's largest departments. 

Furthermore, equity is defined as pay equality between 
1) different genders and 
2) different races

Use your master table and see if there are general pay differences
between genders in the city's 5 largest departments.
Please also calculate the n-size for males and females in each department.
"
#if you write any code for the problem, please include your code/work here

# Pull names of top 5 departments in data
top_5 <- data %>%
  count(DEPARTMENT) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5) %>%
  pull(DEPARTMENT)

# Analyze pay by department by gender
gender_table <- data %>%
  filter(
    DEPARTMENT %in% top_5,
    !is.na(GENDER)
  ) %>%
  group_by(GENDER, DEPARTMENT) %>%
  summarize(
    MEDIAN_PAY = median(ANNUAL_SALARY, na.rm = T),
    MEAN_PAY = mean(ANNUAL_SALARY, na.rm = T),
    N = n() 
  ) %>%
  pivot_wider(id_cols = DEPARTMENT,
              names_from = GENDER,
              values_from = c(MEDIAN_PAY, MEAN_PAY, N)) %>%
  mutate(PROP_F = (N_F / (N_F + N_M)) * 100,
         SKEW_F = MEAN_PAY_F - MEDIAN_PAY_F,
         SKEW_M = MEAN_PAY_M - MEDIAN_PAY_M)

gender_table 

# Based on the above table, we can see that female-coded employees are
# underrepresented among Chicago's 5 largest departments. Chicago's
# Fire department has the lowest representation at ~9%, and the Police
# department has the highest at ~24%. 

# We can also see that females are paid less on average than males, and 
# that the distribution of pay among female employees skews negative in
# 3 of the 5 departments, while male employees only see a negative skew
# in 2. Males have less skew on average than female employees, though
# females have both the minimum and maximum of the skew values. This is 
# likely an effect of their small N-size compared to male employees. 
# Skew indicates a non-normal distribution of pay, with a positive or
# negative skew indicating a direction of that distribution. From this
# we can say that female employees are generally earning less than 
# male employees in the top 5 departments. We cannot determine 
# if this is significant between equivalent ranks without further analysis.

#---PROBLEM 3---

"
Is the difference you observed in problem 2 statistically significant?
"

#if you write any code for the problem, please include your code/work here

# Lengthening the gender_table for the following test
gender_table <- data %>%
  filter(
    DEPARTMENT %in% top_5,
    !is.na(GENDER)
  ) %>%
  group_by(GENDER, DEPARTMENT) %>%
  summarize(
    MEDIAN_PAY = median(ANNUAL_SALARY, na.rm = T),
    MEAN_PAY = mean(ANNUAL_SALARY, na.rm = T),
    N = n() 
  )

# First we must test for a difference in variance to choose which test is appropriate, the T-Test or Wilcox Test
DescTools::LeveneTest(gender_table$N, gender_table$GENDER, center = mean)

# Widening the table for the t.test
gender_table <- gender_table %>%
  pivot_wider(id_cols = DEPARTMENT,
              names_from = GENDER,
              values_from = c(MEDIAN_PAY, MEAN_PAY, N))
  
# The difference in variance between N-size by gender 
# isn't significant (p < .05) so we'll use the paired t-test
t.test(gender_table$N_F, gender_table$N_M, 
       alternative = "less", paired = T)
# Here we see a significant (p < .05) result from our paired t-test, indicating
# a significantly smaller value among female employees than male employees across
# departments.


#---PROBLEM 4---
"
Use your master table and see if there are general pay differences
between races in the city's 5 largest departments.
Please also calculate the n-size for each race subgroup in each department.
"

#if you write any code for the problem, please include your code/work here

# Analyze pay by department by gender
race_table <- data %>%
  filter(
    DEPARTMENT %in% top_5,
    !is.na(RACE)
  ) %>%
  group_by(RACE, DEPARTMENT) %>%
  summarize(
    MEDIAN_PAY = median(ANNUAL_SALARY, na.rm = T),
    MEAN_PAY = mean(ANNUAL_SALARY, na.rm = T),
    N = n()
  ) %>%
  group_by(DEPARTMENT) %>%
  mutate(DEPARTMENT_FREQ = (N / sum(N)) * 100,
         SKEW = MEAN_PAY - MEDIAN_PAY)

race_table 

# Based on the above table, we can see that White coded employees are 
# overrepresented in the top 5 departments compared to the other races
# found in the data. 

# We can also see that black coded employees are not only underrepresented, 
# but they also have negative skew among all departments - the only race
# category to experience this according to the data. 

#---PROBLEM 5---
"
Is the differences statistically significant?
"

#if you write any code for the problem, please include your code/work here

# We have multiple groups to test between, so I'll be performing an ANOVA
# test to see if there is a significant difference in the N values between 
# race categories and departments

anova <- aov(N ~ RACE + DEPARTMENT,
             data = race_table)

summary(anova)

# Here we can see a significant (p < .05) difference in the number of
# employees by race. We do not see a significant difference among employees
# by department among the top 5 departments. Because we're only testing 
# among the top 5 departments, we do not have enough degrees of freedom
# to test for an interaction effect.
