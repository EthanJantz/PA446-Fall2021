# HW2
# your final answers and your final R script can be uploaded via the link below:
# https://forms.gle/pJLZUkj8zpHvqUDi6
# FROM HERE ON OUT, MAKE SURE YOU USE THE FULL SALARIES DATASET, DETAILS BELOW
# Blackboard > PA 446 > Data files > salary_data_full
#

"Now that you have a more complete dataset, 
aligned on goals with the client,
and have a basic understanding of the data wrangling needed, 
you are finally ready to begin data wrangling.
"


#---PROBLEM 1---

"
Break out first, middle and last name into their distinct columns.
Name your new columns first_name, middle_name, last_name.
"

#if you write any code for the problem, please include your code/work here

library(tidyverse)
`%nin%` <- Negate(`%in%`) # For wrangling ssa / gender data

data_raw <- read_csv(here::here("data", "salary_data_full.csv"))

data <- data_raw %>%
  # Break out the last name from the rest of the name data
  separate(Name,
           into = c("LAST_NAME", "FIRST_NAME"),
           sep = "\\,") %>%
  mutate(
    # Remove suffixes like Jr from last name
    LAST_NAME = str_extract(LAST_NAME, "^[A-Z]+"),
    # Extract middle initial with regex
    MIDDLE_INITIAL = str_extract(FIRST_NAME, "\\s[A-Z]$") %>%
           str_remove("\\s"),
    # Remove whitespace and middle initial from first_name
    FIRST_NAME = str_remove(FIRST_NAME, "^\\s\\s") %>%
      str_remove("\\s[A-Z]$")) %>%
  select(FIRST_NAME, MIDDLE_INITIAL, LAST_NAME, 
         everything())

#---PROBLEM 2---
"
We need to one annual_salary for both hourly and salaried employees.
Figure out how to coalesce hourly and annual salary together for these
2 types of workers and create a single column called annual_salary
"

#if you write any code for the problem, please include your code/work here

data <- data %>%
  mutate(
    hourly_salary = (`Hourly Rate` * `Typical Hours`) * 52, # Assuming they're working the same hours every week in the year
    ANNUAL_SALARY = coalesce(`Annual Salary`, hourly_salary)
  ) %>%
  select(FIRST_NAME, MIDDLE_INITIAL, LAST_NAME, 
         ANNUAL_SALARY, -`Annual Salary`, # Get rid of old column w/ same name
         everything())

#---PROBLEM 3---
"
You are missing gender data. However, you are very scrappy and found the
Social Service Administration's data (https://www.ssa.gov/OACT/babynames/limits.html)
for Illinois residents.
The file is called IL.TXT and is available on Blackboard.
Use this data to identify the gender of as many individuals
in the Chicago dataset as possible.
The end product should be a new column in the Chicago dataset, called new_gender.
While new_gender will have more non-NA values than the old gender column,
some rows can still be NA.
REMEMBER TO SHOW YOUR WORK
"


#if you write any code for the problem, please include your code/work here

ssa_raw <- read_csv(here::here("data", "IL.TXT"),
                    col_names = c("STATE", "GENDER", "YEAR","FIRST_NAME", "COUNT")) # Where count is the number of occurrences of that name in the given year

ssa <- ssa_raw %>%
  mutate(
    # Get FIRST_NAME into uppercase form for joining
    FIRST_NAME = str_to_upper(FIRST_NAME)
    ) %>%
  group_by(FIRST_NAME, GENDER) %>%
  summarize(COUNT = sum(COUNT),
            GENDER = GENDER) %>%
  # Remove duplicates
  distinct() %>%
  # We'll be taking the simple max() of COUNT to determine gender
  # for a given name
  group_by(FIRST_NAME) %>%
  filter(COUNT == max(COUNT)) %>%
  # There are four ties in the dataset
  # CHARLY, CODI, MICKY, and TRUE have equal COUNT values
  # between genders. We'll remove them
  filter(FIRST_NAME %nin% c("CHARLY", "CODI", "MICKY", "TRUE")) %>%
  ungroup()

# Test for duplicates
# ssa %>%
#   count(FIRST_NAME) %>%
#   arrange(desc(n))

data <- data %>%
  left_join(ssa, by = c("FIRST_NAME")) %>%
  mutate(NEW_GENDER = as.factor(GENDER)) %>%
  select(FIRST_NAME, MIDDLE_INITIAL, LAST_NAME, 
         JOB_TITLES = "Job Titles",
         DEPARTMENT = "Department",
         NEW_GENDER, 
         ANNUAL_SALARY)

summary(data$NEW_GENDER) # 2,762 NA values in NEW_GENDER, or ~9% of employees


#---PROBLEM 4---
"
A large part of equity also has to do with race. Find two data
sources that can help you create a race column in the Chicago data.
YOU DO NOT NEED TO ACTUALLY WRANGLE A RACE COLUMN, 
JUST FIND THE DATA SOURCES ONLINE AND SHARE THE URL.
"

# You could use census surname data to generate odds of a surname being a certain race:

# https://www.census.gov/data/developers/data-sets/surnames.html

# You could combine the above surname data with the below first name data to generate
# improved odds. Further improved odds would require geocoding the data, which we would be 
# unable to do with the current employee dataset. 
# Methods are described in the "Method" link.

# Data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TYJKEZ
# Method: https://www.tandfonline.com/doi/pdf/10.1080/2330443X.2018.1427012

#if you write any code for the problem, please include your code/work here
