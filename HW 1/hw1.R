# HW1
#your final answers and your final R script can be uploaded via the link below:
#https://forms.gle/qo6XoLamepNuJNcT9

"One of the mayor of Chicago's priority this year is equity in pay for city employees,
especially in some of the city's largest departments. Her office has provided you a dataset
to help figure out where pay inequity might currently exist.
"

#---PROBLEM 1---

"
Pay equity amongst city employees is a large and vague problem.
See if you can build more clarity by asking 4 clarifying questions for
the prompt above.
"

# 1. Are there pay inequities among employees with the same job title in different departments?
# 2. How big is the gender and/or racial pay gap among employees with the same job title within the same departments?
# 3. Are there discrepancies in employee benefits between departments?
# 4. Do all departments track pay in the same way, or are any departments missing from our data?

#if you write any code for the problem, please include your code/work here


#---PROBLEM 2---
"
1. Open the dataset
2. Before you do any analyis on the data, you have to ensure the data is ready for analysis.
Use the R functions you been taught to carefully examine each column - SHOW YOUR WORK. 
Find 3 issues with the data that are either problematic for future analysis you want to do
or just not best practice for tidyverse data analysis.
"

#if you write any code for the problem, please include your code/work here


# Problem 2 - Code --------------------------------------------------------

library(tidyverse)
options(scipen = 999)

data_raw <- read_csv(here::here("data", "hw1.csv"))


# Cleaning ----------------------------------------------------------------

# Issue 1 - Salary and hourly compensation values are separate columns, need to be coalesced so we have proper long data
# Issue 2 - Gender has no values, even after examining the file itself in Notepad. I would contact the supplier of the data to understand why and get this issue fixed.
# Issue 3 - Names should be separated out, and other character columns like Department and Salary or Hourly should be converted to factors

# TODO
# Name needs to be broken out between First/Last x
# Titles need to be separated x
# Department is fine x
# Full/Part time can be a boolean x
# Salary/Hourly can be renamed COMPENSATION_TYPE x
# Coalesce compensation values into one col x
# Compensation values need to be converted to numeric x
# Remove gender, it has no values x

data <- data_raw %>%
  separate(`Job Titles`,
           into = c("TITLE1", "TITLE2"),
           sep = "-|/") %>%
  mutate(
    LAST_NAME = str_extract(Name, "^[A-z\\s]+"),
    FIRST_NAME = str_extract(Name, "[A-z\\s]+$") %>% # Separating out middle initials seems unnecessary for the purposes of this analysis
      str_remove("^\\s\\s"), # Leading whitespace removal for clean names going forward
    FULL_TIME = ifelse(`Full or Part-Time` == "F", T, F),
    TITLE1 = as.factor(TITLE1),
    DEPARTMENT = as.factor(Department),
    COMPENSATION_TYPE = as.factor(`Salary or Hourly`),
    COMPENSATION = coalesce(`Hourly Salary`, `Yearly Salary`) %>%
      str_extract("[^\\$\\-]+") %>% # Remove "$" and "-" values
      str_remove(",") %>% 
      as.numeric()
         ) %>%
  # Looking over the secondary job titles, they don't seem useful or easy to work with for the purpose of this assignment. 
  # Some individuals do have two full job titles, but they're rare enough in the data that I don't see the use in considering them. 
  # This decision should be noted in any report.
  select(LAST_NAME, FIRST_NAME,
         TITLE = "TITLE1",
         DEPARTMENT,
         FULL_TIME,
         COMPENSATION_TYPE,
         COMPENSATION)


# Analysis ----------------------------------------------------------------

### Hourly distribution by department
data %>%
  filter(COMPENSATION_TYPE == "Hourly") %>%
  ggplot(aes(x = COMPENSATION)) +
  geom_histogram(bins = 30) +
  xlim(NA, 100) +
  facet_wrap(~DEPARTMENT)

### Hourly Summary stats
data %>%
  filter(COMPENSATION_TYPE == "Hourly") %>%
  summary() # Base R function, but robust for quick exploration

# 6,939 employees
# 1,216 PT / 5,723 FT
# Top 3 Depts: Streets & San (1,745), Water Mgmt (1,510), Aviation (1,130)
# Min: 3
# Median: 38.35
# Mean: 37.60
# Max: 128.00

### Salary distribution by department
data %>%
  filter(COMPENSATION_TYPE == "Salary") %>%
  ggplot(aes(x = COMPENSATION)) +
  geom_histogram(bins = 30) +
  xlim(NA, 300000) +
  facet_wrap(~DEPARTMENT)

### Salary Summary stats
data %>%
  filter(COMPENSATION_TYPE == "Salary") %>%
  summary()

# 24,831 employees
# 1 PT / 24,830 FT
# Top 3 Depts: Police (13,119), Fire (4,730), Public Library (713)
# Min: 20,400
# Median: 90,024
# Mean: 92,386
# Max: 275,004

### Departments
data %>%
  distinct(DEPARTMENT) # 26 Departments

# Titles by Departments
data %>%
  count(DEPARTMENT, TITLE) %>%
  arrange(desc(n))
