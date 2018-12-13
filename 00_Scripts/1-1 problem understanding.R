# libraries

library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

#load data
path_train <- "00_Data/telco_train.xlsx"
train_raw_tbl <- read_excel(path_train, sheet = 1)

#data subset
dept_job_role_tbl <- train_raw_tbl %>% 
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

# 1 Business Science Problem Framework ----
 
# 1A. View the Business as a machine ----

# Business Units: Department, Job Role
# Define Objectives: Retain high performers
# Assess Outcomes: TBD

dept_job_role_tbl %>% 
  group_by(Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n))

# 1B. Understand the drivers ---- 

# Investigate objectives: 16% attrition
# Synthesize outcomes: 
# Hypothesize drivers: Job role and Departments

# Departments ----
dept_job_role_tbl %>% 
  
  group_by(Department, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department) %>% 
  mutate(pct = n / sum(n)) 

# Job Role ----
dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition %in% c("Yes")) 

# 1C. Measure the Drivers ----

# Collect information on employee Attrition: On going

# Develop KPIs: Industry KPIs 8.8%

dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition %in% c("Yes")) %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_avg = case_when(
      pct > 0.08 ~ "Yes",
      TRUE ~ "No"
    )
  )

