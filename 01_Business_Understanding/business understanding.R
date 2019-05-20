# BUSINESS UNDERSTANDING ----

# libraries

library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)
library(formattable)

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
# mutate(pct = n / sum(n)) 
# need to group by department otherwise we have percent of total
# grouping by department gives us the percentage within a department. 
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
      pct > 0.088 ~ "Yes",
      TRUE ~ "No")) 

# 1D. Uncover Problems and Opportunities ----

calculate_attrition_costs <- function(
  
  # Employee
  n                     = 1,
  salary                = 80000,
  
  #Direct Costs
  separation_cost       = 500,
  vacancy_cost          = 10000,
  acquisition_cost      = 4900,
  placement_cost        = 3500,
  
  #Lost Productivity
  net_revenue_per_employee  =	250000,
  workdays_per_year         = 240,
  workdays_postition_open   = 40,
  workdays_onboarding       = 60,
  onboarding_efficiency     = 0.50
  
) {
  
  #Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  #Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year * 
    (workdays_postition_open + workdays_onboarding * onboarding_efficiency)
  
  #Savings of Salary & Benefits
  salary_benefit_reduction <- salary / workdays_per_year * workdays_postition_open
  
  # Estimated Turnover per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  #total Cost of employee Turnover
  total_cost <- n * currency(cost_per_employee)
  
  return(total_cost)
}

# Calculate Costs By Job Role ----
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
  ) %>% 
  
  mutate(cost = calculate_attrition_costs(n = n, 
                                          salary = 80000))

# Workflow of Attrition ----

###replacing the first two parts with functions.

dept_job_role_tbl %>% 
  #group_by(Department, JobRole, Attrition) %>% 
  #summarise(n = n()) %>% 
  #ungroup() %>% 
  count(JobRole, Attrition) %>% 
  
  #group_by(JobRole) %>% 
  #mutate(pct = n / sum(n)) %>% 
  #ungroup() %>% 
  count_to_pct(JobRole) %>% 
  
  filter(Attrition %in% c("Yes")) %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_avg = case_when(
      pct > 0.08 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% 
  
  mutate(
    cost = calculate_attrition_costs(n = n, 
                                     salary = 80000)
  )

count_to_pct <- function(data, ..., col = n) {
  
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  ret <- data %>% 
    group_by(!!!grouping_vars_expr) %>% 
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>% 
    ungroup()
  
  return(ret)
}

###replacing the 3rd part with a function
dept_job_role_tbl %>%
  count(Department, JobRole, Attrition) %>% 
  count_to_pct(Department, JobRole) %>% 

  filter(Attrition %in% c("Yes")) %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_avg = case_when(
      pct > 0.08 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% 
  
  mutate(
    cost = calculate_attrition_costs(n = n, 
                                     salary = 80000)
  )

assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
  attrition_col_expr <- enquo(attrition_col)
  data %>% 
    filter((!! attrition_col_expr) %in% attrition_value) %>% 
    arrange(desc(pct)) %>% 
    mutate(
      above_industry_avg = case_when(
        pct > baseline_pct ~ "Yes",
        TRUE ~ "No"
      )
    )
}

<<<<<<< HEAD
=======
dept_job_role_tbl %>%
  count(Department, JobRole, Attrition) %>% 
  count_to_pct(Department, JobRole) %>% 
  assess_attrition(Attrition, 
                   attrition_value = "Yes", 
                   baseline_pct = 0.088) %>% 
  mutate(
    cost = calculate_attrition_costs(n = n, 
                                     salary = 80000)
  )

# Visualization of Attrition Cost ----

dept_job_role_tbl %>% 
  count(Department, JobRole, Attrition) %>% 
  count_to_pct(Department, JobRole) %>% 
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>% 
  mutate(cost_of_attrition = calculate_attrition_costs(n = n, 
                                          salary = 80000)) %>% 
  
  # Data Manipulation 
  mutate(name = str_c(Department, JobRole, sep = ": ") %>% as_factor()) %>%
  mutate(name = fct_reorder(name, cost_of_attrition)) %>% 
  mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6, digits = 2),
                           "M", sep = "")) %>% 
  
  # Plotting
  ggplot(aes(x = cost_of_attrition, y = name)) +
  geom_segment(aes(xend = 0, yend = name), color = palette_light()[[1]]) +
  geom_point(aes(size = cost_of_attrition), color = palette_light()[[1]]) +
  scale_x_continuous(labels = scales::dollar) + 
  geom_label(aes(label = cost_text, size = cost_of_attrition),
             hjust = "inward", , color = palette_light()[[1]]) +
  theme_tq() +
  scale_size(range = c(3, 5)) + 
  labs(title = "Estimated Cost of Attrition by Department and Job Role",
       y = "",
       x = "Cost of Attrition") +
  theme(legend.position = "none")
>>>>>>> 0b4f6e418038190661feac05f9bd3ce2dbc599ed
