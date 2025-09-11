

# Packages
library(readxl)
library(tidyverse)

# Read data
# data_orig <- readxl::read_excel("/Users/cfree/Desktop/Harvest-Specifications_2027-28_GFSC_090425.xls.xlsx")

# Format data
r <- 0.075
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Add year since assessment
  mutate(nyr_since_assessed=year-assess_year) %>% 
  # Add sigma
  mutate(sigma=case_when(category==1 ~ 0.5,
                         category==2 ~ 1.0,
                         category==3 ~ 2.0,
                         T ~ NA)) %>% 
  # Add SD
  mutate(sigma_adj=sigma * (1 + r * (nyr_since_assessed-1))) %>% 
  # Calculate buffer
  mutate(buffer_calc=1-qlnorm(pstar, 0, sigma_adj),
         buffer_calc=round(buffer_calc, 3)) %>% 
  # Calculate ABC
  mutate(abc_calc=ofl*(1-buffer_calc),
         abc_calc=round(abc_calc, digits=4)) %>% 
  # Compare buffer and ABCs
  mutate(buffer_diff=buffer_calc-buffer,
         abc_diff=abc_calc-abc) %>% 
  # Simplify
  select(stock_id, assess_year, year, nyr_since_assessed, 
         category, sigma, sigma_adj, pstar, 
         buffer, buffer_calc, buffer_diff, 
         ofl, abc, abc_calc, abc_diff, acl)
