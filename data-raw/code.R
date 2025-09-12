

# Packages
library(readxl)
library(tidyverse)

# Read data
data_orig <- readxl::read_excel("data-raw/Harvest-Specifications_2027-28_GFSC_090425.xls.xlsx")

# See this helpful function
# 

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
  mutate(sigma_adj=case_when(category==3 ~ 2.0,
                             category %in% 1:2 ~ sigma * (1 + r * (nyr_since_assessed-1)),
                             T ~ NA)) %>% 
  # Cap sigma
  mutate(sigma_adj_cap=pmin(sigma_adj, 2)) %>% 
  # Calculate buffer
  mutate(buffer_calc=1-qlnorm(pstar, 0, sigma_adj_cap)) %>% 
  # Calculate ABC
  mutate(abc_calc=ofl*(1-buffer_calc)) %>% 
  # Compare buffer and ABCs
  mutate(buffer_diff=buffer_calc-buffer,
         abc_diff=abc_calc-abc) %>% 
  # Round values
  mutate(buffer_calc=round(buffer_calc, 3),
         buffer_diff=round(buffer_diff,3),
         abc_calc=round(abc_calc, digits=4),
         abc_diff=round(abc_diff, digits=3)) %>% 
  # Simplify
  select(stock_id, assess_year, year, nyr_since_assessed, 
         category, sigma, sigma_adj, sigma_adj_cap, pstar, 
         buffer, buffer_calc, buffer_diff, 
         ofl, abc, abc_calc, abc_diff, acl)

