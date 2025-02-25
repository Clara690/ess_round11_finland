library(tidyverse)
library(haven)

# load the data
ess2023_fi <- read_dta('./ESS11_fi.dta')

# independent v = 'prtvtffi' which stands for party voted for in last election
variables <- c('prtvtffi', 'imbgeco', 'gndr', 'edlvdfi', 'agea') # dtype = double
ess2023_fi <- ess2023_fi %>% 
  select(variables) %>% 
  filter(complete.cases(.)) 

# ess2023_fi$gndr <- factor(ess2023_fi$gndr, levels = c('Male', 'Female'))

# remove missing values and convert some columns to factors
ess2023_fi <- ess2023_fi %>% 
  mutate(party = zap_missing(prtvtffi),
         party = as_factor(party),
         party = fct_lump_n(party, 3),
         imbgeco = as.integer(imbgeco),
         gndr = as_factor(gndr),
         edlvdfi = zap_missing(edlvdfi),
         # highly educated or not
         edlvdfi = as_factor(ifelse(edlvdfi >= 9, 1, 0)), 
         agea = as.integer(agea)) %>% 
  filter(gndr != 'No answer')

# relevel and set National Coaltion Party as a reference category
ess2023_fi <- within(ess2023_fi, party <- relevel(
  party, ref = 'The National Coalition Party'))
#-----------------------------------------------------
library(modelsummary)
library(kableExtra)
# descriptive data on the sample
ess2023_fi %>% 
  mutate(group = case_when(
    party ==  'The National Coalition Party' ~ 1,
    party == 'Social Democratic Party' ~ 2,
    party == 'True Finns' ~ 3,
    party == 'Other' ~ 4
  )) %>% 
  mutate(group = factor(group, labels = c('NCP', 'SDP', 'TF', 'Other'))) %>% 
  select(group, 
         Sex = gndr,
         Age = agea,
         `Higher Education` = edlvdfi,
         `Opinions on Immigrants` = imbgeco) %>%
  datasummary_balance( Sex + Age + `Higher Education`+
                         `Opinions on Immigrants` ~ group,
                       data = .,
                       notes = c('Source: European Social Survey Round 11',
                                 'Higher values in "Opinion on Immigrants"
                                suggest more positive views.'))