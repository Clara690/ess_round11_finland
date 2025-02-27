```{r}
#| echo: false
#| message: false
#| warning: false
library(tidyverse)
library(haven)

# load the data
ess2023_fi <- read_dta('./ESS11_fi.dta')

# independent v = 'prtvtffi' which stands for party voted for in last election
variables <- c('prtvtffi', 'imbgeco', 'gndr', 'edlvdfi', 'agea') # dtype = double
ess2023_fi <- ess2023_fi %>% 
  select(variables) %>% 
  filter(complete.cases(.)) 

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

# relevel and set National Coalition Party as a reference category
ess2023_fi <- within(ess2023_fi, party <- relevel(
  party, ref = 'The National Coalition Party'))

ess2023_fi$gndr <- droplevels(ess2023_fi$gndr)

```
# Data
::: {.landscape}
```{r}
#| echo: false
#| message: false
#| warning: false
#| tbl-cap: 'Descriptive Statistics on Voter Arributes'
#| label: tbl-descriptive

library(gtsummary)
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
  tbl_summary(by = group, include = c(Sex, Age, 
                                      `Higher Education`, `Opinions on Immigrants`)) %>% 
  add_overall() %>% 
   modify_footnote_header("Source: European Social Survey Round 11,
Higher values in *Opinions on Immigrants*
  suggest more positive views.",
                          , columns = all_stat_cols())
```{r}
#| echo: false
#| message: false
#| warning: false
#| tbl-colwidths: [60, 60, 60, 60, 60, 60]
#| tbl-cap: 'Odds Ratios from Logistic Regression Models of Party Choice'
#| label: tbl-md-statistics
library(nnet)
library(broom)
library(modelr)
library(modelsummary)
library(kableExtra)

# estimation using multinomial logistic regression models
# model 1 with opinions on immigrants as the only idependent variable
mod1 <- multinom(party ~ imbgeco, data = ess2023_fi, trace = FALSE)

# model 2 with opinons + demographic attributes
mod2 <- multinom(party ~ imbgeco + gndr + edlvdfi + agea, data = ess2023_fi,
                 trace = FALSE)
# making a table
models <- list(mod1, mod2)
modelsummary(models, shape =  term ~ model + response,
             fmt = 2,
             statistic = 'conf.int',
             exponentiate = TRUE,
             coef_rename = c('Intercept', 'Opinions on Immigrants', 'Female'
                             , 'High Edu', 'Age'),
             gof_omit = 'RMSE')
#| echo: false
#| message: false
#| warning: false
#| tbl-cap: 'Goodness of Model Fit'
#| label: tbl-md-fit

library(lmtest)
library(DescTools)
library(gt)
# model fit statistics
lktest <- lrtest(mod1, mod2)
# Negelkerke's pseuso's R^2
pse_R1 <- round(PseudoR2(mod1, which = 'Nagelkerke'), 2)
pse_R2 <- round(PseudoR2(mod2, which = 'Nagelkerke'), 2)
# share of correct prediction
# model 1
pred_mod1 <- predict(mod1, type = 'class')
sc_mod1 <- (mean(ess2023_fi$party == pred_mod1)) *100
# model 2
pred_mod2 <- predict(mod2, type = 'class')
sc_mod2 <- (mean(ess2023_fi$party == pred_mod2)) *100

# put the information into a table
results <- tibble('term' = c('Likelihood Ratio Test', 
                             'Pseuso R²', 
                             'Share of Correct Prediction (%)'),
                  'Model 1' = c(lktest$Chisq[1], pse_R1, sc_mod1),
                  'Model 2' = c(lktest$Chisq[2], pse_R2, sc_mod2))
results %>% 
  gt() %>% 
  fmt_number(
    columns = everything(),
    decimal = 1
  ) %>% 
  cols_label(
    term = 'Metrics',
    `Model 1` = 'Model 1',
    `Model 2` = 'Model 2') %>% 
  tab_footnote('The Likelihood ratio test for Model 1 is not applicable as it is the most basic model in this analysis.\n*** P < 0.01') %>% 
  text_transform(
    locations = cells_body(
      columns = `Model 2`,
      rows = term == "Likelihood Ratio Test"
    ),
    fn = function(x) paste0(x, '***')
  )
#| echo: false
#| message: false
#| warning: false
#| fig-align: 'center'
#| fig-cap-location: top
#| fig-cap: 'Predictied Party Choice in 2023 Finnish Parliamentary Election'
#| label: fig-pred-graph

library(marginaleffects)
library(scales)
# visualization of predicted prob of different parties by education level
# prepare new data
grid_opnion <- expand_grid(gndr ='Male',
                           edlvdfi = as_factor(c(0, 1)),
                           agea = mean(ess2023_fi$agea),
                           imbgeco = seq(0, 10, 0.5))

# label fransformer
education_labeller <- function(lst){
  for(i in 1:length(lst)){
    if(lst[i] == 0){
      return('Without Higher Education')
    } 
    else
    {
      return('With Higher Education')
    }
  }}

# plot
predictions(mod2, newdata = grid_opnion) %>% 
  mutate(gender = if_else(gndr == 1, "Female", "Male")) %>% 
  ggplot(aes(imbgeco, estimate, color = group)) +
  theme_light() +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group,
                  fill = group), 
              alpha = 0.1, color = NA) +
  facet_wrap(vars(edlvdfi), labeller = as_labeller(c(`0` = "Without", `1` = "With Higher Education"))) +
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0, 10)) +
  labs(subtitle = "Estimates from Multinomial Logistic Regression",
       y = "Predicted probabilities",
       x = 'Opnions on Immigrants',
       color = 'Party',
       caption = 'Data from ESS 2023') +
  guides(fill = guide_legend(title = "Party"))
