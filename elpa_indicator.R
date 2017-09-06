# ELPA Indicator
# Evan Kramer
# 9/6/2017

library(tidyverse)
library(lubridate)
library(ggplot2)
library(haven)
library(readxl)
library(stringr)

rm(list = ls())
date = str_replace_all(as.character(today()), "-", "")
setwd("C:/Users/CA19130/Documents/Data/EL")

# Read in and clean data
## WIDA files
wida2017 = read_csv("K:/Assessment_Data Returns/ACCESS for ELs and ALT/2016-17/TN_Summative_Stud_File.csv")
wida2016 = read_dta("K:/ORP_accountability/data/2016_WIDA_Access/wida_access_scores_2016_with_state_id.dta")

## Historical achievement
h = read_dta("historical_elpa_crosswalked_student_level.dta") %>% 
    bind_rows(read_csv("K:/Assessment_Data Returns/ACCESS for ELs and ALT/2016-17/TN_Summative_Stud_File.csv") %>% 
                  arrange(`State Student ID`, desc(`Composite (Overall) Proficiency Level`), desc(`Literacy Proficiency Level`)) %>% 
                  group_by(`State Student ID`) %>% 
                  summarize_each(funs(first(.)), Grade, `District Number`, `School Number`,
                                 `Literacy Proficiency Level`, `Composite (Overall) Proficiency Level`) %>% 
                  transmute(grade = as.numeric(Grade), id = `State Student ID`, 
                            system = as.numeric(str_replace(`District Number`, "TN", "")),
                            school = as.numeric(`School Number`), year = 2017, 
                            pl_lit = `Literacy Proficiency Level`,
                            pl_comp = `Composite (Overall) Proficiency Level`) %>% 
                  ungroup())

## Differentiated growth standards (based on prior year ELP)
growth_exp = wida2017 %>% 
    transmute(year = 2017, id = `State Student ID`, grade = as.integer(Grade), 
              literacy_pl = `Literacy Proficiency Level`,
              composite_pl = `Composite (Overall) Proficiency Level`) %>% 
    bind_rows(transmute(wida2016, year = 2016, id = statestudentid, grade, 
                        literacy_pl = as.numeric(literacy_pl_new_standard),
                        composite_pl = as.numeric(composite_pl_new_standard))) %>% 
    group_by(year, id) %>% 
    summarize(grade = min(grade, na.rm = T), composite_pl = max(composite_pl, na.rm = T)) %>% 
    ungroup() %>% 
    spread(year, composite_pl) %>% 
    mutate(growth = `2017` - `2016`,
           grade_band = ifelse(grade < 5, "elementary", NA),
           grade_band = ifelse(grade > 4 & grade < 9, "middle", grade_band),
           grade_band = ifelse(grade > 8, "high", grade_band),
           grade_band = factor(grade_band, levels = c("elementary", "middle", "high")),
           score_band = ifelse(`2016` >= 1 & `2016` < 1.5, "1.0-1.4", NA),
           score_band = ifelse(`2016` >= 1.5 & `2016` < 2, "1.5-1.9", score_band),
           score_band = ifelse(`2016` >= 2 & `2016` < 2.5, "2.0-2.4", score_band),
           score_band = ifelse(`2016` >= 2.5 & `2016` < 3, "2.5-2.9", score_band),
           score_band = ifelse(`2016` >= 3 & `2016` < 3.5, "3.0-3.4", score_band), 
           score_band = ifelse(`2016` >= 3.5 & `2016` < 4, "3.5-3.9", score_band),
           score_band = ifelse(`2016` >= 4 & `2016` < 4.5, "4.0-4.4", score_band),
           score_band = ifelse(`2016` >= 4.5 & `2016` < 5, "4.5-4.9", score_band)) %>% 
    #group_by(grade_band, score_band) %>% 
    group_by(score_band) %>% 
    summarize(median_score = median(growth, na.rm = T),
              score_60 = quantile(growth, 0.6, na.rm = T),
              score_70 = quantile(growth, 0.7, na.rm = T),
              n = n()) %>% 
    ungroup() %>% 
    filter(!is.na(score_band))

## Made growth over two years (but not one?)
f = wida2017 %>% 
    transmute(year = 2017, id = `State Student ID`, composite_pl = `Composite (Overall) Proficiency Level`) %>% 
    bind_rows(transmute(wida2016, year = 2016, id = as.integer(statestudentid), composite_pl = as.numeric(composite_pl_new_standard))) %>% 
    bind_rows(transmute(filter(h, year == 2015), year = 2015, id = as.integer(id), composite_pl = pl_comp)) %>% 
    group_by(year, id) %>% 
    summarize(composite_pl = max(composite_pl, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(score_band = ifelse(composite_pl >= 1 & composite_pl < 1.5, "1.0-1.4", NA),
           score_band = ifelse(composite_pl >= 1.5 & composite_pl < 2, "1.5-1.9", score_band),
           score_band = ifelse(composite_pl >= 2 & composite_pl < 2.5, "2.0-2.4", score_band),
           score_band = ifelse(composite_pl >= 2.5 & composite_pl < 3, "2.5-2.9", score_band),
           score_band = ifelse(composite_pl >= 3 & composite_pl < 3.5, "3.0-3.4", score_band), 
           score_band = ifelse(composite_pl >= 3.5 & composite_pl < 4, "3.5-3.9", score_band),
           score_band = ifelse(composite_pl >= 4 & composite_pl < 4.5, "4.0-4.4", score_band),
           score_band = ifelse(composite_pl >= 4.5 & composite_pl < 5, "4.5-4.9", score_band)) %>% 
    left_join(select(growth_exp, score_band, growth_standard = score_60), by = "score_band") %>% 
    select(-score_band) 

# Calculate percentages of students meeting growth standard
elpa = select(filter(f, year == 2017), id, composite_pl2017 = composite_pl,
                         growth_standard2017 = growth_standard) %>% 
    left_join(select(filter(f, year == 2016), id, composite_pl2016 = composite_pl,
                     growth_standard2016 = growth_standard), by = "id") %>% 
    left_join(select(filter(f, year == 2015), id, composite_pl2015 = composite_pl,
                     growth_standard2015 = growth_standard), by = "id") %>% 
    mutate(growth_1yr = composite_pl2017 - composite_pl2016,
           growth_2yr = composite_pl2017 - composite_pl2015,
           growth_standard_1yr = growth_standard2016,
           growth_standard_2yr = growth_standard2015 + growth_standard2016,
           met_growth_1yr = growth_1yr >= growth_standard_1yr,
           met_growth_2yr = growth_2yr >= growth_standard_2yr,
           met_growth_2yr_prev_standard = composite_pl2017 - composite_pl2015 >= 1.4,
           numerator = met_growth_1yr == T | met_growth_2yr == T,
           denominator = !is.na(met_growth_1yr)) %>% 
    left_join(transmute(wida2017, id = `State Student ID`,
                        system = as.integer(str_replace(`District Number`, "TN", "")),
                        school = as.integer(`School Number`)),
              by = "id") %>% 
    group_by(system) %>% 
    summarize(numerator = sum(numerator, na.rm = T),
              denominator = sum(denominator, na.rm = T),
              pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                         sum(denominator, na.rm = T), 1)) %>% 
    ungroup() 

# Calculate quintiles
quantile(elpa$pct_met_growth[elpa$denominator >= 10], probs = seq(0, 1, 0.2))

# Plot distributions
elpa %>%  
    filter(denominator >= 10) %>% 
    ggplot(aes(pct_met_growth)) + 
        geom_histogram() + 
        geom_vline(xintercept = quantile(elpa$pct_met_growth[elpa$denominator >= 10], probs = seq(0, 1, 0.2))[1]) + 
        geom_vline(xintercept = quantile(elpa$pct_met_growth[elpa$denominator >= 10], probs = seq(0, 1, 0.2))[2]) + 
        geom_vline(xintercept = quantile(elpa$pct_met_growth[elpa$denominator >= 10], probs = seq(0, 1, 0.2))[3]) + 
        geom_vline(xintercept = quantile(elpa$pct_met_growth[elpa$denominator >= 10], probs = seq(0, 1, 0.2))[4]) + 
        geom_vline(xintercept = quantile(elpa$pct_met_growth[elpa$denominator >= 10], probs = seq(0, 1, 0.2))[5])

# Output file
write_csv(elpa, "K:/ORP_accountability/projects/Evan/EL/elpa_indicator.csv", na = "")
