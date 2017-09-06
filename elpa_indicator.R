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
ltel = read_csv("elb_20170901.csv", col_types = cols("i", "i", "c", "c", "c")) %>% 
    filter(`English Language Background` %in% c("L", "W")) %>% 
    group_by(`Student Key`) %>% 
    mutate(first_lw = min(`School Year`, na.rm = T),
           last_lw = max(`School Year`, na.rm = T),
           ltel = last_lw - first_lw > 6) %>% 
    summarize(ltel = max(ltel, na.rm = T)) %>% 
    ungroup()

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

two_year_growth = select(filter(f, year == 2017), id, composite_pl2017 = composite_pl,
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
           met_growth_2yr_prev_standard = composite_pl2017 - composite_pl2015 >= 1.4) %>% 
    left_join(ltel, by = c("id" = "Student Key")) %>% 
    mutate(numerator = ifelse(ltel == 1, 1.5 * (met_growth_1yr == T | met_growth_2yr == T), 
                              met_growth_1yr == T | met_growth_2yr == T),
           denominator = !is.na(met_growth_1yr)) %>% 
    left_join(transmute(wida2017, id = `State Student ID`, 
                        system = as.integer(str_replace(`District Number`, "TN", "")),
                        school = as.integer(`School Number`)), 
              by = "id")
    
break

two_year_growth %>% 
    group_by(system) %>% 
    summarize(numerator = sum(numerator, na.rm = T),
              denominator = sum(denominator, na.rm = T),
              pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                         sum(denominator, na.rm = T), 1)) %>% 
    ungroup() %>% 
    filter(denominator >= 10) %>% 
    ggplot(aes(pct_met_growth)) + 
        geom_histogram()














# Calculate percent of students meeting growth standards
g = wida2017 %>% 
    mutate(id = as.numeric(`State Student ID`)) %>% 
    group_by(id) %>% 
    summarize(composite_pl2017 = max(`Composite (Overall) Proficiency Level`, na.rm = T)) %>% 
    ungroup() %>% 
    left_join(wida2016 %>% 
                  rename(id = statestudentid) %>% 
                  group_by(id) %>% 
                  summarize(composite_pl2016 = max(as.numeric(composite_pl_new_standard), na.rm = T)) %>% 
                  ungroup(),
              by = "id") %>%
    mutate(growth = composite_pl2017 - composite_pl2016,
           score_band = ifelse(composite_pl2016 >= 1 & composite_pl2016 < 1.5, "1.0-1.4", NA),
           score_band = ifelse(composite_pl2016 >= 1.5 & composite_pl2016 < 2, "1.5-1.9", score_band),
           score_band = ifelse(composite_pl2016 >= 2 & composite_pl2016 < 2.5, "2.0-2.4", score_band),
           score_band = ifelse(composite_pl2016 >= 2.5 & composite_pl2016 < 3, "2.5-2.9", score_band),
           score_band = ifelse(composite_pl2016 >= 3 & composite_pl2016 < 3.5, "3.0-3.4", score_band), 
           score_band = ifelse(composite_pl2016 >= 3.5 & composite_pl2016 < 4, "3.5-3.9", score_band),
           score_band = ifelse(composite_pl2016 >= 4 & composite_pl2016 < 4.5, "4.0-4.4", score_band),
           score_band = ifelse(composite_pl2016 >= 4.5 & composite_pl2016 < 5, "4.5-4.9", score_band)) %>% 
    left_join(growth_exp, by = "score_band") #%>% 
    #filter(!is.na())
    


# Percent of students meeting growth standard
wida2017 %>% 
    group_by(`State Student ID`) %>% 
    summarize(composite_pl2017 = max(`Composite (Overall) Proficiency Level`, na.rm = T)) %>% 
    ungroup() %>% 
    left_join(wida2016 %>% 
                  group_by(statestudentid) %>% 
                  summarize(composite_pl2016 = max(as.numeric(composite_pl_new_standard), na.rm = T)) %>% 
                  ungroup() %>% 
                  mutate(statestudentid = as.integer(statestudentid)),
              by = c("State Student ID" = "statestudentid")) %>% 
    mutate(valid_tests = !is.na(composite_pl2017) & !is.na(composite_pl2016),
           n_met_growth = valid_tests == T & composite_pl2017 - composite_pl2016 >= 0.7) %>% 
    summarize(valid_tests = sum(valid_tests, na.rm = T),
              n_met_growth = sum(n_met_growth, na.rm = T))


















break

# Generate variable for year expected to exit (and compare to actual exit)
e = h %>% 
    group_by(id) %>% 
    filter(year == min(year, na.rm = T)) %>% 
    mutate(initial_grade = min(grade, na.rm = T),
           year_exp_exit = 7 - round(pl_comp) + round(initial_grade / 12) + year) %>% # added provision for initial grade 
    select(id, year_exp_exit) %>% 
    right_join(h, by = "id") %>%
    mutate(exit_year = ifelse(pl_lit >= 5 & pl_comp >= 5, year, NA),
           most_recent_test = max(year, na.rm = T)) %>% 
    arrange(id, year)

p = e %>% 
    summarize(system = last(system),
              school = last(school),
              year_exp_exit = max(year_exp_exit, na.rm = T),
              exit_year = max(exit_year, na.rm = T),
              most_recent_test = max(most_recent_test, na.rm = T),
              valid_tests = sum(max(year_exp_exit == 2016 & (most_recent_test == 2016 | !is.na(exit_year)), na.rm = T))) %>% 
    ungroup() %>% 
    filter(valid_tests == 1) %>% 
    mutate(n_exit = !is.na(exit_year)) %>% 
    group_by(system, school) %>% 
    summarize(valid_tests = sum(valid_tests, na.rm = T),
              n_exit = sum(n_exit, na.rm = T)) %>% 
    mutate(pct_exit = round(100 * n_exit / valid_tests, 1)) %>%
    ungroup()

## Calculate quintiles
quantile(p$pct_exit[p$valid_tests >= 10], probs = c(20, 40, 60, 80) / 100)

## Plot
ggplot(filter(p, valid_tests >= 10), aes(pct_exit)) + 
    geom_histogram(bins = 30) + 
    geom_vline(xintercept = quantile(p$pct_exit[p$valid_tests >= 10], probs = c(20, 40, 60, 80) / 100)[[1]]) + 
    geom_vline(xintercept = quantile(p$pct_exit[p$valid_tests >= 10], probs = c(20, 40, 60, 80) / 100)[[2]]) + 
    geom_vline(xintercept = quantile(p$pct_exit[p$valid_tests >= 10], probs = c(20, 40, 60, 80) / 100)[[3]]) + 
    geom_vline(xintercept = quantile(p$pct_exit[p$valid_tests >= 10], probs = c(20, 40, 60, 80) / 100)[[4]])

# Generate variable for percent meeting growth standard (vary based on prior ELP and grade at entry)
g = h %>% 
    select(-pl_lit, -grade) %>% 
    filter(year %in% c(2015, 2016)) %>% 
    spread(year, pl_comp) %>%
    rename(initial = `2016`, subseq = `2016`) %>% 
    mutate(n_met_growth = ifelse(initial >= 1 & initial < 1.5, subseq - initial >= 1.6, NA),
           n_met_growth = ifelse(initial >= 1.5 & initial < 2, subseq - initial >= 1.6, n_met_growth),
           n_met_growth = ifelse(initial >= 2 & initial < 2.5, subseq - initial >= 1.2, n_met_growth),
           n_met_growth = ifelse(initial >= 2.5 & initial < 3, subseq - initial >= 0.8, n_met_growth),
           n_met_growth = ifelse(initial >= 3 & initial < 3.5, subseq - initial >= 0.7, n_met_growth),
           n_met_growth = ifelse(initial >= 3.5 & initial < 4, subseq - initial >= 0.5, n_met_growth),
           n_met_growth = ifelse(initial >= 4 & initial < 4.5, subseq - initial >= 0.3, n_met_growth),
           n_met_growth = ifelse(initial >= 4.5 & initial < 5, subseq - initial >= 0.1, n_met_growth)) %>% 
    group_by(system, school) %>% 
    summarize(valid_tests = sum(!is.na(initial) & !is.na(subseq)),
              n_met_growth = sum(n_met_growth, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(pct_met_growth = round(100 * n_met_growth / valid_tests, 1),
           growth_pts = ifelse(valid_tests >= 10 & pct_met_growth < 30, "F", NA),
           growth_pts = ifelse(valid_tests >= 10 & pct_met_growth >= 30 & pct_met_growth < 41.6, "D", growth_pts),
           growth_pts = ifelse(valid_tests >= 10 & pct_met_growth >= 41.6 & pct_met_growth < 51.3, "C", growth_pts),
           growth_pts = ifelse(valid_tests >= 10 & pct_met_growth >= 51.3 & pct_met_growth < 63, "B", growth_pts),
           growth_pts = ifelse(valid_tests >= 10 & pct_met_growth >= 63, "A", growth_pts))

## Calculate quintiles
quantile(g$pct_met_growth[g$valid_tests >= 10], probs = c(20, 40, 60, 80) / 100)

## Plot
ggplot(filter(g, valid_tests >= 10), aes(pct_met_growth)) + 
    geom_histogram(bins = 30) + 
    geom_vline(xintercept = 30) + 
    geom_vline(xintercept = 41.6) + 
    geom_vline(xintercept = 51.3) + 
    geom_vline(xintercept = 63)
