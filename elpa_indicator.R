# ELPA Indicator
# Evan Kramer
# 10/9/2017

library(tidyverse)
library(lubridate)
library(ggplot2)
library(haven)
library(readxl)
library(stringr)

rm(list = ls())
date = str_replace_all(as.character(today()), "-", "")
setwd("C:/Users/CA19130/Documents/Data/EL")

dat = F
sta = F
sys = F
sch = F
sav = F
rel = F
che = T

# Read in and clean data - ADD SAFE HARBOR FOR STUDENTS WHO EXIT (AND DON'T MEET GROWTH STANDARDS)???
if(dat == T) {
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
    
    ## Student level file
    student_level = select(filter(f, year == 2017), id, composite_pl2017 = composite_pl,
                           growth_standard2017 = growth_standard) %>% 
        left_join(select(filter(f, year == 2016), id, composite_pl2016 = composite_pl,
                         growth_standard2016 = growth_standard), by = "id") %>% 
        left_join(select(filter(f, year == 2015), id, composite_pl2015 = composite_pl,
                         growth_standard2015 = growth_standard), by = "id") %>% 
        mutate(growth_1yr = composite_pl2017 - composite_pl2016,
               growth_2yr = composite_pl2017 - composite_pl2015,
               growth_standard_1yr = growth_standard2016,
               growth_standard_2yr = growth_standard2015 + growth_standard2016, #is this accounting for NAs properly?
               met_growth_1yr = growth_1yr >= growth_standard_1yr,
               met_growth_2yr = growth_2yr >= growth_standard_2yr,
               met_growth_2yr_prev_standard = composite_pl2017 - composite_pl2015 >= 1.4,
               numerator = met_growth_1yr == T | met_growth_2yr == T,
               denominator = !is.na(met_growth_1yr)) %>% 
        left_join(transmute(wida2017, id = `State Student ID`,
                            system = as.integer(str_replace(`District Number`, "TN", "")),
                            school = as.integer(`School Number`)),
                  by = "id") %>% 
        ### Merge on demographics
        left_join(transmute(read_dta("K:/Research and Policy/ORP_Data/Student_Information/Student_Demographics/Cleaned_Files/StudentDemographics2017.dta"),
                            year = school_year + 1, id = as.integer(studentid),
                            race_ethnicity = ifelse(white == "Y", "White", NA),
                            race_ethnicity = ifelse(asian == "Y", "Asian", race_ethnicity),
                            race_ethnicity = ifelse(nativehawaiian == "Y", "Hawaiian or Pacific Islander", race_ethnicity),
                            race_ethnicity = ifelse(americanindian == "Y", "Native American or Alaska Native", race_ethnicity),
                            race_ethnicity = ifelse(black == "Y", "Black or African American", race_ethnicity),
                            race_ethnicity = ifelse(ethnicity == "H", "Hispanic or Latino", race_ethnicity),
                            ed = homeless == "Y" | migrant == "Y" | runaway == "Y" | directcertecondis == "Y",
                            el = englishlearner == "Y" | waivedel == "Y",
                            swd = prim_sld == "Y" | prim_intellectdis == "Y" | prim_speechimp == "Y" | 
                                prim_langimp == "Y" | prim_emotionaldis == "Y" | prim_autism == "Y" | 
                                prim_otherhealthimp == "Y" | prim_orthimp == "Y" | prim_deaf == "Y" | 
                                prim_hearingimp == "Y" | prim_blind == "Y" | prim_visualimp == "Y" | 
                                prim_deafblind == "Y" | prim_multipledis == "Y" | prim_devdelay == "Y" | prim_tbi == "Y"),
                  by = "id")
}

# State level percentages of students meeting growth standards
if(sta == T) {
    ## All 
    all = student_level %>% 
        summarize(subgroup = "All Students",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) 
    
    ## BHN
    bhn = student_level %>% 
        filter(race_ethnicity %in% c("Black or African American", "Hispanic or Latino", "Native American or Alaska Native")) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) 
    
    ## ED
    ed = student_level %>% 
        filter(ed == T) %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) 
    ## EL - should we use code below, or just take all students?
    el = student_level %>% 
        filter(el == T) %>% 
        summarize(subgroup = "English Learners",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) 

    ## SWD
    swd = student_level %>% 
        filter(swd == T) %>% 
        summarize(subgroup = "Students with Disabilities",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) 
    
    ## Bind all rows 
    state_level = bind_rows(all, bhn, ed, el, swd) %>% 
        mutate(system = 0) %>% 
        arrange(system, subgroup) %>% 
        left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_system_name_crosswalk.dta"), 
                  by = "system") %>% 
        select(starts_with("system"), everything())
    state_level = replace(state_level, is.na(state_level), NA)
    
    ## Output file
    write_csv(state_level, "K:/ORP_accountability/data/2017_ELPA/state_elpa.csv", na = "")
    
    state_level = student_level %>% 
        summarize(n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup() %>% 
        mutate(elpa_pts = ifelse(valid_tests >= 10 & pct_met_growth < 25, 0, NA),
               elpa_pts = ifelse(valid_tests >= 10 & pct_met_growth >= 25 & pct_met_growth < 40, 1, elpa_pts),
               elpa_pts = ifelse(valid_tests >= 10 & pct_met_growth >= 40 & pct_met_growth < 50, 2, elpa_pts),
               elpa_pts = ifelse(valid_tests >= 10 & pct_met_growth >= 50 & pct_met_growth < 60, 3, elpa_pts),
               elpa_pts = ifelse(valid_tests >= 10 & pct_met_growth >= 60, 4, elpa_pts))
}

# District level percentages of students meeting growth standards
if(sys == T) {
    ## All 
    all = student_level %>% 
        group_by(system) %>% 
        summarize(subgroup = "All Students",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## BHN
    bhn = student_level %>% 
        filter(race_ethnicity %in% c("Black or African American", "Hispanic or Latino", "Native American or Alaska Native")) %>% 
        group_by(system) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## ED
    ed = student_level %>% 
        filter(ed == T) %>% 
        group_by(system) %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## EL - should we use code below, or just take all students?
    el = student_level %>% 
        filter(el == T) %>% 
        group_by(system) %>% 
        summarize(subgroup = "English Learners",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## SWD
    swd = student_level %>% 
        filter(swd == T) %>% 
        group_by(system) %>% 
        summarize(subgroup = "Students with Disabilities",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## Bind all rows 
    district_level = bind_rows(all, bhn, ed, el, swd) %>% 
        mutate(system = as.numeric(system)) %>% 
        arrange(system, subgroup) %>% 
        left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_system_name_crosswalk.dta"), 
                  by = "system") %>% 
        select(starts_with("system"), everything())
    district_level = replace(district_level, is.na(district_level), NA)
    
    ## Output file
    write_csv(district_level, "K:/ORP_accountability/data/2017_ELPA/system_elpa.csv", na = "")
}

# School level percentages of students meeting growth standards
if(sch == T) {
    ## All 
    all = student_level %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "All Students",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## BHN
    bhn = student_level %>% 
        filter(race_ethnicity %in% c("Black or African American", "Hispanic or Latino", "Native American or Alaska Native")) %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## ED
    ed = student_level %>% 
        filter(ed == T) %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## EL - should we use code below, or just take all students?
    el = student_level %>% 
        filter(el == T) %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "English Learners",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## SWD
    swd = student_level %>% 
        filter(swd == T) %>% 
        group_by(system) %>% 
        summarize(subgroup = "Students with Disabilities",
                  n_met_growth = sum(numerator, na.rm = T),
                  valid_tests = sum(denominator, na.rm = T),
                  pct_met_growth = round(100 * sum(numerator, na.rm = T) /
                                             sum(denominator, na.rm = T), 1)) %>% 
        ungroup()
    
    ## Bind all rows 
    district_level = bind_rows(all, bhn, ed, el, swd) %>% 
        mutate(system = as.numeric(system)) %>% 
        arrange(system, school, subgroup) %>% 
        left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_school_crosswalk.dta"), 
                  by = "system") %>% 
        select(starts_with("system"), starts_with("school"), everything())
    school_level = replace(school_level, is.na(school_level), NA)
    
    ## Output file
    write_csv(school_level, "K:/ORP_accountability/data/2017_ELPA/school_elpa.csv", na = "")
}

# Output file
if(sav == T) {
    output = bind_rows(state_level, district_level, school_level) %>% 
        mutate_each(funs(ifelse(is.na(.), 0, .)), system, school) %>% 
        left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/2016_school_crosswalk.dta"), 
                  by = c("system", "school")) %>% 
        mutate(system_name = dendextend::na_locf(system_name),
               school_name = ifelse(system == 190 & school == 296, "East Nashville Middle", school_name),
               school_name = ifelse(system == 190 & school == 8053, "", school_name),
               school_name = ifelse(system == 190 & school == 8105, "KA @ The Crossings", school_name),
               school_name = ifelse(system == 220 & school == 37, "Dickson Intermediate School", school_name),
               school_name = ifelse(system == 570 & school == 22, "Jackson Central-Merry Early College High", school_name),
               school_name = ifelse(system == 570 & school == 24, "Rose Hill School", school_name),
               school_name = ifelse(system == 710 & school == 17, "Baxter Primary", school_name),
               school_name = ifelse(system == 792 & school == 8160, "Nexus STEM Academy Middle School", school_name),
               school_name = ifelse(system == 792 & school == 8165, "Memphis STEM Academy", school_name),
               school_name = ifelse(system == 792 & school == 8175, "Aspire East Academy", school_name),
               school_name = ifelse(system == 940 & school == 71, "Mill Creek Elementary School", school_name),
               school_name = ifelse(system == 940 & school == 72, "Mill Creek Middle School", school_name),
               school_name = ifelse(system == 985 & school == 8130, "Memphis Scholars Raleigh-Egypt", school_name),
               school_name = ifelse(system == 985 & school == 8135, "Kirby Middle School", school_name),
               school_name = ifelse(system == 985 & school == 8140, "Hillcrest High School", school_name))
    write_csv(elpa, "K:/ORP_accountability/projects/Evan/EL/elpa_indicator.csv", na = "")    
}

# Release system and school level files
if(rel == T) {
    student_level_2017 = left_join(
        read_csv("K:/ORP_accountability/projects/Jessica/Data Returns/Data/WIDA/WIDA_student_level2017_formatted.csv"),
        read_tsv("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_classification_JUIH only.txt") %>% 
            group_by(STUDENT_KEY) %>% 
            summarize(ed = 1) %>% 
            ungroup() %>% 
            rename(`State Student ID` = STUDENT_KEY), by = "State Student ID") %>% 
        mutate_each(funs(ifelse(is.na(.), 0, 1)), starts_with("Race"), 
                    `Ethnicity - Hispanic/Latino`, `Primary Disability`, ed) %>% 
        transmute(system = `System Number`, school = `School Number`, id = `State Student ID`,
                  pl_lit_2017 = `Literacy Proficiency Level`, pl_comp_2017 = `Composite (Overall) Proficiency Level`,
                  pl_lit_2016 = `Prior Composite Performance Level New Standard`, 
                  pl_comp_2016 = `Prior Composite Scale Score New Standard`,
                  reported_race = ifelse(`Race - White` == 1, "White", "None Reported"),
                  reported_race = ifelse(`Race - Asian` == 1, "Asian", reported_race),
                  reported_race = ifelse(`Race - Pacific Islander/Hawaiian` == 1, "Native Hawaiian or Pacific Islander", reported_race),
                  reported_race = ifelse(`Race - American Indian/Alaska Native` == 1, "American Indian or Alaska Native", reported_race),
                  reported_race = ifelse(`Race - Black/African American` == 1, "Black or African American", reported_race),
                  reported_race = ifelse(`Ethnicity - Hispanic/Latino` == 1, "Hispanic", reported_race), 
                  swd = `Primary Disability`, ed) %>% 
        arrange(id, desc(pl_comp_2017), desc(pl_lit_2017)) %>%
        group_by(id) %>%
        summarize_each(funs(first(.)), system, school, starts_with("pl_"), contains("race"), swd, ed) %>%
        ungroup() %>% 
        mutate(valid_tests_growth = !is.na(pl_comp_2016) & !is.na(pl_comp_2017),
               valid_tests_exit = !is.na(pl_lit_2017) & !is.na(pl_comp_2017),
               n_met_exit = valid_tests_exit == T & pl_lit_2017 >= 4.0 & pl_comp_2017 >= 4.2,
               growth_standard = ifelse(valid_tests_growth == T & pl_comp_2016 <= 1.4, 1.3, NA),
               growth_standard = ifelse(valid_tests_growth == T & between(pl_comp_2016, 1.5, 1.9), 0.7, growth_standard),
               growth_standard = ifelse(valid_tests_growth == T & between(pl_comp_2016, 2.0, 2.4), 0.8, growth_standard),
               growth_standard = ifelse(valid_tests_growth == T & between(pl_comp_2016, 2.5, 2.9), 0.7, growth_standard),
               growth_standard = ifelse(valid_tests_growth == T & between(pl_comp_2016, 3.0, 3.4), 0.4, growth_standard),
               growth_standard = ifelse(valid_tests_growth == T & between(pl_comp_2016, 3.5, 3.9), 0.5, growth_standard),
               growth_standard = ifelse(valid_tests_growth == T & pl_comp_2016 >= 4, 0.4, growth_standard),
               n_met_growth = valid_tests_growth == T & (n_met_exit == T | pl_comp_2017 - pl_comp_2016 >= growth_standard)) 
    
    # School Level
    ## All Students
    all = student_level_2017 %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), starts_with("valid_tests"), starts_with("n_")) %>% 
        ungroup() %>% 
        mutate(subgroup = "All Students", 
               pct_met_growth = round(100 * n_met_growth / valid_tests_growth, 1),
               pct_met_growth = round(100 * n_met_exit / valid_tests_exit, 1))
    
    ## BHN 
    bhn = student_level_2017 %>% 
        filter(reported_race %in% c("Black or African American", "Hispanic", "American Indian or Alaska Native")) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), starts_with("valid_tests"), starts_with("n_")) %>% 
        ungroup() %>% 
        mutate(subgroup = "Black/Hispanic/Native American", 
               pct_met_growth = round(100 * n_met_growth / valid_tests_growth, 1),
               pct_met_growth = round(100 * n_met_exit / valid_tests_exit, 1))
    
    ## ED
    ed = student_level_2017 %>% 
        filter(ed == 1) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), starts_with("valid_tests"), starts_with("n_")) %>% 
        ungroup() %>% 
        mutate(subgroup = "Black/Hispanic/Native American", 
               pct_met_growth = round(100 * n_met_growth / valid_tests_growth, 1),
               pct_met_growth = round(100 * n_met_exit / valid_tests_exit, 1))
    
    ## EL
    el = mutate(all, subgroup = "English Learners") 
    
    ## SWD
    swd = student_level_2017 %>% 
        filter(swd == 1) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), starts_with("valid_tests"), starts_with("n_")) %>% 
        ungroup() %>% 
        mutate(subgroup = "Students with Disabilities", 
               pct_met_growth = round(100 * n_met_growth / valid_tests_growth, 1),
               pct_met_growth = round(100 * n_met_exit / valid_tests_exit, 1))
    
    ## Individual races/ethnicities
    ind_race = as.tbl(data.frame())
    race_list = sort(unique(student_level_2017$reported_race))
    for(r in seq_along(race_list)) {
        temp = student_level_2017 %>% 
            filter(reported_race == race_list[r]) %>% 
            group_by(system, school) %>% 
            summarize_each(funs(sum(., na.rm = T)), starts_with("valid_tests"), starts_with("n_")) %>% 
            ungroup() %>% 
            mutate(subgroup = race_list[r], 
                   pct_met_growth = round(100 * n_met_growth / valid_tests_growth, 1),
                   pct_met_growth = round(100 * n_met_exit / valid_tests_exit, 1))
        
        ind_race = bind_rows(ind_race, temp)
    }
    
    ## Bind all rows
    output = bind_rows(all, bhn, el, swd, ind_race) %>% 
        arrange(system, school, subgroup) %>% 
        select(system, school, subgroup, everything())
    
    write_csv(output, "K:/ORP_accountability/data/2017_ELPA/school_elpa_EK.csv", na = "")
}

# Check against Jessica's files
if(che == T) {
    setwd("K:/ORP_accountability/data/2017_ELPA")
    
    # School
    jw = read_dta("school_level_elpa_JW.dta")
    ek = read_csv("school_elpa_EK.csv")
    sl = read_csv("K:/ORP_accountability/projects/Jessica/Data Returns/Data/WIDA/WIDA_student_level2017_formatted.csv")
        
    check = full_join(
        filter(ek, subgroup %in% c("All Students", "Black/Hispanic/Native American", 
                                   "English Learners", "Students with Disabilities")) %>% 
            mutate(subgroup = ifelse(subgroup == "English Learners", "English Language Learners", subgroup)),
        filter(jw, str_detect(subgroup, "Non-") == F) %>% 
            mutate_each(funs(as.integer(.)), system, schoolnumber),
        by = c("system", "school" = "schoolnumber", "subgroup")) %>% 
        select(system, school, subgroup, n_validtests_growth, valid_tests_growth, valid_tests, valid_tests_exit) %>%
        filter(n_validtests_growth != valid_tests_growth) %>% 
        filter(subgroup != "Students with Disabilities")
    
    
    
    a = left_join(
        read_csv("K:/ORP_accountability/projects/Jessica/Data Returns/Data/WIDA/WIDA_student_level2017_formatted.csv"),
        read_tsv("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_classification_JUIH only.txt") %>% 
            group_by(STUDENT_KEY) %>% 
            summarize(ed = 1) %>% 
            ungroup() %>% 
            rename(`State Student ID` = STUDENT_KEY), by = "State Student ID") %>% 
        mutate_each(funs(ifelse(is.na(.), 0, 1)), starts_with("Race"), 
                    `Ethnicity - Hispanic/Latino`, `Primary Disability`, ed) %>% 
        transmute(system = `System Number`, school = `School Number`, id = `State Student ID`,
                  pl_lit_2017 = `Literacy Proficiency Level`, pl_comp_2017 = `Composite (Overall) Proficiency Level`,
                  pl_lit_2016 = `Prior Composite Performance Level New Standard`, 
                  pl_comp_2016 = `Prior Composite Scale Score New Standard`,
                  reported_race = ifelse(`Race - White` == 1, "White", "None Reported"),
                  reported_race = ifelse(`Race - Asian` == 1, "Asian", reported_race),
                  reported_race = ifelse(`Race - Pacific Islander/Hawaiian` == 1, "Native Hawaiian or Pacific Islander", reported_race),
                  reported_race = ifelse(`Race - American Indian/Alaska Native` == 1, "American Indian or Alaska Native", reported_race),
                  reported_race = ifelse(`Race - Black/African American` == 1, "Black or African American", reported_race),
                  reported_race = ifelse(`Ethnicity - Hispanic/Latino` == 1, "Hispanic", reported_race), 
                  swd = `Primary Disability`, ed) %>% 
        arrange(id, desc(pl_comp_2017), desc(pl_lit_2017))  
    
    b = a %>% 
        group_by(id) %>% 
        summarize_each(funs(first(.)), system, school, starts_with("pl_"), contains("race"), swd, ed) %>%
        ungroup()
    
    inner_join(a, b, by = "id")
}
