library(rio)
library(lubridate)
library(tidyverse)
library(here)
library(magrittr)
library(zoo)

# Import first cohort -----------------------------------------------------

data <- import(here::here("01_data", "db_wide.RData")) %>% 
  filter(rs_cohort == 1) %>% 
  select(-education_three_levels)

educ <- import(here::here("01_data", "educ.Rdata")) %>% 
  mutate(education = as_factor(education),
         rs_cohort = as.double(rs_cohort))

data <- data %>% 
  left_join(educ, by = c("ergoid", "rs_cohort"))

data <- data %>%  
  filter(!is.na(e1)) %>% 
  filter(age_0 <= 80) %>% 
  # filter(mmse1 >= 26) %>% 
  filter(dementia_inc != 2) %>% 
  filter(!is.na(sbp1)) %>%
  filter(!is.na(bmi1)) %>%
  filter(!is.na(smoke1)) %>% 
  # filter(!is.na(ht_drug1)) %>% 
  filter(!is.na(censor_date)) %>% 
  # filter(!is.na(education)) %>% 
  rename(id = ergoid, cohort = rs_cohort) 

rm(educ)

# Death and censored 2015 -------------------------------------------------

data <- data %>%
  mutate(
    death = ifelse(!is.na(mort_date), 1, 0),
    end_fup_2015 = as_date(ifelse(
      censor_date > ymd("2015-12-23"),
      ymd("2015-12-23"),
      censor_date
    )),
    death_2015 = ifelse(mort_date > ymd("2015-12-23") &
                          !is.na(mort_date), 0, death))


data %<>%
  mutate(
    t2death = round(as.numeric(as.period((e1 %--% end_fup_2015), "months"
    ), "months"), 0),
    t2dem = round(as.numeric(as.period((e1 %--% dementia_date), "months"
    ), "months"), 0),
    t2stroke = round(as.numeric(as.period((e1 %--% stroke_date), "months"
    ), "months"), 0))

data %<>%
  mutate(
    t2death_y = round(as.numeric(as.period((e1 %--% end_fup_2015), "years"
    ), "years"), 3),
    t2dem_y = round(as.numeric(as.period((e1 %--% dementia_date), "years"
    ), "years"), 3),
    t2stroke_y = round(as.numeric(as.period((e1 %--% stroke_date), "years"
    ), "years"), 3))

data %<>%
  mutate(
    t2death_d = round(as.numeric(as.period((e1 %--% end_fup_2015), "days"
    ), "days"), 0),
    t2dem_d = round(as.numeric(as.period((e1 %--% dementia_date), "days"
    ), "days"), 0),
    t2stroke_d = round(as.numeric(as.period((e1 %--% stroke_date), "days"
    ), "days"), 0))


## Dementia: if dementia date is not empty and date is less tha the time of administrative 
## censoring assings 1. 

data <- data %>%
  mutate(
    dementia = case_when(
      !is.na(dementia_date) & dementia_date <= end_fup_2015 ~ 1,
      death_2015 == 1  &
        (is.na(dementia_date) | dementia_date > end_fup_2015) ~ 2,
      TRUE ~ 0
    ),
    t2dem = ifelse((is.na(dementia_date) | dementia_date > end_fup_2015), t2death, t2dem),
    t2dem_y = ifelse((is.na(dementia_date) |dementia_date > end_fup_2015), t2death_y, t2dem_y),
    t2dem_d = ifelse((is.na(dementia_date) |dementia_date > end_fup_2015), t2death_d, t2dem_d),
  ) %>% 
  filter(round(t2death_y,0) > 1)


### Missing data

data %>% count(is.na(education))
data %>% count(is.na(apoe4))
data %>% count(is.na(sbp1))
data %>% count(is.na(bmi1))
data %>% count(is.na(ht_drug1))
data %>% count(is.na(hd_prev))
data %>% count(cancer_prev)
data %>% count(ht1)

data %<>% 
  mutate(
    education = ifelse(is.na(education), "Unknown", education),
    apoe4 = ifelse(is.na(apoe4),"Unknown" , apoe4),
    ht_drug1 = ifelse(is.na(ht_drug1), 2, ht_drug1),
    hd_prev = ifelse(is.na(hd_prev), 2, hd_prev),)




# Wide truncated 20 years -------------------------------------------------

data %>% summarise(max(t2dem))

data <- data %>% 
  mutate(
    dementia_20 = ifelse(t2dem > 240, 0, dementia),
    death_20 = ifelse(t2death > 240, 0, death_2015))
    
data <- data %>% 
  mutate(
    t2dem_20 = ifelse(t2dem > 240, 240, t2dem),
    t2death_20 = ifelse(t2death > 240, 240, t2death))


data %>% 
  count(dementia, dementia_20)

data %>% 
  count(death, death_20)

export(data, here::here("01_data", "wide_noltfu.Rdata"))

# Long format -------------------------------------------------------------

data_long <- data %>% 
  select(id, dementia, death_2015, end_fup_2015, starts_with("t2"), startdat, dementia_date,
         sex, age_0, education, apoe4, hd_prev, hd_date, diabetes_prev, diabetes_date, 
         cancer_prev, cancer_date, stroke_prev, stroke_date) %>% 
  filter(t2death>= 1) %>%
  mutate(t2death_y = round(t2death_y,0)) %>% 
  group_by(id) %>% 
  slice(rep(1:n(), each = (241))) %>% #21 for years, 241 for months, repeat rows by #years until death + 1 since rounding will count some as one year before year of outcome
  ungroup() %>%
  select(everything(), - matches("[1,2,3,4,5]$"), death_2015, end_fup_2015, apoe4) %>% ## delete all covariates
  group_by(id) %>%
  mutate(year = year(startdat),
         year = year + (row_number() - 1)) %>% #an easy roll for years + 1
  ungroup()

### Create the repeated measurement data separate, by visit
rep_cov <- data %>% 
  select(id, 
         starts_with("e"),
         starts_with("bmi"), 
         starts_with("ht"), 
         starts_with("chol"),
         starts_with("mmse"),
         starts_with("oh"),
         starts_with("db_med"),
         starts_with("smoke"),
         starts_with("ht_drug"),
         starts_with("sbp"),
         starts_with("hdl"),
         -c(end_fup_2015, education)) %>% 
  pivot_longer(-1,
               names_to = c(".value", "visit"),
               names_sep = -1,
               values_drop_na = TRUE) %>%
  arrange(id, visit) %>% 
  drop_na(e) %>% #some have values on covariates but no date of visit
  mutate(year = year(e))

rep_cov %>% 
  select(-id, - year, - e) %>% 
  summarise_all(~sum(is.na(.x))) 

rep_cov %>% 
  select(-id, - year, - e) %>% 
  summarise_all(~sum(is.na(.x))/n()) 

#### merge and fill
var_names <- colnames(rep_cov)

data_long <- data_long %>%
  left_join(rep_cov, by = c("id", "year")) %>%
  group_by(id) %>%
  fill(var_names[-1]) %>% 
  ungroup()

data_long %>% 
  select(-id, - year, - e) %>% 
  summarise_all(~sum(is.na(.x))/n()) %>%
  pivot_longer(everything(), names_to = "var", values_to = "missing") %>% 
  gt::gt()


# Make indicators of disease ----------------------------------------------

data_long <- data_long %>% 
  group_by(id) %>% 
  mutate(v1_bin = 1,
         v2_bin = ifelse((visit == 2), 1, NA),
         v2_bin = na.locf(v2_bin, na.rm =  FALSE),
         v2_bin = ifelse(is.na(v2_bin), 0, v2_bin),
         v3_bin = ifelse((visit == 3), 1, NA),
         v3_bin = na.locf(v3_bin, na.rm = FALSE),
         v3_bin = ifelse(is.na(v3_bin), 0, v3_bin),
         v4_bin = ifelse((visit == 4), 1, NA),
         v4_bin = na.locf(v4_bin, na.rm = FALSE),
         v4_bin = ifelse(is.na(v4_bin), 0, v4_bin)) %>% 
  ungroup()


data_long %<>% 
  group_by(id) %>% 
  mutate(
    hd_v = ifelse(hd_prev == 1, 1, NA),
    hd_date = as.numeric(year(hd_date)), 
    hd_v = ifelse((!is.na(hd_date) & hd_date == year), 1, hd_v),
    hd_v = na.locf(hd_v, na.rm = FALSE),
    hd_v = ifelse(is.na(hd_v), 0, hd_v),
    diab_v = ifelse(diabetes_prev == 1, 1, NA),
    diabetes_date = as.numeric(year(diabetes_date)), 
    diab_v = ifelse((!is.na(diabetes_date) & diabetes_date == year), 1, diab_v),
    diab_v = na.locf(diab_v, na.rm = FALSE),
    diab_v = ifelse(is.na(diab_v), 0, diab_v),
    stroke_v = ifelse(stroke_prev == 1, 1, NA),
    stroke_date = as.numeric(year(stroke_date)), 
    stroke_v = ifelse((!is.na(stroke_date) & stroke_date == year), 1, stroke_v),
    stroke_v = na.locf(stroke_v, na.rm = FALSE),
    stroke_v = ifelse(is.na(stroke_v), 0, stroke_v),
    cancer_v = ifelse(cancer_prev == 1, 1, NA),
    cancer_date = as.numeric(year(cancer_date)),
    cancer_v = ifelse((!is.na(cancer_date) & cancer_date == year), 1, cancer_v),
    cancer_v = na.locf(cancer_v, na.rm = FALSE),
    cancer_v = ifelse(is.na(cancer_v),0, cancer_v),
    death_year = as.numeric(year(end_fup_2015)),
    death_v = ifelse(death_year == year & death_2015 == 1, 1, NA),
    death_v = na.locf(death_v, na.rm = FALSE),
    death_v = ifelse(is.na(death_v),0, death_v))

data_long %<>% 
  select(
    id, year, startdat, end_fup_2015, death_2015, dementia, t2dem_y, t2death_y,
    contains("hd_"), contains("cancer_"), contains("stroke_"), 
    contains("stroke_"), everything(), - contains("date"), dementia_date)


#### For long I only need rows until dementia, not until death

data_long_dem <- data_long %>%
  group_by(id) %>% 
  arrange(id, year) %>% 
  mutate(end_dementia_death = ifelse(!is.na(dementia_date), 
                                     as.numeric(year(dementia_date)),
                                     as.numeric(year(end_fup_2015))),
         time = row_number()) %>%
  filter(year <= end_dementia_death,
         time <= 20) %>%
  ungroup() %>% 
  select(id, year, time, end_dementia_death, everything())

data_long_dem %<>%
  group_by(id) %>%
  mutate(
    outcome_plr = ifelse(end_dementia_death == year & dementia == 1, 1, 0),
    competing_plr = ifelse(!is.na(dementia_date), 0, death_v)) %>%
  ungroup() %>%
  arrange(id, time)

data_long_dem %>% 
  count(outcome_plr, competing_plr)

data %>% count(dementia_20)

####
dem_a <- data_long_dem %>% filter(outcome_plr == 1) %>% pull(id)
dem_b <- data %>% filter(dementia_20 == 1) %>% pull(id)

dif <- setdiff(dem_b, dem_a)

data %>% filter(id %in% dif) %>% View()

data_long_dem %>% filter(id %in% dif) %>% View()

export(data_long_dem, here::here("01_data", "dementia_long.RData"))
