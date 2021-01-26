## Descriptive analysis

data <- import(here::here("01_data", "wide_noltfu.RData"))

data %<>% 
  mutate(smoke_dic = ifelse(smoke1 == 0, 0, 1))

data %>% 
count(smoke_dic)

counts <- data %>% 
  group_by(smoke_dic) %>% 
  count(dementia) %>% 
  mutate(prop = round(100*n/sum(n),1),
         total = paste0(n," (", prop, "%)"))

counts
data %>% count(dementia)         

data %>% group_by(dementia) %>% 
  summarise(median = median(t2dem_y),
            death = median(t2death_y))

### Table 1
data_table1 <- data %>%
  select(age_0, sex, education, apoe4, smoke1, sbp1, ht1, bmi1, mmse1, 
         stroke_prev, hd_prev, diabetes_prev, cancer_prev, cancer_inc, hd_inc,
         stroke_inc) %>% 
  mutate(education = str_to_title(str_replace_all(education, "_", " ")),
         sex = ifelse(sex == 1, "Women", "Men"),
         smoke1 = str_to_title(smoke1)) %>%
  rename(Age = age_0,
         Education = education,
         Sex = sex,
         Smoke = smoke1,
         Hypertension = ht1,
         `Systolic Blood Pressure (mmHg)` = sbp1,
         `Body Mass Index` = bmi1)

library(tableone)

colnames(data_table1)
categorical <- colnames(data_table1)[c(2:5, 7, 10:17)]

print(CreateTableOne(data = data_table1, factorVars = categorical))

#### make counts of incident things