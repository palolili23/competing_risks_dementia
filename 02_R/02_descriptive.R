library(kableExtra)
mytablestyle <- function(df){
  kable(df) %>% 
    kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),
                  full_width = F,
                  font_size = 14)}

## Descriptive analysis

data <- import(here::here("01_data", "wide_noltfu.RData"))

data %<>% 
  mutate(smoke_dic = ifelse(smoke1 == 1, 0, 1))

data %>% 
count(smoke_dic) %>% 
  mutate(prop = round(100*n/sum(n),1),
         total = paste0(n," (", prop, "%)"))

data %>% count(dementia_20) %>% 
  mutate(prop = round(100*n/sum(n),1),
         total = paste0(n," (", prop, "%)"))


counts <- data %>% 
  group_by(smoke_dic) %>% 
  count(dementia_20) %>% 
  mutate(prop = round(100*n/sum(n),1),
         total = paste0(n," (", prop, "%)"))

counts %>% 
  filter(dementia_20 != 0) %>% 
  select(-n, -prop)


data %>% group_by(dementia) %>% 
  summarise(dementia_median = median(t2dem_20/12),
            death_median = median(t2death_20/12))

data 
### Table 1
data_table1 <- data %>%
  select(age_0, sex, education, apoe4, smoke_dic, sbp1, ht1, bmi1, mmse1, 
         stroke_prev, hd_prev, diabetes_prev, cancer_prev, cancer_inc, hd_inc,
         stroke_inc) %>% 
  mutate(education = str_to_title(str_replace_all(education, "_", " ")),
         sex = ifelse(sex == 1, "Women", "Men")) %>%
  rename(Age = age_0,
         Education = education,
         Sex = sex,
         Hypertension = ht1,
         `Systolic Blood Pressure (mmHg)` = sbp1,
         `Body Mass Index` = bmi1)

library(tableone)

colnames(data_table1)
categorical <- colnames(data_table1)[c(2:5, 7, 10:17)]

table <-
  print(CreateTableOne(
    data = data_table1,
    factorVars = categorical,
    strata = "smoke_dic",
    test = FALSE
  ))

table_print <-
  print(
    table,
    showAllLevels = TRUE,
    quote = FALSE,
    noSpaces = TRUE,
    printToggle = FALSE
  )

mytablestyle(table_print)
