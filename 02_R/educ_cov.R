library(haven)

educ <- read_sav(here::here("01_data", "Education RS-I-II-III (UNESCO class)_(12-MAR-2015).sav"))

educ <- educ %>%
  rename(education = ses_UNESCO_recoded)

educ <- educ %>%
  mutate(education = labelled(
    education,
    c(
      `Primary education` = 0,
      `lower or intermediate general education OR lower vocational education` = 1,
      `intermediate vocational education OR higher general education` = 2,
      `higher vocational education OR university` = 3)
  ))

export(educ, here::here("01_data", "educ.Rdata"))
