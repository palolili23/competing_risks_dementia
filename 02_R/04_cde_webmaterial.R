#### Direct effect
library(rio)
library(lubridate)
library(tidyverse)
library(magrittr)
library(survival)
library(splines)

## Import data and functions

data_long <- import(here::here("01_data", "dementia_long.RData"))

data_long %>% 
  count(outcome_plr)

## If competing_plr = 1, then outcome_plr = NA

data_long %<>% 
  mutate(outcome_plr = ifelse(competing_plr == 1, NA, outcome_plr))

## Add baseline data
data <- import(here::here("01_data", "wide_noltfu.RData"))

smoke_baseline <- data %>% 
  mutate(smoke_dic = ifelse(smoke1 == 1, 1, 0)) %>% 
  select(id, smoke_dic, sbp1, bmi1, ht1, cohort)

data_long %<>%
  left_join(
    smoke_baseline
  )

data_long %<>% 
  group_by(id) %>% 
  mutate(
    fuptime = time - 1,
    tstart = lag(fuptime),
    tstart = ifelse(is.na(tstart), -1, tstart)) %>% 
  ungroup()

source(here::here("02_R", "04b_auxiliary_fx.R"))



risks_boot_long <- function(data_long, n, seed, ipcw = FALSE){
  # Set seed
  set.seed(seed)
  id_vector <-data_long %>% distinct(id) %>% pull(id)
  n_size <- length(id_vector)
  
  # Creates bootsamples and runs the model to each sample
  bootsamps <- replicate(n = n, expr = {
    d <- sample(id_vector,size = n_size, replace = T)
    ds_b <- data_long %>% filter(id %in% d)
    output <- cde_dem(ds_b, ipcw) 
    return(output)
  }, simplify = F)
  
  totalboot <- bind_rows(bootsamps)
  
  totalboot <- totalboot %>% 
    summarise(across(.cols =everything(), 
                     ~quantile(.x, probs = c(0.025, 0.975))))
  
  return(totalboot)
}




#### Function to estimate CDE conditional on time-fixed cov & CDE unconditional

cde_dem <- function(data_long, ipcw = FALSE) {
  
  smoke_den <-
    glm(smoke_dic ~ bs(age_0) + sex + education + apoe4 + cohort,
        data = data_long,
        family = binomial)
  
  data_long <- data_long %>%
    mutate(
      pa_denom = predict(smoke_den, type = "response"),
      w_smoke = ifelse(smoke_dic == 1, 1 / pa_denom, 1/(1 - pa_denom)))
  
  if (ipcw != FALSE) {
    
    death_den_bl <-
      glm(
        competing_plr ~ smoke_dic + bs(age_0, 3) + sex + education + apoe4 + 
          as.factor(diabetes_prev) + bs(sbp1, 3) + bs(bmi1, 3) + ht1 + cohort,
        data = data_long,
        family = quasibinomial
      )
    
    data_long$p_denom = predict(death_den_bl, data_long, type = "response")

    
    data_long %<>%
      group_by(id) %>%
      mutate(sw_bl = 1 / cumprod(1 - p_denom),
      ) %>% 
      ungroup()
    
    data_long <- data_long %>%
      mutate(sw_bl = ifelse((
        sw_bl > quantile(sw_bl, 0.99)
      ), quantile(sw_bl, 0.99), sw_bl))
    
    data_long %<>%
      mutate(both_weights = sw_bl * w_smoke)
    
    
    dem_model <-
      survfit(
        Surv(tstart, fuptime, outcome_plr) ~ smoke_dic,
        data = data_long,
        cluster = id,
        weights = both_weights
      )
  }
  else{
    dem_model <-
      survfit(
        Surv(tstart, fuptime, outcome_plr) ~ smoke_dic,
        data = data_long,
        cluster = id,
        weights = w_smoke
      )
  }
  
  output <- risks_km(dem_model)
  
  return(output)
}


cde_baseline_est <- cde_dem(data_long, ipcw = TRUE)
cde_unconditional_est <- cde_dem(data_long, ipcw = FALSE)

### Bootstraps

cde_baseline <- risks_boot_long(data_long, n = 500, seed = 123, ipcw = TRUE)

cde_unconditional <- risks_boot_long(data_long, n = 500, seed = 123, ipcw = FALSE)

web_sup <- list(cde_baseline_est, cde_baseline, cde_unconditional_est,
                cde_unconditional)

save(web_sup, file = here::here("02_R", "websup.RData"))
