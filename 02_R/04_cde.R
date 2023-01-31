#### Direct effect
library(rio)
library(lubridate)
library(tidyverse)
library(magrittr)
library(survival)
library(WeightIt)
library(cobalt)
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

# Weights smoke-----------------------------------------------------------------

smoke_den <-
  glm(
    smoke_dic ~ bs(age_0, 3) + sex + education + apoe4 + cohort,
    data = data_long,
    family = binomial
  )

data_long <- data_long %>% 
  mutate(
    p_denom = predict(smoke_den, type = "response"),
    w_smoke = ifelse(smoke_dic == 1, 1/p_denom, 1/(1- p_denom)))

summary(data_long$w_smoke)


# Weights for death -------------------------------------------------------

### baseline_weights --------------------------------------------------------

death_den_bl <-
  glm(
    competing_plr ~ smoke_dic + bs(age_0, 3) + sex + education + apoe4 + 
      as.factor(diabetes_prev) + bs(sbp1, 3) + bs(bmi1, 3) + ht1 + cohort,
    data = data_long,
    family = quasibinomial
  )

death_den_bl %>% broom::tidy(exponentiate = TRUE) %>% View()

data_long$p_denom = predict(death_den_bl, data_long, type = "response")

data_long %<>%
  group_by(id) %>%
  mutate(sw_bl = 1/ cumprod(1 - p_denom),
  ) %>% 
  ungroup()

data_long <- data_long %>%
  mutate(sw_bl = ifelse((
    sw_bl > quantile(sw_bl, 0.99)
  ), quantile(sw_bl, 0.99), sw_bl))

data_long %>% ggplot(aes(x = as_factor(time), y = sw_bl)) +
  geom_boxplot()

# T-v weights -------------------------------------------------------------

death_den <-
  glm(
    competing_plr ~ smoke_dic * bs(time, 3) + bs(age_0, 3) + sex + education + apoe4 + 
     as.factor(diabetes_prev) + bs(sbp1, 3) + bs(bmi1, 3) + ht1 + cohort + 
      bs(sbp, 3) + bs(bmi, 3) + hd_v + cancer_v + stroke_v + diab_v,
    data = data_long,
    family = quasibinomial
  )

summary(death_den)

# death_den %>% broom::tidy(exponentiate = TRUE) %>% View()

data_long$p_denom = predict(death_den, data_long, type = "response")


data_long %<>%
  group_by(id) %>%
  mutate(sw = 1 / cumprod(1 - p_denom),
  ) %>% 
  ungroup()

data_long <- data_long %>%
  mutate(sw = ifelse((
    sw > quantile(sw, 0.99)
  ), quantile(sw, 0.99), sw))

data_long %>% ggplot(aes(x = as_factor(time), y = sw)) +
  geom_boxplot()


# Crude risks -------------------------------------------------------------

km_crude_unconditional <-
  survfit(
    Surv(
      time = tstart,
      time2 = fuptime,
      event = outcome_plr
    ) ~ smoke_dic,
    data = data_long,
    cluster = id
  )

# km_crude_unconditional_plot<- plot_km(km_crude_unconditional, "Direct effect") + 
#   labs(subtitle = "Without IPTW, unconditional censoring")

risks_km(km_crude_unconditional)


# With IPTW, no IPCW ------------------------------------------------------

km_iptw <-
  survfit(
    Surv(
      time = tstart,
      time2 = fuptime,
      event = outcome_plr
    ) ~ smoke_dic,
    data = data_long,
    cluster = id,
    weights = w_smoke
  )

# km_crude_conditional_plot<- plot_km(km_crude_conditional, "Direct effect") + 
#   labs(subtitle = "Without IPTW, conditional on time-varying covariates")

risks_km(km_iptw)

km_iptw %>% broom::tidy() %>% group_by(strata) %>% slice(n())

# With baseline IPCW ---------------------------------------------------------------

data_long <- 
  data_long %>% 
  mutate(both_weights_bl = w_smoke*sw_bl)

km_ipcw_bl <-
  survfit(
    Surv(
      time = tstart,
      time2 = fuptime,
      event = outcome_plr
    ) ~ smoke_dic,
    data = data_long,
    cluster = id,
    weights = both_weights_bl
  )

# km_crude_conditional_plot<- plot_km(km_crude_conditional, "Direct effect") + 
#   labs(subtitle = "Without IPTW, conditional on time-varying covariates")

risks_km(km_ipcw_bl)

km_ipcw_bl %>% broom::tidy() %>% group_by(strata) %>% slice(n())

# With t-v IPCW ----------------------------------------------------------------

data_long <- 
  data_long %>% 
  mutate(both_weights = w_smoke*sw)

km_adjusted_conditional <-
  survfit(
    Surv(tstart, fuptime, outcome_plr
    ) ~ smoke_dic,
    data = data_long,
    cluster = id,
    weights = both_weights
  )

km_adjusted_conditional_plot<- plot_km(km_adjusted_conditional, "Direct effect") +
  labs(subtitle = "With IPTW, conditional on time-varying covariates")

risks_km(km_adjusted_conditional)


# Bootstrap function ------------------------------------------------------


cde_dem <- function(data_long, iptw = FALSE) {
  death_den <-
    glm(
      competing_plr ~ smoke_dic * bs(time, 3) + bs(age_0, 3) + sex + education + apoe4 +
        as.factor(diabetes_prev) + bs(sbp1, 3) + bs(bmi1, 3) + ht1 + cohort +
        bs(sbp, 3) + bs(bmi, 3) + hd_v + cancer_v + stroke_v + diab_v,
      data = data_long,
      family = quasibinomial
    )


  data_long$p_denom = predict(death_den, data_long, type = "response")

  data_long %<>%
    group_by(id) %>%
    mutate(sw = 1 / cumprod(1 - p_denom),
    ) %>%
    ungroup()

  data_long <- data_long %>%
    mutate(sw = ifelse((
      sw > quantile(sw, 0.99)
    ), quantile(sw, 0.99), sw))

  if (iptw != FALSE) {
    smoke_den <-
      glm(smoke_dic ~ bs(age_0) + sex + education + apoe4 + cohort,
          data = data_long,
          family = binomial)

    data_long <- data_long %>%
      mutate(
        pa_denom = predict(smoke_den, type = "response"),
        w_smoke = ifelse(smoke_dic == 1, 1 / pa_denom, 1/(1 - pa_denom))
      )

    data_long %<>%
      mutate(both_weights = sw * w_smoke)


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
        weights = sw
      )
  }

  output <- risks_km(dem_model)

  return(output)
}

cde_dem(data_long, iptw = TRUE)

# Bootstrap ---------------------------------------------------------------

# cde_unadjusted <- risks_boot_long(data_long, n = 500, seed = 123, iptw = FALSE)

cde_adjusted <- risks_boot_long(data_long, n = 500, seed = 123, iptw = TRUE)

saveRDS(list(km_adjusted_conditional, cde_adjusted), here::here("02_R", "direct_effects.rds"))


