#### Total effect
library(rio)
library(lubridate)
library(tidyverse)
library(here)
library(magrittr)
library(zoo)
library(survival)
library(survminer)
library(patchwork)
library(WeightIt)
library(cobalt)
library(splines)
## Import data and functions

data_long <- import(here::here("01_data", "dementia_long.RData"))

data_long %>% 
  count(outcome_plr)

data <- import(here::here("01_data", "wide_noltfu.RData"))

smoke_baseline <- data %>% 
  mutate(smoke_dic = ifelse(smoke1 == 1, 0, 1)) %>% 
  select(id, smoke_dic, sbp1, bmi1, ht1, cohort)

### 
data_long %<>%
  left_join(
    smoke_baseline
  )

# data_long %>% 
#   count(smoke_dic, outcome_plr)

data_long <- data_long %>% 
  group_by(id) %>% 
  mutate(
    fuptime = time - 1,
    tstart = lag(fuptime),
    tstart = ifelse(is.na(tstart), -1, tstart)) %>% 
  ungroup()

source(here::here("02_R", "04b_auxiliary_fx.R"))
# Weights -----------------------------------------------------------------


smoke_den <-
  glm(
    smoke_dic ~ bs(age_0, 3) + sex + education + apoe4 + cohort,
    data = data_long,
    family = binomial
  )


summary(smoke_den)

smoke_num <- glm(smoke_dic ~ 1, data = data_long, family = quasibinomial)

# summary(smoke_num)

data_long <- data_long %>% 
  mutate(
    p_num = predict(smoke_num, type = "response"),
    p_denom = predict(smoke_den, type = "response"),
    w_smoke = ifelse(smoke_dic == 1, p_num/p_denom, (1 - p_num)/(1- p_denom)))

## Check standardized mean

# w.out1 <- weightit(smoke_dic ~ age_0 + I(age_0^2) + sex + education + hd_prev + ht1 + apoe4 + 
#                      sbp1 + I(sbp1^2)+ bmi1 + I(bmi1^2), data = data_long, 
#                    stabilize = TRUE, estimand = "ATE", method = "ps")
# w.out1
# 
# data_long <- data_long %>% 
#   mutate(ps_w = w.out1$weights)
# 
# data_long %>% 
#   ggplot(aes(ps_w, w_smoke, color = smoke_dic)) +
#   geom_point()
# 
# summary(data$w_smoke)
# 
# love.plot(w.out1) +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = c("#011A5E", "#e4a803"))


# Weights for death -------------------------------------------------------

death_den <-
  glm(
    competing_plr ~ smoke_dic * bs(time, 3) + bs(age_0, 3) + sex + education + apoe4 + 
     as.factor(diabetes_prev) + bs(sbp1, 3) + bs(bmi1, 3) + ht1 + cohort + 
      bs(sbp, 3) + bs(bmi, 3) + hd_v + cancer_v + stroke_v + diab_v,
    data = data_long,
    family = quasibinomial
  )

summary(death_den)

death_den %>% broom::tidy(exponentiate = TRUE) %>% View()


death_num <-
  glm(
    competing_plr ~ smoke_dic * bs(time, 3) + bs(age_0, 3) + sex + education + apoe4 +
      as.factor(diabetes_prev) + bs(sbp1, 3) + bs(bmi1, 3) + ht1 + cohort,
    data = data_long,
    family = binomial
  )

data_long$p_denom = predict(death_den, data_long, type = "response")

data_long$p_num = predict(death_num, data_long, type = "response")

data_long %<>%
  group_by(id) %>%
  mutate(sw = cumprod(1 - p_num) / cumprod(1 - p_denom),
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



# With IPCW ---------------------------------------------------------------

km_crude_conditional <-
  survfit(
    Surv(
      time = tstart,
      time2 = fuptime,
      event = outcome_plr
    ) ~ smoke_dic,
    data = data_long,
    cluster = id,
    weights = sw
  )

# km_crude_conditional_plot<- plot_km(km_crude_conditional, "Direct effect") + 
#   labs(subtitle = "Without IPTW, conditional on time-varying covariates")

risks_km(km_crude_conditional)

km_crude_conditional %>% broom::tidy() %>% group_by(strata) %>% slice(n())

# Adjusted ----------------------------------------------------------------

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
# 
# cde_dem <- function(data_long, iptw = FALSE){
# 
#   death_den <- glm(competing_plr ~ smoke_dic*bs(time,3) + bs(age_0,3) + sex + education + apoe4 +
#                      bs(sbp,3) + bs(bmi,3 ) + hd_v + cancer_v +
#                      stroke_v + diab_v, data = data_long, family = quasibinomial)
# 
#   death_num <- glm(competing_plr ~ 1, data = data_long, family = quasibinomial)
# 
#   data_long$p_denom = predict(death_den, data_long, type = "response")
# 
#   data_long$p_num = predict(death_num, data_long, type = "response")
# 
#   data_long %<>%
#     group_by(id) %>%
#     mutate(sw = cumprod(1 - p_num) / cumprod(1 - p_denom),
#     ) %>%
#     ungroup()
# 
#   data_long <- data_long %>%
#     mutate(sw = ifelse((
#       sw > quantile(sw, 0.99)
#     ), quantile(sw, 0.99), sw))
# 
#   if(iptw != FALSE){
# 
#     smoke_den <- glm(smoke_dic ~ bs(age_0) + sex + education + apoe4, data = data_long, family = binomial)
# 
#     smoke_num <- glm(smoke_dic ~ 1, data = data_long)
# 
#     data_long <- data_long %>%
#       mutate(
#         pa_num = predict(smoke_num, type = "response"),
#         pa_denom = predict(smoke_den, type = "response"),
#         w_smoke = ifelse(smoke_dic == 1, pa_num/pa_denom, (1 - pa_num)/(1- pa_denom)))
# 
#     data_long %<>%
#       mutate(both_weights = sw*w_smoke)
# 
# 
#     dem_model <- survfit(Surv(tstart, fuptime, outcome_plr) ~ smoke_dic,
#         data = data_long, cluster = id, weights = both_weights)}
#   else{
#     dem_model <-  survfit(Surv(tstart, fuptime, outcome_plr) ~ smoke_dic,
#                           data = data_long, cluster = id, weights = sw)}
# 
#   output <- risks_km(dem_model)
# 
#   return(output)
# }
# 
# cde_dem(data_long, iptw = TRUE)
# # Bootstrap ---------------------------------------------------------------
# 
# cde_unadjusted <- risks_boot_long(data_long, n = 500, seed = 123, iptw = FALSE)
# 
# cde_adjusted <- risks_boot_long(data_long, n = 500, seed = 123, iptw = TRUE)

