#### Total effect
library(rio)
library(lubridate)
library(tidyverse)
library(magrittr)
library(survival)
library(WeightIt)
library(cobalt)
library(splines)

## Import data and functions

data <- import(here::here("01_data", "wide_noltfu.RData"))
source(here::here("02_R", "03b_auxiliary_functions.R"))

data %<>%
  mutate(smoke_dic = ifelse(smoke1 == 1, 1, 0))

# Weights for smoking --------------------------------------------------

smoke_den <-
  glm(
    smoke_dic ~ bs(age_0, 3) + sex + education + apoe4 + cohort,
    data = data,
    family = binomial
  )

summary(smoke_den)

smoke_num <- glm(smoke_dic ~ 1, data = data)

summary(smoke_num)

data <- data %>%
  mutate(
    p_num = predict(smoke_num, type = "response"),
    p_denom = predict(smoke_den, type = "response"),
    w_smoke = ifelse(smoke_dic == 1, p_num / p_denom, (1 - p_num) / (1 - p_denom))
  )


## Check standardized mean

w.out1 <-
  weightit(
    smoke_dic ~ bs(age_0, 3) + sex + education + apoe4 + cohort,
    data = data,
    stabilize = TRUE,
    estimand = "ATE",
    method = "ps"
  )

w.out1

data <- data %>%
  mutate(ps_w = w.out1$weights)

data %>%
  ggplot(aes(ps_w, w_smoke, color = smoke_dic)) +
  geom_point()

summary(data$w_smoke)

# plot_weights <- love.plot(w.out1) +
#   scale_color_manual(values = c("#011A5E", "#e4a803")) +
#   labs(title = "Covariate balance for current vs. former smoking") +
#   theme_bw(base_family = "serif") +
#   theme(legend.position = "bottom",
#         legend.text = element_text(size=12)) +
#   theme(strip.text.x = element_text(size = 11),
#         # strip.background = element_blank(),
#         strip.background = element_rect(fill=NA),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12))
# 
# ggsave(filename = "loveplot_smoking.tiff",
#        plot = plot_weights,
#        path = here::here("03_figs"),
#        device = "tiff",
#        width = 8,
#        height = 8.1,
#        dpi = "retina")


# 1. Total effect in dementia risk ----------------------------------------------------------


# 1.1. No adjustment for confounding --------------------------------------

dem_crude <-
  survfit(Surv(t2dem_20, as.factor(dementia_20)) ~ smoke_dic, data)

model <- dem_crude

risks_cif(dem_crude)


# 1.2. With IPTW for confounding ------------------------------------------

dem_adjusted <-
  survfit(Surv(t2dem_20, as.factor(dementia_20)) ~ smoke_dic, data, weights = w_smoke)

risks_cif(dem_adjusted)

dem_adjusted_plot <-
  plot_cif(dem_adjusted, title = "Total effect of smoking in the risk of dementia") +
  labs(subtitle = "With IPTW")

# 2. Mortality --------------------------------------------------------------------

# 2.1. No adjustment for confounding --------------------------------------

death_crude <-
  survfit(Surv(t2death_20, as.factor(death_20)) ~ smoke_dic, data)

risks_cif(death_crude)


# 2.2. With IPTW for confounding ------------------------------------------


death_adjusted <-
  survfit(Surv(t2death_20, as.factor(death_20)) ~ smoke_dic, weights = w_smoke, data)

risks_cif(death_adjusted)

#3. Bootstrap confidence intervals total effect dementia -----------------------------------------

total_effect_dem <- function(data, weight = FALSE){

  if(weight != FALSE){
  smoke_den <- glm(smoke_dic ~ bs(age_0) + sex + education + apoe4 + cohort, data = data, family = binomial)

  smoke_num <- glm(smoke_dic ~ 1, data = data)

  data <- data %>%
    mutate(
      p_num = predict(smoke_num, type = "response"),
      p_denom = predict(smoke_den, type = "response"),
      w_smoke = ifelse(smoke_dic == 1, p_num/p_denom, (1 - p_num)/(1- p_denom)))

  dem_model <- survfit(Surv(t2dem_20,as.factor(dementia_20)) ~ smoke_dic, data, weights = w_smoke)}
  else{
    dem_model <- survfit(Surv(t2dem_20,as.factor(dementia_20)) ~ smoke_dic, data)
  }

  output <- risks_cif(dem_model)

  return(output)
  }

# ci_dem_crude <- risks_boots(data, 500, seed = 123, total_effect_dem, weight = FALSE)


ci_dem_weight <- risks_boots(data, 500, seed = 123, total_effect_dem, weight = TRUE)


# 4. Bootstrap confidence intervals death  ------------------------------------------------------

total_effect_death <- function(data, weight = FALSE){

  if(weight != FALSE){
    smoke_den <-
      glm(smoke_dic ~ bs(age_0) + sex + education + apoe4 + cohort,
          data = data,
          family = binomial)
    smoke_num <- glm(smoke_dic ~ 1, data = data)

    data <- data %>%
      mutate(
        p_num = predict(smoke_num, type = "response"),
        p_denom = predict(smoke_den, type = "response"),
        w_smoke = ifelse(smoke_dic == 1, p_num/p_denom, (1 - p_num)/(1- p_denom)))

    death_model <- survfit(Surv(t2death_20,as.factor(death_20)) ~ smoke_dic, data, weights= w_smoke)}
  else{
    death_model <- survfit(Surv(t2death_20,as.factor(death_20)) ~ smoke_dic, data)
  }
  output <- risks_cif(death_model)

  return(output)
}

# ci_death_crude <- risks_boots(data, 500, seed = 123, total_effect_death, weight = FALSE)

ci_death_weight <- risks_boots(data, 500, seed = 123, total_effect_death, weight = TRUE)

