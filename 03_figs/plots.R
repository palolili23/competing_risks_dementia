####

bind_all_models <- function(cif, death, km){
  
  tidy_cif <- cif %>% 
  broom::tidy() %>% 
  filter(state == "1") %>%
  select(time, strata, estimate, conf.high, conf.low) %>%
  rename(cif = estimate) %>% 
  mutate(strata = ifelse(strata == "smoke_dic=0", "Never smoked", "Ever smoked")) %>% 
  filter(
    (strata == "Never smoked" & time %in% c(13, seq(24, 240, 12)))|
      (strata == "Ever smoked" & time %in% c(seq(12, 240, 12)))) %>% 
  arrange(strata, time) %>% 
  group_by(strata) %>% 
  mutate(time = row_number(),
         model = "A. Total effect on dementia risk") %>% ungroup()
  
  tidy_death <- death %>% 
    broom::tidy() %>% 
    filter(state == "1") %>%
    select(time, strata, estimate, conf.high, conf.low) %>%
    rename(cif = estimate) %>% 
    mutate(strata = ifelse(strata == "smoke_dic=0", "Never smoked", "Ever smoked")) %>% 
    filter(
      (strata == "Never smoked" & time %in% c(13, seq(24, 240, 12)))|
        (strata == "Ever smoked" & time %in% c(seq(12, 240, 12)))) %>% 
    arrange(strata, time) %>% 
    group_by(strata) %>% 
    mutate(time = row_number(),
           model = "C. Effect on mortality") %>% ungroup()

tidy_km <- km %>% 
  broom::tidy() %>% 
  transmute(
    time = time, 
    strata = ifelse(strata == "smoke_dic=0", "Ever smoked", "Never smoked"),
    cif = 1 - estimate,
    conf.low2 = 1- conf.low,
    conf.high2 = 1 - conf.high) %>% 
  rename(conf.high = conf.low2,
         conf.low = conf.high2) %>% 
  mutate(model = "B. Direct effect on dementia risk")

output <- bind_rows(tidy_cif, tidy_km, tidy_death)

return(output)
}


# all_unadjusted_models ---------------------------------------------------


all_models_unadj <- bind_all_models(dem_crude, death_crude, 
                              km_crude_conditional)

all_models_unadj %>% 
  ggplot(aes(time, cif, group = strata)) +
  geom_line(aes(color = strata), size = 0.7) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
  scale_color_manual(values = c("#011A5E", "#e4a803")) +
  scale_y_continuous(limits = c(0, 0.70)) +
  facet_wrap(.~model) +
  labs(
    color = NULL,
    y = NULL,
    x = "Years of follow-up") +
  theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = "bottom")


# Plots after adjusting for confounding -----------------------------------

all_models <- bind_all_models(dem_adjusted, death_adjusted, 
                              km_adjusted_conditional)

all_models %>% 
  ggplot(aes(time, cif, group = strata)) +
  geom_line(aes(color = strata), size = 0.7) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
  scale_color_manual(values = c("#011A5E", "#e4a803")) +
  scale_y_continuous(limits = c(0, 0.70)) +
  facet_wrap(.~model) +
  labs(
    color = NULL,
    y = NULL,
    x = "Years of follow-up") +
  theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = "bottom")

