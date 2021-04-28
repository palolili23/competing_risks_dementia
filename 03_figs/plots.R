####

bind_all_models <- function(cif, death, km) {
  tidy_cif <- cif %>%
    broom::tidy() %>%
    filter(state == "1") %>%
    select(time, strata, estimate, conf.high, conf.low) %>%
    rename(cif = estimate) 
  
  time_zero <- tidy_cif %>% 
    group_by(strata) %>% 
    slice(5) %>% 
    ungroup() %>% 
    mutate(across(where(is.numeric), ~.*0))
  
  tidy_cif %<>%
    bind_rows(time_zero) %>% 
    mutate(strata = ifelse(strata == "smoke_dic=1", 
                           "Former smokers", "Current smokers")) %>%
    arrange(strata, time) %>%
    complete(strata, time) %>%
    group_by(strata) %>%
    fill(cif, conf.high, conf.low) %>% 
    filter((strata == "Former smokers" &
              time %in% c(seq(0, 240, 12))) |
             (strata == "Current smokers" &
                time %in% c(seq(0, 240, 12)))) %>%
    mutate(time = row_number() - 1,
           model = "A. Total effect on dementia risk") %>% ungroup()

  tidy_death <- death %>%
    broom::tidy() %>%
    filter(state == "1") %>%
    select(time, strata, estimate, conf.high, conf.low) %>%
    rename(cif = estimate) %>%
    bind_rows(time_zero) %>%
    mutate(strata = ifelse(strata == "smoke_dic=1",
                           "Former smokers", "Current smokers")) %>%
    arrange(strata, time) %>%
    complete(strata, time) %>%
    group_by(strata) %>%
    fill(cif, conf.high, conf.low) %>% 
    filter((strata == "Former smokers" &
              time %in% c(seq(0, 240, 12))) |
             (strata == "Current smokers" &
                time %in% c(seq(0, 240, 12)))) %>%
    mutate(time = row_number() - 1,
           model = "C. Effect on mortality") %>% ungroup()
  
  tidy_km <- km %>%
    broom::tidy() %>%
    transmute(
      time = time,
      strata = strata,
      cif = 1 - estimate,
      conf.low2 = 1 - conf.low,
      conf.high2 = 1 - conf.high
    ) %>%
    rename(conf.high = conf.low2,
           conf.low = conf.high2) %>%
    mutate(
      strata = ifelse(strata == "smoke_dic=1",
                      "Former smokers", "Current smokers")) %>% 
    mutate(model = "B. Direct effect on dementia risk")
  
  output <- bind_rows(tidy_cif, tidy_km, tidy_death)
  
  return(output)
}


# all_unadjusted_models ---------------------------------------------------


all_models_unadj <- bind_all_models(cif = dem_crude, death = death_crude, 
                              km_crude_conditional)

plot_unadj <- all_models_unadj %>% 
  mutate(model = 
    case_when(
      model == "A. Total effect on dementia risk" ~ 
        "A. Dementia risk \n without elimination of death",
      model == "B. Direct effect on dementia risk" ~ 
        "B. Dementia risk \n with elimination of death",
      model == "C. Effect on mortality" ~ 
        "C. Risk of death",
    )
  ) %>% 
  ggplot(aes(time, cif, group = strata)) +
  geom_line(aes(color = strata), size = 0.70) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
  scale_color_manual(values = c("#011A5E", "#e4a803")) +
  scale_y_continuous(limits = c(0, 0.70)) +
  facet_wrap(.~model) +
  labs(
    color = NULL,
    y = NULL,
    x = "Years of follow-up") +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12)) +
  theme(strip.text.x = element_text(size = 11),
        # strip.background = element_blank(),
        strip.background = element_rect(fill=NA),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

ggsave(filename = "plot_unadjusted.tiff",
       plot = plot_unadj,
       path = here::here("03_figs"),
       device = "tiff",
       width = 8,
       height = 4.1,
       dpi = "retina")

# Plots after adjusting for confounding -----------------------------------

all_models <-
  bind_all_models(cif = dem_adjusted, 
                  death = death_adjusted,
                  km = km_adjusted_conditional)

plot_adjusted <- all_models %>% 
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
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12)) +
  theme(strip.text.x = element_text(size = 11),
        # strip.background = element_blank(),
        strip.background = element_rect(fill=NA),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

ggsave(filename = "plot_adjusted.tiff",
       plot = plot_adjusted,
       path = here::here("03_figs"),
       device = "tiff",
       width = 8,
       height = 4.1,
       dpi = "retina")

plot_unadj
plot_adjusted
