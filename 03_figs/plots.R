# figure_data <- list(dem_adjusted, death_adjusted, km_adjusted_conditional)
# saveRDS(figure_data, here::here("02_R", "figs.rds"))

figs <- readRDS(here::here("02_R", "figs.rds"))


cif <- figs[[1]]
death <- figs[[2]]
km <- figs[[3]]

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
                           "Quit smoking", "Continued smoking")) %>%
    arrange(strata, time) %>%
    complete(strata, time) %>%
    group_by(strata) %>%
    fill(cif, conf.high, conf.low) %>% 
    filter((strata == "Quit smoking" &
              time %in% c(seq(0, 240, 12))) |
             (strata == "Continued smoking" &
                time %in% c(seq(0, 240, 12)))) %>%
    mutate(time = row_number() - 1,
           model = "A)") %>% ungroup()

  tidy_death <- death %>%
    broom::tidy() %>%
    filter(state == "1") %>%
    select(time, strata, estimate, conf.high, conf.low) %>%
    rename(cif = estimate) %>%
    bind_rows(time_zero) %>%
    mutate(strata = ifelse(strata == "smoke_dic=1",
                           "Quit smoking", "Continued smoking")) %>%
    arrange(strata, time) %>%
    complete(strata, time) %>%
    group_by(strata) %>%
    fill(cif, conf.high, conf.low) %>% 
    filter((strata == "Quit smoking" &
              time %in% c(seq(0, 240, 12))) |
             (strata == "Continued smoking" &
                time %in% c(seq(0, 240, 12)))) %>%
    mutate(time = row_number() - 1,
           model = "C)") %>% ungroup()
  
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
                      "Quit smoking", "Continued smoking")) %>% 
    mutate(model = "B)")
  
  output <- bind_rows(tidy_cif, tidy_km, tidy_death)
  
  return(output)
}

# Plots after adjusting for confounding -----------------------------------

all_models <-
  bind_all_models(cif = cif, 
                  death = death,
                  km = km)

panel <- all_models %>% 
  ggplot(aes(time, cif, group = strata)) +
  geom_line(aes(linetype = strata), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
  # scale_color_manual(values = c("#011A5E", "#e4a803")) +
  scale_y_continuous(limits = c(0, 0.70)) +
  facet_wrap(.~model, scales = "free") +
  labs(
    linetype = "Intervention",
    y = "Cumulative Incidence",
    x = "Years of Follow-up") +
  theme_bw() +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size = 12),
        strip.text.x = element_text(size = 12, hjust = 0),
        panel.border = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.position = c(0.09,0.85) )

A <- all_models %>% 
  filter(model == "A)") %>% 
  ggplot(aes(time, cif, group = strata)) +
  geom_line(aes(linetype = strata), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
  # scale_color_manual(values = c("#011A5E", "#e4a803")) +
  scale_y_continuous(limits = c(0, 0.70)) +
  labs(
    linetype = "Intervention",
    y = "Cumulative Incidence",
    x = "Years of Follow-up",
    title = "A)") +
  theme_bw() +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size = 12),
        strip.text.x = element_text(size = 12, hjust = 0),
        panel.border = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.position = c(0.3,0.85) )

A

B <- all_models %>% 
  filter(model == "B)") %>% 
  ggplot(aes(time, cif, group = strata)) +
  geom_line(aes(linetype = strata), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
  # scale_color_manual(values = c("#011A5E", "#e4a803")) +
  scale_y_continuous(limits = c(0, 0.70)) +
  labs(
    linetype = NULL,
    y = "Cumulative Incidence",
    x = "Years of Follow-up",
    title = "B") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 12, hjust = 0),
        panel.border = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))
B

C <- all_models %>% 
  filter(model == "C)") %>% 
  ggplot(aes(time, cif, group = strata)) +
  geom_line(aes(linetype = strata), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
  # scale_color_manual(values = c("#011A5E", "#e4a803")) +
  scale_y_continuous(limits = c(0, 0.70)) +
  labs(
    linetype = NULL,
    y = "Cumulative Incidence",
    x = "Years of Follow-up",
    title = "C)") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 12, hjust = 0),
        panel.border = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))
C


ggsave(filename = "A.pdf",
       plot = A,
       path = here::here("03_figs"),
       device = "pdf",
       dpi = "retina")

ggsave(filename = "A.tiff",
       plot = A,
       path = here::here("03_figs"),
       device = "tiff",
       dpi = "retina")

ggsave(filename = "B.tiff",
       plot = B,
       path = here::here("03_figs"),
       device = "tiff",
       dpi = "retina")

ggsave(filename = "B.pdf",
       plot = B,
       path = here::here("03_figs"),
       device = "pdf",
       dpi = "retina")


ggsave(filename = "C.tiff",
       plot = C,
       path = here::here("03_figs"),
       device = "tiff",
       dpi = "retina")

ggsave(filename = "C.pdf",
       plot = C,
       path = here::here("03_figs"),
       device = "pdf",
       dpi = "retina")

ggsave(filename = "panel.tiff",
       plot = panel,
       path = here::here("03_figs"),
       device = "tiff",
       width = 9,
       height = 4.1,
       dpi = "retina")
ggsave(filename = "panel.pdf",
       plot = panel,
       path = here::here("03_figs"),
       device = "pdf",
       width = 9,
       height = 4.1,
       dpi = "retina")


