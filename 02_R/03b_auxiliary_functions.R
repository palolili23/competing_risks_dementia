mytheme <- theme_minimal(base_family = "serif") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black"),
    strip.text = element_text(size = 12, color = "black"),
    plot.caption = element_text(hjust = 0))



risks_cif <- function(model){
  model %>% 
    broom::tidy() %>%
    filter(state == "1") %>%
    group_by(strata) %>% 
    slice(n()) %>% 
    select(estimate, strata) %>% 
    pivot_wider(names_from = strata, values_from = estimate) %>% 
    mutate(rd = .[[2]] - .[[1]],
           rr = .[[2]] / .[[1]]) %>% 
    mutate_at(c(1:3), ~.*100)
  }



plot_cif <- function(model, title, ...){
  
  tidy <- model %>% 
    broom::tidy() %>% 
    filter(state == "1") %>%
    select(time, strata, estimate, conf.high, conf.low) %>%
    rename(CIF = estimate) %>% 
    mutate(strata = ifelse(strata == "smoke_dic=0", "Never smoked", "Ever smoked")) %>% 
    filter(
      (strata == "Never smoked" & time %in% c(13, seq(24, 240, 12)))|
      (strata == "Ever smoked" & time %in% c(seq(12, 240, 12)))) %>% 
    arrange(strata, time) %>% 
    group_by(strata) %>% 
    mutate(time = row_number()) %>% ungroup()
             
  
  plot <- tidy %>% 
    ggplot(aes(time, CIF, group = strata)) +
    geom_line(aes(color = strata), size = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
    scale_color_manual(values = c("#011A5E", "#e4a803")) +
    scale_y_continuous(limits = c(0, 0.70)) +
    labs(
      title = paste0(title),
      color = NULL,
      y = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(plot)
  
}



# Bootstraps code ---------------------------------------------------------

risks_boots <- function(data, n,
                        seed = 123,
                        model, weight = FALSE){
  # Set seed
  set.seed(seed)
  
  # Creates bootsamples and runs the model to each sample
  bootsamps <- replicate(n = n, expr = {
    d <- sample(1:nrow(data),size = nrow(data), replace = T)
    ds_b <- data[d,] %>%
      mutate(id = row_number()) 
    output <- model(ds_b, weight)
    return(output)
  }, simplify = F)
  
  totalboot <- bind_rows(bootsamps)
  
  totalboot <- totalboot %>% 
    summarise(across(.cols =everything(), 
                     ~quantile(.x, probs = c(0.025, 0.975))))
  
  return(totalboot)
}


