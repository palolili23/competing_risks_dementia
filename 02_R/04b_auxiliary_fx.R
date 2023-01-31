plot_km <- function(model, title, ...){
  tidy <- model %>% 
    broom::tidy() %>% 
    transmute(
      time = time, 
      strata = ifelse(strata == "smoke_dic=1", 
                             "Quit smoking", "Continued smoking"),
      cif = 1 - estimate,
      conf.low2 = 1- conf.low,
      conf.high2 = 1 - conf.high) %>% 
    rename(conf.high = conf.low2,
           conf.low = conf.high2)
  
  plot <- tidy %>% 
    ggplot(aes(time, cif, group = strata)) +
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

risks_km <- function(model){
  model %>% 
    broom::tidy() %>%
    group_by(strata) %>% 
    slice(n()) %>% 
    mutate(estimate = 1-estimate) %>% 
    select(estimate, strata) %>% 
    pivot_wider(names_from = strata, values_from = estimate) %>% 
    mutate(rd = .[[2]] - .[[1]],
           rr = .[[2]] / .[[1]]) %>% 
    mutate_at(c(1:3), ~.*100)} 



risks_boot_long <- function(data_long, n, seed, iptw = FALSE){
  # Set seed
  set.seed(seed)
  id_vector <-data_long %>% distinct(id) %>% pull(id)
  n_size <- length(id_vector)
  
  # Creates bootsamples and runs the model to each sample
  bootsamps <- replicate(n = n, expr = {
    d <- sample(id_vector,size = n_size, replace = T)
    ds_b <- data_long %>% filter(id %in% d)
    output <- cde_dem(ds_b, iptw) 
    return(output)
  }, simplify = F)
  
  totalboot <- bind_rows(bootsamps)
  
  totalboot <- totalboot %>% 
    summarise(across(.cols =everything(), 
                     ~quantile(.x, probs = c(0.025, 0.975))))
  
  return(totalboot)
}
