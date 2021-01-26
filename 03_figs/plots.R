dem_crude_plot <- plot_cif(
  dem_crude, "Risk of dementia among ever vs. never smokers") +
  labs(subtitle = "Without elimination of death" )

death_crude_plot <- plot_cif(death_crude, "Risk of death among ever vs. never smokers")

km_crude_conditional_plot<- plot_cif(km_crude_conditional, "Risk of dementia among ever vs. never smokers") + 
  labs(subtitle = "Eliminating death conditional on time-varying covariates")

dem_adjusted_plot <-
  plot_cif(dem_weights, title = "Total effect of smoking in the risk of dementia") +
  labs(subtitle = "With IPTW")

km_adjusted_conditional_plot<- plot_cif(km_adjusted_conditional, "Direct effect of smoking in the risk of dementia") + 
  labs(subtitle = "With IPTW, IPCW conditional on time-varying covariates")

death_adjusted_plot <-
  plot_cif(death_adjusted, "Total effect of smoking in mortality") +
  labs(subtitle = "With IPTW")




dem_crude_plot + km_crude_conditional_plot + death_crude_plot 
dem_adjusted_plot + km_adjusted_conditional_plot + death_adjusted_plot 
