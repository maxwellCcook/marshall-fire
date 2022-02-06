shinyServer(function(input, output, session) {
  
  # Waiter ------------------------------------------------------------------
  
  waiter_hide()
  
  # Plot output calls for all 'left' plots ----------------------------------
  # WILL GET MOVED INTO INDIVIDUAL MODULES
  
  # Commuter mode shift
  output$commuter_map_left <- renderCachedPlot({
    
    quant_car_share <- car_share %>% mutate(quant3 = ntile(car_share$Car_per, 3))
    
    p <- ggplot(quant_car_share) +
      geom_sf(aes(fill = as.factor(quant3)), color = "white", 
              size = 0.05) +
      scale_fill_manual(values = rev(colors[c(4:6)])) +
      theme_map()
    
    cowplot::ggdraw() + 
      cowplot::draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
      cowplot::draw_plot(p, scale = .85) 
    
  },
  cacheKeyExpr = paste("commute_mode_left"),
  cache = diskCache("./app-cache")
  )
  
  # Pedestrian social distancing capacity map
  output$pedestrian_map_left <- renderCachedPlot({
    
    p <- 
      ggplot() +
      geom_sf(data = census_circular, fill = "transparent", color = "black", 
              size = 0.05) +
      geom_sf(data = census_analysis_quantile,
              aes(fill = as.factor(
                social_distancing_capacity_pop_perc_2m_quant3)),
              color = "white", size = 0.03) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_void() +
      theme(legend.position = "none")
    
    cowplot::ggdraw() + 
      cowplot::draw_image(dropshadow2, scale = 1.85, vjust = 0.01) +
      cowplot::draw_plot(p) +
      cowplot::draw_image(uni_legend, scale = .45, vjust = 0.3, hjust = 0.3)
    
  },
  cacheKeyExpr = "pedestrian_left",
  cache = diskCache("./app-cache")
  )
  
  
 
  
  # Modules -----------------------------------------------------------------
  
  Marshall_fire_server("marshall_fire")    
  
  
})
