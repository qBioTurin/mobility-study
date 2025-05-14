# Pattern and clustering.
#
# Inputs:
#   - dir_name_comparison: directory in which save the plots
#   - dir_name_models:     directory in which save the models
#   - countries:           considered countries
#   - custom_order:        custom countries order
#   - data_countries:      country data
countries_comparison <- function(dir_name_comparison, dir_name_models, countries, custom_order, data_countries, method){
  ratio_resinf_all <- data.frame(date=NULL, value=NULL, country=NULL, country_short_col=NULL)
  ratio_mobinf_all <- data.frame(date=NULL, value=NULL, country=NULL, country_short_col=NULL)
  ratio_resmob_all <- data.frame(date=NULL, value=NULL, country=NULL, country_short_col=NULL)

  for(i in seq(1, nrow(countries))){
    country_long_en <- countries$country.name.en[i]
    country_short <- countries$iso2c[i]
    country_short_3c <- countries$iso3c[i]
    
    
    # Compute the ratio between stringency index and infection rates
    model_data <- data_countries[[i]]$model_data_ratios %>%
      select(date, StringencyIndex_Average, average_mobility, infection_rates) %>%
      filter(!if_any(everything(), is.na))
    
    ratio_resinf <- data.frame(date=rep(NA, nrow(model_data)), value=rep(NA, nrow(model_data)), country=rep(country_long_en, nrow(model_data)), country_short_col=rep(country_short, nrow(model_data)))
    ratio_resinf$date <- model_data$date
    ratio_resinf$value <- model_data$StringencyIndex_Average / model_data$infection_rates
    
    png(paste0(dir_name_comparison, "ratio_resinf_", gsub(" ", "_", country_long_en), ".png"), units="in", width=20, height=25, res=150)
    plot <- ggplot(ratio_resinf) +
      geom_line(aes(x=as.Date(date), y=value), linewidth=1.5) +
      theme_bw() +
      labs(x = "Date", y = "Ratio") +
      theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
      guides(color = guide_legend(override.aes = list(size = 16), nrow=4, byrow=TRUE))
    print(plot)
    dev.off()
    
    ratio_resinf_all <- rbind(ratio_resinf_all, ratio_resinf)
    
    
    # Compute the ratio between average mobility and infection rates
    ratio_mobinf <- data.frame(date=rep(NA, nrow(model_data)), value=rep(NA, nrow(model_data)), country=rep(country_long_en, nrow(model_data)), country_short_col=rep(country_short, nrow(model_data)))
    ratio_mobinf$date <- model_data$date
    ratio_mobinf$value <- model_data$average_mobility / model_data$infection_rates
    
    png(paste0(dir_name_comparison, "ratio_mobinf_", gsub(" ", "_", country_long_en), ".png"), units="in", width=20, height=25, res=150)
    plot <- ggplot(ratio_mobinf) +
      geom_line(aes(x=as.Date(date), y=value), linewidth=1.5) +
      theme_bw() +
      labs(x = "Date", y = "Ratio") +
      theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
      guides(color = guide_legend(override.aes = list(size = 16), nrow=4, byrow=TRUE))
    print(plot)
    dev.off()
    
    ratio_mobinf_all <- rbind(ratio_mobinf_all, ratio_mobinf)
    
    
    # Compute the ratio between stringency index and average mobility
    ratio_resmob <- data.frame(date=rep(NA, nrow(model_data)), value=rep(NA, nrow(model_data)), country=rep(country_long_en, nrow(model_data)), country_short_col=rep(country_short, nrow(model_data)))
    ratio_resmob$date <- model_data$date
    ratio_resmob$value <- model_data$StringencyIndex_Average / model_data$average_mobility
    
    png(paste0(dir_name_comparison, "ratio_resmob_", gsub(" ", "_", country_long_en), ".png"), units="in", width=20, height=25, res=150)
    plot <- ggplot(ratio_resmob) +
      geom_line(aes(x=as.Date(date), y=value), linewidth=1.5) +
      theme_bw() +
      labs(x = "Date", y = "Ratio") +
      theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
      guides(color = guide_legend(override.aes = list(size = 16), nrow=4, byrow=TRUE))
    print(plot)
    dev.off()
    
    ratio_resmob_all <- rbind(ratio_resmob_all, ratio_resmob)
  }
  
  # Statistics on the three ratios
  # Correlation among them
  ratio_resmob_all_avg <- ratio_resmob_all %>%
    group_by(date) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE)) %>%
    mutate(type = "StringencyOnMobility(t)")
  
  # Repeat same structure for other ratios
  ratio_mobinf_all_avg <- ratio_mobinf_all %>% 
    group_by(date) %>%
    summarise(mean_value = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE)) %>%
    mutate(type = "MobilityOnInfRates(t)")
  
  ratio_resinf_all_avg <- ratio_resinf_all %>% 
    group_by(date) %>%
    summarise(mean_value = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE)) %>%
    mutate(type = "StringencyOnInfRates(t)")
  
  corr_resmob_mobinf <- cor(ratio_resmob_all_avg$mean_value, ratio_mobinf_all_avg$mean_value, method = method)
  corr_resmob_resinf <- cor(ratio_resmob_all_avg$mean_value, ratio_resinf_all_avg$mean_value, method = method)
  corr_mobinf_resinf <- cor(ratio_mobinf_all_avg$mean_value, ratio_resinf_all_avg$mean_value, method = method)
  
  ratios_df <- rbind(ratio_resmob_all_avg, ratio_mobinf_all_avg, ratio_resinf_all_avg)
  
  ratios_df$type <- factor(ratios_df$type, levels = c("StringencyOnMobility(t)", "MobilityOnInfRates(t)", "StringencyOnInfRates(t)"))
  
  ## Updated Visualization
  png(paste0(dir_name_comparison, "ratios.png"), units="in", width=40, height=20, res=150)
  plot <- ggplot(ratios_df) +
    geom_line(aes(x = as.Date(date), y = mean_value, color = type), linewidth = 1.5) +
    geom_ribbon(aes(x = as.Date(date), 
                    ymin = mean_value-sd, 
                    ymax = mean_value+sd, 
                    fill = type), 
                alpha = 0.2) +
    theme_bw() +
    labs(x = "Date", y = "Value", color = "Ratio", fill = "Ratio") +
    scale_color_manual(values = c("#ff8b94", "lightgreen", "#6B95DB")) +
    scale_fill_manual(values = c("#ff8b94", "lightgreen", "#6B95DB")) +
    ylim(0, 8.20) +
    theme(legend.position = "bottom", title = element_text(size = 54, face = "bold"), legend.key.size = unit(3, 'cm'), axis.title = element_text(size = 50), axis.text = element_text(size = 45), legend.title = element_text(size = 50), legend.text = element_text(size = 45))
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  # Perform Shapiro-Wilk test
  resmob_shapiro <- shapiro.test(ratio_resmob_all_avg$mean_value)$p.value
  resinf_shapiro <- shapiro.test(ratio_resinf_all_avg$mean_value)$p.value
  mobinf_shapiro <- shapiro.test(ratio_mobinf_all_avg$mean_value)$p.value
  
  # Perform the KS test
  resmob_resinf_ks <- ks.test(ratio_resmob_all_avg$mean_value, ratio_resinf_all_avg$mean_value)$p.value
  resmob_mobinf_ks <- ks.test(ratio_resmob_all_avg$mean_value, ratio_mobinf_all_avg$mean_value)$p.value
  mobinf_resinf_ks <- ks.test(ratio_mobinf_all_avg$mean_value, ratio_resinf_all_avg$mean_value)$p.value
  
  # Perform Mann-Whitney U test
  resmob_resinf_wilcox <- wilcox.test(ratio_resmob_all_avg$mean_value, ratio_resinf_all_avg$mean_value)$p.value
  resmob_mobinf_wilcox <- wilcox.test(ratio_resmob_all_avg$mean_value, ratio_mobinf_all_avg$mean_value)$p.value
  mobinf_resinf_wilcox <- wilcox.test(ratio_mobinf_all_avg$mean_value, ratio_resinf_all_avg$mean_value)$p.value
  
  # Calculate Quartiles for each dataset
  resmob_q1 <- quantile(ratio_resmob_all_avg$mean_value, 0.25)
  resmob_q2 <- quantile(ratio_resmob_all_avg$mean_value, 0.50)
  resmob_q3 <- quantile(ratio_resmob_all_avg$mean_value, 0.75)
  
  resinf_q1 <- quantile(ratio_resinf_all_avg$mean_value, 0.25)
  resinf_q2 <- quantile(ratio_resinf_all_avg$mean_value, 0.50)
  resinf_q3 <- quantile(ratio_resinf_all_avg$mean_value, 0.75)
  
  mobinf_q1 <- quantile(ratio_mobinf_all_avg$mean_value, 0.25)
  mobinf_q2 <- quantile(ratio_mobinf_all_avg$mean_value, 0.50)
  mobinf_q3 <- quantile(ratio_mobinf_all_avg$mean_value, 0.75)
  
  # Distribution Plot for ratio_resinf_all_avg
  resinf_lines <- data.frame(
    xintercept = c(resinf_q1, resinf_q2, resinf_q3, mean(ratio_resinf_all_avg$mean_value)),
    Statistic = c("Quantiles", "Quantiles", "Quantiles", "Mean")
  )
  
  png(paste0(dir_name_comparison, "ratio_resinf_all_density_cdf.png"), units="in", width=30, height=25, res=150)
  ratio_resinf_all_avg$cdf <- ecdf(ratio_resinf_all_avg$mean_value)(ratio_resinf_all_avg$mean_value)
  max_density <- max(density(ratio_resinf_all_avg$mean_value)$y)
  ratio_resinf_all_avg$cdf_scaled <- ratio_resinf_all_avg$cdf * max_density
  ratio_resinf_all_avg$curve_type <- "CDF"
  
  plot <- ggplot(ratio_resinf_all_avg, aes(x = mean_value)) +
    geom_density(fill = "#6B95DB", alpha = 0.6) +
    geom_line(aes(y = cdf_scaled, color = curve_type), size = 1.5) +
    geom_vline(data = resinf_lines, aes(xintercept = xintercept, linetype = Statistic), color = "black", size = 2) +
    scale_y_continuous(
      name = "Density",
      sec.axis = sec_axis(~ . / max_density, name = "Cumulative Probability")
    ) +
    scale_color_manual(name = "Curve", values = c("CDF" = "purple")) +
    scale_linetype_manual(values = c("Quantiles" = "dashed", "Mean" = "dotted")) +
    labs(
      x = "Ratio",
      title = "Distribution and CDF of StringencyOnInfRates(t)",
      linetype = "Statistic"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      title = element_text(size = 54, face = "bold"),
      legend.key.size = unit(3, 'cm'),
      axis.title = element_text(size = 50),
      axis.text = element_text(size = 45),
      legend.title = element_text(size = 50),
      legend.text = element_text(size = 45)
    )
  print(plot)
  dev.off()
  
  # Distribution Plot for ratio_resmob_all_avg
  resmob_lines <- data.frame(
    xintercept = c(resmob_q1, resmob_q2, resmob_q3, mean(ratio_resmob_all_avg$mean_value)),
    Statistic = c("Quantiles", "Quantiles", "Quantiles", "Mean")
  )
  
  png(paste0(dir_name_comparison, "ratio_resmob_all_density_cdf.png"), units="in", width=30, height=25, res=150)
  ratio_resmob_all_avg$cdf <- ecdf(ratio_resmob_all_avg$mean_value)(ratio_resmob_all_avg$mean_value)
  max_density <- max(density(ratio_resmob_all_avg$mean_value)$y)
  ratio_resmob_all_avg$cdf_scaled <- ratio_resmob_all_avg$cdf * max_density
  ratio_resmob_all_avg$curve_type <- "CDF"
  
  plot <- ggplot(ratio_resmob_all_avg, aes(x = mean_value)) +
    geom_density(fill = "#ff8b94", alpha = 0.6) +
    geom_line(aes(y = cdf_scaled, color = curve_type), size = 1.5) +
    geom_vline(data = resmob_lines, aes(xintercept = xintercept, linetype = Statistic), color = "black", size = 2) +
    scale_y_continuous(
      name = "Density",
      sec.axis = sec_axis(~ . / max_density, name = "Cumulative Probability")
    ) +
    scale_color_manual(name = "Curve", values = c("CDF" = "purple")) +
    scale_linetype_manual(values = c("Quantiles" = "dashed", "Mean" = "dotted")) +
    labs(
      x = "Ratio",
      title = "Distribution and CDF of StringencyOnMobility(t)",
      linetype = "Statistic"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      title = element_text(size = 54, face = "bold"),
      legend.key.size = unit(3, 'cm'),
      axis.title = element_text(size = 50),
      axis.text = element_text(size = 45),
      legend.title = element_text(size = 50),
      legend.text = element_text(size = 45)
    )
  print(plot)
  dev.off()
  
  # Distribution Plot for ratio_mobinf_all_avg
  mobinf_lines <- data.frame(
    xintercept = c(mobinf_q1, mobinf_q2, mobinf_q3, mean(ratio_mobinf_all_avg$mean_value)),
    Statistic = c("Quantiles", "Quantiles", "Quantiles", "Mean")
  )
  
  png(paste0(dir_name_comparison, "ratio_mobinf_all_density_cdf.png"), units="in", width=30, height=25, res=150)
  ratio_mobinf_all_avg$cdf <- ecdf(ratio_mobinf_all_avg$mean_value)(ratio_mobinf_all_avg$mean_value)
  max_density <- max(density(ratio_mobinf_all_avg$mean_value)$y)
  ratio_mobinf_all_avg$cdf_scaled <- ratio_mobinf_all_avg$cdf * max_density
  ratio_mobinf_all_avg$curve_type <- "CDF"
  
  plot <- ggplot(ratio_mobinf_all_avg, aes(x = mean_value)) +
    geom_density(fill = "lightgreen", alpha = 0.6) +
    geom_line(aes(y = cdf_scaled, color = curve_type), size = 1.5) +
    geom_vline(data = mobinf_lines, aes(xintercept = xintercept, linetype = Statistic), color = "black", size = 2) +
    scale_y_continuous(
      name = "Density",
      sec.axis = sec_axis(~ . / max_density, name = "Cumulative Probability")
    ) +
    scale_color_manual(name = "Curve", values = c("CDF" = "purple")) +
    scale_linetype_manual(values = c("Quantiles" = "dashed", "Mean" = "dotted")) +
    labs(
      x = "Ratio",
      title = "Distribution and CDF of MobilityOnInfRates(t)",
      linetype = "Statistic"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      title = element_text(size = 54, face = "bold"),
      legend.key.size = unit(3, 'cm'),
      axis.title = element_text(size = 50),
      axis.text = element_text(size = 45),
      legend.title = element_text(size = 50),
      legend.text = element_text(size = 45)
    )
  print(plot)
  dev.off()
  
  
  
  
  dates_count <- ratio_resinf_all %>%
    group_by(date) %>%
    count() %>%
    filter(n == nrow(countries))
  
  names(custom_order) <- gsub("United Kingdom", "UK", names(custom_order))
  
  # Plots ratio from stringency index to infection rates 
  ratio_resinf_all <- ratio_resinf_all %>%
    filter(date %in% dates_count$date)
  
  ratio_resinf_all$country <- gsub("United Kingdom", "UK", ratio_resinf_all$country)
  
  png(paste0(dir_name_comparison, "ratio_resinf_all.png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(ratio_resinf_all) +
    geom_line(aes(x=as.Date(date), y=value, color=country), linewidth=1.5) +
    scale_color_manual(values = c(moma.colors("Klein"), moma.colors("Connors"))) +
    theme_bw() +
    labs(x = "Date", y = "Ratio", color="Country") +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=4, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  ratio_resinf_all$country <- factor(ratio_resinf_all$country, levels=rev(names(custom_order)))
  
  png(paste0(dir_name_comparison, "ratio_resinf_all_heatmap.png"), units="in", width=40, height=20, res=150)
  plot <- ggplot(ratio_resinf_all) +
    geom_raster(aes(x = as.Date(date), y = country, fill = value)) +
    scale_fill_gradientn(colors = moma.colors("Ernst")) +
    theme_bw() +
    scale_x_date(expand = c(0, 0)) +
    labs(title= "StringencyOnInfRates(t)", x = "Date", y = "Country", fill="Ratio") +
    theme(legend.position = "bottom", title = element_text(size = 54, face = "bold"), legend.key.size = unit(3, 'cm'), axis.title = element_text(size = 50), axis.text = element_text(size = 45), legend.title = element_text(size = 50), legend.text = element_text(size = 45)) +
    guides(fill = guide_colorbar(title.vjust = 0.75))
  print(plot)
  dev.off()
  
  
  # Plots ratio from average mobility to infection rates
  ratio_mobinf_all <- ratio_mobinf_all %>%
    filter(date %in% dates_count$date)
  
  ratio_mobinf_all$country <- gsub("United Kingdom", "UK", ratio_mobinf_all$country)
  
  png(paste0(dir_name_comparison, "ratio_mobinf_all.png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(ratio_mobinf_all) +
    geom_line(aes(x=as.Date(date), y=value, color=country), linewidth=1.5) +
    scale_color_manual(values = c(moma.colors("Klein"), moma.colors("Connors"))) +
    theme_bw() +
    labs(x = "Date", y = "Ratio", color="Country") +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=4, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  ratio_mobinf_all$country <- factor(ratio_mobinf_all$country, levels=rev(names(custom_order)))
  
  png(paste0(dir_name_comparison, "ratio_mobinf_all_heatmap.png"), units="in", width=40, height=20, res=150)
  plot <- ggplot(ratio_mobinf_all) +
    geom_raster(aes(x = as.Date(date), y = country, fill = value)) +
    scale_fill_gradientn(colors = moma.colors("Ernst")) +
    theme_bw() +
    scale_x_date(expand = c(0, 0)) +
    labs(title = "MobilityOnInfRates(t)", x = "Date", y = "Country", fill="Ratio") +
    theme(legend.position = "bottom", title = element_text(size = 54, face = "bold"), legend.key.size = unit(3, 'cm'), axis.title = element_text(size = 50), axis.text = element_text(size = 45), legend.title = element_text(size = 50), legend.text = element_text(size = 45)) +
    guides(fill = guide_colorbar(title.vjust = 0.75))
  print(plot)
  dev.off()
  
  
  # Plots ratio from stringency index to average mobility
  ratio_resmob_all <- ratio_resmob_all %>%
    filter(date %in% dates_count$date)
  
  ratio_resmob_all$country <- gsub("United Kingdom", "UK", ratio_resmob_all$country)
  
  png(paste0(dir_name_comparison, "ratio_resmob_all.png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(ratio_resmob_all) +
    geom_line(aes(x=as.Date(date), y=value, color=country), linewidth=1.5) +
    scale_color_manual(values = c(moma.colors("Klein"), moma.colors("Connors"))) +
    theme_bw() +
    labs(x = "Date", y = "Ratio", color="Country") +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=4, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  ratio_resmob_all$country <- factor(ratio_resmob_all$country, levels=rev(names(custom_order)))
  
  png(paste0(dir_name_comparison, "ratio_resmob_all_heatmap.png"), units="in", width=40, height=20, res=150)
  plot <- ggplot(ratio_resmob_all) +
    geom_raster(aes(x = as.Date(date), y = country, fill = value)) +
    scale_fill_gradientn(colors = moma.colors("Ernst")) +
    theme_bw() +
    scale_x_date(expand = c(0, 0)) +
    labs(title = "StringencyOnMobility(t)", x = "Date", y = "Country", fill="Ratio") +
    theme(legend.position = "bottom", title = element_text(size = 54, face = "bold"), legend.key.size = unit(3, 'cm'), axis.title = element_text(size = 50), axis.text = element_text(size = 45), legend.title = element_text(size = 50), legend.text = element_text(size = 45)) +
    guides(fill = guide_colorbar(title.vjust = 0.75))
  print(plot)
  dev.off()

  
  # Clustering (K-means and CONNECTOR)
  plots <- list()
  silhouette_df <- data.frame(k=NULL, value=NULL, type=NULL, ratio=NULL)
  data <- clustering(ratio_resmob_all, "resmob", 8, c(2, 5, 6), c(2, 3, 5), dir_name_comparison, plots, silhouette_df)
  plots <- data[[1]]
  silhouette_df <- data[[2]]
  
  data <- clustering(ratio_mobinf_all, "mobinf", 11, c(2, 5, 6), c(2, 5, 6), dir_name_comparison, plots, silhouette_df)
  plots <- data[[1]]
  silhouette_df <- data[[2]]
  
  data <- clustering(ratio_resinf_all, "resinf", 11, c(2, 5, 6), c(2, 3, 6), dir_name_comparison, plots, silhouette_df)
  plots <- data[[1]]
  silhouette_df <- data[[2]]

  

  png(paste0(dir_name_comparison, "silhouette_scores.png"), units="in", width=34, height=15, res=300)
  p <- ggplot(silhouette_df) +
    geom_line(aes(x=k, y=value, linetype=ratio, color=type), linewidth=3) +
    theme_bw() +
    geom_point(aes(x=k, y=value), shape=1, size=8) +
    scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
    scale_color_manual(values=c("#559e83", "#ff8b94")) +
    labs(x = "Number of clusters", y = "Average Silhouette Scores", color="Used data", linetype="Ratio") +
    theme(legend.position = "bottom", title = element_text(size = 54, face = "bold"), legend.key.size = unit(3, 'cm'), axis.title = element_text(size = 50), axis.text = element_text(size = 45), legend.title = element_text(size = 50), legend.text = element_text(size = 45)) +
    guides(color = guide_legend(nrow=2, byrow=TRUE), linetype = guide_legend(nrow=2, byrow=TRUE))
  print(p)
  dev.off()
  
  # Clustering plots with K-means
  # Average
  p <- (plots[[1]] + plots[[5]]) / (plots[[7]] + plots[[9]]) / (plots[[13]] + plots[[15]]) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", axis.title = element_blank())
  
  png(paste0(dir_name_comparison, "clusters_mean.png"), units="in", width=40, height=15, res=300)
  print(p)
  dev.off()
  
  # Average + standard deviation
  p <- (plots[[2]] + plots[[4]]) / (plots[[8]] + plots[[12]]) / (plots[[14]] + plots[[18]]) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", axis.title = element_blank())
  
  png(paste0(dir_name_comparison, "clusters_meanstd.png"), units="in", width=40, height=15, res=300)
  print(p)
  dev.off()
}

# Clustering using K-means and CONNECTOR.
#
# Inputs:
#   - data:                 data from one of defined ratios
#   - type:                 ratio
#   - p_selected:           selected p for CONNECTOR
#   - G_clusters:           selected number of clusters for CONNECTOR
#   - k_clusters:           selected number of clusters for K-means
#   - dir_name_comparison:  directory in which save the plots
#   - plots:                plots
clustering <- function(data, type, p_selected, G_clusters, k_clusters, dir_name_comparison, plots, silhouette_df){
  pastel_color_colors <- c(
    "#D64D4D",
    "#6FFF6D",
    "#6DBBFF",
    "#D68B4D",
    "#9A6DFF",
    "#FFB3FC"
  )
  
  title_mapping <- c("mobinf"="MobilityOnInfRates(t)", "resmob"="StringencyOnMobility(t)", "resinf"="StringencyOnInfRates(t)")
  
  data_local <- data %>%
    select(country, country_short_col, value) %>%
    group_by(country, country_short_col) %>%
    summarize(mean = mean(value, na.rm = TRUE), std = sd(value, na.rm = TRUE)) %>%
    ungroup()
  
  data_local$country <- factor(data_local$country, levels=rev(names(custom_order)))
  
  data_meanstd <- data_local %>%
    arrange(country)
  
  data_mean <- data_local %>%
    select(country, country_short_col, mean) %>%
    arrange(country)
  
  # K-means
  silhouette_score <- function(k){
    km <- kmeans(data_mean %>% select(mean), k, nstart=25)
    ss <- silhouette(km$cluster, dist(data_mean %>% select(mean)))
    mean(ss[, 3])
  }
  k <- 2:6
  avg_sil <- sapply(k, silhouette_score)
  
  silhouette_df <- rbind(silhouette_df, data.frame(k=k, value=avg_sil, type="Mean", ratio=title_mapping[type]))
  
  
  silhouette_score_meanstd <- function(k){
    km <- kmeans(data_meanstd %>% select(mean, std), k, nstart=25)
    ss <- silhouette(km$cluster, dist(data_meanstd %>% select(mean, std)))
    mean(ss[, 3])
  }
  k <- 2:6
  avg_sil <- sapply(k, silhouette_score_meanstd)
  
  silhouette_df <- rbind(silhouette_df, data.frame(k=k, value=avg_sil, type="Mean and std", ratio=title_mapping[type]))
  
  
  for(i in 1:length(k_clusters)){
    # Clustering with K-means
    km.res_mean <- kmeans(data_mean %>% select(mean), k_clusters[i], nstart = 25)
    km.res_meanstd <- kmeans(data_meanstd %>% select(mean, std), k_clusters[i], nstart = 25)
    
    data_local_cluster_mean <- cbind(data_mean, data.frame(kmeans=km.res_mean$cluster))
    data_local_cluster_meanstd <- cbind(data_meanstd, data.frame(kmeans=km.res_meanstd$cluster))

    
    
    data_local_cluster_mean <- data_local_cluster_mean %>%
      arrange(mean) %>%
      mutate(vjust = ifelse(row_number() %% 2 == 1, -1.5, 2.5))
    
    centers_mean <- as.data.frame(km.res_mean$centers)
    centers_mean$kmeans <- rownames(centers_mean)
    
    data_local_cluster_mean$source <- "Data"
    centers_mean$source <- "Centroids"
    centers_mean$country <- ""
    centers_mean$country_short_col <- ""
    centers_mean$vjust <- 0
    combined_data <- rbind(data_local_cluster_mean, centers_mean)
    combined_data$country_short_col[which(combined_data$country_short_col == "GB")] <- "UK"
    
    
    
    cluster_sizes <- data_local_cluster_mean %>%
      group_by(kmeans) %>%
      summarise(size = n())
    
    data_local_cluster_mean <- data_local_cluster_mean %>%
      left_join(cluster_sizes, by = "kmeans")
    
    png(paste0(dir_name_comparison, "kmeans_", type, "_", k_clusters[i], "_mean.png"), unit="in", width=40, height=5, res=150)
    p <- ggplot(data=combined_data, aes(x = mean, y = 0, color = as.factor(kmeans))) +
      geom_point(data=data_local_cluster_mean, aes(shape = "Data"), size = 25, alpha = 0.6) +
      geom_point(data=centers_mean, aes(shape = "Centroids"), size = 25, stroke = 5) +
      geom_text(data = combined_data, aes(label = country_short_col, vjust = vjust), hjust = 0.5, size = 12, fontface="bold", color = "black") +
      theme_minimal() +
      labs(title=paste0(title_mapping[type], ", ", k_clusters[i], " clusters"), x = "Mean", y = "", color = "Cluster", shape = "Type") +
      scale_shape_manual(values = c("Data" = 16, "Centroids" = 4)) +
      scale_color_manual(values = pastel_color_colors) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 54, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 50, face = "bold"),
        axis.text = element_text(size = 45),
        legend.title = element_text(size = 50, face = "bold"),
        legend.text = element_text(size = 45),
        axis.text.y = element_blank()
      ) +
      xlim(min(data_local_cluster_mean$mean), max(data_local_cluster_mean$mean)) +
      guides(fill = "none", color =if(k_clusters[i] != 5) "none")
    print(p)
    dev.off()
    

    plots[[length(plots) + 1]] <- p
    
    colnames(data_local_cluster_mean)[colnames(data_local_cluster_mean) == "kmeans"] <- paste0("kmeans", k_clusters[i])
    saveRDS(data_local_cluster_mean, paste0(dir_name_comparison, "ratio_", type, "_", k_clusters[i], "clusters_all_mean.RDs"))
    
    
    
    if(type == "mobinf"){
      data_local_cluster_meanstd <- data_local_cluster_meanstd %>%
        arrange(mean) %>%
        mutate(vjust = ifelse(country == "France", 2.5, ifelse(country == "Portugal", -1.5, 0.5)))
    }
    else{
      data_local_cluster_meanstd <- data_local_cluster_meanstd %>%
        arrange(mean) %>%
        mutate(vjust = 0.5)
    }
    
    centers_meanstd <- as.data.frame(km.res_meanstd$centers)
    centers_meanstd$kmeans <- rownames(centers_mean)
    
    data_local_cluster_meanstd$source <- "Data"
    centers_meanstd$source <- "Centroids"
    centers_meanstd$country <- ""
    centers_meanstd$country_short_col <- ""
    centers_meanstd$vjust <- 0
    combined_data <- rbind(data_local_cluster_meanstd, centers_meanstd)
    combined_data$country_short_col[which(combined_data$country_short_col == "GB")] <- "UK"
    
    png(paste0(dir_name_comparison, "kmeans_", type, "_", k_clusters[i], "_meanstd.png"), unit="in", width=40, height=10, res=150)
    p <- ggplot(data=combined_data, aes(x = mean, y = std, color = as.factor(kmeans))) +
      geom_point(data=data_local_cluster_meanstd, aes(shape = "Data"), size = 25, alpha = 0.6) +
      geom_point(
        data = centers_meanstd,
        aes(x = mean, y = std, shape = "Centroids"),
        size = 25,
        stroke = 5
      ) +
      geom_text(data = combined_data, aes(label = country_short_col, vjust = vjust), hjust = 0.5, fontface="bold", size = 12, color = "black") +
      theme_minimal() +
      labs(
        title=paste0(title_mapping[type], ", ", k_clusters[i], " clusters"),
        x = "Mean",
        y = "Standard Deviation",
        color = "Cluster",
        shape = "Type"
      ) +
      scale_shape_manual(values = c("Data" = 16, "Centroids" = 4)) +
      scale_color_manual(values = pastel_color_colors) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 54, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 50, face = "bold"),
        axis.text = element_text(size = 45),
        legend.title = element_text(size = 50, face = "bold"),
        legend.text = element_text(size = 45)
      ) +
      ylim(min(data_local_cluster_meanstd$std) - 0.3, max(data_local_cluster_meanstd$std) + 0.3) +
      xlim(min(data_local_cluster_meanstd$mean), max(data_local_cluster_meanstd$mean)) +
      guides(fill = "none", color = if(k_clusters[i] != 6) "none")
    print(p)
    dev.off()
    
    plots[[length(plots) + 1]] <- p
    
    colnames(data_local_cluster_meanstd)[colnames(data_local_cluster_meanstd) == "kmeans"] <- paste0("kmeans", k_clusters[i])
    saveRDS(data_local_cluster_meanstd, paste0(dir_name_comparison, "ratio_", type, "_", k_clusters[i], "clusters_all_meanstd.RDs"))
  }
  
  
  names(custom_order)[6] <- "UK"

  # Clustering with CONNECTOR
  data_local <- data %>%
    mutate(ID = recode(country, !!!custom_order), Observation = value, Time = date) %>%
    select(ID, Observation, Time)
  data_local$Time <- as.numeric(data_local$Time)
  data_ann <- data.frame(ID=unname(custom_order), Country=names(custom_order), row.names = NULL)

  CONNECTORList <- DataFrameImport(data_local, data_ann)

  CrossLogLike <- BasisDimension.Choice(data = CONNECTORList,
                                        p = 2:12, Cores = 8)

  png(paste0(dir_name_comparison, "CrossLogLikePlot_", type, ".png"), units="in", width=34, height=15, res=300)
  p <- ggplot_build(CrossLogLike$CrossLogLikePlot)
  p$plot <- p$plot +
    labs(title = paste0("Cross-LogLikelihood Plot (", title_mapping[type], ")")) +
    theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24))
    print(p$plot)
  dev.off()


  # Best value selected from BasisDimension.Choice(data = CONNECTORList, p = 2:12)
  ClusteringList <-ClusterAnalysis(CONNECTORList,
                                   G=2:6,
                                   p=p_selected,
                                   runs=50,
                                   Cores=8)

  IndexesPlot.Extrapolation(ClusteringList)-> indexes

  png(paste0(dir_name_comparison, "indexesPlot_", type, ".png"), units="in", width=34, height=15, res=300)
  p <- ggplot_build(indexes$Plot)
  p$plot <- p$plot +
    labs(title = title_mapping[type]) +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          plot.title = element_text(size = 54, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 50, face = "bold"),
          axis.text = element_text(size = 45),
          legend.title = element_text(size = 50, face = "bold"),
          legend.text = element_text(size = 45),
          strip.text.x = element_text(size = 35))
  print(p$plot)
  dev.off()

  ConsMatrix<-ConsMatrix.Extrapolation(stability.list = ClusteringList)


  for(i in 1:length(G_clusters)){
    png(paste0(dir_name_comparison, "G", G_clusters[i], "_", type, ".png"), units="in", width=34, height=15, res=300)
    p <- ggplot_build(ConsMatrix[[paste0("G", G_clusters[i])]]$ConsensusPlot)
    p$plot <- p$plot +
      theme(legend.position = "bottom",
            legend.box = "vertical",
            plot.title = element_text(size = 54, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 50, face = "bold"),
            axis.text = element_text(size = 45),
            legend.title = element_text(size = 50, face = "bold"),
            legend.text = element_text(size = 45))
    print(p$plot)
    dev.off()

    CONNECTORList.FCM.opt <- MostProbableClustering.Extrapolation(
      stability.list = ClusteringList,
      G = G_clusters[i])

    FCMplots <- ClusterWithMeanCurve(clusterdata = CONNECTORList.FCM.opt,
                                     feature = "Country",
                                     labels = c("Day", "Value"))

    CONNECTORList.FCM.opt$FCM$cluster$ClustCurve <- CONNECTORList.FCM.opt$FCM$cluster$ClustCurve %>%
      mutate(ID = names(custom_order)[ID], Time = as.Date(Time), Cluster = CONNECTORList.FCM.opt$FCM$cluster$cluster.names[Cluster])

    CONNECTORList.FCM.opt$FCM$cluster$meancurves <- CONNECTORList.FCM.opt$FCM$cluster$meancurves %>%
      pivot_longer(cols = -Time, names_to = "Cluster", values_to = "Value") %>%
      mutate(Time = as.Date(Time))

    png(paste0(dir_name_comparison, "clustering_connector_", type, "_", G_clusters[i], ".png"), units="in", width=34, height=15, res=300)
    plot <- ggplot() +
      geom_line(data=CONNECTORList.FCM.opt$FCM$cluster$ClustCurve, aes(x=Time, y=Observation, color=ID), linewidth=1.5) +
      geom_line(data=CONNECTORList.FCM.opt$FCM$cluster$meancurves, aes(x=Time, y=Value, linetype=Cluster), linewidth=1.5) +
      facet_wrap(~Cluster, ncol = 3) +
      theme_bw() +
      scale_color_manual(values=moma.colors("Warhol", 13, type="discrete")) +
      labs(title = paste0(title_mapping[type], ", ", G_clusters[i], " clusters"), color="Country") +
      theme(legend.position = "bottom",
            legend.box = "vertical",
            legend.key.size = unit(1.5, 'cm'),
            strip.text.x = element_text(size = 35),
            plot.title = element_text(size = 54, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 50, face = "bold"),
            axis.text = element_text(size = 35),
            legend.title = element_text(size = 50, face = "bold"),
            legend.text = element_text(size = 45)) +
      guides(linetype = "none")
    print(plot)
    dev.off()
  }

  return(list(plots, silhouette_df))
}