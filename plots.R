# Normalize data in x in [0, 1].
#
# Inputs:
#   - x:              data to normalize
#
# Output:
#   - normalized_x:   normalized data
normalize <- function(x){
  normalized_x  =(x - min(x)) / (max(x) - min(x))
  
  return(normalized_x)
}

# Plot infection rates with I for comparison.
#
# Inputs:
#   - dir_name_plots:         directory in which save the plots
#   - country_long_en:        long country name
#   - infection_rates_global: infection rates
#   - SIRD_global:            evolution of compartmental model
#   - method:                 correlation method
infection_rates_I_plot <- function(dir_name_plots, country_long_en, infection_rates_global, SIRD_global, method){
  colors <- c("Infection rates" = "#000000", "I" = "#ff8b94")
  
  png(paste0(dir_name_plots, gsub(" ", "_", country_long_en), "/infection_rates_I.png"), units="in", width=20, height=25, res=150)
  plot <- ggplot() +
    geom_line(data=infection_rates_global, aes(x=date, y=normalize(value), color="Infection rates"), linewidth=1.5) +
    geom_line(data=SIRD_global, aes(x=date, y=normalize(I), color="I"), linewidth=1.5) +
    labs(x = "Date", y = "Values", title = paste0(country_long_en, " (", method, " correlation: ", cor(infection_rates_global$value, SIRD_global$I, method = method), ")")) +
    theme_bw() +
    scale_color_manual(values = colors) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=3, byrow=TRUE))
  print(plot)
  dev.off()
}

# Generate some initial plots using infection rates, stringency index and mobility variables.
#
# Inputs:
#   - dir_name:             directory in which save the plots
#   - dates:                considered dates
#   - restrictions:         restrictions data
#   - response:             mobility data
#   - infections_rates:     infection rates
#   - country_long:         long country name
#   - mob_type:             Facebook or Google
mobility_plot <- function(dir_name, dates, restrictions, response, infection_rates, country_long, mob_type, plots){
  corr_indeces <- which(complete.cases(response$residential_moved, response$workplaces_moved, response$transit_stations_moved, response$retail_and_recreation_moved, response$grocery_and_pharmacy_stores_moved, response$PCA_mobility_moved, response$masks_moved, infection_rates$value, restrictions$StringencyIndex_Average_moved))

  infection_rates_series <- infection_rates$value[corr_indeces]
  residential_series <- response$residential_moved[corr_indeces]
  transit_stations_series <- response$transit_stations_moved[corr_indeces]
  workplaces_series <- response$workplaces_moved[corr_indeces]
  retail_and_recreation_series <- response$retail_and_recreation_moved[corr_indeces]
  grocery_and_pharmacy_stores_series <- response$grocery_and_pharmacy_stores_moved[corr_indeces]
  masks_series <- response$masks[corr_indeces]
  PCA_mobility_series <- response$PCA_mobility_moved[corr_indeces]
  average_mobility <- (response$residential_moved[corr_indeces] + response$transit_stations_moved[corr_indeces] + response$workplaces_moved[corr_indeces] + response$retail_and_recreation_moved[corr_indeces] + response$grocery_and_pharmacy_stores_moved[corr_indeces]) / 5
  stringency_index_series <- restrictions$StringencyIndex_Average_moved[corr_indeces]
  
  date <- c(rep(dates$date[corr_indeces], 8))
  value <- c(masks_series, residential_series, workplaces_series, transit_stations_series, retail_and_recreation_series, grocery_and_pharmacy_stores_series, infection_rates_series, stringency_index_series)
  type <- c(rep("Masks (Complement)", length(corr_indeces)), rep("Residential (Complement)", length(corr_indeces)), rep("Workplaces", length(corr_indeces)), rep("Transit stations", length(corr_indeces)), rep("Retail and recreation", length(corr_indeces)), rep("Grocery and pharmacy stores", length(corr_indeces)), rep("Infection rates (similar to Rt)", length(corr_indeces)), rep("Stringency index (Complement)", length(corr_indeces)))
  
  mobilities <- data.frame(date=as.Date(date), value=value, type=type)
  
  system(paste0("mkdir -p ", dir_name, gsub(" ", "_", country_long)))
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_all_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=date, y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#2FFFCE", "#494949", "#985453", "#6B95DB", "#c3cb71", "#ff8b94", "#559e83", "#c9c9ff")) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=3, byrow=TRUE))
  
  if(mob_type == "Facebook") plot <- plot + ylim(0, 1) else plot <- plot + ylim(-1, 1)
  print(plot)
  dev.off()
  
  plots[[length(plots) + 1]] <- plot
  

  value <- c(normalize(masks_series), normalize(residential_series), normalize(workplaces_series), normalize(transit_stations_series), normalize(retail_and_recreation_series), normalize(grocery_and_pharmacy_stores_series), normalize(infection_rates_series), normalize(stringency_index_series))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_all_norm_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Normalized values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#2FFFCE", "#494949", "#985453", "#6B95DB", "#c3cb71", "#ff8b94", "#559e83", "#c9c9ff")) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=3, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  
  date <- c(rep(dates$date[corr_indeces], 2))
  value <- c(PCA_mobility_series, infection_rates_series)
  type <- c(rep("PCA mobility", length(corr_indeces)), rep("Infection rates (similar to Rt)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_PCA_infrates_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#494949", "#ae5a41")) +
    ylim(-1, 1) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE))
  print(plot)
  dev.off()
  
  
  date <- c(rep(dates$date[corr_indeces], 2))
  value <- c(normalize(PCA_mobility_series), normalize(infection_rates_series))
  type <- c(rep("PCA mobility", length(corr_indeces)), rep("Infection rates (similar to Rt)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_PCA_infrates_norm_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Normalized values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#494949", "#ae5a41")) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  
  date <- c(rep(dates$date[corr_indeces], 2))
  value <- c(PCA_mobility_series, stringency_index_series)
  type <- c(rep("PCA mobility", length(corr_indeces)), rep("Stringency index (Complement)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_PCA_stridx_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#ae5a41", "#ff8b94")) +
    ylim(-1, 1) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE))
  print(plot)
  dev.off()
  
  
  date <- c(rep(dates$date[corr_indeces], 2))
  value <- c(normalize(PCA_mobility_series), normalize(stringency_index_series))
  type <- c(rep("PCA mobility", length(corr_indeces)), rep("Stringency index (Complement)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_PCA_stridx_norm_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Normalized values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#ae5a41", "#ff8b94")) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  
  date <- c(rep(dates$date[corr_indeces], 3))
  value <- c(PCA_mobility_series, stringency_index_series, infection_rates_series)
  type <- c(rep("PCA mobility", length(corr_indeces)), rep("Stringency index (Complement)", length(corr_indeces)), rep("Infection rates (similar to Rt)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_PCA_stridx_infrates_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#494949", "#ae5a41", "#ff8b94")) +
    ylim(-1, 1) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE))
  print(plot)
  dev.off()
  
  
  date <- c(rep(dates$date[corr_indeces], 3))
  value <- c(normalize(PCA_mobility_series), normalize(stringency_index_series), normalize(infection_rates_series))
  type <- c(rep("PCA mobility", length(corr_indeces)), rep("Stringency index (Complement)", length(corr_indeces)), rep("Infection rates (similar to Rt)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_PCA_stridx_infrates_norm_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Normalized values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#494949", "#ae5a41", "#ff8b94")) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  
  date <- c(rep(dates$date[corr_indeces], 3))
  value <- c(average_mobility, stringency_index_series, infection_rates_series)
  type <- c(rep("Average mobility", length(corr_indeces)), rep("Stringency index (Complement)", length(corr_indeces)), rep("Infection rates (similar to Rt)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_avgmob_stridx_infrates_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#ae5a41", "#494949", "#ff8b94")) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE))
  
  if(mob_type == "Facebook") plot <- plot + ylim(0, 1) else plot <- plot + ylim(-1, 1)
  print(plot)
  dev.off()
  
  plots[[length(plots) + 1]] <- plot
  
  
  date <- c(rep(dates$date[corr_indeces], 3))
  value <- c(normalize(average_mobility), normalize(stringency_index_series), normalize(infection_rates_series))
  type <- c(rep("Average mobility", length(corr_indeces)), rep("Stringency index (Complement)", length(corr_indeces)), rep("Infection rates (similar to Rt)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_avgmob_stridx_infrates_norm_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Normalized values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#ae5a41", "#494949", "#ff8b94")) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  
  date <- c(rep(dates$date[corr_indeces], 2))
  value <- c(infection_rates_series, stringency_index_series)
  type <- c(rep("Infection rates (similar to Rt)", length(corr_indeces)), rep("Stringency index (Complement)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_infrates_stridx_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#494949", "#ff8b94")) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE))
  
  if(mob_type == "Facebook") plot <- plot + ylim(0, 1) else plot <- plot + ylim(-1, 1)
  print(plot)
  dev.off()
  
  
  date <- c(rep(dates$date[corr_indeces], 2))
  value <- c(normalize(infection_rates_series), normalize(stringency_index_series))
  type <- c(rep("Infection rates (similar to Rt)", length(corr_indeces)), rep("Stringency index (Complement)", length(corr_indeces)))
  
  mobilities <- data.frame(date=date, value=value, type=type)
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_infrates_stridx_norm_", mob_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(mobilities) +
    geom_line(aes(x=as.Date(date), y=value, color=type), linewidth=1.5) +
    labs(x = "Date", y = "Normalized values", color = "Type", title = country_long) +
    theme_bw() +
    scale_color_manual(values = c("#494949", "#ff8b94")) +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=1, byrow=TRUE)) 
  print(plot)
  dev.off()
  
  return(plots)
}

# Generate preliminary plots for each country using Facebook and Google data.
#
# Inputs:
#   - dir_name:              directory in which save the plots
#   - plots_Facebook:        Facebook's plots
#   - plots_Google:          Google's plots
preliminary_plots <- function(dir_name, plots_Facebook, plots_Google){
  png(paste0(dir_name, "/preliminary_analysis_Facebook_1.png"), units="in", width=40, height=40, res=150)
  plot <- (plots_Facebook[[5]] + plots_Facebook[[17]]) / (plots_Facebook[[23]] + plots_Facebook[[3]]) / (plots_Facebook[[13]] + plots_Facebook[[25]]) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", title = element_text(size = 44), axis.title = element_text(size = 40), axis.text = element_text(size = 36), legend.title = element_text(size = 44), legend.text = element_text(size = 38))
  print(plot)
  dev.off()
  
  png(paste0(dir_name, "/preliminary_analysis_Facebook_2.png"), units="in", width=40, height=55, res=150)
  plot <- (plots_Facebook[[7]] + plots_Facebook[[9]]) / (plots_Facebook[[1]] + plots_Facebook[[15]]) / (plots_Facebook[[11]] + plots_Facebook[[21]]) / (plots_Facebook[[19]] + plot_spacer()) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", title = element_text(size = 48), axis.title = element_text(size = 44), axis.text = element_text(size = 40), legend.title = element_text(size = 48), legend.text = element_text(size = 42))
  print(plot)
  dev.off()
  
  
  png(paste0(dir_name, "/preliminary_analysis_Google_1.png"), units="in", width=40, height=40, res=150)
  plot <- (plots_Google[[5]] + plots_Google[[17]]) / (plots_Google[[23]] + plots_Google[[3]]) / (plots_Google[[13]] + plots_Google[[25]]) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", title = element_text(size = 44), axis.title = element_text(size = 40), axis.text = element_text(size = 36), legend.title = element_text(size = 44), legend.text = element_text(size = 38))
  print(plot)
  dev.off()
  
  png(paste0(dir_name, "/preliminary_analysis_Google_2.png"), units="in", width=40, height=55, res=150)
  plot <- (plots_Google[[7]] + plots_Google[[9]]) / (plots_Google[[1]] + plots_Google[[15]]) / (plots_Google[[11]] + plots_Google[[21]])  / (plots_Google[[19]] + plot_spacer()) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", title = element_text(size = 48), axis.title = element_text(size = 44), axis.text = element_text(size = 40), legend.title = element_text(size = 48), legend.text = element_text(size = 42))
  print(plot)
  dev.off()
  
  
  png(paste0(dir_name, "/preliminary_analysis_Facebook_avg_1.png"), units="in", width=40, height=40, res=150)
  plot <- (plots_Facebook[[6]] + plots_Facebook[[18]]) / (plots_Facebook[[24]] + plots_Facebook[[4]]) / (plots_Facebook[[14]] + plots_Facebook[[26]]) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", title = element_text(size = 44), axis.title = element_text(size = 40), axis.text = element_text(size = 36), legend.title = element_text(size = 44), legend.text = element_text(size = 38))
  print(plot)
  dev.off()
  
  png(paste0(dir_name, "/preliminary_analysis_Facebook_avg_2.png"), units="in", width=40, height=55, res=150)
  plot <- (plots_Facebook[[8]] + plots_Facebook[[10]]) / (plots_Facebook[[2]] + plots_Facebook[[16]]) / (plots_Facebook[[12]] + plots_Facebook[[22]]) / (plots_Facebook[[20]] + plot_spacer()) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", title = element_text(size = 48), axis.title = element_text(size = 44), axis.text = element_text(size = 40), legend.title = element_text(size = 48), legend.text = element_text(size = 42))
  print(plot)
  dev.off()
  
  
  png(paste0(dir_name, "/preliminary_analysis_Google_avg_1.png"), units="in", width=40, height=40, res=150)
  plot <- (plots_Google[[6]] + plots_Google[[18]]) / (plots_Google[[24]] + plots_Google[[4]]) / (plots_Google[[14]] + plots_Google[[26]]) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", title = element_text(size = 44), axis.title = element_text(size = 40), axis.text = element_text(size = 36), legend.title = element_text(size = 44), legend.text = element_text(size = 38))
  print(plot)
  dev.off()
  
  png(paste0(dir_name, "/preliminary_analysis_Google_avg_2.png"), units="in", width=40, height=55, res=150)
  plot <- (plots_Google[[8]] + plots_Google[[10]]) / (plots_Google[[2]] + plots_Google[[16]]) / (plots_Google[[12]] + plots_Google[[22]])  / (plots_Google[[20]] + plot_spacer()) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", title = element_text(size = 48), axis.title = element_text(size = 44), axis.text = element_text(size = 40), legend.title = element_text(size = 48), legend.text = element_text(size = 42))
  print(plot)
  dev.off()
}


# Generate some plots using specific restriction indices and Google and Facebook's mobility data.
#
# Inputs:
#   - google_string:            Google's names
#   - facebook_string:          Facebook's names
#   - country_long:             long country name
#   - dir_name:                 directory in which save the plots
#   - mobility_type:            considered mobility variable
#   - dates:                    considered dates
#   - restrictions:             restrictions data
#   - response_Facebook_Google: Facebook and Google's mobility data
mobility_and_restrictions_plot <- function(google_string, facebook_string, country_long, dir_name, mobility_type, dates, restrictions, response_Facebook_Google){
  people_slept_string <- "People sleep in the same place"
  num_rooms_string <- "Rooms used to sleep"
  
  # Prepare dataframe with the trajectories to plot
  trajectories <- data.frame(date=dates$date,
                             value=restrictions$value,
                             color=restrictions_string)
  
  if(mobility_type != "masks"){
    trajectories <- rbind(trajectories, data.frame(date=dates$date,
                                                   value=response_Facebook_Google$value_Google,
                                                   color=google_string))
  }
  
  trajectories <- rbind(trajectories, data.frame(date=dates$date,
                                                 value=response_Facebook_Google$value_Facebook,
                                                 color=facebook_string))
  
  if(mobility_type == "residential"){
    trajectories <- rbind(trajectories, data.frame(date=dates$date,
                                                   value=response_Facebook_Google$people_slept,
                                                   color=people_slept_string))
    
    trajectories <- rbind(trajectories, data.frame(date=dates$date,
                                                   value=response_Facebook_Google$num_rooms,
                                                   color=num_rooms_string))
  }
  
  # Plot the trajectories
  png(paste0(dir_name, gsub(" ", "_", country_long), "/comparison_", gsub(" ", "_", country_long), "_", mobility_type, ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(trajectories) +
    geom_line(aes(x=date, y=value, color=color), linewidth=1.5) +
    labs(x = "Date", y = "Values", color = "Type", title = country_long)
  
  if(mobility_type != "residential"){
    if(mobility_type != "masks"){
      plot <- plot +
        ylim(-1, 1) +
        scale_color_manual(values = c("#6B95DB", "#559e83", "#F3474D")) +
        guides(color = guide_legend(override.aes = list(size = 16), nrow=3, byrow=TRUE))
    }
    else{
      plot <- plot +
        ylim(0, 1.05) +
        scale_color_manual(values = c("#6B95DB", "#F3474D")) +
        guides(color = guide_legend(override.aes = list(size = 16), nrow=2, byrow=TRUE))
    }
  }
  else{
    plot <- plot +
      ylim(0, 1.05) +
      scale_color_manual(values = c("#6B95DB", "#559e83", "#FAAD00", "#F3474D", "#DDAA99")) +
      guides(color = guide_legend(override.aes = list(size = 16), nrow=3, byrow=TRUE))
  }
  
  plot <- plot + 
    theme_bw() +
    theme(legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24))
  print(plot)
  dev.off()
  
  return(trajectories)
}


# Generate IFR plot.
#
# Inputs:
#   - dir_name:               directory in which save the plots
#   - country_short:          short country name
#   - country_long:           long country name
#   - internal_dir_name:      names of the internal directories (for Sybil)
#   - global_initial_date:    global initial date
#   - global_final_date:      global final date
IFR_plot <- function(dir_name, country_short, country_long, internal_dir_name, global_initial_date, global_final_date){
  COVID19_data <- read.csv(paste0("Sybil/", country_short, "/", internal_dir_name, "data.csv"))
  COVID19_data <- COVID19_data %>% filter(date >= global_initial_date, date <= global_final_date)
  COVID19_data$fatalities_over_cases <- COVID19_data$total_deaths / COVID19_data$total_cases
  
  png(paste0(dir_name, gsub(" ", "_", country_long), "/IFR_", gsub(" ", "_", country_long), ".png"), units="in", width=20, height=25, res=150)
  plot <- ggplot(COVID19_data) +
    theme_bw() +
    geom_line(aes(x=as.Date(date), y=fatalities_over_cases), color="#494949", linewidth=1.5) +
    theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 26), legend.text = element_text(size = 22)) +
    labs(x = "Date", y = "Values", title = country_long)
  print(plot)
  dev.off()
}

# Generate boxplot for preliminary data analysis.
#
# Inputs:
#   - model_data_global:  data
boxplots <- function(model_data_global){
  model_data_global <- model_data_global %>%
    select(date, residential, workplaces, transit_stations, retail_and_recreation, grocery_and_pharmacy_stores, masks, StringencyIndex_Average, infection_rates)
  
  model_data_global$StringencyIndex_Average <- 1 - model_data_global$StringencyIndex_Average / 100
  
  df <- pivot_longer(model_data_global, cols = c(residential, workplaces, transit_stations, retail_and_recreation, grocery_and_pharmacy_stores, masks, StringencyIndex_Average, infection_rates), 
                     names_to = "type", values_to = "value")
  
  df$type[which(df$type == "residential")] <- "Residential (Google)"
  df$type[which(df$type == "transit_stations")] <- "Transit stations (Google)"
  df$type[which(df$type == "retail_and_recreation")] <- "Retail and recreation (Google)"
  df$type[which(df$type == "grocery_and_pharmacy_stores")] <- "Grocery and pharmacy stores (Google)"
  df$type[which(df$type == "masks")] <- "Masks (Facebook)"
  df$type[which(df$type == "StringencyIndex_Average")] <- "Complement of Stringency Index (OxCGRT)"
  df$type[which(df$type == "workplaces")] <- "Workplaces (Google)"
  df$type[which(df$type == "infection_rates")] <- "Infection Rates (Sybil)"
  
  df$type <- factor(df$type, levels = c("Grocery and pharmacy stores (Google)", "Retail and recreation (Google)", "Workplaces (Google)", "Transit stations (Google)", "Residential (Google)", "Masks (Facebook)", "Complement of Stringency Index (OxCGRT)", "Infection Rates (Sybil)"))
  
  png("PreliminaryPlots.png", units="in", width=45, height=30, res=150)
  plot <- ggplot(df) +
    geom_boxplot(aes(x=date, y=value, color=type, group=date)) +
    labs(title = "", x = "Date", y = "Value", color = "Type") +
    theme_bw() +
    facet_wrap(~ type, ncol=2, scales = "free") +
    scale_color_manual(values = c("#2FFFCE", "#c3cb71", "#c9c9ff", "#559e83", "#6B95DB", "#985453", "#ff8b94", "#494949")) +
    theme(legend.position = "none", axis.text.x = element_text(size = 30, angle = 45, hjust = 1), axis.text.y = element_text(size = 30), legend.key.size = unit(2, 'cm'), axis.text=element_text(size=45), axis.title=element_text(size=40, face="bold"), plot.title = element_text(size=50, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=25), strip.text.x = element_text(size=40)) +
    guides(color = guide_legend(override.aes = list(size = 16), nrow=3, byrow=TRUE))
  print(plot)
  dev.off()
}