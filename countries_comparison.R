# Pattern and clustering.
#
# Inputs:
#   - dir_name_comparison: directory in which save the plots
#   - dir_name_models:     directory in which save the models
#   - countries:           considered countries
#   - custom_order:        custom countries order
#   - data_countries:      country data
countries_comparison <- function(dir_name_comparison, dir_name_models, countries, custom_order, data_countries){
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
  
  dates_count <- ratio_resinf_all %>%
    group_by(date) %>%
    count() %>%
    filter(n == nrow(countries))
  
  
  # Plots ratio from stringency index to infection rates 
  ratio_resinf_all <- ratio_resinf_all %>%
    filter(date %in% dates_count$date)
  
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
    labs(title= "ComplStrIdxOnInfRates(t)", x = "Date", y = "Country", fill="Ratio") +
    theme(legend.position = "right", title = element_text(size = 34), legend.key.size = unit(2, 'cm'), axis.title = element_text(size = 34), axis.text = element_text(size = 32), legend.title = element_text(size = 30), legend.text = element_text(size = 28))
  print(plot)
  dev.off()
  
  
  # Plots ratio from average mobility to infection rates
  ratio_mobinf_all <- ratio_mobinf_all %>%
    filter(date %in% dates_count$date)
  
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
    labs(title = "AvgMobOnInfRates(t)", x = "Date", y = "Country", fill="Ratio") +
    theme(legend.position = "right", title = element_text(size = 34), legend.key.size = unit(2, 'cm'), axis.title = element_text(size = 34), axis.text = element_text(size = 32), legend.title = element_text(size = 30), legend.text = element_text(size = 28))
  print(plot)
  dev.off()
  
  
  # Plots ratio from stringency index to average mobility
  ratio_resmob_all <- ratio_resmob_all %>%
    filter(date %in% dates_count$date)
  
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
    labs(title = "ComplStrIdxOnAvgMob(t)", x = "Date", y = "Country", fill="Ratio") +
    theme(legend.position = "right", title = element_text(size = 34), legend.key.size = unit(2, 'cm'), axis.title = element_text(size = 34), axis.text = element_text(size = 32), legend.title = element_text(size = 30), legend.text = element_text(size = 28))
  print(plot)
  dev.off()

  
  # Clustering (K-means and CONNECTOR)
  plots <- list()
  plots <- clustering(ratio_resmob_all, "resmob", 8, c(2, 5, 6), c(2, 3, 5), dir_name_comparison, plots)
  plots <- clustering(ratio_mobinf_all, "mobinf", 11, c(2, 5, 6), c(2, 5, 6), dir_name_comparison, plots)
  plots <- clustering(ratio_resinf_all, "resinf", 11, c(2, 5, 6), c(2, 3, 6), dir_name_comparison, plots)
  
  

  # Clustering plots with K-means
  # Average
  p <- plots[[1]] / plots[[5]] / plots[[7]] / plots[[9]] / plots[[13]] / plots[[15]] +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name_comparison, "clusters_mean.png"), units="in", width=40, height=30, res=300)
  print(p)
  dev.off()
  
  # Average + standard deviation
  p <- plots[[2]] / plots[[4]] / plots[[8]] / plots[[12]] / plots[[14]] / plots[[18]] +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name_comparison, "clusters_meanstd.png"), units="in", width=40, height=30, res=300)
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
clustering <- function(data, type, p_selected, G_clusters, k_clusters, dir_name_comparison, plots){
  pastel_color_colors <- c(
    "#D64D4D",
    "#6FFF6D",
    "#6DBBFF",
    "#D68B4D",
    "#9A6DFF",
    "#FFB3FC"
  )
  
  title_mapping <- c("mobinf"="AvgMobOnInfRates(t)", "resmob"="ComplStrIdxOnAvgMob(t)", "resinf"="ComplStrIdxOnInfRates(t)")
  
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
  
  silhouette_df <- data.frame(k=k, value=avg_sil, type="Mean")
  
  
  silhouette_score_meanstd <- function(k){
    km <- kmeans(data_meanstd %>% select(mean, std), k, nstart=25)
    ss <- silhouette(km$cluster, dist(data_meanstd %>% select(mean, std)))
    mean(ss[, 3])
  }
  k <- 2:6
  avg_sil <- sapply(k, silhouette_score_meanstd)
  
  silhouette_df <- rbind(silhouette_df, data.frame(k=k, value=avg_sil, type="Mean and std"))
  
  png(paste0(dir_name_comparison, "silhouette_scores_", type, ".png"), units="in", width=34, height=15, res=300)
  p <- ggplot(silhouette_df) +
    geom_line(aes(x=k, y=value, linetype=type), linewidth=2) +
    theme_bw() +
    geom_point(aes(x=k, y=value), shape=1, size=8) +
    labs(title=title_mapping[type], x = "Number of clusters", y = "Average Silhouette Scores", linetype="Used data:") +
    theme(legend.key.size = unit(2, 'cm'), legend.position = "bottom", title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24))
  print(p)
  dev.off()
  
  
  for(i in 1:length(k_clusters)){
    # Clustering with K-means
    km.res_mean <- kmeans(data_mean %>% select(mean), k_clusters[i], nstart = 25)
    km.res_meanstd <- kmeans(data_meanstd %>% select(mean, std), k_clusters[i], nstart = 25)
    
    data_local_cluster_mean <- cbind(data_mean, data.frame(kmeans=km.res_mean$cluster))
    data_local_cluster_meanstd <- cbind(data_meanstd, data.frame(kmeans=km.res_meanstd$cluster))
    
    
    
    centers_mean <- as.data.frame(km.res_mean$centers)
    centers_mean$kmeans <- rownames(centers_mean)
    
    data_local_cluster_mean$source <- "Data"
    centers_mean$source <- "Centroids"
    centers_mean$country <- ""
    centers_mean$country_short_col <- ""
    combined_data <- rbind(data_local_cluster_mean, centers_mean)
    
    cluster_sizes <- data_local_cluster_mean %>%
      group_by(kmeans) %>%
      summarise(size = n())
    
    data_local_cluster_mean <- data_local_cluster_mean %>%
      left_join(cluster_sizes, by = "kmeans")
    
    png(paste0(dir_name_comparison, "kmeans_", type, "_", k_clusters[i], "_mean.png"), unit="in", width=40, height=5, res=150)
    p <- ggplot(data=combined_data, aes(x = mean, y = 0, color = as.factor(kmeans))) +
      geom_point(data=data_local_cluster_mean, aes(shape = "Data"), size = 20, alpha = 0.7) +
      geom_point(data=centers_mean, aes(shape = "Centroids"), size = 20, stroke = 4) +
      geom_text(data = combined_data, aes(label = country_short_col), vjust = -2, hjust = 0.5, size = 10, color = "black") +
      theme_minimal() +
      labs(title=paste0(title_mapping[type], " ", k_clusters[i], " clusters"), x = "Mean", y = "", color = "Cluster", shape = "Type") +
      scale_shape_manual(values = c("Data" = 16, "Centroids" = 4)) +
      scale_color_manual(values = pastel_color_colors) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 45, face = "bold"),
        axis.text = element_text(size = 35),
        legend.title = element_text(size = 45, face = "bold"),
        legend.text = element_text(size = 40),
        axis.text.y = element_blank()
      ) +
      xlim(min(data_local_cluster_mean$mean), max(data_local_cluster_mean$mean)) +
      guides(fill = "none", color =if(k_clusters[i] != 5) "none")
    print(p)
    dev.off()
    
    plots[[length(plots) + 1]] <- p
    
    colnames(data_local_cluster_mean)[colnames(data_local_cluster_mean) == "kmeans"] <- paste0("kmeans", k_clusters[i])
    saveRDS(data_local_cluster_mean, paste0(dir_name_comparison, "ratio_", type, "_", k_clusters[i], "clusters_all_mean.RDs"))
    
    
    
    
    centers_meanstd <- as.data.frame(km.res_meanstd$centers)
    centers_meanstd$kmeans <- rownames(centers_mean)
    
    data_local_cluster_meanstd$source <- "Data"
    centers_meanstd$source <- "Centroids"
    centers_meanstd$country <- ""
    centers_meanstd$country_short_col <- ""
    combined_data <- rbind(data_local_cluster_meanstd, centers_meanstd)
    
    png(paste0(dir_name_comparison, "kmeans_", type, "_", k_clusters[i], "_meanstd.png"), unit="in", width=40, height=10, res=150)
    p <- ggplot(data=combined_data, aes(x = mean, y = std, color = as.factor(kmeans))) +
      geom_point(data=data_local_cluster_meanstd, aes(shape = "Data"), size = 20, alpha = 0.7) +
      geom_point(
        data = centers_meanstd,
        aes(x = mean, y = std, shape = "Centroids"),
        size = 20,
        stroke = 4
      ) +
      geom_text(data = combined_data, aes(label = country_short_col), vjust = -2, hjust = 0.5, size = 10, color = "black") +
      theme_minimal() +
      labs(
        title=paste0(title_mapping[type], " ", k_clusters[i], " clusters"),
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
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 45, face = "bold"),
        axis.text = element_text(size = 35),
        legend.title = element_text(size = 45, face = "bold"),
        legend.text = element_text(size = 40)
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
  
  
  
  # Clustering with CONNECTOR
  data_local <- data %>%
    mutate(ID = recode(country, !!!custom_order), Observation = value, Time = date) %>%
    select(ID, Observation, Time)
  data_local$Time <- as.numeric(data_local$Time)
  data_ann <- data.frame(ID=unname(custom_order), Country=names(custom_order), row.names = NULL)

  CONNECTORList <- DataFrameImport(data_local, data_ann)

  CrossLogLike <- BasisDimension.Choice(data = CONNECTORList,
                                        p = 2:12)

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
                                   Cores=4)

  IndexesPlot.Extrapolation(ClusteringList)-> indexes

  png(paste0(dir_name_comparison, "indexesPlot_", type, ".png"), units="in", width=34, height=15, res=300)
  p <- ggplot_build(indexes$Plot)
  p$plot <- p$plot +
    labs(title = title_mapping[type]) +
    theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24))
  print(p$plot)
  dev.off()

  ConsMatrix<-ConsMatrix.Extrapolation(stability.list = ClusteringList)


  for(i in 1:length(G_clusters)){
    png(paste0(dir_name_comparison, "G", G_clusters[i], "_", type, ".png"), units="in", width=34, height=15, res=300)
    p <- ggplot_build(ConsMatrix[[paste0("G", G_clusters[i])]]$ConsensusPlot)
    p$plot <- p$plot +
      theme(legend.key.size = unit(1.5, 'cm'), title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24))
    print(p$plot)
    dev.off()

    CONNECTORList.FCM.opt <- MostProbableClustering.Extrapolation(
      stability.list = ClusteringList,
      G = G_clusters[i])

    FCMplots <- ClusterWithMeanCurve(clusterdata = CONNECTORList.FCM.opt,
                                     feature = "Country",
                                     labels = c("Day", "Ratio"))

    png(paste0(dir_name_comparison, "clustering_connector_", type, "_", G_clusters[i], ".png"), units="in", width=34, height=15, res=300)
    plot <- FCMplots$plotsCluster$ALL +
      labs(title = paste0("Other parameters p = ", p_selected, ", h = ", G_clusters[i] - 1, ", G = ", G_clusters[i], " (", title_mapping[type], ")")) +
      theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24), strip.text.x = element_text(size=30))
    print(plot)
    dev.off()
  }

  return(plots)
}