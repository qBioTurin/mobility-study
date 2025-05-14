# Mobility study
#
# Author: Daniele Baccega
# Data:   COVID19 R library, official data (https://cran.r-project.org/web/packages/COVID19/index.html),
#         OxCGRT, restrictions data (https://github.com/OxCGRT/covid-policy-dataset),
#         Facebook mobility data, UMD Global CTIS (https://github.com/GCGImdea/coronasurveys/), 
#         Google mobility data (https://www.google.com/covid19/mobility/),
#         CoronaSurveys, estimate of active COVID-19 cases (https://coronasurveys.org/).


# Import the necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)
library(COVID19)
library(patchwork)
library(countrycode)
library(plotfunctions)
library(reshape)
library(MoMAColors)
library(scales)
library(xgboost)
library(cluster)
library(stringr)
library(DTWBI)
library(data.table)
library(connector)
library(tidyr)

rm(list = ls())


# Include Sybil's functions and other functions useful for the study
source("Sybil/Sybil.R")
source("Sybil/prepare_data_FixedRecoveryRateAndCoronasurveys.R")
source("Sybil/common_functions.R")
source("Sybil/plots.R")

source("functions.R")
source("plots.R")
source("countries_comparison.R")

# Set date format and seed
Sys.setlocale("LC_TIME", "en_US.UTF-8")
set.seed(4542724)


# Obtain the list of the interested countries
countries <- codelist %>%
  select(country.name.en, iso2c, iso3c) %>%
  filter(iso2c %in% c("AT", "BE", "DE", "FR", "IT", "ES", "PT", "GR", "IE", "FI", "SE", "NO", "GB"))

custom_order <- c("Finland" = 1, "Norway" = 2, "Sweden" = 3, "Belgium" = 4,
                  "Ireland" = 5, "United Kingdom" = 6, "France" = 7, "Germany" = 8,
                  "Austria" = 9, "Italy" = 10, "Greece" = 11, "Spain" = 12,
                  "Portugal" = 13)


# Select the configuration for Sybil (with or without variants, daily or daily spline data)
# If in a particular country data are weekly you have to to use daily spline data.
daily_spline <- FALSE
variants <- FALSE

# Initialize the rates
immunization_end_rate <- 1 / 180
recovery_rate <- 1 / 14

# Initialize some variables (roll mean parameter, correlation method)
kfolds <- 5
krollmean <- 7
corr_method = "spearman"


# Mobility types (+ masks) and restriction indices (with maximum values from OxCGRT)
mobility_types <- list("residential", "workplaces", "grocery_and_pharmacy_stores", "transit_stations", "retail_and_recreation", "masks")
restriction_types <- list("C6M_Stay.at.home.requirements", "C2M_Workplace.closing", "StringencyIndex_Average", "C5M_Close.public.transport", c("C3M_Cancel.public.events", "C4M_Restrictions.on.gatherings"), "H6M_Facial.Coverings")
max_restriction_values <- list(3, 3, 100, 2, c(2, 4), 4)


# Delays variables
delay_infrates_mobility <- 0
delay_restrictions_mobility <- 0


# Global directory names
internal_dir_name <- paste0("SIRD", if(variants) "_Variants" else "", if(immunization_end_rate > 0) "_Reinfection" else "", if(daily_spline) "_DailySpline" else "", "_FixedRecoveryRate/")


# Global dates
global_initial_date <- as.Date("2020-04-26")
global_final_date <- as.Date("2022-03-14")
global_dates <- data.frame(date=seq(global_initial_date, global_final_date, 1))

# Dates
initial_date <- as.Date("2020-04-26")
final_date <- as.Date("2021-05-19")
dates <- data.frame(date=seq(initial_date, final_date, 1))


# Local directory names
dir_name_backup <- "backup_data/"

dir_name_plots <- paste0("Delay", delay_infrates_mobility, "/plots/")
dir_name_models <- paste0("Delay", delay_infrates_mobility, "/models/")
dir_name_correlations <- paste0("Delay", delay_infrates_mobility, "/correlations/")
dir_name_regression <- paste0("Delay", delay_infrates_mobility, "/regression/")
dir_name_countries_comparison <- paste0("Delay", delay_infrates_mobility, "/comparison/")

dir_name_regression_plots_xgboost_fromrestomob <- paste0(dir_name_regression, "xgboost_fromrestomob/")
dir_name_regression_plots_xgboost_fromobtoinfrates <- paste0(dir_name_regression, "xgboost_fromobtoinfrates/")


# Local directory creation
system(paste0("mkdir -p ", dir_name_backup))

system(paste0("mkdir -p ", dir_name_plots))
system(paste0("mkdir -p ", dir_name_models))
system(paste0("mkdir -p ", dir_name_correlations))
system(paste0("mkdir -p ", dir_name_regression))
system(paste0("mkdir -p ", dir_name_countries_comparison))

system(paste0("mkdir -p ", dir_name_regression_plots_xgboost_fromobtoinfrates))
system(paste0("mkdir -p ", dir_name_regression_plots_xgboost_fromrestomob))


# Dataframes initialization
correlations_infrates_mobility <- data.frame()
correlations_restrictions_mobility <- data.frame()
correlations_restrinctions <- data.frame()
Facebook_PCA_importance <- data.frame(country=NULL,
                                      residential=NULL, workplaces=NULL, retail_and_recreation=NULL, grocery_and_pharmacy_stores=NULL, transit_stations=NULL)
Google_PCA_importance <- data.frame(country=NULL,
                                    residential=NULL, workplaces=NULL, retail_and_recreation=NULL, grocery_and_pharmacy_stores=NULL, transit_stations=NULL)

xgboost_fromrestomob_8020_errors <- data.frame(country=NULL, rmse=NULL, nmae=NULL)
xgboost_fromobtoinfrates_8020_errors <- data.frame(country=NULL, mobility=NULL, rmse=NULL, nmae=NULL)
xgboost_fromrestomob_barplot <- data.frame(country=NULL, value=NULL, type=NULL)
xgboost_fromobtoinfrates_barplot <- data.frame(country=NULL, value=NULL, type=NULL)

data_countries <- list()

# Prepare data for each country
for(i in seq(1, nrow(countries))){
  # Country information
  country_long_en <- countries$country.name.en[i]
  country_short <- countries$iso2c[i]
  country_short_3c <- countries$iso3c[i]
  external_dir_name <- paste0("Sybil/", country_short, "/")
  
  system(paste0("mkdir -p ", dir_name_backup, gsub(" ", "_", country_long_en)))
  system(paste0("mkdir -p ", dir_name_plots, gsub(" ", "_", country_long_en)))
  system(paste0("mkdir -p ", dir_name_models, gsub(" ", "_", country_long_en)))
  
  # Infection rates and SIRD evolution from Sybil
  data <- load_rates(dir_name_backup, country_short, country_long_en, variants, daily_spline, global_initial_date, global_final_date, internal_dir_name, external_dir_name, immunization_end_rate, recovery_rate)
  rates <- data[[1]]
  SIRD <- data[[2]]
  
  if(is.null(rates) || is.null(SIRD)){
    print(paste0("There are no (or not enough) COVID-19 data for ", country_long_en, ". Skipped."))
    next
  }
  
  
  
  
  # Restrictions from OxCGRT
  data <- load_restrictions(dir_name_backup, global_initial_date, global_final_date, country_long_en, delay_restrictions_mobility, delay_infrates_mobility)
  restrictions_all <- data[[1]]
  restrictions <- data[[2]]
  
  if(nrow(restrictions) == 0){
    print(paste0("There are no (or not enough) restrictions data for ", country_long_en, ". Skipped."))
    next
  }
  
  
  
  
  # People response (Google)
  data <- load_Google_mobility_data(country_long_en, dir_name_backup, country_short, global_initial_date, global_final_date, krollmean, Google_PCA_importance, delay_infrates_mobility)
  response_Google <- data[[1]]
  Google_PCA_importance <- data[[2]]
  
  if(is.null(response_Google)){
    print(paste0("There are no (or not enough) Google's mobility data for ", country_long_en, ". Skipped."))
    next
  }
  
  
  
  
  # People response (Facebook)
  data <- load_Facebook_mobility_data(country_long_en, dir_name_backup, country_short, global_initial_date, global_final_date, krollmean, Facebook_PCA_importance, delay_infrates_mobility)
  response_Facebook <- data[[1]]
  Facebook_PCA_importance <- data[[2]]
  
  if(is.null(response_Facebook)){
    print(paste0("There are no (or not enough) Facebook's mobility data for ", country_long_en, ". Skipped."))
    next
  }

  response_Google <- merge(response_Google, data.frame(date=response_Facebook$date, masks=response_Facebook$masks, masks_moved=response_Facebook$masks_moved), by="date")
  
  

    
  global_initial_date_for_ratios <- max(rates$date[1], restrictions$date[1], response_Facebook$date[1])
  
  data_countries[[i]] <- list(country_long_en=country_long_en,
                              country_short=country_short,
                              country_short_3c=country_short_3c,
                              rates=rates, SIRD=SIRD,
                              restrictions_all=restrictions_all, restrictions=restrictions,
                              response_Google=response_Google, Google_PCA_importance=Google_PCA_importance,
                              response_Facebook=response_Facebook, Facebook_PCA_importance=Facebook_PCA_importance,
                              global_initial_date=max(rates$date[1], restrictions$date[1], response_Google$date[1], response_Facebook$date[1]),
                              global_final_date=min(rates$date[nrow(rates)], restrictions$date[nrow(restrictions)], response_Google$date[nrow(response_Google)], response_Facebook$date[nrow(response_Facebook)]),
                              global_initial_date_for_ratios=global_initial_date_for_ratios)
}

# Set dates based on countries data
global_initial_date_for_ratios <- as.Date(max(sapply(data_countries, function(x){ return(x$global_initial_date_for_ratios) })))
global_dates_for_ratios <- global_dates %>%
  filter(date >= global_initial_date_for_ratios, date <= final_date)

global_initial_date <- as.Date(max(sapply(data_countries, function(x){ return(x$global_initial_date) })))
global_final_date <- as.Date(min(sapply(data_countries, function(x){ return(x$global_final_date) })))
global_dates <- global_dates %>%
  filter(date >= global_initial_date, date <= global_final_date)

global_dates$fold <- sample.int(kfolds, size = nrow(global_dates), replace=TRUE)


initial_date <- global_initial_date
dates <- dates %>%
  filter(date >= initial_date, date <= final_date)


plots_Facebook <- list()
plots_Google <- list()
# Filtering data and some initial plots
for(i in seq(1, nrow(countries))){
  # filtering data
  data_countries[[i]]$rates <- merge(data_countries[[i]]$rates, global_dates, by="date")
  data_countries[[i]]$SIRD <- merge(data_countries[[i]]$SIRD, global_dates, by="date")
  data_countries[[i]]$restrictions <- merge(data_countries[[i]]$restrictions, global_dates, by="date")
  data_countries[[i]]$restrictions_all <- merge(data_countries[[i]]$restrictions_all, global_dates, by="date")
  data_countries[[i]]$response_Google <- merge(data_countries[[i]]$response_Google, global_dates, by="date")
  data_countries[[i]]$response_Facebook <- merge(data_countries[[i]]$response_Facebook, global_dates, by="date")
  
  data_countries[[i]]$rates_for_ratios <- merge(data_countries[[i]]$rates, global_dates_for_ratios, by="date")
  data_countries[[i]]$restrictions_for_ratios <- merge(data_countries[[i]]$restrictions, global_dates_for_ratios, by="date")
  data_countries[[i]]$response_Facebook_for_ratios <- merge(data_countries[[i]]$response_Facebook, global_dates_for_ratios, by="date")
  
  
  data_countries[[i]]$rates_global <- data_countries[[i]]$rates %>%
    filter(date >= global_initial_date, date <= global_final_date)
  
  data_countries[[i]]$SIRD_global <- data_countries[[i]]$SIRD %>%
    filter(date >= global_initial_date, date <= global_final_date)
  
  data_countries[[i]]$restrictions_global <- data_countries[[i]]$restrictions %>%
    filter(date >= global_initial_date, date <= global_final_date)
  
  data_countries[[i]]$restrictions_all_global <- data_countries[[i]]$restrictions_all %>%
    filter(date >= global_initial_date, date <= global_final_date)
  
  data_countries[[i]]$response_Google_global <- data_countries[[i]]$response_Google %>%
    filter(date >= global_initial_date, date <= global_final_date)
  
  data_countries[[i]]$response_Facebook_global <- data_countries[[i]]$response_Facebook %>%
    filter(date >= global_initial_date, date <= global_final_date)
  
  data_countries[[i]]$infection_rates_global <- data.frame(date=data_countries[[i]]$rates_global$date,
                                                           value=data_countries[[i]]$rates_global$infection_rates)
  
  
  data_countries[[i]]$rates_global_for_ratios <- data_countries[[i]]$rates_for_ratios %>%
    filter(date >= global_initial_date_for_ratios, date <= final_date)
  
  data_countries[[i]]$restrictions_global_for_ratios <- data_countries[[i]]$restrictions_for_ratios %>%
    filter(date >= global_initial_date_for_ratios, date <= final_date)
  
  data_countries[[i]]$response_Facebook_global_for_ratios <- data_countries[[i]]$response_Facebook_for_ratios %>%
    filter(date >= global_initial_date_for_ratios, date <= final_date)
  
  data_countries[[i]]$infection_rates_global_for_ratios <- data.frame(date=data_countries[[i]]$rates_global_for_ratios$date,
                                                           value=data_countries[[i]]$rates_global_for_ratios$infection_rates)
  
  # Model data for models from restrictions to mobility
  data_countries[[i]]$model_data_fromrestomob <- merge(global_dates, data_countries[[i]]$response_Google_global %>% select(date, transit_stations, grocery_and_pharmacy_stores, retail_and_recreation, workplaces, residential, masks), by="date", all=TRUE)
  data_countries[[i]]$model_data_fromrestomob <- merge(data_countries[[i]]$model_data_fromrestomob, data_countries[[i]]$restrictions_all_global %>% select(-fold), by="date", all=TRUE)
  data_countries[[i]]$model_data_fromrestomob <- data_countries[[i]]$model_data_fromrestomob %>%
    filter(!if_any(everything(), is.na))

  
  
  
  # Model data
  data_countries[[i]]$model_data <- data.frame(date=data_countries[[i]]$response_Google_global$date,
                                               average_mobility=(data_countries[[i]]$response_Google_global$workplaces_moved + data_countries[[i]]$response_Google_global$residential_moved + data_countries[[i]]$response_Google_global$transit_stations_moved + data_countries[[i]]$response_Google_global$grocery_and_pharmacy_stores_moved + data_countries[[i]]$response_Google_global$retail_and_recreation_moved) / 5,
                                               average_mobility_not_moved=(data_countries[[i]]$response_Google_global$workplaces + data_countries[[i]]$response_Google_global$residential + data_countries[[i]]$response_Google_global$transit_stations + data_countries[[i]]$response_Google_global$grocery_and_pharmacy_stores + data_countries[[i]]$response_Google_global$retail_and_recreation) / 5,
                                               PCA_mobility=data_countries[[i]]$response_Google_global$PCA_mobility_moved,
                                               PCA_mobility_not_moved=data_countries[[i]]$response_Google_global$PCA_mobility,
                                               residential=data_countries[[i]]$response_Google_global$residential_moved, workplaces=data_countries[[i]]$response_Google_global$workplaces_moved, retail_and_recreation=data_countries[[i]]$response_Google_global$retail_and_recreation_moved, grocery_and_pharmacy_stores=data_countries[[i]]$response_Google_global$grocery_and_pharmacy_stores_moved, transit_stations=data_countries[[i]]$response_Google_global$transit_stations_moved, masks=data_countries[[i]]$response_Google_global$masks_moved,
                                               residential_not_moved=data_countries[[i]]$response_Google_global$residential, workplaces_not_moved=data_countries[[i]]$response_Google_global$workplaces, retail_and_recreation_not_moved=data_countries[[i]]$response_Google_global$retail_and_recreation, grocery_and_pharmacy_stores_not_moved=data_countries[[i]]$response_Google_global$grocery_and_pharmacy_stores, transit_stations_not_moved=data_countries[[i]]$response_Google_global$transit_stations, masks_not_moved=data_countries[[i]]$response_Google_global$masks,
                                               infection_rates=data_countries[[i]]$infection_rates_global$value)
  
  data_countries[[i]]$model_data <- cbind(data_countries[[i]]$model_data, data_countries[[i]]$restrictions_all %>% select(-c(date)))

  data_countries[[i]]$model_data <- data_countries[[i]]$model_data %>%
    filter(!if_any(everything(), is.na))
  
      

  
  # filtering data
  data_countries[[i]]$infection_rates <- data_countries[[i]]$infection_rates_global %>%
    filter(date >= initial_date, date <= final_date)
  
  data_countries[[i]]$restrictions <- data_countries[[i]]$restrictions %>%
    filter(date >= initial_date, date <= final_date)
  
  data_countries[[i]]$response_Google <- data_countries[[i]]$response_Google %>%
    filter(date >= initial_date, date <= final_date)
  
  data_countries[[i]]$response_Facebook <- data_countries[[i]]$response_Facebook %>%
    filter(date >= initial_date, date <= final_date)
  
  
  
  
  # Model data for ratios
  data_countries[[i]]$model_data_ratios <- data.frame(date=data_countries[[i]]$response_Facebook_global_for_ratios$date,
                                                      average_mobility=(data_countries[[i]]$response_Facebook_global_for_ratios$workplaces_moved + data_countries[[i]]$response_Facebook_global_for_ratios$residential_moved + data_countries[[i]]$response_Facebook_global_for_ratios$transit_stations_moved + data_countries[[i]]$response_Facebook_global_for_ratios$grocery_and_pharmacy_stores_moved + data_countries[[i]]$response_Facebook_global_for_ratios$retail_and_recreation_moved) / 5,
                                                      PCA_mobility=data_countries[[i]]$response_Facebook_global_for_ratios$PCA_mobility_moved,
                                                      residential=data_countries[[i]]$response_Facebook_global_for_ratios$residential_moved, workplaces=data_countries[[i]]$response_Facebook_global_for_ratios$workplaces_moved, retail_and_recreation=data_countries[[i]]$response_Facebook_global_for_ratios$retail_and_recreation_moved, grocery_and_pharmacy_stores=data_countries[[i]]$response_Facebook_global_for_ratios$grocery_and_pharmacy_stores_moved, transit_stations=data_countries[[i]]$response_Facebook_global_for_ratios$transit_stations_moved, masks=data_countries[[i]]$response_Facebook_global_for_ratios$masks_moved,
                                                      StringencyIndex_Average=data_countries[[i]]$restrictions_global_for_ratios$StringencyIndex_Average_moved, C6M_Stay.at.home.requirements=data_countries[[i]]$restrictions_global_for_ratios$C6M_Stay.at.home.requirements_moved, C2M_Workplace.closing=data_countries[[i]]$restrictions_global_for_ratios$C2M_Workplace.closing_moved, C5M_Close.public.transport=data_countries[[i]]$restrictions_global_for_ratios$C5M_Close.public.transport_moved, C3M_Cancel.public.events=data_countries[[i]]$restrictions_global_for_ratios$C3M_Cancel.public.events_moved, C4M_Restrictions.on.gatherings=data_countries[[i]]$restrictions_global_for_ratios$C4M_Restrictions.on.gatherings_moved, H6M_Facial.Coverings=data_countries[[i]]$restrictions_global_for_ratios$H6M_Facial.Coverings_moved,
                                                      infection_rates=data_countries[[i]]$infection_rates_global_for_ratios$value)
  
  data_countries[[i]]$model_data_ratios <- data_countries[[i]]$model_data_ratios %>%
    filter(!if_any(everything(), is.na))
  
  
  # Correlations
  correlations_infrates_mobility <- infrates_mobility_correlation(delay_infrates_mobility, correlations_infrates_mobility, data_countries[[i]]$model_data, corr_method, data_countries[[i]]$country_long_en, data_countries[[i]]$country_short, data_countries[[i]]$country_short_3c)
  correlations_restrictions_mobility <- restrictions_mobility_correlation(delay_restrictions_mobility, correlations_restrictions_mobility, data_countries[[i]]$model_data, corr_method, data_countries[[i]]$country_long_en, data_countries[[i]]$country_short, data_countries[[i]]$country_short_3c)

  
  # Mobility plot
  infection_rates_I_plot(dir_name_plots, data_countries[[i]]$country_long_en, data_countries[[i]]$infection_rates_global, data_countries[[i]]$SIRD_global, corr_method)
  plots_Google <- mobility_plot(dir_name_plots, global_dates, data_countries[[i]]$restrictions_global, data_countries[[i]]$response_Google_global, data_countries[[i]]$infection_rates_global, data_countries[[i]]$country_long_en, "Google", plots_Google)
  plots_Facebook <- mobility_plot(dir_name_plots, dates, data_countries[[i]]$restrictions, data_countries[[i]]$response_Facebook, data_countries[[i]]$infection_rates, data_countries[[i]]$country_long_en, "Facebook", plots_Facebook)
  
  # XGBoost for missing data
  xgboost_fromrestomob_8020_errors <- xgboost_fromrestomob_8020_model(dir_name_models, data_countries[[i]]$country_long_en, data_countries[[i]]$model_data, dir_name_regression_plots_xgboost_fromrestomob, kfolds, xgboost_fromrestomob_8020_errors, mobility_types, data_countries[[i]]$restrictions_all)
  
  p1 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromrestomob, "fitted_test_set_", mobility_types[[1]], "_", data_countries[[i]]$country_long_en, "_fold1.RDs"))
  p2 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromrestomob, "fitted_test_set_", mobility_types[[2]], "_", data_countries[[i]]$country_long_en, "_fold1.RDs"))
  p3 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromrestomob, "fitted_test_set_", mobility_types[[3]], "_", data_countries[[i]]$country_long_en, "_fold1.RDs"))
  p4 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromrestomob, "fitted_test_set_", mobility_types[[4]], "_", data_countries[[i]]$country_long_en, "_fold1.RDs"))
  p5 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromrestomob, "fitted_test_set_", mobility_types[[5]], "_", data_countries[[i]]$country_long_en, "_fold1.RDs"))
  p6 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromrestomob, "fitted_test_set_", mobility_types[[6]], "_", data_countries[[i]]$country_long_en, "_fold1.RDs"))
  
  p <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
    plot_annotation(title = gsub("_", " ", data_countries[[i]]$country_long_en), theme = theme(plot.title = element_text(size = 55, face = "bold"))) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name_regression_plots_xgboost_fromrestomob, "fitted_test_set_", data_countries[[i]]$country_long_en, ".png"), units="in", width=40, height=30, res=300)
  print(p)
  dev.off()
  
  
  xgboost_fromobtoinfrates_8020_errors <- xgboost_fromobtoinfrates_8020_model(dir_name_models, data_countries[[i]]$country_long_en, data_countries[[i]]$model_data, dir_name_regression_plots_xgboost_fromobtoinfrates, kfolds, xgboost_fromobtoinfrates_8020_errors)

  # XGBoost (100% training for feature importance)
  xgboost_fromrestomob_barplot <- xgboost_fromrestomob_100_model(dir_name_models, data_countries[[i]]$country_long_en, data_countries[[i]]$model_data, dir_name_regression_plots_xgboost_fromrestomob, mobility_types, data_countries[[i]]$restrictions_all, xgboost_fromrestomob_barplot)
  xgboost_fromobtoinfrates_barplot <- xgboost_fromobtoinfrates_100_model(dir_name_models, data_countries[[i]]$country_long_en, data_countries[[i]]$model_data, dir_name_regression_plots_xgboost_fromobtoinfrates, xgboost_fromobtoinfrates_barplot)

  
  
  
  # Consider each mobility type
  correlation_Google_Facebook <- rep(NA, length(mobility_types))
  correlation_restr_Google <- rep(NA, length(mobility_types))
  correlation_restr_Facebook <- rep(NA, length(mobility_types))
  correlation_inf_restr <- rep(NA, length(mobility_types))
  for(j in seq(1, length(mobility_types))){
    # Compute restrictions
    data <- compute_restrictions(data_countries[[i]]$restrictions, restriction_types, max_restriction_values, dates, delay_restrictions_mobility+delay_infrates_mobility)
    normalized_restrictions <- data[[1]]
    restrictions_string <- data[[2]]




    # Merge Google and Facebook mobility data inside a dataframe
    roll_response_Facebook_Google <- merge_Facebook_and_Google_mobility_data(dir_name_backup, data_countries[[i]]$response_Google, data_countries[[i]]$response_Facebook, mobility_types[[j]], data_countries[[i]]$country_long_en, dates)




    # Mobility and restrictions plot
    google_string <- paste0("Mobility (Google, ", gsub("_", " ", mobility_types[[j]]), ")")
    facebook_string <- paste0("Mobility (Facebook, ", gsub("_", " ", mobility_types[[j]]), ")")
    neutral_string <- paste0("Mobility (", gsub("_", " ", mobility_types[[j]]), ")")

    trajectories <- mobility_and_restrictions_plot(google_string, facebook_string, data_countries[[i]]$country_long_en, dir_name_plots, mobility_types[[j]], dates, normalized_restrictions, roll_response_Facebook_Google)




    # Plot total fatalities over total detected infections (IFR)
    IFR_plot(dir_name_plots, data_countries[[i]]$country_short, data_countries[[i]]$country_long_en, internal_dir_name, global_initial_date, global_final_date)




    # Compute correlations between mobility and restrictions
    new_correlations <- compute_correlations(data_countries[[i]]$infection_rates, trajectories, restrictions_string, google_string, facebook_string, corr_method, mobility_types[[j]])

    correlations_restrinctions <- rbind(correlations_restrinctions,
                                        data.frame(country_long=data_countries[[i]]$country_long_en, country_short=data_countries[[i]]$country_short, iso_alpha=data_countries[[i]]$country_short_3c,
                                                   correlation_Google_Facebook=new_correlations[[1]], correlation_restr_Google=new_correlations[[2]], correlation_restr_Facebook=new_correlations[[3]], correlation_inf_restr=new_correlations[[4]],
                                                   mobility_type=neutral_string, restriction_type=restrictions_string))
  }
  
  model_data_global <- if (exists("model_data_global")) rbind(model_data_global, data_countries[[i]]$model_data) else data_countries[[i]]$model_data
  model_data_fromrestomob_global <- if (exists("model_data_fromrestomob_global")) rbind(model_data_fromrestomob_global, data_countries[[i]]$model_data_fromrestomob) else data_countries[[i]]$model_data_fromrestomob
  model_data_ratios_global <- if (exists("model_data_ratios_global")) rbind(model_data_ratios_global, data_countries[[i]]$model_data_ratios) else data_countries[[i]]$model_data_ratios
  
  
  saveRDS(data_countries[[i]]$model_data, paste0(dir_name_models, gsub(" ", "_", data_countries[[i]]$country_long_en), "/model_data.RDs"))
  saveRDS(data_countries[[i]]$model_data_fromrestomob, paste0(dir_name_models, gsub(" ", "_", data_countries[[i]]$country_long_en), "/model_data_fromrestomob.RDs"))
  saveRDS(data_countries[[i]]$model_data_ratios, paste0(dir_name_models, gsub(" ", "_", data_countries[[i]]$country_long_en), "/model_data_ratios.RDs"))
}

write.csv(correlations_infrates_mobility, paste0(dir_name_correlations, "correlations_infrates_mobility.csv"))
write.csv(correlations_restrictions_mobility, paste0(dir_name_correlations, "correlations_restrictions_mobility.csv"))
write.csv(correlations_restrinctions, paste0(dir_name_correlations, "correlations_restrictions.csv"))

saveRDS(model_data_global, paste0(dir_name_models, "model_data_global.RDs"))
saveRDS(model_data_fromrestomob_global, paste0(dir_name_models, "model_data_fromrestomob_global.RDs"))
saveRDS(model_data_ratios_global, paste0(dir_name_models, "model_data_ratios_global.RDs"))

saveRDS(data_countries, paste0(dir_name_models, "data_countries.RDs"))

preliminary_plots(dir_name_plots, plots_Facebook, plots_Google)

p1 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[1]]$country_long_en, "_fold1.RDs"))
p2 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[2]]$country_long_en, "_fold1.RDs"))
p3 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[3]]$country_long_en, "_fold1.RDs"))
p4 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[4]]$country_long_en, "_fold1.RDs"))
p5 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[5]]$country_long_en, "_fold1.RDs"))
p6 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[6]]$country_long_en, "_fold1.RDs"))
p7 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[7]]$country_long_en, "_fold1.RDs"))
p8 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[8]]$country_long_en, "_fold1.RDs"))
p9 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[9]]$country_long_en, "_fold1.RDs"))
p10 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[10]]$country_long_en, "_fold1.RDs"))
p11 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[11]]$country_long_en, "_fold1.RDs"))
p12 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[12]]$country_long_en, "_fold1.RDs"))
p13 <- readRDS(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_", data_countries[[13]]$country_long_en, "_fold1.RDs"))

p <- (p3 + p9) / (p12 + p2) / (p7 + p13) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical")

png(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_countries1.png"), units="in", width=40, height=30, res=300)
print(p)
dev.off()

p <- (p4 + p5) / (p12 + p8) / (p6 + p11) / (p10 + plot_spacer()) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical")

png(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "fitted_test_set_countries2.png"), units="in", width=40, height=40, res=300)
print(p)
dev.off()




# Preliminary plot
boxplots(model_data_global)

# Ratios and clustering
countries_comparison(dir_name_countries_comparison, dir_name_models, countries, custom_order, data_countries, corr_method)

# XGBoost study (100% training for feature importance)
xgboost_study(xgboost_fromrestomob_barplot, xgboost_fromobtoinfrates_barplot, dir_name_regression_plots_xgboost_fromrestomob, dir_name_regression_plots_xgboost_fromobtoinfrates, custom_order)
