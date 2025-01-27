# Generate the rates (infection, recovery, and fatality) and the evolution of
# the compartmental model using Sybil
#
# Inputs:
#   - dir_name_backup:        name of the directory in which save the infection rates
#   - country_short:          short country name
#   - country_long:           long country name
#   - variants:               true if we are considering variants, false otherwise
#   - daily_spline:           true if we approximate daily data with a spline, false otherwise
#   - global_initial_date:    global initial date
#   - global_final_date:      global final date
#   - internal_dir_name:      names of the internal directories (for Sybil)
#   - external_dir_name:      names of the external directories (for Sybil)
#   - immunization_end_rate:  immunization end rate
#   - recovery_rate:          recovery rate
#
# Output:
#   - rates:                  rates (infection, recovery, and fatality)
#   - SIRD:                   evolution of the compartmental model
load_rates <- function(dir_name_backup, country_short, country_long, variants, daily_spline, global_initial_date, global_final_date, internal_dir_name, external_dir_name, immunization_end_rate, recovery_rate){
  if(file.exists(paste0(dir_name_backup, gsub(" ", "_", country_long), "/rates.RData"))){
    load(paste0(dir_name_backup, gsub(" ", "_", country_long), "/rates.RData"))
    load(paste0(dir_name_backup, gsub(" ", "_", country_long), "/SIRD.RData"))
  }
  else{
    # Check if there exist data on the considered country in the COVID-19 dataset
    country_data <- covid19(country = country_short, verbose = FALSE)
    
    if(nrow(country_data) == 0)
      return(NULL)
    
    if(!file.exists(paste0("Sybil/", country_short))){
      system(paste0("mkdir -p Sybil/", country_short))
    }

    # Call Sybil here to extract the rates (without doing a forecast)
    data <- prepare_data(gsub(" ", "_", country_long), global_initial_date, global_final_date, immunization_end_rate, recovery_rate, variants, list(), list(), list(), daily_spline)
    if(!is.list(data))
      return(NULL)
    
    df_variants_all <- data[[1]]
    df_disease_all <- data[[2]]
    SIRDS_initial_marking <- data[[3]]


    Sybil(df_disease_all, df_variants_all, SIRDS_initial_marking, variants, FALSE, daily_spline, external_dir_name, immunization_end_rate, recovery_rate)

    # Filter the rates extracted with Sybil
    rates <- read.csv(paste0("Sybil/", country_short, "/", internal_dir_name, "rates.csv"))

    rates <- merge(data.frame(date=seq.Date(global_initial_date, global_final_date, 1)), rates, by="date", all=TRUE)
    rates$infection_rates[which(rates$infection_rates < 0)] <- 0

    rates <- rates %>%
      filter(!is.na(infection_rates))
    
    evolution <- read.csv(paste0(external_dir_name, internal_dir_name, "evolution.csv"))
    SIRD <- data.frame(date=as.Date(unique(evolution$date)), S=evolution$S[1:length(unique(evolution$date))], R=evolution$R[1:length(unique(evolution$date))], D=evolution$D[1:length(unique(evolution$date))], I=evolution$I[1:length(unique(evolution$date))])
    
    save(rates, file=paste0(dir_name_backup, gsub(" ", "_", country_long), "/rates.RData"))
    save(SIRD, file=paste0(dir_name_backup, gsub(" ", "_", country_long), "/SIRD.RData"))
  }
  
  return(list(rates, SIRD))
}


# Load restrictions data.
#
# Inputs:
#   - dir_name_backup:              name of the directory in which save restrictions data
#   - global_initial_date:          global initial date
#   - global_final_date:            global final date
#   - country_long:                 long country name
#   - delay_restrictions_mobility:  delay between restrictions and mobility
#   - delay_infrates_mobility:      delay between mobility and infection rates
#
# Output:
#   - restrictions_all:         all restrictions
#   - restrictions:             specific restrictions
load_restrictions <- function(dir_name_backup, global_initial_date, global_final_date, country_long, delay_restrictions_mobility, delay_infrates_mobility){
  if(file.exists(paste0(dir_name_backup, gsub(" ", "_", country_long), "/restrictions_all.RData"))){
    load(paste0(dir_name_backup, gsub(" ", "_", country_long), "/restrictions_all.RData"))
  }
  else{
    # Load restrictions data
    restrictions_all <- read.csv("covid-policy-dataset/data/OxCGRT_compact_national_v1.csv")
    restrictions_all$Date <- as.Date(paste("", restrictions_all$Date), format="%Y%m%d")
    restrictions_all <- restrictions_all %>%
      mutate(date = Date) %>%
      filter(date <= global_final_date, date >= global_initial_date, CountryName == country_long) %>%
      select(date, C1M_School.closing, C2M_Workplace.closing, C3M_Cancel.public.events, C4M_Restrictions.on.gatherings, C5M_Close.public.transport, C6M_Stay.at.home.requirements, C7M_Restrictions.on.internal.movement, C8EV_International.travel.controls,
             E1_Income.support, E2_Debt.contract.relief, E3_Fiscal.measures, E4_International.support,
             H1_Public.information.campaigns, H2_Testing.policy, H3_Contact.tracing, H4_Emergency.investment.in.healthcare, H5_Investment.in.vaccines, H6M_Facial.Coverings, H7_Vaccination.policy, H8M_Protection.of.elderly.people,
             V1_Vaccine.Prioritisation..summary., V2A_Vaccine.Availability..summary., V2D_Medically..clinically.vulnerable..Non.elderly., V2E_Education, V2F_Frontline.workers...non.healthcare., V2G_Frontline.workers...healthcare., V3_Vaccine.Financial.Support..summary., V4_Mandatory.Vaccination..summary.,
             StringencyIndex_Average, GovernmentResponseIndex_Average, ContainmentHealthIndex_Average, EconomicSupportIndex)
    
    restrictions_all[is.na(restrictions_all)] <- 0
    
    save(restrictions_all, file=paste0(dir_name_backup, gsub(" ", "_", country_long), "/restrictions_all.RData"))
  }
  
  # Filter and move specific restrictions
  restrictions <- restrictions_all %>%
    select(date, StringencyIndex_Average, C6M_Stay.at.home.requirements, C2M_Workplace.closing, C5M_Close.public.transport, C3M_Cancel.public.events, C4M_Restrictions.on.gatherings, H6M_Facial.Coverings) %>%
    mutate(StringencyIndex_Average = 1 - (StringencyIndex_Average / 100))
  
  restrictions$StringencyIndex_Average_moved <- move_n_point(restrictions$StringencyIndex_Average, n=delay_restrictions_mobility+delay_infrates_mobility)
  restrictions$C6M_Stay.at.home.requirements_moved <- move_n_point(restrictions$C6M_Stay.at.home.requirements, n=delay_restrictions_mobility+delay_infrates_mobility)
  restrictions$C2M_Workplace.closing_moved <- move_n_point(restrictions$C2M_Workplace.closing, n=delay_restrictions_mobility+delay_infrates_mobility)
  restrictions$C5M_Close.public.transport_moved <- move_n_point(restrictions$C5M_Close.public.transport, n=delay_restrictions_mobility+delay_infrates_mobility)
  restrictions$C3M_Cancel.public.events_moved <- move_n_point(restrictions$C3M_Cancel.public.events, n=delay_restrictions_mobility+delay_infrates_mobility)
  restrictions$C4M_Restrictions.on.gatherings_moved <- move_n_point(restrictions$C4M_Restrictions.on.gatherings, n=delay_restrictions_mobility+delay_infrates_mobility)
  restrictions$H6M_Facial.Coverings_moved <- move_n_point(restrictions$H6M_Facial.Coverings, n=delay_restrictions_mobility+delay_infrates_mobility)
  
  return(list(restrictions_all, restrictions))
}

# Process restrictions data.
#
# Inputs:
#   - restrictions:               restrictions data
#   - restriction_types:          restrictions variables associated to each mobility variable
#   - max_restr_values:           restrictions variables maximum values for normalization
#   - dates:                      considered dates
#   - delay:                      delay between restrictions and mobility
#
# Output:
#   - normalized_restrictions:    normalized restrictions data
#   - restrictions_string:        restrictions names
compute_restrictions <- function(restrictions, restriction_types, max_restr_values, dates, delay){
  normalized_restrictions <- rep(0, nrow(restrictions))
  
  restrictions_string <- "Restriction ("
  for(l in seq(1, length(restriction_types[[j]]))){
    normalized_restrictions <- normalized_restrictions + (restrictions[, restriction_types[[j]][[l]]])
    restrictions_string <- paste0(restrictions_string, restriction_types[[j]][[l]])
    if(l != length(restriction_types[[j]]))
      restrictions_string <- paste0(restrictions_string, ", ")
  }
  restrictions_string <- paste0(restrictions_string, ")")
  
  normalized_values <- 1 - (normalized_restrictions / sum(max_restr_values[[j]]))
  
  normalized_restrictions <- merge(dates, data.frame(date=restrictions$date, value=normalized_values, by="date", all=TRUE))
  normalized_restrictions$value <- move_n_point(normalized_restrictions$value, n=delay)
  
  return(list(normalized_restrictions, restrictions_string))
}


# Load Google's mobility data.
#
# Inputs:
#   - country_long:             long country name
#   - dir_name_backup:          name of the directory in which save Google's mobility data
#   - country_short:            short country name
#   - global_initial_date:      global initial date
#   - global_final_date:        global final date
#   - k:                        rollmean parameter
#   - Google_PCA_importance:    dataframe with PCA importance of each mobility variable for all the considered countries
#   - delay:                    delay between mobility and infection rates
#
# Output:
#   - response_Google:          Google's mobility data
#   - Google_PCA_importance:    dataframe with PCA importance of each mobility variable for all the considered countries
load_Google_mobility_data <- function(country_long, dir_name_backup, country_short, global_initial_date, global_final_date, k, Google_PCA_importance, delay){
  if(file.exists(paste0(dir_name_backup, gsub(" ", "_", country_long), "/response_Google.RData"))){
    load(paste0(dir_name_backup, gsub(" ", "_", country_long), "/response_Google.RData"))
    load(paste0(dir_name_backup, gsub(" ", "_", country_long), "/Google_PCA_importance.RData"))
  }
  else{
    # Load Google mobility data
    years <- c("2020", "2021", "2022")
    response_Google_filename <- paste0("Region_Mobility_Report_CSVs/", years, "_", country_short, "_Region_Mobility_Report.csv")
    
    if(!all(file.exists(response_Google_filename)))
      return(NULL)
    
    response_Google <- do.call(rbind, 
                               lapply(response_Google_filename, read.csv, na.strings = ""))
    
    response_Google <- response_Google %>%
      filter(is.na(sub_region_1), is.na(sub_region_2), is.na(metro_area)) %>%
      mutate(parks = rollmean(na.approx(parks_percent_change_from_baseline / 100, na.rm = FALSE), k, align = "center", na.pad = TRUE),
             residential = rollmean(na.approx(residential_percent_change_from_baseline / 100, na.rm = FALSE), k, align = "center", na.pad = TRUE),
             workplaces = rollmean(na.approx(workplaces_percent_change_from_baseline / 100, na.rm = FALSE), k, align = "center", na.pad = TRUE),
             grocery_and_pharmacy_stores = rollmean(na.approx(grocery_and_pharmacy_percent_change_from_baseline / 100, na.rm = FALSE), k, align = "center", na.pad = TRUE),
             transit_stations = rollmean(na.approx(transit_stations_percent_change_from_baseline / 100, na.rm = FALSE), k, align = "center", na.pad = TRUE),
             retail_and_recreation = rollmean(na.approx(retail_and_recreation_percent_change_from_baseline / 100, na.rm = FALSE), k, align = "center", na.pad = TRUE)) %>%
      filter(date <= global_final_date, date >= global_initial_date)
    
    response_Google <- merge(data.frame(date=seq.Date(global_initial_date, global_final_date, 1)), response_Google, by="date", all=TRUE)
    
    response_Google <- response_Google %>%
      filter(!is.na(grocery_and_pharmacy_stores), !is.na(transit_stations), !is.na(retail_and_recreation), !is.na(workplaces), !is.na(residential))
    
    
    # Compute PCA
    PCA <- princomp(response_Google %>% select(grocery_and_pharmacy_stores, workplaces, residential, retail_and_recreation, transit_stations))
    
    response_Google$PCA_mobility <- PCA$scores[, 1]
    contributions <- PCA$loadings^2
    Google_PCA_importance <- rbind(Google_PCA_importance, data.frame(country=country_long, grocery_and_pharmacy_stores=contributions[1], workplaces=contributions[2], residential=contributions[3], retail_and_recreation=contributions[4], transit_stations=contributions[5]))
    
    save(response_Google, file=paste0(dir_name_backup, gsub(" ", "_", country_long), "/response_Google.RData"))
    save(Google_PCA_importance, file=paste0(dir_name_backup, gsub(" ", "_", country_long), "/Google_PCA_importance.RData"))
  }
  
  response_Google$PCA_mobility_moved <- move_n_point(response_Google$PCA_mobility, n=delay)
  response_Google$residential_moved <- move_n_point(response_Google$residential, n=delay)
  response_Google$transit_stations_moved <- move_n_point(response_Google$transit_stations, n=delay)
  response_Google$workplaces_moved <- move_n_point(response_Google$workplaces, n=delay)
  response_Google$retail_and_recreation_moved <- move_n_point(response_Google$retail_and_recreation, n=delay)
  response_Google$grocery_and_pharmacy_stores_moved <- move_n_point(response_Google$grocery_and_pharmacy_stores, n=delay)
  
  return(list(response_Google, Google_PCA_importance))
}


# Load Facebook's mobility data.
#
# Inputs:
#   - country_long:             long country name
#   - dir_name_backup:          name of the directory in which save Facebook's mobility data
#   - country_short:            short country name
#   - global_initial_date:      global initial date
#   - global_final_date:        global final date
#   - k:                        rollmean parameter
#   - Facebook_PCA_importance:  dataframe with PCA importance of each mobility variable for all the considered countries
#   - delay:                    delay between mobility and infection rates
#
# Output:
#   - response_Google:          Facebook's mobility data
#   - Facebook_PCA_importance:  dataframe with PCA importance of each mobility variable for all the considered countries
load_Facebook_mobility_data <- function(country_long, dir_name_backup, country_short, global_initial_date, global_final_date, k, Facebook_PCA_importance, delay){
  if(file.exists(paste0(dir_name_backup, gsub(" ", "_", country_long), "/response_Facebook.RData"))){
    load(paste0(dir_name_backup, gsub(" ", "_", country_long), "/response_Facebook.RData"))
    load(paste0(dir_name_backup, gsub(" ", "_", country_long), "/Facebook_PCA_importance.RData"))
  }
  else{
    # Load Facebook mobility data
    initial_month <- as.numeric(format(global_initial_date, "%m"))
    initial_year <- as.numeric(format(global_initial_date, "%Y"))
    final_month <- as.numeric(format(global_final_date, "%m"))
    final_year <- as.numeric(format(global_final_date, "%Y"))
    
    initial_quarter_number <- ceiling(initial_month / 3)
    final_quarter_number <- ceiling(final_month / 3)
    
    dirs <- data.frame(directory=list.dirs("aggregatesUMD", recursive = FALSE)) %>%
      filter(directory >= paste0("aggregatesUMD/", initial_year, "-Q", initial_quarter_number), directory <= paste0("aggregatesUMD/", final_year, "-Q", final_quarter_number))
    
    dirs$directory <- paste0(dirs$directory, "/aggregates/country/", country_short, ".csv")
    
    response_Facebook <- do.call(rbind,
                                 lapply(dirs$directory, function(x){
                                   response_Facebook_local <- read.csv(x, na.strings = "")
                                   response_Facebook_local <- response_Facebook_local %>%
                                     filter(date >= global_initial_date)
                                   
                                   if(all(c("C0_1.0", "C0_2.0", "C0_3.0", "C0_4.0", "C0_5.0", "C0_6.0") %in% colnames(response_Facebook_local)) &&
                                      !any(c("C0_1.2", "C0_2.2", "C0_3.2", "C0_4.2", "C0_5.2", "C0_6.2") %in% colnames(response_Facebook_local))){
                                     response_Facebook_local <- response_Facebook_local %>%
                                       dplyr::rename(C0_1.2 = C0_1.0, C0_2.2 = C0_2.0, C0_3.2 = C0_3.0, C0_4.2 = C0_4.0, C0_5.2 = C0_5.0, C0_6.2 = C0_6.0)
                                   }
                                   
                                   if(all(c("C0_1.0", "C0_2.0", "C0_3.0", "C0_4.0", "C0_5.0", "C0_6.0") %in% colnames(response_Facebook_local)) &&
                                      all(c("C0_1.2", "C0_2.2", "C0_3.2", "C0_4.2", "C0_5.2", "C0_6.2") %in% colnames(response_Facebook_local))){
                                     response_Facebook_local <- response_Facebook_local %>%
                                       mutate(C0_1.2 = C0_1.0 + C0_1.2, C0_2.2 = C0_2.0 + C0_2.2, C0_3.2 = C0_3.0 + C0_3.2, C0_4.2 = C0_4.0 + C0_4.2, C0_5.2 = C0_5.0 + C0_5.2, C0_6.2 = C0_6.0 + C0_6.2)
                                   }
                                   
                                   if(all(c("C2.0", "C2.1", "C2.2", "C2.3") %in% colnames(response_Facebook_local)) &&
                                      !("C2.4" %in% colnames(response_Facebook_local))){
                                     response_Facebook_local <- response_Facebook_local %>%
                                       dplyr::rename(C2.4 = C2.0 + C2.4)
                                   }
                                   
                                   if(all(c("C2.0", "C2.1", "C2.2", "C2.3") %in% colnames(response_Facebook_local)) &&
                                      "C2.4" %in% colnames(response_Facebook_local)){
                                     response_Facebook_local <- response_Facebook_local %>%
                                       mutate(C2.4 = C2.0)
                                   }
                                   
                                   if(!any(c("C5.1", "C5.2", "C5.3", "C5.4", "C5.5", "C5.5") %in% colnames(response_Facebook_local))){
                                     response_Facebook_local <- response_Facebook_local %>%
                                       mutate(C5.1 = NA, C5.2 = NA, C5.3 = NA, C5.4 = NA, C5.5 = NA, C5.5 = NA)
                                   }
                                   
                                   if(all(c("C2.1", "C2.2", "C2.3", "C2.4", "C2.NA") %in% colnames(response_Facebook_local))){
                                     response_Facebook_local <- response_Facebook_local %>%
                                       select(country_agg, date, Finished, C0_1.1, C0_1.2, C0_2.1, C0_2.2, C0_3.1, C0_3.2, C0_4.1, C0_4.2, C0_5.1, C0_5.2, C0_6.1, C0_6.2, C2.1, C2.2, C2.3, C2.4, C2.NA, E5, E7, C5.1, C5.2, C5.3, C5.4, C5.5)
                                   }
                                   else{
                                     response_Facebook_local <- response_Facebook_local %>%
                                       select(country_agg, date, Finished, C0_1.1, C0_1.2, C0_2.1, C0_2.2, C0_3.1, C0_3.2, C0_4.1, C0_4.2, C0_5.1, C0_5.2, C0_6.1, C0_6.2, E5, E7, C5.1, C5.2, C5.3, C5.4, C5.5) %>%
                                       mutate(C2.1=NA, C2.2=NA, C2.3=NA, C2.4=NA, C2.NA=NA)
                                   }
                                   
                                   response_Facebook_local$E7[which(response_Facebook_local$E7 == 0)] <- NA
                                   
                                   return(response_Facebook_local)
                                 })
                                )

    response_Facebook <- response_Facebook %>%
      mutate(residential = rollmean(C0_4.1 / (C0_4.1 + C0_4.2), k, align = "center", na.pad = TRUE),
             workplaces = rollmean(C0_1.1 / (C0_1.1 + C0_1.2), k, align = "center", na.pad = TRUE), 
             grocery_and_pharmacy_stores = rollmean(C0_2.1 / (C0_2.1 + C0_2.2), k, align = "center", na.pad = TRUE),
             transit_stations = rollmean(C0_6.1 / (C0_6.1 + C0_6.2), k, align = "center", na.pad = TRUE),
             retail_and_recreation = rollmean((C0_3.1 + C0_5.1) / (C0_3.1 + C0_3.2 + C0_5.1 + C0_5.2), k, align = "center", na.pad = TRUE),
             masks = rollmean(1- ((1 * C5.1 + 0.75 * C5.2 + 0.5 * C5.3 + 0.25 * C5.4 + 0 * C5.5) / (C5.1 + C5.2 + C5.3 + C5.4 + C5.5)), k, align = "center", na.pad = TRUE),
             direct_contacts = rollmean((2.5 * C2.1 + 7 * C2.2 + 14.5 * C2.3 + 20 * C2.4) / (2.5 * C2.1 + 7 * C2.2 + 14.5 * C2.3 + 20 * C2.4 + C2.NA), k, align = "center", na.pad = TRUE),
             people_slept = rollmean(1 - Finished / E5, k, align = "center", na.pad = TRUE),
             num_rooms = rollmean(1 - Finished / E7, k, align = "center", na.pad = TRUE)) %>%
      filter(date >= global_initial_date, date <= global_final_date)
    
    response_Facebook$num_rooms[which(is.infinite(response_Facebook$num_rooms) | response_Facebook$num_rooms < 0)] <- NA
    response_Facebook$direct_contacts[response_Facebook$direct_contacts <= 0] <- NA
    
    response_Facebook <- merge(data.frame(date=seq.Date(global_initial_date, global_final_date, 1)), response_Facebook, by="date", all=TRUE)
    
    response_Facebook <- response_Facebook %>%
      filter(!is.na(grocery_and_pharmacy_stores), !is.na(transit_stations), !is.na(retail_and_recreation), !is.na(workplaces), !is.na(residential))
    
    
    # Compute PCA
    PCA <- princomp(response_Facebook %>% select(grocery_and_pharmacy_stores, workplaces, residential, retail_and_recreation, transit_stations))
    
    response_Facebook$PCA_mobility <- PCA$scores[, 1]
    contributions <- PCA$loadings^2
    Facebook_PCA_importance <- rbind(Facebook_PCA_importance, data.frame(country=country_long, grocery_and_pharmacy_stores=contributions[1], workplaces=contributions[2], residential=contributions[3], retail_and_recreation=contributions[4], transit_stations=contributions[5]))
    
    save(response_Facebook, file=paste0(dir_name_backup, gsub(" ", "_", country_long), "/response_Facebook.RData"))
    save(Facebook_PCA_importance, file=paste0(dir_name_backup, gsub(" ", "_", country_long), "/Facebook_PCA_importance.RData"))
  }
  
  response_Facebook$PCA_mobility_moved <- move_n_point(response_Facebook$PCA_mobility, n=delay)
  response_Facebook$residential_moved <- move_n_point(response_Facebook$residential, n=delay)
  response_Facebook$transit_stations_moved <- move_n_point(response_Facebook$transit_stations, n=delay)
  response_Facebook$workplaces_moved <- move_n_point(response_Facebook$workplaces, n=delay)
  response_Facebook$retail_and_recreation_moved <- move_n_point(response_Facebook$retail_and_recreation, n=delay)
  response_Facebook$grocery_and_pharmacy_stores_moved <- move_n_point(response_Facebook$grocery_and_pharmacy_stores, n=delay)
  response_Facebook$masks_moved <- move_n_point(response_Facebook$masks, n=delay)
  
  return(list(response_Facebook, Facebook_PCA_importance))
}


# Compute correlation among infection rates and mobility variable (and with PCA)
#
# Inputs:
#   - n:                      delay
#   - correlations:           dataframe with computed correlations
#   - data:                   mobility data and infection rates
#   - method:                 correlation method
#   - country_long:           long country name
#   - country_short:          short country name
#   - country_short_3c:       short country name (3 characters)
#
# Output:
#   - correlations:           updated dataframe with computed correlations
infrates_mobility_correlation <- function(n, correlations, data, method, country_long, country_short, country_short_3c){
  infection_rates_series <- data$infection_rates
  residential_series <- data$residential
  transit_stations_series <- data$transit_stations
  workplaces_series <- data$workplaces
  retail_and_recreation_series <- data$retail_and_recreation
  grocery_and_pharmacy_stores_series <- data$grocery_and_pharmacy_stores
  masks_series <- data$masks
  PCA_mobility_series <- data$PCA_mobility
  
  corr_indeces <- which(complete.cases(infection_rates_series, residential_series, transit_stations_series, workplaces_series, retail_and_recreation_series, grocery_and_pharmacy_stores_series, masks_series, PCA_mobility_series))
  
  # Compute correlation between infection rates and residential
  correlation_inf_resid <- cor(infection_rates_series[corr_indeces], residential_series[corr_indeces], method = method)
  
  # Compute correlation between infection rates and public transports
  correlation_inf_transstat <- cor(infection_rates_series[corr_indeces], transit_stations_series[corr_indeces], method = method)
  
  # Compute correlation between infection rates and workplaces
  correlation_inf_work <- cor(infection_rates_series[corr_indeces], workplaces_series[corr_indeces], method = method)
  
  # Compute correlation between infection rates and retail and recreation
  correlation_inf_retrec <- cor(infection_rates_series[corr_indeces], retail_and_recreation_series[corr_indeces], method = method)
  
  # Compute correlation between infection rates and grocery and pharmacy stores
  correlation_inf_gropha <- cor(infection_rates_series[corr_indeces], grocery_and_pharmacy_stores_series[corr_indeces], method = method)
  
  # Compute correlation between infection rates and masks
  correlation_inf_masks <- cor(infection_rates_series[corr_indeces], masks_series[corr_indeces], method = method)
  
  
  # Compute correlation between residential and public transports
  correlation_resid_transstat <- cor(residential_series[corr_indeces], transit_stations_series[corr_indeces], method = method)
  
  # Compute correlation between residential and workplaces
  correlation_resid_work <- cor(residential_series[corr_indeces], workplaces_series[corr_indeces], method = method)
  
  # Compute correlation between residential and retail and recreation
  correlation_resid_retrec <- cor(residential_series[corr_indeces], retail_and_recreation_series[corr_indeces], method = method)
  
  # Compute correlation between residential and grocery and pharmacy stores
  correlation_resid_gropha <- cor(residential_series[corr_indeces], grocery_and_pharmacy_stores_series[corr_indeces], method = method)
  
  # Compute correlation between residential and masks
  correlation_resid_masks <- cor(residential_series[corr_indeces], masks_series[corr_indeces], method = method)
  
  
  # Compute correlation between public transports and workplaces
  correlation_transstat_work <- cor(transit_stations_series[corr_indeces], workplaces_series[corr_indeces], method = method)
  
  # Compute correlation between public transports and retail and recreation
  correlation_transstat_retrec <- cor(transit_stations_series[corr_indeces], retail_and_recreation_series[corr_indeces], method = method)
  
  # Compute correlation between public transports and grocery and pharmacy stores
  correlation_transstat_gropha <- cor(transit_stations_series[corr_indeces], grocery_and_pharmacy_stores_series[corr_indeces], method = method)
  
  # Compute correlation between public transports and masks
  correlation_transstat_masks <- cor(transit_stations_series[corr_indeces], masks_series[corr_indeces], method = method)
  
  
  
  # Compute correlation between workplaces and retail and recreation
  correlation_work_retrec <- cor(workplaces_series[corr_indeces], retail_and_recreation_series[corr_indeces], method = method)
  
  # Compute correlation between workplaces and grocery and pharmacy stores
  correlation_work_gropha <- cor(workplaces_series[corr_indeces], grocery_and_pharmacy_stores_series[corr_indeces], method = method)
  
  # Compute correlation between workplaces and masks
  correlation_work_masks <- cor(workplaces_series[corr_indeces], masks_series[corr_indeces], method = method)
  
  
  # Compute correlation between retail and recreation and grocery and pharmacy stores
  correlation_retrec_gropha <- cor(retail_and_recreation_series[corr_indeces], grocery_and_pharmacy_stores_series[corr_indeces], method = method)
  
  # Compute correlation between retail and recreation and masks
  correlation_retrec_masks <- cor(retail_and_recreation_series[corr_indeces], masks_series[corr_indeces], method = method)
  
  
  # Compute correlation between grocery and pharmacy stores and masks
  correlation_gropha_masks <- cor(grocery_and_pharmacy_stores_series[corr_indeces], masks_series[corr_indeces], method = method)
  
  
  # Compute correlation between infection rates and PCA mobility
  correlation_inf_PCA <- cor(infection_rates_series[corr_indeces], PCA_mobility_series[corr_indeces], method = method)
  
    
  correlations <- rbind(correlations, data.frame(n=n, country_long=country_long, country_short=country_short, iso_alpha=country_short_3c,
                                                 correlation_inf_resid=correlation_inf_resid, correlation_inf_transstat=correlation_inf_transstat, correlation_inf_work=correlation_inf_work, correlation_inf_retrec=correlation_inf_retrec, correlation_inf_gropha=correlation_inf_gropha, correlation_inf_masks=correlation_inf_masks,
                                                 correlation_resid_transstat=correlation_resid_transstat, correlation_resid_work=correlation_resid_work, correlation_resid_retrec=correlation_resid_retrec, correlation_resid_gropha=correlation_resid_gropha, correlation_resid_masks=correlation_resid_masks,
                                                 correlation_transstat_work=correlation_transstat_work, correlation_transstat_retrec=correlation_transstat_retrec, correlation_transstat_gropha=correlation_transstat_gropha, correlation_transstat_masks=correlation_transstat_masks,
                                                 correlation_work_retrec=correlation_work_retrec, correlation_work_gropha=correlation_work_gropha, correlation_work_masks=correlation_work_masks,
                                                 correlation_retrec_gropha=correlation_retrec_gropha, correlation_retrec_masks=correlation_retrec_masks,
                                                 correlation_gropha_masks=correlation_gropha_masks,
                                                 correlation_inf_PCA=correlation_inf_PCA))
  
  return(correlations) 
}


# Compute correlation among mobility, the PCA computed on the mobility variable and stringency index
#
# Inputs:
#   - n:                      delay
#   - correlations:           dataframe with computed correlations
#   - model_data:             mobility data and stringency index
#   - method:                 correlation methods
#   - country_long:           long country name
#   - country_short:          short country name
#   - country_short_3c:       short country name (3 characters)
#
# Output:
#   - correlations:           updated dataframe with computed correlations
restrictions_mobility_correlation <- function(n, correlations, model_data, method, country_long, country_short, country_short_3c){
  PCA_mobility_series <- model_data$PCA_mobility
  infection_rates_series <- model_data$infection_rates
  residential_series <- model_data$residential
  transit_stations_series <- model_data$transit_stations
  workplaces_series <- model_data$workplaces
  retail_and_recreation_series <- model_data$retail_and_recreation
  grocery_and_pharmacy_stores_series <- model_data$grocery_and_pharmacy_stores
  masks_series <- model_data$masks
  stringency_index_series <- 1 - model_data$StringencyIndex_Average / 100
  
  corr_indeces <- which(complete.cases(PCA_mobility_series, stringency_index_series, infection_rates_series, residential_series, transit_stations_series, workplaces_series, retail_and_recreation_series, grocery_and_pharmacy_stores_series, masks_series))

  # Compute correlation between stringency index and PCA mobility
  correlation_stridx_PCA <- cor(stringency_index_series[corr_indeces], PCA_mobility_series[corr_indeces], method = method)
  
  # Compute correlation between stringency index and PCA mobility
  correlation_stridx_workplaces <- cor(stringency_index_series[corr_indeces], workplaces_series[corr_indeces], method = method)
  
  # Compute correlation between stringency index and PCA mobility
  correlation_stridx_transit_stations <- cor(stringency_index_series[corr_indeces], transit_stations_series[corr_indeces], method = method)
  
  # Compute correlation between stringency index and PCA mobility
  correlation_stridx_retail_and_recreation <- cor(stringency_index_series[corr_indeces], retail_and_recreation_series[corr_indeces], method = method)
  
  # Compute correlation between stringency index and PCA mobility
  correlation_stridx_grocery_and_pharmacy_stores <- cor(stringency_index_series[corr_indeces], grocery_and_pharmacy_stores_series[corr_indeces], method = method)
  
  # Compute correlation between stringency index and PCA mobility
  correlation_stridx_masks <- cor(stringency_index_series[corr_indeces], masks_series[corr_indeces], method = method)
  
  # Compute correlation between stringency index and PCA mobility
  correlation_stridx_residential <- cor(stringency_index_series[corr_indeces], residential_series[corr_indeces], method = method)
  
  # Compute correlation between stringency index and PCA mobility
  correlation_stridx_inf_rates <- cor(stringency_index_series[corr_indeces], infection_rates_series[corr_indeces], method = method)
  
  
  correlations <- rbind(correlations, data.frame(n=n, country_long=country_long, country_short=country_short, iso_alpha=country_short_3c,
                                                 correlation_stridx_workplaces=correlation_stridx_workplaces,
                                                 correlation_stridx_transit_stations=correlation_stridx_transit_stations,
                                                 correlation_stridx_retail_and_recreation=correlation_stridx_retail_and_recreation,
                                                 correlation_stridx_grocery_and_pharmacy_stores=correlation_stridx_grocery_and_pharmacy_stores,
                                                 correlation_stridx_masks=correlation_stridx_masks,
                                                 correlation_stridx_residential=correlation_stridx_residential,
                                                 correlation_stridx_inf_rates=correlation_stridx_inf_rates,
                                                 correlation_stridx_PCA=correlation_stridx_PCA))
  
  return(correlations)
}

# Merge Facebook and Google's mobility data.
#
# Inputs:
#   - dir_name_backup:                name of the directory in which save Facebook and Google's mobility data
#   - response_Google:                Google's mobility data
#   - response_Facebook:              Facebook's mobility data
#   - mobility_type:                  considered mobility variable
#   - country_long:                   long country name
#   - dates:                          considered dates
#
# Output:
#   - roll_response_Facebook_Google:  dataframe with merged Facebook and Google's mobility data
merge_Facebook_and_Google_mobility_data <- function(dir_name_backup, response_Google, response_Facebook, mobility_type, country_long, dates){
  if(file.exists(paste0(dir_name_backup, gsub(" ", "_", country_long), "/roll_response_Facebook_Google", mobility_type, ".RData"))){
    load(paste0(dir_name_backup, gsub(" ", "_", country_long), "/roll_response_Facebook_Google", mobility_type, ".RData"))
  }
  else{
    roll_response_Google <- data.frame(date=response_Google$date, value_Google=response_Google[, mobility_type])
    colnames(roll_response_Google) <- c("date", "value_Google")
    
    roll_response_Facebook <- data.frame(date=response_Facebook$date, value_Facebook=response_Facebook[, mobility_type])
    colnames(roll_response_Facebook) <- c("date", "value_Facebook")
    
    # Facebook question E5
    people_slept <- data.frame(date=response_Facebook$date, people_slept=response_Facebook$people_slept)
    colnames(people_slept) <- c("date", "people_slept")
    
    # Facebook question E7
    num_rooms <- data.frame(date=response_Facebook$date, num_rooms=response_Facebook$num_rooms)
    colnames(num_rooms) <- c("date", "num_rooms")
    
    
    roll_response_Facebook_Google <- data.frame(date=dates$date)
    if(mobility_type != "masks"){
      roll_response_Facebook_Google <- merge(roll_response_Facebook_Google, roll_response_Google, by="date", all =TRUE)
    }
    roll_response_Facebook_Google <- merge(roll_response_Facebook_Google, roll_response_Facebook, by="date", all =TRUE)
    roll_response_Facebook_Google <- merge(roll_response_Facebook_Google, people_slept, by="date", all =TRUE)
    roll_response_Facebook_Google <- merge(roll_response_Facebook_Google, num_rooms, by="date", all =TRUE)
    roll_response_Facebook_Google <- roll_response_Facebook_Google %>%
      filter(!is.na(date))
  
    save(roll_response_Facebook_Google, file=paste0(dir_name_backup, gsub(" ", "_", country_long), "/roll_response_Facebook_Google", mobility_type, ".RData"))
  }
  
  return(roll_response_Facebook_Google)
}

# Compute the correlations between Facebook and Google's mobility data,
# restrictions and Facebook's mobility data, restrictions and Google's mobility
# data, and infection rates and restrictions.
#
# Inputs:
#   - infections_rates:             infection rates
#   - trajectories:                 dataframe with the data
#   - restrictions_string:          restrictions' names
#   - google_string:                Google's names
#   - facebook_string:              Facebook's names
#   - method:                       correlation methods
#   - mobility_type:                types of mobility
#
# Output:
#   - correlation_Google_Facebook:  correlation between Google and Facebook mobility
#   - correlation_restr_Google:     correlation between Google mobility and restrictions
#   - correlation_restr_Facebook:   correlation between Facebook mobility and restrictions
#   - correlation_inf_restr:        correlation between infection rates and restrictions
compute_correlations <- function(infections_rates, trajectories, restrictions_string, google_string, facebook_string, method, mobility_type){
  infection_rates_series <- infections_rates$value
  restrictions_series <- trajectories$value[which(trajectories$color == restrictions_string)]
  if(mobility_type != "masks")
    google_series <- trajectories$value[which(trajectories$color == google_string)]
  else
    google_series <- rep(0, length(infection_rates_series))
  facebook_series <- trajectories$value[which(trajectories$color == facebook_string)]
  
  corr_indeces <- which(complete.cases(facebook_series, google_series, restrictions_series, infection_rates_series))
  
  # Compute correlation between people response (Google) and people response (Facebook)
  correlation_Google_Facebook <- cor(google_series[corr_indeces], facebook_series[corr_indeces], method = method)
  
  # Compute correlation between restrictions and people response (Google)
  correlation_restr_Google <- cor(restrictions_series[corr_indeces], google_series[corr_indeces], method = method)
  
  # Compute correlation between restrictions and people response (Facebook)
  correlation_restr_Facebook <- cor(restrictions_series[corr_indeces], facebook_series[corr_indeces], method = method)
  
  # Compute correlation between infection rates and restrictions
  correlation_inf_restr <- cor(infection_rates_series[corr_indeces], restrictions_series[corr_indeces], method = method)  
  
  return(list(correlation_Google_Facebook, correlation_restr_Google, correlation_restr_Facebook, correlation_inf_restr))
}

# Build the XGBoost model for estimating missing mobility values from restrictions.
#
# Inputs:
#   - dir_name_models:                    directory in which save the models
#   - country_long:                       long country name
#   - data:                               dataframe with infection rates and mobility variables
#   - dir_name_regression_plots_xgboost:  directory in which save the plots
#   - kfolds:                             number of folds for k-folds cross-validation
#   - errors:                             RMSE and NMAE on the test set of XGBoost model
#   - mobility_types:                     mobility types
#   - restrictions_all:                   restrictions
#
# Output:
#   - errors:                             RMSE and NMAE on the test set of XGBoost model
xgboost_fromrestomob_8020_model <- function(dir_name_models, country_long, data, dir_name_regression_plots_xgboost, kfolds, errors, mobility_types, restrictions_all, plots_XGBoost){
  mobility_string <- c("residential"="Residential", "workplaces"="Workplaces", "grocery_and_pharmacy_stores"="Grocery and pharmacy stores", "transit_stations"="Transit stations", "retail_and_recreation"="Retail and recreation", "masks"="Masks")
  for(m in 1:length(mobility_types)){
    errors_local <- data.frame(country=country_long, mobility=mobility_types[[m]], rmse=0, nmae=0)
    for(k in seq(1, kfolds)){
      data_X_train <- restrictions_all %>%
        filter(fold != k)
      
      data_y_train <- data %>%
        filter(fold != k)
      
      X_train <- data_X_train %>% select(-fold)
      y_train <- data_y_train[, mobility_types[[m]]]
      
      data_X_test <- restrictions_all %>%
        filter(fold == k)
      
      data_y_test <- data %>%
        filter(fold == k)
      
      X_test <- data_X_test %>% select(-fold)
      y_test <- data_y_test[, mobility_types[[m]]]
      
      xgboost_m <- xgboost(data = as.matrix(X_train %>% select(-date)), label = as.matrix(y_train), nthread = 4, nrounds = 400, early_stopping_rounds =10, objective = "reg:squarederror")
      
      pred <- predict(xgboost_m, as.matrix(X_test %>% select(-date)))
      
      colors <- c("Real"="#494949", "Predicted"="#ff8b94")
      
      X_test$y_test <- y_test
      X_test$pred <- pred
      
      png(paste0(dir_name_regression_plots_xgboost, "fitted_test_set_", mobility_types[[m]], "_", country_long, "_fold", k, ".png"), units="in", width=34, height=15, res=300)
      p <- ggplot(X_test) +
        theme_bw() +
        geom_point(aes(x = date, y = y_test, color = 'Real'), size = 5) +
        geom_point(aes(x = date, y = pred, color = 'Predicted'), size = 5) +
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 35),
          axis.text = element_text(size = 35),
          legend.title = element_text(size = 45, face = "bold"),
          legend.text = element_text(size = 40)
        ) +
        labs(title=mobility_string[mobility_types[[m]]], x = "Date", y = "Value", color = "Data") +
        scale_color_manual(values = colors)
      print(p)
      dev.off()
      
      if(k == 1)
        saveRDS(p, paste0(dir_name_regression_plots_xgboost, "fitted_test_set_", mobility_types[[m]], "_", country_long, "_fold", k, ".RDs"))
      
      errors_local$rmse <- errors_local$rmse + compute.rmse(pred, y_test)
      errors_local$nmae <- errors_local$nmae + compute.nmae(pred, y_test)
      
      print(paste0("#################### ", country_long, " (xgboost, fold ", k, ") ####################"))
      print(paste0('RMSE: ', compute.rmse(pred, y_test)))
      print(paste0('NMAE: ', compute.nmae(pred, y_test)))
      print(paste0("#################### ", country_long, " (xgboost, fold ", k, ") ####################"))
    }
    
    errors_local$rmse <- errors_local$rmse / kfolds
    errors_local$nmae <- errors_local$nmae / kfolds
    
    errors <- rbind(errors, errors_local)
  }

  return(errors)
}

# Build the XGBoost model for estimating missing nfection rates values from mobility
#
# Inputs:
#   - dir_name_models:                    directory in which save the models
#   - country_long:                       long country name
#   - data:                               dataframe with infection rates and mobility variables
#   - dir_name_regression_plots_xgboost:  directory in which save the plots
#   - kfolds:                             number of folds for k-folds cross-validation
#   - errors:                             RMSE and NMAE on the test set of XGBoost model
#
# Output:
#   - errors:                             RMSE and NMAE on the test set of XGBoost model
xgboost_fromobtoinfrates_8020_model <- function(dir_name_models, country_long, data, dir_name_regression_plots_xgboost, kfolds, errors){
  data <- data %>%
    select(date, fold, infection_rates, transit_stations, retail_and_recreation, workplaces, residential, masks, grocery_and_pharmacy_stores)
  
  errors_local <- data.frame(country=country_long, rmse=0, nmae=0)
  for(k in seq(1, kfolds)){
    data_train <- data %>%
      filter(fold != k)
    
    X_train <- data_train[, c("date", "transit_stations", "retail_and_recreation", "workplaces", "residential", "masks", "grocery_and_pharmacy_stores")]
    y_train <- data_train[, "infection_rates"]
    
    data_test <- data %>%
      filter(fold == k)
    
    X_test <- data_test[, c("date", "transit_stations", "retail_and_recreation", "workplaces", "residential", "masks", "grocery_and_pharmacy_stores")]
    y_test <- data_test[, "infection_rates"]
    
    xgboost_m <- xgboost(data = as.matrix(X_train %>% select(-date)), label = as.matrix(y_train), nthread = 4, nrounds = 400, early_stopping_rounds =10, objective = "reg:squarederror")
    
    pred <- predict(xgboost_m, as.matrix(X_test %>% select(-date)))
    
    colors <- c("Real"="#494949", "Predicted"="#ff8b94")
    
    X_test$y_test <- y_test
    X_test$pred <- pred
    
    png(paste0(dir_name_regression_plots_xgboost, "fitted_test_set_", country_long, "_fold", k, ".png"), units="in", width=34, height=15, res=300)
    p <- ggplot(X_test) +
      theme_bw() +
      geom_point(aes(x = date, y = y_test, color = 'Real'), size = 5) +
      geom_point(aes(x = date, y = pred, color = 'Predicted'), size = 5) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 35),
        axis.text = element_text(size = 35),
        legend.title = element_text(size = 45, face = "bold"),
        legend.text = element_text(size = 40)
      ) +
      labs(title= gsub("_", " ", country_long), x = "Date", y = "Infection rates", color = "Data") +
      scale_color_manual(values = colors)
    print(p)
    dev.off()
    
    if(k == 1)
      saveRDS(p, paste0(dir_name_regression_plots_xgboost, "fitted_test_set_", country_long, "_fold", k, ".RDs"))
    
    errors_local$rmse <- errors_local$rmse + compute.rmse(pred, y_test)
    errors_local$nmae <- errors_local$nmae + compute.nmae(pred, y_test)
    
    print(paste0("#################### ", country_long, " (xgboost, fold ", k, ") ####################"))
    print(paste0('RMSE: ', compute.rmse(pred, y_test)))
    print(paste0('NMAE: ', compute.nmae(pred, y_test)))
    print(paste0("#################### ", country_long, " (xgboost, fold ", k, ") ####################"))
  }
  
  errors_local$rmse <- errors_local$rmse / kfolds
  errors_local$nmae <- errors_local$nmae / kfolds
  
  errors <- rbind(errors, errors_local)
  
  return(errors)
}

# Build the XGBoost model using 100% of the data as training from restrictions to mobility.
#
# Inputs:
#   - dir_name_models:                    directory in which save the models
#   - country_long:                       long country name
#   - data:                               dataframe with infection rates and mobility variables
#   - dir_name_regression_plots_xgboost:  directory in which save the plots
#   - mobility_types:                     mobility types
#   - restrictions_all:                   restrictions
#   - importances:                        feature importance XGBoost model
#
# Output:
#   - importances:                        feature importance XGBoost model
xgboost_fromrestomob_100_model <- function(dir_name_models, country_long, data, dir_name_regression_plots_xgboost, mobility_types, restrictions_all, importances){
  X_train <- restrictions_all %>%  select(-c(fold, StringencyIndex_Average, EconomicSupportIndex, ContainmentHealthIndex_Average, GovernmentResponseIndex_Average))
  y_train <- data[, "average_mobility"]
  
  xgboost_m <- xgboost(data = as.matrix(X_train %>% select(-date)), label = as.matrix(y_train), nthread = 4, nrounds = 400, early_stopping_rounds = 10, objective = "reg:squarederror")
  
  importance_matrix <- xgb.importance(model = xgboost_m)
  
  restrictions_all_local <- restrictions_all %>%
    select(-c(date, fold))

  new_importance <- data.frame(country=rep(country_long, ncol(restrictions_all_local)),
                              value=rep(0, ncol(restrictions_all_local)),
                              type=colnames(restrictions_all_local))
  
  for(i in 1:ncol(restrictions_all_local)){
    if(length(importance_matrix$Gain[which(importance_matrix$Feature == colnames(restrictions_all_local)[i])]) > 0)
      new_importance[i, "value"] <- importance_matrix$Gain[which(importance_matrix$Feature == colnames(restrictions_all_local)[i])]
  }
  
  importances <- rbind(importances, new_importance)
  
  return(importances)
}

# Build the XGBoost model using 100% of the data as training from mobility to infection rates.
#
# Inputs:
#   - dir_name_models:                    directory in which save the models
#   - country_long:                       long country name
#   - data:                               dataframe with infection rates and mobility variables
#   - dir_name_regression_plots_xgboost:  directory in which save the plots
#   - importances:                        feature importance XGBoost model
#
# Output:
#   - importances:                        feature importance XGBoost model
xgboost_fromobtoinfrates_100_model <- function(dir_name_models, country_long, data, dir_name_regression_plots_xgboost, importances){
  data <- data %>%
    select(date, infection_rates, transit_stations, retail_and_recreation, workplaces, residential, masks, grocery_and_pharmacy_stores)
  
  X_train <- data[, c("date", "transit_stations", "retail_and_recreation", "workplaces", "residential", "masks", "grocery_and_pharmacy_stores")]
  y_train <- data[, "infection_rates"]
  
  
  xgboost_m <- xgboost(data = as.matrix(X_train %>% select(-date)), label = as.matrix(y_train), nthread = 4, nrounds = 400, early_stopping_rounds = 10, objective = "reg:squarederror")
  
  importance_matrix <- xgb.importance(model = xgboost_m)

  new_importance <- data.frame(country=rep(country_long, 6),
                              value=c(importance_matrix$Gain[importance_matrix$Feature == "masks"], importance_matrix$Gain[importance_matrix$Feature == "transit_stations"], importance_matrix$Gain[importance_matrix$Feature == "retail_and_recreation"], importance_matrix$Gain[importance_matrix$Feature == "workplaces"], importance_matrix$Gain[importance_matrix$Feature == "residential"], importance_matrix$Gain[importance_matrix$Feature == "grocery_and_pharmacy_stores"]),
                              type=c("Masks", "Transit station", "Retail and recreation", "Workplaces", "Residential", "Grocery and pharmacy stores"))
  
  importances <- rbind(importances, new_importance)
    
  return(importances)
}

# Feature importances from XGBoost model using 100% of the data as trining.
#
# Inputs:
#   - xgboost_fromrestomob_barplot:                         feature importances for the model from restrictions to mobility
#   - xgboost_fromobtoinfrates_barplot:                     feature importances for the model from mobility to infection rates
#   - dir_name_regression_plots_xgboost_fromrestomob:       directory in which save the plots from restrictions to mobility
#   - dir_name_regression_plots_xgboost_fromobtoinfrates:   directory in which save the plots from mobility to infection rates
#   - custom_order:                                         custom countries order
xgboost_study <- function(xgboost_fromrestomob_barplot, xgboost_fromobtoinfrates_barplot, dir_name_regression_plots_xgboost_fromrestomob, dir_name_regression_plots_xgboost_fromobtoinfrates, custom_order){
  xgboost_fromobtoinfrates_barplot$country <- factor(xgboost_fromobtoinfrates_barplot$country, levels = names(custom_order))
  
  png(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "xgboost_fromobtoinfrates_models_importance_100.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(data=xgboost_fromobtoinfrates_barplot, aes(x=country, y=value, fill=type)) +
    geom_bar(stat="identity", position=position_dodge(), width = 0.5) +
    theme_bw() +
    scale_color_manual(values = c("#2FFFCE", "#985453", "#6B95DB", "#c3cb71", "#559e83", "#c9c9ff")) +
    theme(legend.position = "bottom", legend.box = "vertical", panel.spacing = unit(1, "cm"), legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38), strip.text.x = element_blank()) +
    labs(y = "Importance", x = "Country", fill = "Mobility variable")
  print(plot)
  dev.off()
  
  
  xgboost_fromobtoinfrates_barplot_wide <- data.frame(pivot_wider(xgboost_fromobtoinfrates_barplot, names_from = type, values_from = value))
  
  # K-means
  silhouette_score <- function(k){
    km <- kmeans(xgboost_fromobtoinfrates_barplot_wide %>% select(-country), k, nstart=25)
    ss <- silhouette(km$cluster, dist(xgboost_fromobtoinfrates_barplot_wide %>% select(-country)))
    mean(ss[, 3])
  }
  k <- 2:6
  avg_sil <- sapply(k, silhouette_score)
  
  silhouette_df <- data.frame(k=k, value=avg_sil)
  
  png(paste0(dir_name_regression_plots_xgboost_fromobtoinfrates, "silhouette_scores.png"), units="in", width=34, height=15, res=300)
  p <- ggplot(silhouette_df) +
    geom_line(aes(x=k, y=value)) +
    geom_point(aes(x=k, y=value), shape=1, size=5) +
    theme_bw() +
    labs(x = "Number of clusters", y = "Average Silhouette Scores") +
    theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24))
  print(p)
  dev.off()
  
  km <- kmeans(xgboost_fromobtoinfrates_barplot_wide %>% select(-country), silhouette_df$k[which(silhouette_df$value == max(silhouette_df$value))], nstart = 25)
  
  clusters_fromobtoinfrates <- cbind(xgboost_fromobtoinfrates_barplot_wide %>% select(country), data.frame(kmeans=km$cluster))
  
  
  
  
  xgboost_fromrestomob_barplot$country <- factor(xgboost_fromrestomob_barplot$country, levels = names(custom_order))
  
  top_5_df <- xgboost_fromrestomob_barplot %>%
    group_by(country) %>%
    top_n(5, value) %>%
    ungroup()
  
  png(paste0(dir_name_regression_plots_xgboost_fromrestomob, "xgboost_fromrestomob_models_importance_100_fromrestomob.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(data=top_5_df, aes(x=country, y=value, fill=type)) +
    geom_bar(stat="identity", position=position_dodge(), width = 0.5) +
    theme_bw() +
    scale_fill_manual(values = c(
      "#FF5733", "#33FF57", "#3357FF", "#FF33A1", "#A133FF", # Bright and distinct hues
      "#33FFF3", "#FFD133", "#8D33FF", "#33FFA1", "#FF6F33", # Complementary and varied
      "#33A1FF", "#FF33D1", "#B3FF33", "#FF333F", "#33FFD1", # Vibrant mixes
      "#A1FF33", "#5733FF", "#FF33FF", "#33FF6F", "#FF8D33", # Contrast-rich shades
      "#33D1FF", "#FFA133", "#D133FF", "#57FF33", "#FF3357"  # More variety
    )) +
    theme(legend.position = "bottom", legend.box = "vertical", panel.spacing = unit(1, "cm"), legend.key.size = unit(1, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=30, face="bold"), legend.text=element_text(size=28), strip.text.x = element_blank()) +
    labs(y = "Importance", x = "Country", fill = "Restrictions variable") +
    guides(fill = guide_legend(override.aes = list(size = 10), nrow=7, byrow=TRUE))
  print(plot)
  dev.off()
  
  
  xgboost_fromrestomob_barplot_wide <- data.frame(pivot_wider(xgboost_fromrestomob_barplot, names_from = type, values_from = value))
  
  # K-means
  silhouette_score <- function(k){
    km <- kmeans(xgboost_fromrestomob_barplot_wide %>% select(-country), k, nstart=25)
    ss <- silhouette(km$cluster, dist(xgboost_fromrestomob_barplot_wide %>% select(-country)))
    mean(ss[, 3])
  }
  k <- 2:6
  avg_sil <- sapply(k, silhouette_score)
  
  silhouette_df <- data.frame(k=k, value=avg_sil)
  
  png(paste0(dir_name_regression_plots_xgboost_fromrestomob, "silhouette_scores_fromrestomob.png"), units="in", width=34, height=15, res=300)
  p <- ggplot(silhouette_df) +
    geom_line(aes(x=k, y=value)) +
    geom_point(aes(x=k, y=value), shape=1, size=5) +
    theme_bw() +
    labs(x = "Number of clusters", y = "Average Silhouette Scores") +
    theme(title = element_text(size = 34), axis.title = element_text(size = 26), axis.text = element_text(size = 22), legend.title = element_text(size = 30), legend.text = element_text(size = 24))
  print(p)
  dev.off()
  
  km <- kmeans(xgboost_fromrestomob_barplot_wide %>% select(-country), silhouette_df$k[which(silhouette_df$value == max(silhouette_df$value))], nstart = 25)
  
  clusters_fromrestomob <- cbind(xgboost_fromrestomob_barplot_wide %>% select(country), data.frame(kmeans=km$cluster))
}