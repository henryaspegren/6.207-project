require('lubridate')
require('stringr')

# Dataset
file <- '/Users/henryaspegren/Dropbox (MIT)/Academics/MIT/Senior Spring (2017)/6.207/Project/Data/Chicago_Crimes_2005_to_2007_modified.csv'
file2 <- '/Users/henryaspegren/Dropbox (MIT)/Academics/MIT/Senior Spring (2017)/6.207/Project/Data/Chicago_Crimes_2012_to_2017.csv'
crimesdf <- read.csv(file2)
crimesdf$Date <- as.Date(crimesdf$Date , "%m/%d/%Y %I:%M:%S %p")
census_data <- read.csv('/Users/henryaspegren/Dropbox (MIT)/Academics/MIT/Senior Spring (2017)/6.207/Project/Data/census_data.csv')
census_data$community_area <- census_data$GeogKey
census_data_indicators <- read.csv('/Users/henryaspegren/Dropbox (MIT)/Academics/MIT/Senior Spring (2017)/6.207/Project/Data/census_data_indicators.csv')
census_data_indicators$community_area <- census_data_indicators$Community.Area.Number
census_data_indicators <- census_data_indicators[1:77,]

# Aggregated Totals 
aggregate_by_community_area_by_type <- function(crime_data){
  community_areas <- sort(unique(crime_data$Community.Area))
  crime_types <- sort(unique(crime_data$Primary.Type))
  
  result <- data.frame('community_area'=community_areas)
  
  for(crime in (crime_types)){
    count <- rep(0, length(community_areas))
    for(i in 1:length(community_areas)){
      count[i] <- length(which(crime_data$Community.Area == community_areas[i] & crime_data$Primary.Type == crime))
    } 
    colname <- paste('Y_', str_replace_all(crime,"\\s+","_"), sep='')
    result[, colname] <- count
  }
  
  # Non-domestic homicides 
  non_domestic_homicides <- rep(0, length(community_areas))
  
  for(i in 1:length(community_areas)){
    count <- length(which(crime_data$Community.Area == community_areas[i] & crime_data$Primary.Type == 'HOMICIDE' 
                          & crime_data$Domestic == 'False'))
    non_domestic_homicides[i] <- count
  }
  result[, 'Y_NON_DOM_HOMICIDE'] <- non_domestic_homicides
  
  
  # Domestic homicides
  domestic_homicides <- rep(0, length(community_areas))
  
  for(i in 1:length(community_areas)){
    count <- length(which(crime_data$Community.Area == community_areas[i] & crime_data$Primary.Type == 'HOMICIDE' 
                          & crime_data$Domestic == 'True'))
    domestic_homicides[i] <- count
  }
  result[, 'Y_DOM_HOMICIDE'] <- domestic_homicides 
  
  return(result)
}

crimes_aggregated <- aggregate_by_community_area_by_type(crimesdf)

X_data <- merge(census_data, census_data_indicators)

# here we take only the numeric columns and exclude any dependent variables
X_data_pca <- data.frame(prcomp(X_data[, c(4:68, 71:76)], scale. = T)$x)
X_data_pca[, 'community_area'] <- X_data$community_area

full_dataset <- merge(merge(X_data, crimes_aggregated),X_data_pca)



