
# Load map data

read_map <- function(){
  
  invisible(capture.output(m1 <- rgdal::readOGR("CNTR_RG_20M_2016_4326.shp")))
  
  # Drop Antarctica
  countries <- as.vector.factor(m1$ISO3_CODE)
  countries <- countries[-4]
  m1 <- m1[m1$ISO3_CODE %in% countries,]
  m1@data <- m1@data %>%  mutate(id = row_number())
  
  return(m1)
}

# Type 01 confirmed, 02 recovered, 03 deaths
download_corona_data <- function(type){
  # Load confirmed cases - Source Johns Hopkins Github
  myfile <- ""
  switch (type,
    {
      myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
      },
    {
      myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
    },
    {
      myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
    }
  )
  invisible(capture.output(download <- read_csv(myfile)))
  download <- janitor::clean_names(download)
  download <- download %>% dplyr::select(-c(province_state,lat, long))
  names(download)[1] <- "country"
  download <- download %>%  mutate(ISO3_CODE = countrycode(download$country, origin = 'country.name', destination = 'iso3c', nomatch =NA)) %>% 
    select(-country)
  download <- download %>%  group_by(ISO3_CODE) %>% summarise_all( sum)
  download <- download %>%  drop_na(ISO3_CODE) 

  return(download)
}


prepare_map_data <- function(map, infections){

    # Merge data to map
  map@data <- map@data %>% full_join(infections)
  map@data <- map@data %>%  drop_na(id) 
  map@data <- map@data %>%  arrange(map@data$id)
  
  # Add population data
  pop <- read_csv("API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv",skip = 3)
  pop <- pop %>% select(`Country Code`,`2018`)
  names(pop) <- c("ISO3_CODE", "pop2018")
  
  # Merege population data with map
  map@data <- map@data %>% full_join(pop)
  map@data <- map@data %>%  drop_na(id) 
  map@data <- map@data %>% arrange(map@data$id)
  
  return(map)
}

# create_map_data <- function(m1, date){
#   
#   d <- as.Date(date)
#   day <- format(d, "%d")
#   month <- as.numeric(format(d, "%m"))
#   
#   var <- paste0("x",month, "_", day,"_",20)
#   
#   m1@data$cases = m1@data[[var]]
#   
#   m1@data <- m1@data %>% select(c(id, NAME_ENGL, pop2018, ISO3_CODE, cases))
#   
#   m1@data <- m1@data %>% mutate(cases_pc = cases/pop2018*(10^6))
#   m1@data <- m1@data %>% mutate(cases_pc = round(cases_pc,2))
#   m1@data <- m1@data %>% arrange(m1@data$id)
#   return(m1)
# }





per_capita_data <- function(m1){
  m1@data <- m1@data  %>% mutate_at(names(select(m1@data, contains("x"))), ~./pop2018*(10^6))
  return(m1)
  }



growth_rate <- function(m2){
  m2@data <- m2@data %>% 
    gather(date, cases, names(select(m2@data, contains("x")))) %>% 
    group_by(id) %>% 
    mutate(prev_cases = lag(cases),
           pct_change = round((cases/prev_cases -1)*100))
  m2@data$pct_change[is.nan(m2@data$pct_change)]<-0
  m2@data <- m2@data %>% 
    select(-c(cases, prev_cases)) %>% 
    spread(date, pct_change)
  return(m2)
}#






# 
# prepare_map_data <- function(m1){
#   # Load confirmed cases
#   myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
#   confirmed <- read_csv(myfile)
#   
#   confirmed <- confirmed %>% dplyr::select(`Country/Region`, `3/20/20`)
#   names(confirmed) <- c("country", "_cases")
#   confirmed <- confirmed %>%  group_by(country) %>% summarize(cases = sum(`_cases`))
#   confirmed <- confirmed %>%  mutate(ISO3_CODE = countrycode(confirmed$country, origin = 'country.name', destination = 'iso3c', nomatch =NA)) %>% 
#     select(ISO3_CODE, cases)
#   
#   # Merge data to map
#   m1@data <- m1@data %>%  mutate(id = row_number())
#   m1@data <- m1@data %>% full_join(confirmed)
#   m1@data <- m1@data %>%  drop_na(id) 
#   m1@data <- m1@data %>%  arrange(m1@data$id)
#   
#   # Add population data
#   pop <- read_csv("API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv",skip = 3)
#   pop <- pop %>% select(`Country Code`,`2018`)
#   names(pop) <- c("ISO3_CODE", "pop2018")
#   
#   # Merege population data with map
#   m1@data <- m1@data %>% full_join(pop)
#   m1@data <- m1@data %>%  drop_na(id) 
#   m1@data <- m1@data %>% arrange(m1@data$id)
#   m1@data <- m1@data %>% mutate(cases_pc = cases/pop2018*(10^6))
#   m1@data <- m1@data %>% mutate(cases_pc = round(cases_pc,2))
#   
#   return(m1)
# }