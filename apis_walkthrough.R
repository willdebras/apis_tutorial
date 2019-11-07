###WEB APIS

library(rtweet)
library(tidycensus)
library(ctar)
library(ggplot2)
library(magrittr)


#Twitter API uses

plath <- rtweet::get_timeline(user = "SylviaPlathBot", n = 200)

will_foll <- rtweet::get_friends(user = "_willdebras")

will_friends <- rtweet::lookup_users(will_foll$user_id)

norc <- rtweet::search_users("norc", n = 100)

#Census API uses

#find variables from ACS

acs_vars <- tidycensus::load_variables(2017, "acs5", cache = TRUE)

#basic query to get data for that variable

inc_state <- tidycensus::get_acs(geography = "state", variables = "B19013_001", year = 2017)

#plot and take a look

inc_state %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_point() +
  theme_minimal()


#Use httr::GET to grab raw json or XML for Western bus chicago
raw_cta <- httr::GET("http://www.ctabustracker.com/bustime/api/v2/getvehicles?key=xxxxxxxxxxxxx&rt=49&format=json")


#Parse this raw data into a format we like in R
parsed_cta <- jsonlite::fromJSON(httr::content(raw_cta, "text"), simplifyDataFrame = TRUE)

#Look through the data to see what we want
locations <- parsed_cta$`bustime-response`$vehicle

locations$tmpstmp <- as.POSIXct(locations$tmstmp, format = "%Y%m%d %H:%M")


#Let's wrap these into a useful function

get_bus_location <- function(
  routes=NULL, #the route parameter is the bus route
  vehicle_ids=NULL, #vehicle id is a unique indentifier for each bus on the routee
  time_resolution="s", #time_resolution is how detailed we want the time response to be
  key = Sys.getenv("ctar_api_key") #this is our key supplied in the environment.
  #usethis::edit_r_environ() to change the environment key
) {
  

  
  base_url <- "http://www.ctabustracker.com/bustime/api/v2/" 
  endpoint <- "getvehicles" #We want to specify the endpoint, which is just the "end" of the URL before parameters
  url <- paste0(base_url, endpoint)
  
  #Allows for multiple requests 
  
  if (!is.null(vehicle_ids)) {
    vehicle_ids <- paste(vehicle_ids, collapse=",")
  }
  
  if (!is.null(routes)) {
    routes <- paste(routes, collapse=",")
  }
  
  
  #we use the query parameter in `modify_url()` to add parameters to our url
  
  raw <- httr::GET(url, query=list( 
    key=key, format="json", 
    vid=vehicle_ids, rt=routes, tmres=time_resolution
  ))
  
  if (httr::http_type(raw) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  #We again parse these data
  
  parsed <- jsonlite::fromJSON(
    httr::content(raw, "text"),
    simplifyDataFrame = TRUE
  )
  
  df <- parsed$`bustime-response`$vehicle
  
  #Some basic error handling with the stop() command to prompt an error if there is an API error
  
  if (is.null(df)) {
    
    if (is.null(parsed$`bustime-response`$error)) {
      stop("No data found for search parameters")
    }
    else {
      stop(
        paste("Error", paste(parsed$`bustime-response`$error, collapse=", "))
      )
    }
  }
  
  #Coerce time formats
  
  if (time_resolution == "s") {
    time_format <- "%Y%m%d %H:%M:%S"
  } else {
    time_format <- "%Y%m%d %H:%M"
  }
  df$tmstmp <- as.POSIXct(df$tmstmp, format=time_format, tz="America/Chicago")
  
  #Use return function to explicitly return the dataframe
  
  return(df)
}


locations_western <- get_bus_location(route = 44)
