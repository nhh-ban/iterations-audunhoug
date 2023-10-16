

# transform metadata to dataframe function
transform_metadata_to_df <- function(metadata){
  stations_metadata[[1]] %>% 
    map_df(as_tibble) %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>%
    unnest_wider(location) %>% 
    unnest_wider(latLon) -> df
  return(df)
}

# to_iso8601 function

to_iso8601 <- function(datetime, offset) {
  datetime <- as_datetime(datetime, tz = "UTC") + days(offset)
  datetimeiso8601 <- iso8601(datetime)
  datetimeiso8601 <- paste0(datetimeiso8601, "Z")
  return(datetimeiso8601)
}



