# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <- # this purpose of this function is to check 
  # the column names
  function(df) {
    # creating a list of correct/expected names 
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    # an if/else code checks if all column names are correct in order 
    # for the test to pass
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }

test_stations_metadata_nrows <- # this function checks if the dataframe has a 
  # reasonable amount of rows 
  function(df) {
    # here a broad range of expected values are defined by a minimum and maximum
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    # an if/else statement checks if the number of rows are within the reasonable 
    # expected range
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

test_stations_metadata_coltypes <- # this functions checks the column types 
  # in the data frame
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    # an if/else statement checks if the column type mathces the defined list 
    # from the line above 
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
  
test_stations_metadata_nmissing <- # this function checks if the number of
  # missing values in the datafra are within a reasonable range 
  function(df) {
    max_miss_vals <- 200
    # the if/else statement below checks if the sum of missing values are 
    # below the defined maximum
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

test_stations_metadata_latestdata_timezone <- # this function checks the defined 
  # timezone for the latestData column
  function(df) {
    # yet again an if/else statement checks if the timezone is set to "UTC"
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }


test_stations_metadata <- # this function is the one being called in the other 
  # file, and it contains and runs all the previous functions/test on the input
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }





