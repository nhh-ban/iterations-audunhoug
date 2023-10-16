
# GQL volume query function

vol_qry <- function(id, from, to) {
  query <- glue::glue(
    '{
      trafficData(trafficRegistrationPointId: "<<id>>") {
        volume {
          byHour(from: "<<from>>", to: "<<to>>") {
            edges {
              node {
                from
                to
                total {
                  volumeNumbers {
                    volume
                  }
                }
              }
            }
          }
        }
      }
    }
    ',
    .open = "<<",
    .close = ">>",
    id = id,
    from = from,
    to = to
  )
  
  return(query)
}

vol_qry(id = "97411V72313", 
        from = "2022-05-01T06:55:47.172Z", 
        to = "2022-05-08T06:55:47.172Z")


test <- GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

# transform volumes function

transform_volumes <- function(metadata){
  num_edges <- length(metadata$trafficData$volume$byHour$edges)
  
  df <- data.frame(
    from = as_datetime(rep(NA, num_edges), tz = "UTC"),
    volume = rep(NA, num_edges)
  )
  
  for (i in 1:num_edges) {
    df$from[i] <- as_datetime(metadata$trafficData$volume$byHour$edges[[i]]$node$from, tz = "UTC")
    df$volume[i] <- metadata$trafficData$volume$byHour$edges[[i]]$node$total$volumeNumbers$volume
  }
  
  return(df)
  
}












