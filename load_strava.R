#remotes::install_github("grimbough/FITfileR")
#devtools::install("biketrainr-master")
library(remotes)
library(FITfileR)
library(magrittr)
library(tidyverse)

file_name <- "Lunch_Ride.fit"
# file_name <- "C:/Users/User11/Downloads" %>%
#   {file.path(., list.files(.))} %>%
#   file.info() %>%
#   {rownames(.[which.max(.$atime), ])}
# file_name


# keep <- strava_records %>%
#   sapply(nrow) %>%
#   which.max

load_strava <- function(file_name){
  strava <- FITfileR::readFitFile(file_name)
  strava_records <- FITfileR::records(strava)
  
  # take the one with the most records
  # but the variables could be split across records, because
  # not all observations have all variables, e.g. heart_rate
  
  # all records share the timestamp. so join the records via
  # the timestamp variable.
  
  strava_data <- strava_records[[1]]
  nr <- 2
  for(nr in seq(strava_records)[-1]){
    strava_data <- merge(strava_data, strava_records[[nr]], all = TRUE, by = "timestamp", no.dups = TRUE)
  }
  
  strava_data %>% head
  amt_na <- apply(strava_data, 2, function(col) sum(is.na(col))) / dim(strava_data)[1]
  keep <- which(amt_na < 0.99)
  strava_data <- strava_data[, keep]
  
  strava_data <- zoo::na.locf(strava_data)
  names(strava_data) <- gsub(pattern = "[.].*", replacement = "", x = names(strava_data))
  strava_data %>% head
  strava_data
  
  # keep <- strava_records %>%
  #   lapply(dim) %>%
  #   lapply(prod) %>%
  #   which.max
  #   # sapply(nrow) %>%
  #   # which.max
  #
  # strava_data <- strava_records[[keep]]
  # strava_data
}

# file_name <- "Radfahrt_am_Morgen.fit"
# records <- load_strava(file_name)

parse_strava <- function(file_name){
  records <- load_strava(file_name)
  
  time_range <- records$timestamp %>% range
  hr_avg <- mean(records$heart_rate)
  median(records$heart_rate)
  max(records$heart_rate)
  max(records$speed)*0.621371
  date <- as.Date(records[1, ]$timestamp)
  speed_avg <- mean(records$speed)

  xx <- diff(time_range)
  distance <- max(records$distance)
  duration <- as.numeric(xx)*60
  hours <- floor(duration/60)
  minutes_raw <- duration - hours*60
  minutes <- floor(minutes_raw)
  seconds <- ceiling((minutes_raw - minutes)*60)

  meta <-  list(
    distance = distance,
    hours = hours,
    date = date,
    duration = duration,
    minutes = minutes,
    seconds = seconds,
    speed_avg = speed_avg,
    hr_avg = hr_avg
  )
  
  list(
    records = records,
    meta = meta
  )

}
# # head(records)
# time_range <- records$timestamp %>% range
# hr_avg <- mean(records$heart_rate)
# median(records$heart_rate)
# max(records$heart_rate)
# max(records$speed)*0.621371
# date <- as.Date(records[1, ]$timestamp)
# speed_avg <- mean(records$speed)
# 
# xx <- diff(time_range)
# max(records$distance)
# time_diff <- as.numeric(xx)*60
# hours <- floor(time_diff/60)
# minutes_raw <- time_diff - hours*60
# minutes <- floor(minutes_raw)
# seconds <- ceiling((minutes_raw - minutes)*60)
# 
# hours
# minutes
# seconds
# speed_avg
# hr_avg



check_power <- function(records){
  has_power <- !is.null(records$power)
  if(has_power){
    
    max_power <- 800
    clean_avg <- records$power[records$power <= max_power] %>% mean
    records$power[records$power > max_power] <- clean_avg
    return(records)
  }
  
  w <- sapply(records$heart_rate, function(hr) which.min(abs(hr - nrg$hr)))
  
  plot(nrg$watt[w], records$power)
}

# records <- check_power(records)


# efficiency <- records$power / records$heart_rate
# plot(efficiency, type = "l")
# efficiency %>% mean


used_carbs <- function(records){
  avg_power <- records$power %>% mean
  time <- records$timestamp %>%
    range %>%
    {difftime(time1 = .[2], time2 = .[1], units = "hours")} %>%
    as.numeric
  
  idx <- which(nrg$watt == round(avg_power))[1]
  carbs <- nrg[idx, ]$carbs
  time*carbs
  
  records$power
}

# used_carbs(records)

