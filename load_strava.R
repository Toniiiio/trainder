#remotes::install_github("grimbough/FITfileR")
#devtools::install("biketrainr-master")
library(remotes)
library(FITfileR)
library(magrittr)
library(plyr)

# file_name <- "biketrainr-master/data/2022-06-12-091231-UBERDROID7506-100-0.fit"
# file_name <- "C:/Users/Tonio/Downloads/Afternoon_Ride (2).fit"
# file_name <- "C:/Users/Tonio/Downloads/Been_a_while_.fit"
# file_name <- "C:/Users/User11/Downloads/" %>%
#   {file.path(., list.files(.))} %>%
#   file.info() %>%
#   {rownames(.[which.max(.$atime), ])}
# file_name
file_name <- "biketrainr-master/data/Offsite.fit"

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
  # plot(strava_data$power)
  # plot(strava_data$power.x)
  # plot(strava_data$power.y)
  
  strava_data %>% head
  amt_na <- apply(strava_data, 2, function(col) sum(is.na(col))) / dim(strava_data)[1]
  keep <- which(amt_na < 0.99)
  strava_data <- strava_data[, keep]
  
  # targets <- c("heart_rate", "timestamp", "cadence", "gps_accuracy", "position", "power", "speed")
  # matches <- sapply(targets, grepl, names(strava_data))
  # keep <- apply(matches, 2, which) %>% unlist
  # strava_data <- strava_data[, keep]
  
  strava_data <- zoo::na.locf(strava_data, na.rm = FALSE)  
  idx <- which(grepl("distance", names(strava_data)))
  rr <- !is.na(strava_data[, idx]) %>% as.matrix
  start <- apply(rr, 2, which) %>% unlist %>% min

  strava_data <- strava_data[start:nrow(strava_data), ]
  
  names(strava_data) <- gsub(pattern = "[.].*", replacement = "", x = names(strava_data))
  nms <- names(strava_data)
  dup1 <- nms %>% duplicated %>% which
  dup2 <- nms %>% duplicated(fromLast = TRUE) %>% which
  nr <- dup1[5]
  keep <- c()
  for(nr in dup1){
    name <- nms[nr]
    idx <- which(name == nms)
    keep <- c(keep, idx[which.min(colSums(is.na(strava_data[, idx])))])
  }
  
  non_dups <- setdiff(seq(nms), c(dup1, dup2))
  keep1 <- unique(c(non_dups, unique(keep)))
  strava_data <- strava_data[, keep1]
    
  # extrem starke Steigung unrealistisch, aber mit Zeitstempel doppelchecken
  # ggf. wurden zeitpunkte ausgelassen 
  #which(diff(records$altitude) < -20.6) 
  # which(abs(diff(records$position_lat)) > 1) # for long / lat bounces
  # strava_data$altitude[strava_data$altitude > 5000] <- NA
  gpx_idx <- which(strava_data$gps_accuracy == 255)
  if(length(gpx_idx)){
    targets <- c("position_lat", "position_long", "altitude", "speed", "enhanced_altitude", "speed")
    col_idx <- sapply(targets, function(n) which(n == colnames(strava_data))) %>% unlist %>% unique
    strava_data[gpx_idx, col_idx] <- NA #   # keep: heart_rate, cadence, timestamp, power
    strava_data[c(min(gpx_idx) - 1, gpx_idx), ] <- zoo::na.locf(strava_data[c(min(gpx_idx) - 1, gpx_idx), ], na.rm = FALSE)
  }
    
  strava_data$speed <- strava_data$speed*3.6 # m/s -> km/h
  idx_speed0 <- which(strava_data$speed == 0)
  # diffs <- c(1, diff(idx_speed0))
  # diff_diff <- abs(diff(diffs))
  # diff_diff_before <- c(diff_diff[-1], 0)
  # diff_diff_after <- c(0, diff_diff[-length(diff_diff)])
  # anamolies <- which(diff_diff > 0 & diff_diff_before >0 & diff_diff_after > 0)
  # rles <- rle(diffs)
  # remove <- rles$lengths == 1
  # end <- cumsum(rles$lengths)
  # start <- c(1, 1 + end[-length(end)])
  # cbind(start, end, remove)
                    # strava_data <- strava_data[-idx_speed0, ]
  # rec <- xxx$records[, c("speed", "timestamp")]
  diffs <- diff(strava_data$timestamp)
  idx <- which(diffs != 1)
  has_break <- length(idx)
  if(has_break){
    n <- nrow(strava_data)
    # nrr <- 1
    for(nrr in 1:length(idx)){
      nr <- idx[nrr]
      strava_data$timestamp[(nr + 1):n] <- strava_data$timestamp[(nr + 1):n] - as.numeric(diffs[nr]) + 1
    }
  }
  
  power_idx <- which(strava_data$power == 65535)
  if(length(power_idx)){
    targets <- c("cadence", "power")
    col_idx <- sapply(targets, function(n) which(n == colnames(strava_data))) %>% unlist
    strava_data[power_idx, col_idx]  <- NA #   # keep: heart_rate, cadence, timestamp, power
    strava_data[c(min(power_idx) - 1, power_idx), ] <- zoo::na.locf(strava_data[c(min(power_idx) - 1, power_idx), ], na.rm = FALSE)
  }
  
  # plot(strava_data$speed, type = "l")
  # plot(strava_data2$speed, type = "l")
  
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

# file_name <- "biketrainr-master/data/Cardada.fit"
# records <- load_strava(file_name)
# plot(records$altitude, type = "l")
# plot(records$position_lat)
# plot(records$position_long)

# file_name <- "C:/Users/Tonio/Downloads/Afternoon_Ride.fit"
# file_name <- "biketrainr-master/data/wahoo_25_05.fit"
# parsed <- parse_strava(file_name)
# parsed %>% head
# parsed$meta$altitude

calculate_meta <- function(records, require_stitching = FALSE){
 
  time_range <- records$timestamp %>% range
  hr_avg <- mean(records$heart_rate)
  date <- as.Date(records[1, ]$timestamp)
  speed_avg <- mean(records$speed)
  
  if(is.null(records$enhanced_altitude)) records$enhanced_altitude <- records$altitude
  
  if(!is.null(records$enhanced_altitude)){
    records$enhanced_altitude <- caTools::runmean(records$enhanced_altitude, 150)
    # plot(records$enhanced_altitude, type = "l")
    altitude_diff <- diff(records$enhanced_altitude)
    uphill_only <- altitude_diff > 0
    sum_altitude <- sum(altitude_diff[uphill_only])
  }else{
    sum_altitude <- 0
  }
  
  distance <- max(records$distance)
  stitch_value <- which(diff(records$distance) < 0) # does distance start lower again? --> is stichted
  records$distance %>% head
  records$distance[stitch_value]
  plot(records$distance[4300:4600], type ="l")
  plot(records$distance, type ="l")
  
  
  if(length(stitch_value) & require_stitching){
    print("insiiiide")
    dist_1 <- records$distance[stitch_value]
    dist_2 <- records$distance[length(records$distance)]
    distance <- dist_1 + dist_2
  }

  duration <- as.numeric(diff(time_range))*60
  hours <- floor(duration/60)
  minutes_raw <- duration - hours*60
  minutes <- floor(minutes_raw)
  seconds <- ceiling((minutes_raw - minutes)*60)
  
  meta <-  list(
    is_stitched = FALSE,
    added_at = Sys.time(),
    distance = distance,
    hours = hours,
    date = date,
    altitude = sum_altitude,
    duration = duration,
    minutes = minutes,
    seconds = seconds,
    speed_avg = speed_avg,
    hr_avg = hr_avg
  )
  meta
}

parse_strava <- function(file_name){
  records <- load_strava(file_name)

  meta <- calculate_meta(records)  
  
  list(
    records = records,
    meta = meta
  )
  
}

# xx <- parse_strava(file_name)
# summary(rec$timestamp)
# qxts <- xts(rec, order.by = rec$timestamp)
# qxts$timestamp
# dygraph(qxts)

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

