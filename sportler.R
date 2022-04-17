# Requirements:

# Custom 
#   interface per user 
#   stats per user --> have to be calculated per functions
# --> object oriented.

# Storing TSS, CTL, ATL, TSB
# Could either expand time series everytime a new workout is uploaded.
# Or take predefined time interval, e.g. 1.1.2000 - today. That would 
# be 8000+ values. Most of them empty. 
# However, everytime a new workout is uploaded it could be an older workout
# as well, where CTL, ATL etc. would have to be recalculated anyway.
# Therefore, recalculation is a required feature anyway. 
# So if a new workout is added, expand the TSS values and recalculate the rest.
# Time horizon is one day before oldest workout until one day ahead new workout / 
# or better one day ahead of today / tomorrow.
# So a recalcuation is required, everyday and everytime a new workout is uploaded.
# A redefinition of the time horizon is required, everyday and everytime a workout older than the 
# oldest is uploaded.

# Storing would be the same time dimension across TSS, CTL, ATL and TSB. THerefore, they could be
# added within a data.frame, with time being in the rows. 
# If the time horizon expands, one could either create a new data frame and add the info of the old
# one. Create a new data.frame with the missing values. And then recalculate ATL, CTL and TSB anyway.
# Adding old values to a new data.frame is rather dirty. Then better add new block to the old one
# and recalculate everything. 
# Szenario 1: No data existed. Take first workout and create data.frame. 
# Szenario 2: Data existed. New workout within existing time horizon. Just add TSS and recalculate.
# Szenario 2: Data existed. New workout outside existing time horizon. Add new block
#             add TSS and recalculate.


# Storing data
# Requirements:
# 1. TSS, CTL, ASB and ATL per day --> meta
# 2. TSS, IF, Duration, etc. per workout (can have multiple workouts per day) --> workout_details
# 3. Check if workout was already uploaded --> workouts


library(R6)
library(lubridate)
source("load_strava.R")
source("biketrainr-master/R/gen_energy_data.R")

user_name <- "Toniiiio"

cyclist <- R6::R6Class(
  classname = "Cyclist", 
  public = list(
    FTP = 240,
    file_names = NULL,
    workout = NULL,
    workout_raw = NULL,
    meta = NULL,
    watt = NULL,
    workout_details = data.frame(
      id = character(0),
      duration = numeric(0),
      TSS = numeric(0),
      IF = numeric(0),
      file_name = character(0)
    ),
    workouts = list(),
    config = list(np_ma_amt_days = 30, ctl_avg_amt_days = 42, atl_avg_amt_days = 7,
                  ctl_start_val = 70, atl_start_val = 70)
  )
)


cyclist$set("public", "calc_NP", function(watts){
  (mean(caTools::runmean(watts, self$config$np_ma_amt_days)^4))^0.25 
})


cyclist$set("public", "calc_tss", function(NP = 184, sec, workout_date){
  # TSS = (sec * NP * IF)/(FTP * 3600) * 100
  # IF = NP / FTP
  self$meta[self$meta$dates == workout_date, ]$TSS = sec*NP^2 / (self$FTP^2 * 3600)*100
})

cyclist$set("public", "watt_quality_check", function(){
  missing_power <- self$watt == 65535
  if(sum(missing_power)){
    message("Performing Watt quality check.")
    message("Detecting watt values equal to 65535.")
    message("Probably connection to Wahoo kickr core was temporarily timeout out.")
    message("Replacing 65535 with 0.")
    self$watt[missing_power] <- 0 
  }
  
})


cyclist$set("public", "upload_workouts", function(){
  for(file_name in self$file_names){
    print(file_name)
    self$add_workout(file_name)
  }
})

cyclist$set("public", "create_watt", function(hr){
  
  nrg = create_nrg()
  nrg$hr <- round(nrg$hr)
  nrg$power <- nrg$watt
  nrg[nrg$hr < 80, "watt"] <- 0
  nrg$heart_rate <- nrg$hr
  
  has_hr <- !is.null(self$workout$records$heart_rate)
  if(!has_hr){
    
    message("Could not find any heart rate data. Set all watt values to 100.")
    self$watt <- rep(100, nrow(self$workout$records))
    
  }else{
    self$workout$records <- plyr::join(self$workout$records, nrg, by = "heart_rate")
    has_cadence <- !is.null(self$workout$records$cadence)
    if(has_cadence){
      self$workout[self$workout$records$cadence == 0, "power"] <- 0
    }
    self$watt <- self$workout$records$power
  }
  
})

cyclist$set("public", "add_workout", function(file_name){
  
  self$workout_raw <- parse_strava(file_name)
  self$workout <- self$workout_raw
  
  workout_duplicate <- sum(as.numeric(sapply(self$workouts, function(w){
    identical(w$meta, y = self$workout_raw$meta)
    # self$workout_raw$meta$distance == w$meta$distance & w$meta$date == w$meta$date
  })))
  
  if(workout_duplicate){
    message("Workout was already uploaded!")
    return()
  }
  
  self$watt <- self$workout$records$power
  no_watts <- is.null(self$watt)
  if(no_watts) self$create_watt(self$workout$records$heart_rate)
  self$watt_quality_check()
  
  
  # approximation n = amount secs
  n_secs <- length(self$watt)
  
  duration <- gsub(pattern = "d |H |M ", replacement = ":", x = lubridate::seconds_to_period(n_secs))
  duration <- gsub(pattern = "S", replacement = "", x = duration)
  NP <- self$calc_NP(self$watt)
  
  has_workouts <- length(self$workouts)
  workout_date = self$workout$meta$date
  
  if(!has_workouts){
    start_date <- workout_date - 1
    end_date <- Sys.Date() + 1
    dates <- seq.Date(start_date, end_date, "days")

    n_dates <- length(dates)
    self$meta <- data.frame(
      dates = dates,
      TSS = rep(0, n_dates),
      CTL = rep(self$config$ctl_start_val, n_dates),
      ATL = rep(self$config$atl_start_val, n_dates),
      TSB = rep(0, n_dates)
    )
    
    self$meta$TSS[length(self$meta$TSS)] <- NA
    
  }else{
    
    # also take if it is equal to lowest values, since one day before is required for CTL and ATL values.
    require_update <-  workout_date <= min(self$meta$dates)
    
    if(require_update){
      dates <- seq.Date((workout_date - 1), min(self$meta$dates), "days")
      n_dates <- length(dates)
      meta_to_add <- data.frame(
        dates = dates,
        TSS = rep(0, n_dates),
        CTL = rep(self$config$ctl_start_val, n_dates),
        ATL = rep(self$config$atl_start_val, n_dates),
        TSB = rep(0, n_dates)
      )
      self$meta <- rbind(
        meta_to_add,
        self$meta
      )
    }

  }
  
  max_date <- max(self$meta$dates)
  req_end_date <- Sys.Date() + 1
  req_prolong <- max_date < req_end_date
  if(req_prolong){
    dates <- seq.Date((workout_date - 1), min(self$meta$dates), "days")
    n_dates <- length(dates)
    meta_to_add <- data.frame(
      dates = dates,
      TSS = rep(0, n_dates),
      CTL = rep(self$config$ctl_start_val, n_dates),
      ATL = rep(self$config$atl_start_val, n_dates),
      TSB = rep(0, n_dates)
    )
    self$meta <- rbind(
      self$meta,
      meta_to_add
    )
  }
  
  TSS = n_secs*NP^2 / (self$FTP^2 * 3600)*100
  self$meta[self$meta$dates == workout_date, ]$TSS <- TSS
  self$calc_meta()
  
  self$workouts <- c(self$workouts, list(self$workout_raw))
  
  workout_details_add <- data.frame(
    id = paste0(user_name, "_", nrow(self$workout_details) + 1),
    date = workout_date,
    duration = duration,
    n_secs = n_secs,
    TSS = TSS,
    IF = NP/self$FTP,
    NP = NP,
    file_name = file_name
  )
  
  self$workout_details <- rbind(self$workout_details, workout_details_add)
  
})


cyclist$set("public", "calc_meta", function(){
  
  n <- nrow(self$meta)
  for(nr in 2:n){
    self$meta$CTL[nr] <- self$meta$CTL[nr - 1] + (self$meta$TSS[nr] - self$meta$CTL[nr - 1])/self$config$ctl_avg_amt_days 
  }
  
  for(nr in 2:n){
    self$meta$ATL[nr] <- self$meta$ATL[nr - 1] + (self$meta$TSS[nr] - self$meta$ATL[nr - 1])/self$config$atl_avg_amt_days
  }
  
  TSB_calc = self$meta$CTL - self$meta$ATL
  self$meta$TSB <- c(0, TSB_calc[-length(TSB_calc)])
  
})


source("load_strava.R")
source("biketrainr-master/R/gen_energy_data.R")

# sportler <- list(name = user_name)
# sportler$cyclist <- cyclist$new()
# 
# sportler$cyclist$file_names <- "biketrainr-master/data/Morga.fit"
# path <- "biketrainr-master/data/"
# sportler$cyclist$file_names <- file.path(path, list.files(path)[9])
# sportler$cyclist$upload_workouts()
# sportler$cyclist$workouts
# 
# sapply(sportler$cyclist$workouts, identical, y = sportler$cyclist$workout_raw)

# sportler$cyclist$meta
# sportler$cyclist$workout_details
# 
# 
# # saveRDS(object = sportler, file = paste0(user_name, ".RData"))
# # sportler = readRDS(file = paste0(user_name, ".RData"))
# sportler$name
# m <- sportler$cyclist$meta
# sportler$cyclist$workouts

