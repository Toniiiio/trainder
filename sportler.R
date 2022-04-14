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



library(R6)
source("load_strava.R")

cyclist <- R6::R6Class("Cyclist", list(
  FTP = 240,
  file_name= NULL,
  workout = NULL,
  meta = NULL,
  workouts = list(),
  config = list(np_ma_amt_days = 30, ctl_avg_amt_days = 42, atl_avg_amt_days = 7,
                ctl_start_val = 70, atl_start_val = 70)
))


cyclist$set("public", "calc_NP", function(watts){
  (mean(caTools::runmean(watts, self$config$np_ma_amt_days)^4))^0.25 
})


cyclist$set("public", "calc_tss", function(NP = 184, sec, workout_date){
  # TSS = (sec * NP * IF)/(FTP * 3600) * 100
  # IF = NP / FTP
  self$meta[self$meta$dates == workout_date, ]$TSS = sec*NP^2 / (self$FTP^2 * 3600)*100
})

cyclist$set("public", "add_workout", function(){
  
  self$workout <- parse_strava(self$file_name)
  # approximation n = amount secs
  n_secs <- length(self$workout$records$power)
  NP <- self$calc_NP(self$workout$records$power)
  
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
    
    workout_duplicate <- sum(sapply(self$workouts, identical, y = self$workout))
    message("Workout was already uploaded!")
    
    # also take if it is equal to lowest values, since one day before is required for CTL and ATL values.
    require_update <-  workout_date <= min(cyclist_1$meta$dates)
    
    if(require_update){
      dates <- seq.Date((workout_date - 1), min(cyclist_1$meta$dates), "days")
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
    dates <- seq.Date((workout_date - 1), min(cyclist_1$meta$dates), "days")
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
  
  
  self$workouts <- c(self$workouts, self$workout)
  
  self$calc_meta()
})


cyclist$set("public", "calc_meta", function(){
  
  n <- nrow(self$meta)
  for(nr in 2:n){
    self$meta$CTL[nr] <- self$meta$CTL[nr - 1] + (self$meta$TSS[nr] - self$meta$CTL[nr - 1])/self$config$ctl_avg_amt_days 
  }
  print(self$meta$CTL)
  
  for(nr in 2:n){
    self$meta$ATL[nr] <- self$meta$ATL[nr - 1] + (self$meta$TSS[nr] - self$meta$ATL[nr - 1])/self$config$atl_avg_amt_days
  }
  print(self$meta$ATL)
  
  TSB_calc = self$meta$CTL - self$meta$ATL
  self$meta$TSB <- c(0, TSB_calc[-length(TSB_calc)])
  
})


cyclist_1 = cyclist$new()
# cyclist_1$file_name <- "biketrainr-master/data/02_04_2022_LIT.fit"
cyclist_1$file_name <- "biketrainr-master/data/27_03_2022.fit"
cyclist_1$add_workout()
cyclist_1$meta

# saveRDS(cyclist_1, file = "cyclist_1.RData")
# load("cyclist_1.RData")



