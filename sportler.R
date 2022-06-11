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
# as well, where CTL, ATL etc. would have to be recalculated 
.
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


# Find id for workouts: 
# Process: Upload workout and add it to other workouts. Also add and remove workouts.
# --> So numerating 1:x might cause problems, because i can add workouts 1:3, remove 1+3
# and if i want to add another workout, it would get the id 2, but there is already one
# with id = 2.
# Variants 1: Take random id for each workout. --> How can i identify then?
# Variants 2: Have counter independent of elimination --> 
# Variants 3: Dont permanently delete workout, keep info that there was one.
# delete data security related data, but keep rest behind ui. 
# --> Generate random id.


library(R6)
library(lubridate)
source("load_strava.R")
source("biketrainr-master/R/gen_energy_data.R")
source("calendar/analyse_energy.R")

user_name <- "Toniiiio"

cyclist <- R6::R6Class(
  classname = "Cyclist",
  private = list(
    reactiveDep = NULL
  ),
  public = list(
    energy = NULL,
    FTP = 240,
    file_names = NULL,
    workout = NULL,
    workout_raw = NULL,
    meta = NULL,
    watt = NULL,
    workout_details = data.frame(
      id = character(0),
      duration = numeric(0),
      avg_power = numeric(0),
      TSS = numeric(0),
      IF = numeric(0),
      file_name = character(0),
      altitude = numeric(0)
    ), 
    nrg = NULL,
    workouts = list(),
    config = list(np_ma_amt_days = 30, ctl_avg_amt_days = 42, atl_avg_amt_days = 7,
                  ctl_start_val = 70, atl_start_val = 70),
    initialize = function() {
      private$reactiveDep <- reactiveVal(0)
      self$nrg <- create_nrg()
    },
    reactive = function() {
      reactive({
        private$reactiveDep()
        self
      })
    },
    changewd = function(x) {
      private$reactiveDep(isolate(private$reactiveDep()) + 1)
      self$workout_details <- x
    },
    
    update_workouts = function(idx, workout_raw) {
      private$reactiveDep(isolate(private$reactiveDep()) + 1)
      print("within update_workouts")
      
      existing_workout <- self$workouts[[idx]]
      id <- existing_workout$meta$id
      print("id")
      print(id)
      idx_del <- which(self$workout_details$id == id)
      print("idx")
      print(idx_del)
      self$workout_details <- self$workout_details[-idx_del, ]
      
      recs <- list(
        existing_workout$records,
        workout_raw$records
      )
      start_first <- max(recs[[1]]$timestamp) < max(recs[[2]]$timestamp)
      if(start_first){
        recs[[2]]$distance <- recs[[2]]$distance + max(recs[[1]]$distance)
        records <- plyr::rbind.fill(recs[[1]], recs[[2]])
      }else{
        recs[[1]]$distance <- recs[[1]]$distance + max(recs[[2]]$distance)
        records <- plyr::rbind.fill(recs[[2]], recs[[1]])
      }
      plot(records$distance, type = "l")
      print("max(records$distance)")
      print(max(records$distance))
      self$workout$meta <- calculate_meta(records)
      print("self$workout$metaaaaa")
      print(self$workout$meta)
      
      self$workouts[[idx]] <- NULL
      return(records)
    }
  )
)


cyclist$set("public", "calc_NP", function(watts){
  if(is.null(watts)) return(0)
  print("starting calc_NP")
  (mean(caTools::runmean(watts, self$config$np_ma_amt_days)^4))^0.25 
})


cyclist$set("public", "calc_tss", function(NP = 184, sec, workout_date){
  # TSS = (sec * NP * IF)/(FTP * 3600) * 100
  # IF = NP / FTP
  self$meta[self$meta$dates == workout_date, ]$TSS = sec*NP^2 / (self$FTP^2 * 3600)*100
})

cyclist$set("public", "watt_quality_check", function(watt){
  watt[is.na(watt)] <- 0
  missing_power <- watt == 65535
  print("missing_power")
  print(missing_power)
  if(sum(missing_power)){
    message("Performing Watt quality check.")
    message("Detecting watt values equal to 65535.")
    message("Probably connection to Wahoo kickr core was temporarily timeout out.")
    message("Replacing 65535 with 0.")
    watt[missing_power] <- 0 
  }
  return(watt)
  
})


cyclist$set("public", "set_file_names", function(file_names){
  self$file_names <- file_names
})


cyclist$set("public", "reload_workouts", function(sportler){
  self$workout_details <- sportler$workout_details
  self$workouts <- sportler$workouts
  self$meta <- sportler$meta
  self$meta$dates <- as.Date(self$meta$dates)
  private$reactiveDep(isolate(private$reactiveDep()) + 1)
})

cyclist$set("public", "upload_workouts", function(to_stitch = FALSE){
  
  for(nr in seq(self$file_names)){
    file_name <- self$file_names[nr]
    stitch <- to_stitch & nr != 1
    self$add_workout(file_name, stitch = stitch)
  }
  
})

cyclist$set("public", "create_watt", function(records){
  
  print("inside create_watt")
  nrg = create_nrg()
  nrg$hr <- round(nrg$hr)
  nrg$power <- nrg$watt
  nrg[nrg$hr < 80, "watt"] <- 0
  nrg$heart_rate <- nrg$hr
  
  has_hr <- !is.null(self$workout$records$heart_rate)
  if(!has_hr){
    
    message("Could not find any heart rate data. Set all watt values to 100.")
    records$watt <- rep(100, nrow(records))
    
  }else{
    records <- plyr::join(records, nrg, by = "heart_rate")
    has_cadence <- !is.null(records$cadence)
    if(has_cadence){
      records[records$cadence == 0, "power"] <- 0
    }
    records$watt <- records$power
  }
  return(records)
})

# file_name <- "biketrainr-master/data/6_5min_2min_295_o_.fit"
# file_name <- "C:/Users/Tonio/Downloads/Evening_Ride.fit"
# self <- list()
cyclist$set("public", "add_workout", function(file_name, stitch){
  
  print("inside add_workout")
  workout_raw <- tryCatch(parse_strava(file_name),
                               error = function(e){
                                 print(e)
                                 message(paste0("There are difficulties processing the "))
                                 message(paste0("Fit contents for file with name: ", file_name, " might not be correct correct."))
                                 return(NULL)
                               }
  )

  if(is.null(workout_raw)) return()

  workout_duplicate <- sum(as.numeric(sapply(self$workouts, function(w){
    names(workout_raw$meta)
    identical(w$meta, y = workout_raw$meta)
    # self$workout_raw$meta$distance == w$meta$distance & w$meta$date == w$meta$date
  })))
  
  if(workout_duplicate){
    message("Workout was already uploaded!")
    return()
  }
  
  # check for stitching
  # stich
  same_day <- sapply(self$workouts, function(w){
    names(workout_raw$meta)
    w$meta$date == workout_raw$meta$date
    # self$workout_raw$meta$distance == w$meta$distance & w$meta$date == w$meta$date
  })
  
  self$workout <- workout_raw
  
  print("same_day")
  print(same_day)
  has_same_day <- sum(unlist(same_day))
  if(has_same_day & stitch){
    idx <- same_day[1]
    records <- self$update_workouts(idx, workout_raw)
    self$workout$records <- records
  }
  
  workout_id <- uuid::UUIDgenerate(use.time = TRUE)
  self$workout$meta$id <- workout_id
  
  # todo: could refactor that self$watt is not necessary
  no_watts <- is.null(self$workout$records$power)
  if(no_watts) self$workout$records <- self$create_watt(records = self$workout$records)
  # watt <- self$workout$records$power
  self$workout$records$power <- self$watt_quality_check(watt = self$workout$records$power)
  
  print("starting used energy")
  print(summary(self$workout$records$power))
  print("error?")
  print(self$nrg)
  ## debug: nrg <- create_nrg(); energy <- used_energy(records$power, nrg)
  self$energy <- used_energy(self$workout$records$power, self$nrg)
  
  print("njet")
  # approximation n = amount secs
  # n_secs <- length(self$watt)
  n_secs <- nrow(self$workout$records)
  print("n_secs")
  print(n_secs)
  
  duration <- gsub(pattern = "d |H |M ", replacement = ":", x = lubridate::seconds_to_period(n_secs))
  duration <- gsub(pattern = "S", replacement = "", x = duration)
  duration <- sapply(strsplit(duration, "[:]")[[1]], function(c) ifelse(nchar(c) == 1, paste0("0", c), c)) %>% 
    paste(collapse = ":")# add zeros
  
  
  NP <- self$calc_NP(self$workout$records$power)
  avg_power <- mean(self$workout$records$power)
  
  has_workouts <- length(self$workouts)
  workout_date <- as.Date(self$workout$meta$date)
  
  print("has_workouts")
  print(has_workouts)
  if(!has_workouts){
    start_date <- workout_date - 1
    end_date <- Sys.Date() + 1
    dates <- seq.Date(start_date, end_date, "days")

    n_dates <- length(dates)
    self$meta <- data.frame(
      id = workout_id,
      dates = dates,
      TSS = rep(0, n_dates),
      CTL = rep(self$config$ctl_start_val, n_dates),
      ATL = rep(self$config$atl_start_val, n_dates),
      TSB = rep(0, n_dates),
      altitude = rep(0, n_dates),
      distance = rep(0, n_dates),
      kcal = rep(0, n_dates),
      carbs = rep(0, n_dates),
      fat = rep(0, n_dates)
    )
    
    self$meta$TSS[length(self$meta$TSS)] <- NA
    
  }else{
    
    # also take if it is equal to lowest values, since one day before is required for CTL and ATL values.
    require_update <-  workout_date <= min(self$meta$dates)
    print("require_update")
    print(require_update)
    print(is.Date(self$meta$dates))
    
    if(require_update){
      dates <- seq.Date((workout_date - 1), min(self$meta$dates), "days")
      n_dates <- length(dates)
      meta_to_add <- data.frame(
        id = workout_id,
        dates = dates,
        TSS = rep(0, n_dates),
        CTL = rep(self$config$ctl_start_val, n_dates),
        ATL = rep(self$config$atl_start_val, n_dates),
        TSB = rep(0, n_dates),
        altitude = rep(0, n_dates),
        distance = rep(0, n_dates),
        kcal = rep(0, n_dates),
        carbs = rep(0, n_dates),
        fat = rep(0, n_dates)
      )
      self$meta <- rbind(
        meta_to_add,
        self$meta
      )
    }

  }
  
  print("meta")
  print(self$meta)
  print(self$meta$dates)
  
  max_date <- max(self$meta$dates)
  req_end_date <- Sys.Date() + 1
  req_prolong <- max_date < req_end_date
  if(req_prolong){
    print("workout_date")
    print(workout_date - 1)
    print(is.Date(workout_date))
    # workout_date <- as.Date("2022-06-09")
    all_dates <- c(workout_date, self$meta$dates)
    dates <- seq.Date(min(all_dates), max(all_dates), "days")
    print("dates")
    print(dates)
    n_dates <- length(dates)
    meta_to_add <- data.frame(
      id = workout_id,
      dates = dates,
      TSS = rep(0, n_dates),
      CTL = rep(self$config$ctl_start_val, n_dates),
      ATL = rep(self$config$atl_start_val, n_dates),
      TSB = rep(0, n_dates),
      altitude = rep(0, n_dates),
      distance = rep(0, n_dates),
      kcal = rep(0, n_dates),
      carbs = rep(0, n_dates),
      fat = rep(0, n_dates)
    )
    self$meta <- rbind(
      self$meta,
      meta_to_add
    )
  }
  
  print("self$workout$meta$distance")
  print(self$workout$meta$distance)
  
  calc_TSS <- function(NP, FTP){
    TSS = n_secs*NP^2 / (self$FTP^2 * 3600)*100
    if(!length(TSS)) TSS <- 0
    TSS
  }
  TSS <- calc_TSS(NP, FTP)
  
  self$meta[self$meta$dates == workout_date, ]$TSS <- TSS
  self$meta[self$meta$dates == workout_date, ]$altitude <- self$workout$meta$altitude
  self$meta[self$meta$dates == workout_date, ]$distance <- self$workout$meta$distance / 1000
  
  self$meta[self$meta$dates == workout_date, ]$kcal <- self$energy$kcal
  self$meta[self$meta$dates == workout_date, ]$carbs <- self$energy[[1]]$carbs_from_ma %>% round
  self$meta[self$meta$dates == workout_date, ]$fat <- self$energy[[2]]$fat_from_ma %>% round
  
  self$energy
  
  print("starting calc meta")
  self$calc_meta()
  
  self$workouts <- c(self$workouts, list(self$workout))
  
  print("self$workout$meta$distance")
  print(self$workout$meta$distance)
  
  print(TSS)
  print(NP)
  print(avg_power)
  
  workout_details_add <- data.frame(
    id = workout_id,
    date = workout_date,
    duration = duration,
    n_secs = n_secs,
    TSS = TSS,
    IF = NP/self$FTP,
    NP = NP,
    avg_power = avg_power,
    file_name = file_name,
    altitude = self$workout$meta$altitude,
    distance = self$workout$meta$distance / 1000,
    kcal = self$energy$kcal,
    carbs = self$energy[[1]]$carbs_from_ma %>% round,
    fat = self$energy[[2]]$fat_from_ma %>% round
  )

  self$workout_details <- rbind(self$workout_details, workout_details_add)
  
  # trigger reactive reaction
  private$reactiveDep(isolate(private$reactiveDep()) + 1)
})

cyclist$set("public", "del_wd_entries", function(id){

  print("self$workout_details$id")
  print(self$workout_details$id)
  print("id")
  print(id)
  to_delete_details <- which(self$workout_details$id == id)
  print("to_delete_details")
  print(to_delete_details)
  
  if(length(to_delete_details)) self$workout_details <- self$workout_details[-to_delete_details, ]

  n <- length(self$workouts)  
  ids <- rep("", n)
  for(nr in 1:n){
    ids[nr] <- self$workouts[[nr]]$meta$id
  }
  print("ids")
  print(ids)
  
  to_delete <- which(ids == id)
  print("to_delete")
  print(to_delete)
  
  if(length(to_delete)) self$workouts <- self$workouts[-to_delete]
  
  # trigger reactive reaction
  private$reactiveDep(isolate(private$reactiveDep()) + 1)

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



# user_name <- "shiny"
# sportler <- list(name = user_name)
# sportler$cyclist <- cyclist$new()
# # sportler$cyclist$nrg
# # # # # #
# file_name <- "biketrainr-master/data/stitch (1).fit"
# # file_name <- "C:/Users/Tonio/Downloads/Evening_Ride.fit"
# sportler$cyclist$file_names <- file_name
# sportler$cyclist$upload_workouts()
# sportler$cyclist$workouts
  # sportler$cyclist$energy

# file_name <- "C:/Users/Tonio/Downloads/WorkoutFileExport-Liebrand-Tonio-2021-05-31-2022-05-27/2022-03-09-190632-UBERDROID7506-27-0.fit"
# # # file_name <- "C:/Users/Tonio/Downloads/WorkoutFileExport-Liebrand-Tonio-2021-05-31-2022-05-27/fitfiletools.fit"
# # # all <- list.files(file_path, pattern = "*.fit")
# # # exclude <- list.files(file_path, pattern = "*.gz")
# # #
# # # # sportler$cyclist$file_names <- "biketrainr-master/data/Morga.fit"
# # # # path <- "biketrainr-master/data/"
# # # file_name <- file.path(file_path, setdiff(all, exclude))

  # sportler$cyclist$file_names <- file_name
  # sportler$cyclist$upload_workouts()
  # sportler$cyclist$workouts
  # sportler$cyclist$workout_details
# sportler$cyclist$file_names
# #
# # sapply(sportler$cyclist$workouts, identical, y = sportler$cyclist$workout_raw)
# 
# sportler$cyclist$meta
# sportler$cyclist$workout_details
# 
# 
# # saveRDS(object = sportler, file = paste0(user_name, ".RData"))

# user_name <- "shiny"
# sportler = readRDS(file = "user_data/shiny.RData")
# # sportler$name
# # m <- sportler$cyclist$meta
# sportler$cyclist$workout_details
# sportler$cyclist$workout_details



