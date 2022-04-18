
workout_meta <- function(type = "eb", ...){
  
  
  inputs <- as.list(substitute(list(...)))
  
  if(type == "eb"){
    
    intervall <- eb_intervalls(
      duration = inputs$duration, 
      repetition = inputs$repetition, 
      break_size = inputs$break_size, 
      watt_hit = inputs$watt_hit, 
      watt_break = inputs$watt_break
    )
    
  }else if(type == "ib"){
    
    intervall <- ie_intervalls(
      sec = inputs$sec, 
      rep_intensity = inputs$rep_intensity, 
      rep_sets = inputs$rep_sets, 
      break_set_duration = inputs$break_set_duration, 
      break_intensity_ratio = inputs$break_intensity_ratio, 
      watt_hit = inputs$watt_hit, 
      watt_break = inputs$watt_break
    )
    
  }else{
    
    message("provided type is not valid. Choose either eb or ib.")
    return()
    
  }
  
  watt_table <- rbind(
    warmup(),
    watt_stairs(),
    intervall,
    cool_down()    
  )
  
  nrg <- biketrainr::create_nrg()
  nrg <- nrg[!duplicated(nrg), ]
  
  nrgs <- plyr::join(watt_table, nrg, by = "watt")
  nrgs$hr <- NULL
  nrgs$carbs <- nrgs$carbs * nrgs$seconds/3600
  nrgs$fat <- nrgs$fat * nrgs$seconds/3600
  
  carbs <- sum(nrgs$carbs)
  fat <- sum(nrgs$fat)
  
  n_secs <- sum(nrgs$seconds)
  FTP <- 270
  
  np_ma_amt_days <- 30
  watt <- rep(nrgs$watt, nrgs$seconds)
  
  NP <- (mean(caTools::runmean(watt, np_ma_amt_days)^4))^0.25
  IF = NP/FTP
  TSS <- n_secs*IF^2 / 36
  
  data.frame(
    TSS = round(TSS),
    AVG_WATT = round(mean(watt)),
    NP = round(NP),
    IF = round(IF, 2),
    carbs = round(carbs),
    fat = round(fat)
  )
  
}

workout_meta(type = "eb", duration = 8, repetition = 4, break_size = 0.5, watt_hit = 295, watt_break = 130)
workout_meta(type = "ib", sec = 30, rep_intensity = 13, rep_sets = 3, break_set_duration = NULL, break_intensity_ratio = 0.5, watt_hit = 340, watt_break = 130)

             