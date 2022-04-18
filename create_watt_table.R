

# Requirements: have to create
# -  custom wattage for LIT / HIT
# - custom duration?
# - custom watt stairs

watt_table_input <- data.frame(
  seconds = c(),
  watt = c()
)

watt_table_input <- rbind(watt_table_input, data.frame(seconds = 300, watt = 130))

warmup <- function(seconds = 180, watt = 130){
  data.frame(
    seconds = seconds, 
    watt = watt
  )
}

watt_stairs <- function(final_watt = 250, step_size = 30, step_amt = 4, duration = 120){
  data.frame(
    seconds = rep(duration, step_amt), 
    watt = seq(from = final_watt - step_size*(step_amt - 1) ,to = final_watt, by = step_size)
  )
}

cool_down <- function(seconds = 180, watt = 130){
  data.frame(
    seconds = seconds, 
    watt = watt
  )
}

eb_intervalls <- function(duration = 4, repetition = 4, break_size = 0.5, watt_hit = 300, watt_break = 130){
  data.frame(
    seconds = rep(c(duration*60, duration*60*break_size), repetition), 
    watt = rep(c(watt_hit, watt_break), repetition)
  )
}

ie_intervalls <- function(sec = 30, rep_intensity = 13, rep_sets = 3, break_set_duration = NULL, break_intensity_ratio = 0.5, watt_hit = 340, watt_break = 130){
  
  if(is.null(break_set_duration)) break_set_duration <- round(rep_intensity*0.5*0.5)*60
    
  set <- rbind(
    data.frame(
      seconds = rep(c(sec, sec*break_intensity_ratio), rep_intensity), 
      watt = rep(c(watt_hit, watt_break), rep_intensity)
    ),
    data.frame(
      seconds = break_set_duration,
      watt = watt_break
    )
  )
  
  do.call(rbind,
    replicate(rep_sets, set, simplify = FALSE)
  )
}
ie_intervalls()


create_workout <- function(type = "eb", ...){

  inputs <- as.list(substitute(list(...)))
  print(inputs)
  
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
  
  watt_table

}


