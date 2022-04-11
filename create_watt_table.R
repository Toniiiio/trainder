

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

constant_intervalls <- function(duration = 4, repetition = 4, break_size = 0.5, watt_hit = 300, watt_break = 130){
  data.frame(
    seconds = rep(c(duration*60, duration*60*break_size), repetition), 
    watt = rep(c(watt_hit, watt_break), repetition)
  )
}

ie_intervalls <- function(sec = 30, duration = 13, repetition = 3, break_size = 0.5, watt_hit = 340, watt_break = 130){
  do.call(rbind,
    replicate(3,
        data.frame(
          seconds = rep(c(30, sec*break_size), duration), 
          watt = rep(c(watt_hit, watt_break), duration)
        ), 
    simplify = FALSE)
  )
}
ie_intervalls()


create_workout <- function(){

  watt_table <- rbind(
    warmup(),
    watt_stairs(),
    ie_intervalls(),
    cool_down()    
  )
  
  watt_table

}

