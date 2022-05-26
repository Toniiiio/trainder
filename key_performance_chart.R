# Definition:
# gestrige CTL  + ( heutiger TSS – gestrige CTL) / 42 = heutige CTL

# Start CTL:
# https://help.trainingpeaks.com/hc/en-us/articles/230903988-Estimate-Starting-Fitness-CTL-
# 10 hours per week as cyclist: 70

# Vorlage: https://scientifictriathlon.com/data-driven-triathlon-training/


#### Parameter:
np_ma_amt_days = 30 # amount days for moving average calculation of normalised power
ctl_avg_amt_days = 42 # amount days for average calculation of ctl
atl_avg_amt_days = 7 # amount days for average calculation of ctl


file_name <- "biketrainr-master/data/27_03_2022.fit"
records <- load_strava(file_name)

calc_NP <- function(watts, np_ma_amt_days = 30){
  (mean(caTools::runmean(watts, np_ma_amt_days)^4))^0.25 
}



# str <- parse_strava(file_name = file_name)
# 
# str$records
# mean(records$power)


calc_tss <- function(FTP = 270, NP = 184, sec){
  # TSS = (sec * NP * IF)/(FTP * 3600) * 100
  # IF = NP / FTP
  TSS = sec*NP^2 / (FTP^2 * 3600)*100
  TSS
}

NP <- calc_NP(watts = records$power)
secs <- length(records$power)
TSS <- calc_tss(FTP = 270, NP = NP, sec = secs)


start_date <- as.Date("2022-03-27") - 1
end_date <- Sys.Date() + 1
dates <- seq.Date(start_date, end_date, "days")

n <- length(dates)
TSS <- rep(0, n)
TSS1 <- round(sample(20:200, n)/2 + 1:n)
training_days = sample(1:n, round(n/2), replace = FALSE)
TSS[training_days] <- TSS1[training_days]
names(TSS) <- dates
TSS[length(TSS)] <- NA

# TSS <- rep(90, 100)
# names(TSS) <- seq.Date(Sys.Date() - 1, Sys.Date() + 98, "days")

CTL <- rep(70, n)
names(CTL) <- dates
for(nr in 2:n){
  CTL[nr] <- CTL[nr - 1] + (TSS[nr] - CTL[nr - 1])/ctl_avg_amt_days 
}
CTL

ATL <- rep(50, n)
names(ATL) <- dates
for(nr in 2:n){
  ATL[nr] <- ATL[nr - 1] + (TSS[nr] - ATL[nr - 1])/atl_avg_amt_days 
}
ATL

# TSB_t = CTL_t-1 - ATL_t-1
# TSB from first day is unknown since it would require data from a day before.
# TSB from tomorrow (future) is known since the CTL, ATL, TSS of today is known.
# If i have TSS of 01.01.2020, i would need CTL and ATL of day before
#    CTL[nr] <- CTL[nr - 1] + (TSS[nr] - CTL[nr - 1]), but they are unknown
#    I can calculate CTL, ATL anyway, since i would take CTL and  ATL starting values. 
# --> CTL, ATL time series have to start one day prior to first TSS value. 
# --> TSB start on day of first TSS value and end one day after last TSS value
# ---> If TSS values are (uncompletely) available from 01.01.2020 to 10.01.2020. 
#      time series should go from 31.12.2019 to 11.01.2021. Missing values mean no
#      training so TSS of 0.

# TSB does not exist for first day or is 0. But is not NA for last day as for other time
# series. Therefore, set first value to zero or NA, remove last value and give date range of 
# other time series over newly created time series.
TSB_calc = CTL - ATL
TSB <- c(0, TSB_calc[-length(TSB_calc)])
names(TSB) <- dates
TSB

d <- data.frame(CTL, ATL, TSB, TSS)

dont_run <- function(){
  library(xts)
  library(dygraphs)
  dygraph(as.xts(d), main = "Performance Chart") %>%
    dySeries("TSS", drawPoints = TRUE,  color = "red", strokeWidth = 0) %>%
    dySeries("ATL", color = "purple") %>%
    dySeries("CTL", fillGraph = TRUE,  color = "blue") %>%
    dySeries("TSB", color = "yellow") #%>%
  #dyOptions(fillGraph = TRUE, fillAlpha = 0.4)
}
# dont_run()

