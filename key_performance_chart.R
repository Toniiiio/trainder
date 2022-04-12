# Definition:
# gestrige CTL  + ( heutiger TSS – gestrige CTL) / 42 = heutige CTL

# Start CTL:
# https://help.trainingpeaks.com/hc/en-us/articles/230903988-Estimate-Starting-Fitness-CTL-
# 10 hours per week as cyclist: 70

# Vorlage: https://scientifictriathlon.com/data-driven-triathlon-training/

file_name <- "biketrainr-master/data/27_03_2022.fit"
records <- load_strava(file_name)


watts <- records$power
start <- seq(1, length(watts), by = 30)
end <- seq(30, length(watts), by = 30)
if(max(start) > max(end)) end <- c(end, length(watts))
start
end
avg <- rep(NA, length(start))
for(nr in seq(start)){
  avg[nr] <- mean(watts[start[nr] : end[nr]])
}
avg

watts <- 1:10

watts <- records$power

calc_NP <- function(watts){
  (mean(caTools::runmean(watts, 30)^4))^0.25 
}


str <- parse_strava(file_name = file_name)

str$records
mean(records$power)

NP <- calc_NP(watts = records$power)
secs <- length(records$power)
TSS <- calc_tss(FTP = 270, NP = NP, sec = secs)

calc_tss <- function(FTP = 270, NP = 184, sec){
  # TSS = (sec * NP * IF)/(FTP * 3600) * 100
  # IF = NP / FTP
  TSS = sec*NP^2 / (FTP^2 * 3600)*100
  TSS
}

TSS <- rep(100, 100)
names(TSS) <- seq.Date(Sys.Date() - 1, Sys.Date() + 98, "days")

TSS <- rep(90, 100)
names(TSS) <- seq.Date(Sys.Date() - 1, Sys.Date() + 98, "days")

CTL <- rep(70, 100)
names(CTL) <- seq.Date(Sys.Date() - 1, Sys.Date() + 98, "days")

ATL <- rep(50, 100)
names(ATL) <- seq.Date(Sys.Date() - 1, Sys.Date() + 98, "days")


idx_t <- which(Sys.Date() == names(CTL))

CTL_t1 <- CTL[idx_t - 1]
TSS_t <- TSS[idx_t]

CTL_t = CTL_t1 + (TSS_t - CTL_t1)/42
CTL_t

ATL_t1 <- ATL[idx_t - 1]
ATL_t = ATL_t1 + (TSS_t - ATL_t1)/7
ATL_t


#gestrige CTL  – gestrige ATL  –  = heutige TSB

TSB = CTL - ATL
TSB

d <- data.frame(CTL, ATL, TSB, TSS)


library(dygraphs)
dygraph(as.xts(d), main = "Performance Chart") %>%
  dySeries("TSS", drawPoints = TRUE,  color = "red", strokeWidth = 0) %>%
  dySeries("ATL", color = "purple") %>%
  dySeries("CTL", fillGraph = TRUE,  color = "blue") %>% 
  dySeries("TSB", stepPlot = TRUE, color = "yellow") #%>% 
  #dyOptions(fillGraph = TRUE, fillAlpha = 0.4)


dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dySeries("mdeaths", drawPoints = TRUE, pointShape = "square", color = "blue") %>%
  dySeries("fdeaths", stepPlot = TRUE, fillGraph = TRUE, color = "red")
