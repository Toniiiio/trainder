nrg <- biketrainr::create_nrg()

watts <- create_workout()
nrgs <- plyr::join(watts, nrg, by = "watt")
nrgs$hr <- NULL
nrgs$carbs <- nrgs$carbs*nrgs$seconds/3600
nrgs$fat <- nrgs$fat*nrgs$seconds/3600

sum(nrgs$carbs)
sum(nrgs$fat)
