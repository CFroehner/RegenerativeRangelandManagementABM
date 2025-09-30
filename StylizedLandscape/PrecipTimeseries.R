PrecipTimeseries<-function(steps=TimeSteps,scn_mean = 0, StandardDev=scn_sd){
  
  Change<-rnorm(n=steps,mean=scn_mean,sd=StandardDev)
  Rain<-terra::rast(rstack[["T0Rain"]]) #vectorize plots
  sim_rasters<-list()
  sim_rasters[[1]]<-Rain #Include t0 rain
  
  for(i in 1:steps){
    
    r_sim <- Rain
    r_sim[]<-Rain[]+Change[i]*Rain[]
    r_sim[] <- pmax(r_sim[], 0) # make sure rain is not < 0 across time-series
    sim_rasters[[(i+1)]] <- r_sim
  }
  
  PrecipStack<-terra::rast(sim_rasters)
  PrecipStack<-c(PrecipStack,terra::rast(rstack[["CommID"]]),terra::rast(rstack[["PlotID"]]))
  PrecipStack<-terra::as.polygons(PrecipStack,dissolve=FALSE)
  PrecipStack<-sf::st_as_sf(PrecipStack)
  
  names(PrecipStack)[1:(steps+1)]<-0:steps
  PrecipStack <- PrecipStack %>% tidyr::pivot_longer(cols = 0:(steps + 1),
                                                     names_to = "Time",
                                                     values_to = "Precip") %>%
    mutate(Time = as.integer(Time))
  
}
