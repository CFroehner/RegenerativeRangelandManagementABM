library(data.table)
library(doParallel)
library(doRNG)
library(sp)

# Create parallel cluster
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# Create output folders if it does not exist
if (!dir.exists("Plots_Tables")) {
  dir.create("Plots_Tables")
}

if (!dir.exists("Ts_Data")) {
  dir.create("Ts_Data")
}

# Consistent settings
# Stylized landscape control parameters
N_Communities<-100 #Must be perfect square number and a multiple of the number of plots for the code below to work. 
N_Plots<-4 #Must be perfect square number for the code below to work. 
Starting_Prop_Conservation<-.25
StartingGrassMin<-0.2#lower bound of grass/rain. i.e., the worst areas have 20% of the best (before grazing)
Spatial_Autocorrelation_Rain<-0.98 # this affects the spatial autocorelation of grass growth (0 is fully random, 1 is completely determined by space)
Animal_cost<-5 #Price of animals. Used for buying/selling and also financial gains from having animals
GrassPerCow<-0.15 # will need to play with this
Min_cows<-2 #min cows per person
Cow_Inequality<-1 #exponential rate for within-community cattle distribution. higher numbers = more EQUAL
Econ_Inequality<-0.1 # Scalar, lower number = less within-community inequality (centered at community mean)

Supplemental_fodder<-TRUE #does Cons grazing give supp food
Set_Adoption<-TRUE #Is adoption of Conservation set at the starting number
Forecasts<- "No_one" #Can take values of "No_one", "Everyone", "Rich","Poor", "Conservation"
ForecastError<-0 #How wrong are forecasts.#basically ranges 0:100
Fodder_Price_Per_Cow<-1 #play with this. Set high to essentially eliminate supplemental food buying from model
CapitalGainRate<-0.02 #Financial gains from money in the bank
AnimalGainRate<- 0.02 # Financial gains from having animals (multiplied by the value of the animals and paid in cash)
AnimalObservedValueRate<-0.1 #Multiplied by the economic value of animals and added to the observed economic wellbeing for social learning
MaxGeneralSale<-0.25 #Maximum proportion of animals people can sell when NOT in conservation
MaxConservationSale<-0.50 #Maximum proportion of animals people in conservation can sell
AnimalBaseline<-2 #people never sell below this number
TimeSteps<-100

# Define experimental conditions
# scenarios with individual sd of change
scenarios <- c("bad","ok",
               # "baseline",
               "good")
scn_sd <- setNames(c(1, 0.4, 
                     # 0.3,
                     0.3), scenarios)
# source("PredictedRainfall.R")
# scn_sd <- setNames(
#   round(extreme_pixels$sd_change_percent, 1),
#   extreme_pixels$Scenario
# )

conditions <- readRDS("settings_1.rds") # Matt
# settings_2 <- readRDS("settings_2.rds") # Cosima

# Number of repetitions
n_reps <- 50

# Loop over each condition
for (cond_id in 1:nrow(conditions)) {
  
# Set params for this condition
  Forecasts <- conditions$Forecasts[cond_id]
  Set_Adoption <- conditions$Set_Adoption[cond_id]
  Supplemental_fodder <- conditions$Supplemental_fodder[cond_id]
  Starting_Prop_Conservation <- conditions$Starting_Prop_Conservation[cond_id]
  ForecastError<-conditions$ForecastError[cond_id]
  
  message("Running condition ", cond_id, " of ", nrow(conditions),
          ": Forecasts=", Forecasts,
          ": Set_Adoption=", Set_Adoption,
          ", Supplemental_fodder=", Supplemental_fodder,
          ", Starting_Prop_Conservation=", Starting_Prop_Conservation
          ,", ForecastError=", ForecastError
          )
  
# Create an experiment label for output filenames
exp_setting <- paste0(
  "FOREC_", Forecasts,
                      "_ADOP_", Set_Adoption,
                      "_FODD_", Supplemental_fodder,
                      "_PROPCONS_", Starting_Prop_Conservation,
                      "_FORECERR_", ForecastError
                      )

AllSimResults <- vector("list", n_reps)

for (rep in 1:n_reps) {
  cat("  Rep:", rep, "\n")
  set.seed(123 + rep)
  # ResultsList <- list()
  
  # Build T0 for this rep
  source("MakeStylizedLandscape.R")
  rstack0   <- rstack
  Ranchers0 <- Ranchers
  
  # Run scenarios in parallel
  ResultsList <- foreach(
    target_scenario = scenarios,
    .packages = c("data.table","dplyr","raster","ineq"),
    .combine  = 'list', .multicombine = TRUE, .init = list(), .errorhandling = "stop"
      ) %dorng% {
    
  # Set precipitation timeseries based on Stylized Landscape of this rep and 
  # scenario specific change
  source("PrecipTimeseries.R", local = TRUE)
    
    # reset state for this scenario
    rstack   <- rstack0
    Ranchers <- Ranchers0
    
    # Make change in rainfall
    # source("PrecipTimeseries.R")
    PrecipStack <- PrecipTimeseries(
      StandardDev = scn_sd[target_scenario]
    )
    
    SummaryDat<-data.frame(Time=0,AvgMoney=mean(Ranchers$Money),
                           Gini = ineq::ineq(Ranchers$TotalEconomicWellbeing,type="Gini"),
                           AvgCows = mean(Ranchers$stock_count),
                           TotalCows = sum(Ranchers$stock_count),
                           AvgGrass = mean(rstack[["T0Grass"]][]),
                           TotalGrass = sum(rstack[["T0Grass"]][]),
                           PropCons = length(which(rstack[["Cons"]][]==T))/
                             (N_Communities*N_Plots),
                           Scenario = target_scenario)
    
    
    #Get rid of some t0 variables
    rstack$Grass<-rstack$T0Grass
    T0GrassIndex<-which(names(rstack) == "T0Grass")
    rstack <- rstack[[ -T0GrassIndex ]]
    
    T0RainIndex<-which(names(rstack) == "T0Rain")
    rstack <- rstack[[ -T0RainIndex ]]
    
    DeprivIndex<-which(names(rstack) == "Depriv")
    rstack <- rstack[[ -DeprivIndex ]]
    
    PopIndex<-which(names(rstack) == "Pop")
    rstack <- rstack[[ -PopIndex ]]
    
    
    
    for(ts in 1:TimeSteps){
      
      cat("TimeSteps:", ts, "\n")
      
      
      RanchersPast<-Ranchers
      rstackPast<-rstack
      
      
      #####Model scheduling ##########################
      #####Module 1 - Grazing#####
      #vectorized version of code used to graze in the starting landscape
      df<-raster::as.data.frame(rstackPast,xy=T)# Easier as a dataframe
      
      ########
      # Step 1: Summarize grazed plots by community
      df2 <- df %>% 
        filter(Grazed == 1) %>% # for just the grazed plots
        group_by(CommID) %>%
        summarize(
          Grass = sum(Grass), #calculate the amount of grass and total plots/community
          n = n(),
          .groups = 'drop'
        ) 
      
      CommAnimals<-df%>%dplyr::group_by(CommID)%>% #group by community
        dplyr::summarize(Animal=sum(Animal)) #Total animals (in both grazed and ungrazed plots)
      
      df2<-merge(df2,CommAnimals,by="CommID")%>% #Add these to one df
        mutate(
          Grass2 = ceiling(Grass - (GrassPerCow * Animal)), #animals eat evenly across grazed plots
          DeficitGrass = ifelse(Grass2 < 0, Grass2, 0), #save the deficit
          Grass = ifelse(Grass2 <= 0, 1, Grass2) #don't let the final number go all the way to 0 (for multiplicative growth)
        ) %>%
        dplyr::select(-Grass2)
      
      # Step 2: Join the group-level summary back to the main df
      df<- df %>%
        left_join(df2, by = "CommID", suffix = c("", ".comm")) %>%
        mutate(
          Grass = case_when(
            Grazed == 1 ~ ceiling(Grass.comm / n), #for the grazed plots, distribute the grass/community
            TRUE ~ Grass
          )
        ) %>%
        dplyr::select(-Grass.comm, -Animal.comm, -n, -DeficitGrass) #remove cols no longer using
      ############
      rGrass <- rasterFromXYZ(df[order(df$y, df$x), c("x", "y", "Grass")])
      
      #Remove old grass
      GrassIndex<-which(names(rstack) == "Grass")
      rstack <- rstack[[ -GrassIndex ]]
      
      #Add new grass
      rstack<-stack(rstack,rGrass)
      
      
      ######Module 2 - Supplemental fodder#####
      #we identify which communities did not have enough grass to feed all animals. 
      #Ranchers buy supplemental fodder to feed animals as necessary and as they are able (given economic wellbeing). 
      
      #If no one is in deficit. They all get 0
      if(length(which(df2$DeficitGrass<0))==0){
        Ranchers$StockDeficit<-0
        
      }
      
      
      #If there are deficits, do the following:
      if(length(which(df2$DeficitGrass<0))>0){ #This continues to wrap the next module
        
        #substitute to be just communities with a deficit
        df2<-df2%>%filter(DeficitGrass<0)
        
        RanchersDef<-Ranchers%>%filter(CommID %in% df2$CommID) #only the ranchers from communities with deficit
        RanchersNotDef<-Ranchers%>%filter(!CommID %in% df2$CommID) #Ranchers that were not deficient 
        
        df2$AnimalsDeficit<-df2$DeficitGrass/GrassPerCow #How many animals short was the community grass
        df2$ProportionDeficit<-abs(df2$AnimalsDeficit)/df2$Animal #what proportion of animals were they unable to feed
        
        #This handles NAs produced when there are no animals in a community
        df2$ProportionDeficit<-ifelse(is.na(df2$ProportionDeficit)==T,0,df2$ProportionDeficit) 
        
        RanchersDef<-RanchersDef%>%
          left_join(df2[,c("CommID","ProportionDeficit")], by = "CommID") %>% #get how many animals need food per person
          mutate(StockDeficit  = ceiling(stock_count  * ProportionDeficit))%>% #ceiling since partially feeding an animal counts as not feeding it!
          dplyr::select(-ProportionDeficit)
        
        #In conditions where no supplemental fodder is available from the conservation program, animals die. 
        #In conditions where supplemental fodder is given by the conservation grazing program, it is distributed as necessary.
        if(Supplemental_fodder ==TRUE){ #give free food to people in conservation. Limit to 10 or fewer.
          
          RanchersDef$StockDeficit<-ifelse(RanchersDef$Conservation==TRUE & RanchersDef$stock_count <=10 ,0,RanchersDef$StockDeficit)
          
        }
        
        #############
        #Now reduce the stock deficit and the money of each person as they buy fodder
        # Calculate how many cows each person can afford fodder for
        RanchersDef$CowsAffordable <- floor(RanchersDef$Money / Fodder_Price_Per_Cow) 
        
        # Determine how many cows they can actually feed (can't exceed StockDeficit)
        RanchersDef$CowsToFeed <- pmin(RanchersDef$CowsAffordable, RanchersDef$StockDeficit)
        
        # Calculate the actual money spent
        RanchersDef$MoneySpent <- RanchersDef$CowsToFeed * Fodder_Price_Per_Cow
        
        # Update Money and StockDeficit accordingly
        RanchersDef$Money <- RanchersDef$Money - RanchersDef$MoneySpent
        RanchersDef$StockDeficit <- RanchersDef$StockDeficit - RanchersDef$CowsToFeed
        
        #remove intermediate columns
        RanchersDef<-RanchersDef%>%dplyr::select(-c(CowsAffordable,CowsToFeed,MoneySpent))
        
        ######Module 3 - Animals die#####
        
        
        RanchersDef$stock_count<-RanchersDef$stock_count-RanchersDef$StockDeficit #reduce stock by deficit number. 
        RanchersDef$stock_count[RanchersDef$stock_count<0] <- 0 #No negative numbers
        
        #Merge the deficit and not deficit ranchers
        RanchersNotDef$StockDeficit<-0
        Ranchers<-rbind(RanchersDef,RanchersNotDef)%>%arrange(CommID,person_id)
        
        #change number of animals in the raster
        ranimals<-Ranchers%>%group_by(CommID)%>%summarise(Animal=sum(stock_count)/N_Plots) #summarise as table, divide by N_Plots as this is the total number per community
        df<-df%>%dplyr::select(-Animal)#remove old number from raster df
        ranimals<-merge(df,ranimals,by="CommID") #add new animal column
        ranimals <- rasterFromXYZ(ranimals[order(ranimals$y, ranimals$x), c("x", "y", "Animal")])#make a raster
        
        AnIndex<-which(names(rstack) == "Animal") #remove old raster layer
        rstack <- rstack[[ -AnIndex ]] 
        rstack<-stack(rstack,ranimals)#add new animal raster layer
        
      }
      
      #####Module 4 #PEOPLE GET MONEY FROM COWS AND MONEY ####
      
      #We want to increase people's money by some percentage, so say 1.25* their current money.
      #We also want to give them money for each animal beyond what that animal is worth (as animals have more value than just sale value)
      
      #Make money from money
      Ranchers$Money<-Ranchers$Money*CapitalGainRate
      
      #money from animals
      Ranchers$Money<-Ranchers$Money+(Ranchers$stock_count*Animal_cost*AnimalGainRate)
      
      #Recalculate total economic wellbeing
      Ranchers<-Ranchers%>%mutate(TotalEconomicWellbeing=Money+(stock_count *Animal_cost))
      
      
      
      df2<-Ranchers%>%group_by(CommID)%>%summarise(Mean_economic_wellbeing=mean(TotalEconomicWellbeing),
                                                   Conservation=as.logical(min(Conservation)))
      
      #Update raster with mean economic wellbeing by community
      df<-df%>%dplyr::select(-Mean_economic_wellbeing)
      rWell<-merge(df,df2,by="CommID")
      rWell <- rasterFromXYZ(rWell[order(rWell$y, rWell$x), c("x", "y", "Mean_economic_wellbeing")])
      
      WellIndex<-which(names(rstack) == "Mean_economic_wellbeing")
      rstack <- rstack[[ -WellIndex ]]
      rstack<-stack(rstack,rWell)
      
      
      #####Module 5 - Conservation adoption#####
      #Communities then observe all surrounding communities (queen’s case) and use success biased imitation 
      ##This is handled in the make stylized landscape script, where we make the 'adj_list' object
      #to disproportionately copy the communities with the highest average rancher economic wellbeing. 
      
      
      
      #Only do this if conservation proportion is not fixed
      if(Set_Adoption==FALSE){
        
        #remake df2 with stock count  
        df2<-Ranchers%>%group_by(CommID)%>%summarise(Mean_economic_wellbeing=mean(TotalEconomicWellbeing),
                                                     stock_count =mean(stock_count ),
                                                     Conservation=as.logical(min(Conservation)))
        
        # Convert df to data.table for speed
        df2 <- as.data.table(df2)
        
        # Initialize vector to store new behaviors
        new_behavior <- character(nrow(df2))
        
        # Vectorized helper: pre-extract payoff and behavior
        behaviors <- df2$Conservation
        
        #Make payoffs for cows more observable
        payoffs <- df2$Mean_economic_wellbeing+(df2$stock_count*Animal_cost*
                                                  AnimalObservedValueRate)
        
        
        
        # Loop over each community (can be parallelized if needed)
        for (i in seq_len(nrow(df2))) {
          neighbors <- adj_list[[i]]
          
          
          neighbor_behaviors <- behaviors[neighbors]
          neighbor_payoffs   <- payoffs[neighbors]
          
          # Mean payoffs by behavior
          payoff_A <- mean(neighbor_payoffs[neighbor_behaviors == TRUE], na.rm = TRUE)
          payoff_B <- mean(neighbor_payoffs[neighbor_behaviors == FALSE], na.rm = TRUE)
          
          # Handle case where all neighbors are A or all B
          if (is.nan(payoff_A)) payoff_A <- 0
          if (is.nan(payoff_B)) payoff_B <- 0
          
          # Payoff-biased probability of adopting A
          #pA <- exp(.1 * payoff_A) / (exp(.1 * payoff_A) + exp(.1 * payoff_B))
          pA <- 1 / (1 + exp(-0.1 * (payoff_A - payoff_B)))
          
          # Draw new behavior
          new_behavior[i] <- sample(c(TRUE, FALSE), size = 1, prob = c(pA, 1 - pA))
        }
        
        # Update the dataframe
        df2[, Conservation := new_behavior]
        
        df2<-as.data.frame(df2)
        
        #####Need to assign conservation in the raster
        df2$Conservation<-as.numeric(as.logical(df2$Conservation)) #make into 1s and 0s
        
        df<-df%>%dplyr::select(-Cons)
        rCons<-merge(df,df2,by="CommID")
        rCons <- rasterFromXYZ(rCons[order(rCons$y, rCons$x), c("x", "y", "Conservation")])
        names(rCons)<-"Cons" #to match above
        
        ConsIndex<-which(names(rstack) == "Cons")
        rstack <- rstack[[ -ConsIndex ]]
        rstack<-stack(rstack,rCons)
        
      }
      
      ######Module 5b - Reassign conservation plots#####
      #For communities that have adopted the conservation grazing program, the plot with the lowest 
      #grass cover is allocated for protection. 
      
      df<-raster::as.data.frame(rstack,xy=T)
      dfCons<-df%>%filter(Cons==1)#for just the conservation communities
      
      if(nrow(dfCons)>0){ #if there are still conservation communities
        dfCons<-dfCons%>%group_by(CommID)%>%
          mutate(
            min_grass = min(Grass, na.rm = TRUE),
            is_min = Grass  == min_grass
          ) %>%
          group_modify(~ {
            # Randomly select one of the tied plots
            chosen <- sample(which(.x$is_min), 1)
            .x$Grazed <- 1
            .x$Grazed[chosen] <- 0
            .x
          }) %>%
          ungroup() %>%
          dplyr::select(-min_grass, -is_min)
        
        dfCons <- dfCons %>%
          relocate(CommID , .after = y)
      }
      
      dfNotCons<-df%>%filter(Cons==0)#for just the conservation communities
      
      if(nrow(dfNotCons)>0){ #only if there are some not cons communities
        dfNotCons$Grazed<-1 #All grazed in non-conservation communities
      }
      df<-rbind(dfCons,dfNotCons)
      
      rGrazed  <- rasterFromXYZ(df[order(df$y, df$x), c("x", "y", "Grazed")]) #make this into a raster
      
      GrazedIndex<-which(names(rstack) == "Grazed") #get the index
      rstack <- rstack[[ -GrazedIndex ]] #remove the old grazing plan
      rstack<-stack(rstack,rGrazed)
      #######
      
      
      
      
      
      
      ######Module 6 - buying / selling animals#####
      #In anticipation of the next season rain, ranchers decide whether to buy or sell animals and how many. 
      #In baseline conditions, ranchers randomly select five other ranchers and disproportionately adopt 
      #the buying/selling strategy of the most successful (i.e., success biased behavioral transmission).
      
      # Payoff-biased imitation function
      PayoffUpdate<-function(dff){
        
        #Make the observed economic wellbeing where cows are more observable.
        dff$ObservedEconomicWellbeing<-dff$TotalEconomicWellbeing+(dff$stock_count*Animal_cost*AnimalObservedValueRate)
        
        dt <- as.data.table(dff) #Make into a data table object, much faster than df
        
        # Preallocate new column
        dt[, new_strategy := PastStockChangeProp]  # initialize with default
        
        # Process each community
        dt[, new_strategy := {
          n <- .N
          result <- PastStockChangeProp  # initialize
          #get their own wellbeing and past strategy
          for (i in seq_len(n)) {
            self_id <- person_id[i]
            self_val <- ObservedEconomicWellbeing[i]
            self_strategy <- PastStockChangeProp[i]
            
            # Exclude self from peer pool
            peer_idx <- setdiff(seq_len(n), i)
            sampled_idx <- sample(peer_idx, size = min(10, length(peer_idx)), replace = FALSE) #sample up to 10 people
            
            peer_payoffs <- ObservedEconomicWellbeing[sampled_idx]
            peer_strategies <- PastStockChangeProp[sampled_idx]
            
            ##MATT DOUBLE CHECK HERE TOO
            if (sum(peer_payoffs) == 0) {
              probs <- rep(1 / length(sampled_idx), length(sampled_idx))
            } else {
              probs <- peer_payoffs / sum(peer_payoffs)
            }
            
            chosen_peer <- sampled_idx[sample(seq_along(sampled_idx), size = 1, prob = probs)]
            
            if (ObservedEconomicWellbeing[chosen_peer] > self_val) {
              result[i] <- PastStockChangeProp[chosen_peer]
            }
            # else keep original strategy
          }
          
          result
        }, by = CommID]
        return(as.data.frame(dt)%>%dplyr::select(-ObservedEconomicWellbeing))
      }
      
      #### Now, apply to only the groups not using forecasts
      if(Forecasts=="No_one"){Ranchers<-PayoffUpdate(Ranchers) } #everyone learns socially
      
      if(Forecasts=="Everyone"){Ranchers<-Ranchers%>%mutate(new_strategy=NA) } #No one learns socially
      
      if(Forecasts=="Conservation"){
        Forecasters<-Ranchers%>%dplyr::filter(Conservation==TRUE)%>%mutate(new_strategy=NA) #Only non-conservation communities learn socially
        Learners<-Ranchers%>%dplyr::filter(Conservation==FALSE)
        Learners<-PayoffUpdate(Learners)
        Ranchers<-rbind(Forecasters,Learners)
      }
      
      if(Forecasts=="Rich"){
        Rich<-Ranchers%>%group_by(CommID)%>%summarise(wellbeing=mean(TotalEconomicWellbeing ))%>%
          mutate(Rich=wellbeing>=quantile(wellbeing,0.75))%>%dplyr::select(-wellbeing)
        Ranchers<-base::merge(Ranchers,Rich,by="CommID")
        
        Rich<-Ranchers%>%dplyr::filter(Rich==TRUE)%>%mutate(new_strategy=NA) #Rich don't learn socially
        Poor<-Ranchers%>%dplyr::filter(Rich==FALSE) 
        Poor<-PayoffUpdate(Poor) #everyone else learns socially
        Ranchers<-rbind(Rich,Poor)
        Ranchers<-Ranchers%>%dplyr::select(-Rich) #remove this column, we don't need it anymore
      }  
      
      if(Forecasts=="Poor"){
        Poor<-Ranchers%>%group_by(CommID)%>%summarise(wellbeing=mean(TotalEconomicWellbeing ))%>%
          mutate(Poor=wellbeing<=quantile(wellbeing,0.25))%>%dplyr::select(-wellbeing)
        Ranchers<-base::merge(Ranchers,Poor,by="CommID")
        
        Poor<-Ranchers%>%dplyr::filter(Poor==TRUE)%>%mutate(new_strategy=NA) #poor don't learn socially
        Rich<-Ranchers%>%dplyr::filter(Poor==FALSE) 
        Rich<-PayoffUpdate(Rich) #everyone else learns socially
        Ranchers<-rbind(Rich,Poor)
        Ranchers<-Ranchers%>%dplyr::select(-Poor)#remove this column, we don't need it anymore
      } 
      
      
      # Now need to assign new strategy to people/groups with forecasts
      
      df<-raster::as.data.frame(rstack,xy=T)
      
      NextGrass<-PrecipStack%>%filter(Time==ts)%>%as.data.frame(.)%>%dplyr::select(-geometry,Time)
      
      df<-base::merge(df,NextGrass,by=c("CommID","PlotID"))
      
      
      # ### MODULE 7 SET/PREDICT GRASS GROWTH
      
      # new fct with linear effect of rain quality (for grass growth) on grass growth
      #played with these parameters. r_max controls response to rain. 
      #multiplier on current grass controls response to prexisting grass
      # max_rain_quality <- max(PrecipStack$Precip)
      grass_growth <- function(current_grass, 
                               rain_quality, 
                               r_max = 300,
                               # maxPrecip,
                               K = 100) {
        
        # Logistic growth equation modified by rainfall
        # growth <- rain_effect * current_grass * 0.01 * (1 - current_grass / K)
        growth <- (r_max * (rain_quality / 100)) * current_grass * 0.01 * (1 - current_grass / K)
        # Update
        new_grass <- current_grass + growth
        # Ensure grass does not exceed carrying capacity and is positive
        # new_grass <- pmin(new_grass, K)
        new_grass <- pmax(new_grass, 0)
        new_grass
      }
      
      # Update grass level. Note that this is NOT additive! but next grass is highly dependent on past 
      df$NextGrass<- grass_growth(current_grass = df$Grass, rain_quality = df$Precip)
      
      #ggplot(df,aes(x=Precip,y=NextGrass,color=Grass))+geom_point(size=4,alpha=0.5)
      #ggplot(df,aes(x=Grass,y=NextGrass,color=Precip))+geom_point(size=4,alpha=0.5)
      NextGrass<-df #Make an object so we can remake df without losing it
      
      #make the error in the forecast
      ForecastedGrass=rnorm(n=nrow(df), mean= df$NextGrass, sd= ForecastError)
      df$ForecastedGrass<-ForecastedGrass
      rm(ForecastedGrass)
      
      
      df2<-df%>%group_by(CommID)%>%
        summarise(
          Animals = sum(Animal, na.rm = TRUE),  # Sum all animals in the group
          ForecastedGrass = sum(ForecastedGrass[Grazed == 1], na.rm = TRUE))%>%   # Sum grass only where Grazed
        # dplyr::mutate(Animals = ifelse(Animals == 0, 1, Animals)) %>%
        mutate(ForcastedAnimalCapacity=ForecastedGrass/GrassPerCow)%>% #how many animals can this sustain
        #what would be the ideal change. Multiply by 0.85 to keep from aiming to go all the way to 0
        mutate(ForecastedOptimalStrategy=(ForcastedAnimalCapacity*0.85 -Animals) / Animals)%>% 
        dplyr::select(CommID,ForecastedOptimalStrategy)
      
      df2$ForecastedOptimalStrategy <- ifelse(is.na(df2$ForecastedOptimalStrategy), 1, df2$ForecastedOptimalStrategy)
      
      Forecasters<-Ranchers%>%filter(is.na(new_strategy)==TRUE) #just people using forecasts
      Learners<-Ranchers%>%filter(is.na(new_strategy)==FALSE) #just people not using forecasts
      Forecasters<-base::merge(Forecasters,df2,by="CommID") #apply to whole community
      Forecasters$new_strategy<-Forecasters$ForecastedOptimalStrategy #match column name of learners
      Forecasters<-Forecasters%>%dplyr::select(-ForecastedOptimalStrategy) #get rid of extra column
      
      Ranchers<-rbind(Forecasters,Learners)
      
      #Now need to actually buy/sell
      
      #There are two constraints 1) opportunity to sell and 2) money to buy
      
      #Buying animals is dependent on rancher economic wellbeing (do they have enough money).
      #selling cows is easier if you are involved in conservation 
      #people keep a baseline number of cows
      
      # Copy original animals for reference
      Ranchers$animals_before <- Ranchers$stock_count
      
      # Apply buying/selling logic
      Ranchers<- within(Ranchers, {
        
        # Proposed change: positive (buy), negative (sell)
        
        proposed_change <- round(new_strategy  * stock_count)  # round to ensure whole animals
         
        #for people that have 0, but want to buy, make 1.
        #could use a different number than 1 dependent on their money
        proposed_change[stock_count == 0 & new_strategy >= 0.5] <- 1
        
        # --- Buying Logic ---
        buying <- proposed_change > 0
        max_affordable <- floor(Money / Animal_cost)  # maximum animals they can buy
        actual_change <- ifelse(buying, pmin(proposed_change, max_affordable), 0) #if buying, buy the smaller of what they want or what they can afford
        
        # --- Selling Logic ---
        selling <- proposed_change < 0
        # Max proportion of herd allowed to sell
        max_sell_prop <- ifelse(Conservation == 1, MaxConservationSale, MaxGeneralSale) #The max they can sell; COSIMA: short vector length, reason??
        max_sell <- floor(stock_count * max_sell_prop) #how many animals could they possibly sell; COSIMA: max_sell_prop gets recycled?!
        allowed_sell <- pmax(stock_count - AnimalBaseline, 0) #If they have two or fewer animals, they wont sell
        max_allowed_sell <- pmin(max_sell, allowed_sell) #What could they sell, smaller # of allowed and able to
        
        # Final selling change: negative integer, bounded
        sell_change <- pmax(proposed_change, -max_allowed_sell) #if they want to sell more than their max, don't allow
        
        # Combine both
        actual_change[selling] <- sell_change[selling]
        
        # Update animals and wealth
        stock_count <- stock_count + actual_change
        Money <- Money  - Animal_cost * actual_change  # Note: selling gives positive change to wealth
        
        actual_animals_change <- actual_change
      })
      
      # ANDREAS/COSIMA: SUGGESTION FOR SMOOTH TRANSITION FROM FEW TO MANY COWS
      # define weights to balance relative social buying strategy and available money
      # dependent on the difference between the median cows of my community and my stock count
      # rough idea: "if they have many, and I can afford many, but currently have 0, I will buy a bit more than max 1"
      
      # Copy original animals for reference
      # Ranchers$animals_before <- Ranchers$stock_count
      # 
      # # define few / many cows cut-off as median of cows in the rancher's community
      # Ranchers$comm_median_cows <-
      #   ave(Ranchers$stock_count, Ranchers$CommID,
      #                                  FUN = function(x) median(x, na.rm = TRUE))
      # 
      # Ranchers<- within(Ranchers, {
      # 
      #   # Proposed change based on new strategy: positive (buy), negative (sell)
      # 
      #   # --- Buying Logic ---
      #   buying <- round(new_strategy) > 0
      #   max_affordable <- floor(Money / Animal_cost) # maximum animals they can buy
      #   w <- 1 / (1 + exp(comm_median_cows/2 - stock_count)) # COSIMA: potentially set median on tuning param list
      #   proposed_buy <- round(w * Ranchers$new_strategy * stock_count + (1 - w) * max_affordable)
      #   actual_buy <- ifelse(buying, pmin(proposed_buy, max_affordable), 0) #if buying, buy the smaller of what they want or what they can afford
      # 
      #   # --- Selling Logic ---
      #   selling <- round(Forecasters$new_strategy) < 0
      #   proposed_sell <- round(new_strategy  * stock_count)
      #   max_sell_prop <- ifelse(Conservation == 1, MaxConservationSale, MaxGeneralSale) #The max they can sell, Max proportion of herd allowed to sell
      #   max_sell <- floor(stock_count * max_sell_prop) #how many animals could they possibly sell
      #   allowed_sell <- pmax(stock_count - AnimalBaseline, 0) #If they have two or fewer animals, they wont sell
      #   max_allowed_sell <- pmin(max_sell, allowed_sell) #What could they sell, smaller # of allowed and able to
      #   actual_sell <- ifelse(selling, pmax(proposed_sell, -max_allowed_sell), 0)
      #   # # Final selling change: negative integer, bounded
      #   # sell_change <- pmax(proposed_change, -max_allowed_sell) #if they want to sell more than their max, don't allow
      #   # actual_change[selling] <- sell_change[selling]
      # 
      #   # Combine buy and sell changes
      #   actual_change <- actual_buy + actual_sell
      # 
      #   # Update animals and wealth
      #   stock_count <- stock_count + actual_change
      #   Money <- Money  - Animal_cost * actual_change  # Note: selling gives positive change to wealth
      # 
      #   actual_animals_change <- actual_change
      # })
      # 
      # Ranchers$comm_median_cows <- NULL
      
      Ranchers$PastStockChangeAnimal<-Ranchers$actual_animals_change 
      Ranchers$PastStockChangeProp <-Ranchers$actual_animals_change /Ranchers$animals_before
      #If infinite (if they had 0 before and bought 1)
      Ranchers$PastStockChangeProp<-ifelse(is.finite(Ranchers$PastStockChangeProp),Ranchers$PastStockChangeProp,1.0)
      #If NA, make 0
      Ranchers$PastStockChangeProp<-ifelse(is.na(Ranchers$PastStockChangeProp),0,Ranchers$PastStockChangeProp)
      Ranchers$PastStock <-Ranchers$animals_before
      
      Ranchers <- Ranchers %>% dplyr::select(
        -c(
          StockDeficit,
          new_strategy,
          animals_before,
          actual_animals_change,
          # sell_change,
          max_allowed_sell,
          allowed_sell,
          max_sell,
          max_sell_prop,
          selling,
          actual_change,
          max_affordable,
          buying,
          # proposed_change
        )
      )
      
      
      #Make sure this is updated for the next round.
      Ranchers<-Ranchers%>%
        mutate(TotalEconomicWellbeing=Money+(stock_count *Animal_cost))
      
      
      #Remake the Cows raster
      #change number of animals in the raster
      df<-raster::as.data.frame(rstack,xy=T)
      ranimals<-Ranchers%>%group_by(CommID)%>%summarise(Animal=sum(stock_count)/N_Plots) #summarise as table, divide by N_Plots as this is the total number per community
      df<-df%>%dplyr::select(-Animal)
      ranimals<-merge(df,ranimals,by="CommID")
      ranimals <- rasterFromXYZ(ranimals[order(ranimals$y, ranimals$x), c("x", "y", "Animal")])
      
      AnIndex<-which(names(rstack) == "Animal")
      rstack <- rstack[[ -AnIndex ]]
      rstack<-stack(rstack,ranimals)
      
      ######Module 7 - Rain#####
      #Rains then replenish the grass cover of the landscape following the climatic conditions 
      #of the specific simulation and following a logistic growth curve. 
      
      
      df<-raster::as.data.frame(rstack,xy=T)
      df<-df%>%dplyr::select(-Grass)
      NextGrass<-NextGrass[c("x","y","NextGrass")]
      names(NextGrass)[3]<-"Grass"
      df<-base::merge(df,NextGrass,by=c("x","y"))
      rstack<-rasterFromXYZ(df[order(df$y, df$x),])
      
      #Shoul dprobably add a money layer
      # plot(rstack[[c("Cons","Mean_economic_wellbeing","Grazed","Animal","Grass")]])
      #plot(rstack[["Grass"]])
      
      #### Add summary stats to table
      
      SummaryDat2<-data.frame(Time=ts,AvgMoney=mean(Ranchers$Money),
                              Gini = ineq::ineq(Ranchers$TotalEconomicWellbeing,type="Gini"),
                              AvgCows = mean(Ranchers$stock_count),
                              TotalCows = sum(Ranchers$stock_count),
                              AvgGrass = mean(rstack[["Grass"]][]),
                              TotalGrass = sum(rstack[["Grass"]][]),
                              PropCons = length(which(rstack[["Cons"]][]==T))/
                                (N_Communities*N_Plots))
      
      
      SummaryDat2$Scenario <- target_scenario
      SummaryDat <- rbind(SummaryDat, SummaryDat2)
      # ResultsList[[target_scenario]] <- SummaryDat
      
    }
    
    SummaryDat
    
    # AllResults <- do.call(rbind, ResultsList)
    
    
    # p1<-ggplot(SummaryDat,aes(x=Time,y=AvgMoney))+geom_line()+ggtitle("Money")
    # p2<-ggplot(SummaryDat,aes(x=Time,y=AvgCows))+geom_line()+ggtitle("Cows")
    # p3<-ggplot(SummaryDat,aes(x=Time,y=AvgGrass))+geom_line()+ggtitle("Grass")
    # p4<-ggplot(SummaryDat,aes(x=Time,y=Gini))+geom_line()+ggtitle("Gini")
    #is set conservation = FALSE
    #p5<-ggplot(SummaryDat,aes(x=Time,y=PropCons))+geom_line()+ggtitle("Cons") 
    
    #cowplot::plot_grid(p1, p2,p3,p4,p5)
    # cowplot::plot_grid(p1, p2,p3,p4)
    # 
    # ResultsList[[target_scenario]] <- SummaryDat
  }
  
  names(ResultsList) <- scenarios
  RepResults <- data.table::rbindlist(ResultsList, use.names = TRUE, fill = TRUE)
  RepResults$Rep <- rep
  AllSimResults[[rep]] <- RepResults
  
}

# AllSimResultsDF <- do.call(rbind, AllSimResults)
# Combine all repetitions for this condition
AllSimResultsDF <- data.table::rbindlist(AllSimResults, use.names = TRUE, fill = TRUE)

# Save outputs with condition in filename
write.csv(AllSimResultsDF,
          paste0("Ts_Data/", exp_setting, ".csv"),
          row.names = FALSE)

}

stopCluster(cl)

# # Visualizations ----------------------------------------------------------
# 
# SummaryStats <- AllSimResultsDF %>%
#   group_by(Scenario, Time) %>%
#   summarize(
#     AvgMoney_mean = mean(AvgMoney),
#     AvgMoney_sd = sd(AvgMoney),
#     AvgCows_mean = mean(AvgCows),
#     AvgCows_sd = sd(AvgCows),
#     AvgGrass_mean = mean(AvgGrass),
#     AvgGrass_sd = sd(AvgGrass),
#     Gini_mean = mean(Gini),
#     Gini_sd = sd(Gini),
#     .groups = "drop"
#   )
# 
# # Rename scenarios and set colouring
# cols <- c(
#   "SSP1-2.6" = "#009E73",
#   "SSP2-4.5" = "#F0E442",
#   "SSP5-8.5" = "#D55E00",
#   "baseline" = "darkgrey"
# )
# 
# SummaryStats <- SummaryStats %>%
#   mutate(
#     Scenario = recode(
#       Scenario,
#       "good"     = "SSP1-2.6",
#       "ok"       = "SSP2-4.5",
#       "bad"      = "SSP5-8.5",
#       "baseline" = "baseline",
#       .default   = Scenario
#     ),
#     Scenario = factor(Scenario, levels = names(cols))
#   )
# 
# AllSimResultsDF <- AllSimResultsDF %>%
#   mutate(
#     Scenario = recode(
#       Scenario,
#       "good"     = "SSP1-2.6",
#       "ok"       = "SSP2-4.5",
#       "bad"      = "SSP5-8.5",
#       "baseline" = "baseline",
#       .default   = Scenario
#     ),
#     Scenario = factor(Scenario, levels = names(cols))
#   )
# 
# scale_cols  <- scale_color_manual(values = cols, breaks = names(cols), drop = FALSE)
# scale_fills <- scale_fill_manual(values = cols,  breaks = names(cols), drop = FALSE)
# 
# p1 <- ggplot(SummaryStats, aes(Time, AvgMoney_mean, color = Scenario, fill = Scenario)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = AvgMoney_mean - AvgMoney_sd, ymax = AvgMoney_mean + AvgMoney_sd),
#               alpha = 0.2, color = NA) +
#   ggtitle("Money") + scale_cols + scale_fills + labs(color = "Scenario", fill = "Scenario")
# 
# p2 <- ggplot(SummaryStats, aes(Time, AvgCows_mean, color = Scenario, fill = Scenario)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = AvgCows_mean - AvgCows_sd, ymax = AvgCows_mean + AvgCows_sd),
#               alpha = 0.2, color = NA) +
#   ggtitle("Cows") + scale_cols + scale_fills + labs(color = "Scenario", fill = "Scenario")
# 
# p3 <- ggplot(SummaryStats, aes(Time, Gini_mean, color = Scenario, fill = Scenario)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = Gini_mean - Gini_sd, ymax = Gini_mean + Gini_sd),
#               alpha = 0.2, color = NA) +
#   ggtitle("Gini") + scale_cols + scale_fills + labs(color = "Scenario", fill = "Scenario")
# 
# p4 <- ggplot(SummaryStats, aes(Time, AvgGrass_mean, color = Scenario, fill = Scenario)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = AvgGrass_mean - AvgGrass_sd, ymax = AvgGrass_mean + AvgGrass_sd),
#               alpha = 0.2, color = NA) +
#   ggtitle("Grass") + scale_cols + scale_fills + labs(color = "Scenario", fill = "Scenario")
# 
# combined_plot <- cowplot::plot_grid(p1, p2, p3, p4)
# combined_plot
# 
# ggsave(
#   filename = paste0("Plots_Tables/Combined_Plot_Parallel", exp_setting, ".png"),
#   plot = combined_plot,
#   width = 10, height = 8, dpi = 300
# )
# 
# # Save the summary statistics table as CSV
# write.csv(SummaryStats, paste0("Plots_Tables/SummaryStatsParallel_", exp_setting, ".csv"), row.names = FALSE)
# write.csv(AllSimResultsDF, paste0("Plots_Tables/AllSimResultsDFParallel_", exp_setting, ".csv"), row.names = FALSE)
