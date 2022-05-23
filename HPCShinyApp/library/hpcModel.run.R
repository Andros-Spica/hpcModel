#### CODE STYLE NOTES: ####
# - All functions names with verbs
# - Main (public) functions inside the main pseudo class "hpcModel" using dot (.) as separator
# - higher-level composite objects in uppercase
# - function arguments using underscore (_) as separator
# - local variables and functions using camel case

# fitness function
getFitness <- function(type_indexes, utility_popB_to_popA, utility_other_to_popA)
{
  return(((length(type_indexes) - type_indexes) * utility_other_to_popA + type_indexes * utility_popB_to_popA) / (utility_other_to_popA + utility_popB_to_popA))
}

# coevolution coefficients:
# between -1, i.e. all pop in type 1, 
# and 1, i.e. all pop in type n
getCoevolutionCoefficient <- function(type_proportions, type_indexes)
{
  sum(type_proportions * (type_indexes - 1)) / max(type_indexes - 1) * 2 - 1
}

hpcModel.run <- function(
  # initial populations
  initial_population_humans = 10,
  initial_population_plants = 10,
  # number of discrete types
  number_types_humans = 30,         
  number_types_plants = 30,        
  # undirected variation 
  undirected_variation_humans = 0.15,
  undirected_variation_plants = 0.15,
  # intrinsic growth rate 
  intrinsic_growth_rate_humans = 0.04, 
  intrinsic_growth_rate_plants = 0.1, 
  # Utility per capita of individuals of type N
  utility_per_capita_type_n_plants_to_humans = 1.5,
  utility_per_capita_type_n_humans_to_plants = 1,
  # Utility per capita of individuals of type 1
  utility_per_capita_type_1_plants_to_humans = 0.15,                                  
  utility_per_capita_type_1_humans_to_plants = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  utility_other_to_type_n_humans = 10,                                
  utility_other_to_type_n_plants = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  utility_other_to_type_1_humans = 80,                               
  utility_other_to_type_1_plants = 100,                                
  # maximum local area to be used by plant population
  max_area = 200, 
  # settings 
  # simulation flow
  max_iterations = 5000,
  reltol_exponential = 6,
  coevolution_threshold = 0.5,
  # plotting, data & other options
  save_trajectories = FALSE,
  save_device_width = 1000,
  save_device_height = 1000,
  messages = TRUE, 
  plot_preview = FALSE, 
  plot_sleep = 0.05,
  plot_save = FALSE,
  plot_save_every = 5,
  plot_directory = "plots/runPlot/",
  plot_file_name = "runPlot")
{
  #-----------SETUP-------------------------------------------------------------------
  
  RESULTS <- list()
  RESULTS$PARAMETERS <- list(
    initial_population_humans = initial_population_humans,
    initial_population_plants = initial_population_plants,
    number_types_humans = number_types_humans,         
    number_types_plants = number_types_plants,        
    undirected_variation_humans = undirected_variation_humans,
    undirected_variation_plants = undirected_variation_plants,
    intrinsic_growth_rate_humans = intrinsic_growth_rate_humans, 
    intrinsic_growth_rate_plants = intrinsic_growth_rate_plants, 
    utility_per_capita_type_n_plants_to_humans = utility_per_capita_type_n_plants_to_humans,
    utility_per_capita_type_n_humans_to_plants = utility_per_capita_type_n_humans_to_plants,
    utility_per_capita_type_1_plants_to_humans = utility_per_capita_type_1_plants_to_humans,                                  
    utility_per_capita_type_1_humans_to_plants = utility_per_capita_type_1_humans_to_plants,                                   
    utility_other_to_type_n_humans = utility_other_to_type_n_humans,                                
    utility_other_to_type_n_plants = utility_other_to_type_n_plants, 
    utility_other_to_type_1_humans = utility_other_to_type_1_humans,                               
    utility_other_to_type_1_plants = utility_other_to_type_1_plants,                                
    max_area = max_area,
    max_iterations = max_iterations, 
    reltol_exponential = reltol_exponential,
    coevolution_threshold = coevolution_threshold
    )
  
  ### declare fixed vectors of utility anb basic resources ===========================
  # utility
  type_utility_per_capita_plants_to_humans <- seq(utility_per_capita_type_1_plants_to_humans, utility_per_capita_type_n_plants_to_humans, length.out = number_types_plants)
  type_utility_per_capita_humans_to_plants <- seq(utility_per_capita_type_1_humans_to_plants, utility_per_capita_type_n_humans_to_plants, length.out = number_types_humans)
  # basic resources
  type_utility_other_to_humans <- seq(utility_other_to_type_1_humans, utility_other_to_type_n_humans, length.out = number_types_humans)
  type_utility_other_to_plants <- seq(utility_other_to_type_1_plants, utility_other_to_type_n_plants, length.out = number_types_plants)
  
  ### declare cumulative vectors =====================================================
  ### declare states (cumulative)
  # population
  humans <- rep(NA, max_iterations)
  plants <- rep(NA, max_iterations)
  # set initial conditions 
  humans[1] <- initial_population_humans    
  plants[1] <- initial_population_plants
  # total carrying capacity
  carrying_capacity_humans <- rep(NA, max_iterations)
  carrying_capacity_plants <- rep(NA, max_iterations)
  # basic resources 
  utility_other_to_humans <- rep(NA, max_iterations)
  utility_other_to_plants <- rep(NA, max_iterations)
  # Utility
  utility_plants_to_humans <- rep(NA, max_iterations)
  utility_humans_to_plants <- rep(NA, max_iterations)
  # fitness of every type
  fitness_humans <- matrix(NA, max_iterations, number_types_humans)
  fitness_plants <- matrix(NA, max_iterations, number_types_plants)
  # population proportions of every type
  type_proportions_humans <- matrix(NA, max_iterations, number_types_humans)
  type_proportions_plants <- matrix(NA, max_iterations, number_types_plants)
  # set initial conditions
  type_proportions_humans[1,] <- c(1, rep(0, number_types_humans - 1))
  type_proportions_plants[1,] <- c(1, rep(0, number_types_plants - 1))
  ### stats (cumulative) -------------------------------------------------------------
  # increment
  population_change_humans <- rep(NA, max_iterations)
  population_change_plants <- rep(NA, max_iterations)
  # fitness slope
  type_indexes_humans <- 1:number_types_humans
  type_indexes_plants <- 1:number_types_plants
  coevolution_coefficient_humans <- rep(NA, max_iterations)
  coevolution_coefficient_plants <- rep(NA, max_iterations)
  dependency_coefficient_humans <- rep(NA, max_iterations)
  dependency_coefficient_plants <- rep(NA, max_iterations)
  # evolution time 
  timing_humans <- 0
  timing_plants <- 0

  #-----------ITERATIONS-------------------------------------------------------------------
  if (messages) { cat('running simulation...\n') }
  #### main loop =====================================================================
  for (t in 1:(max_iterations - 1)) 
  {
    ### carrying capacity ----------------------------------------------------------
    # set utilities
    utility_plants_to_humans[t] <- sum(plants[t] * type_proportions_plants[t,] * type_utility_per_capita_plants_to_humans)
    utility_humans_to_plants[t] <- sum(humans[t] * type_proportions_humans[t,] * type_utility_per_capita_humans_to_plants)
    # set basic resources 
    utility_other_to_humans[t] <- sum(type_proportions_humans[t,] * type_utility_other_to_humans)
    utility_other_to_plants[t] <- sum(type_proportions_plants[t,] * type_utility_other_to_plants)
    # update carrying capacity
    carrying_capacity_humans[t] <- sum(utility_plants_to_humans[t], utility_other_to_humans[t])
    carrying_capacity_plants[t] <- min(sum(utility_humans_to_plants[t], utility_other_to_plants[t]), max_area)
    ### update population types ----------------------------------------------------
    # set fitness
    fitness_humans[t,] <- getFitness(1:number_types_humans, utility_plants_to_humans[t], utility_other_to_humans[t])
    fitness_plants[t,] <- getFitness(1:number_types_plants, utility_humans_to_plants[t], utility_other_to_plants[t])
    # set undirected variation 
    type_proportions_humans[t,] <- type_proportions_humans[t,] + undirected_variation_humans * ((1/number_types_humans) - type_proportions_humans[t,])
    type_proportions_plants[t,] <- type_proportions_plants[t,] + undirected_variation_plants * ((1/number_types_plants) - type_proportions_plants[t,])
    # replicator dynamics
    type_proportions_humans[t+1,] <- fitness_humans[t,] * type_proportions_humans[t,] / sum(fitness_humans[t,] * type_proportions_humans[t,])
    type_proportions_plants[t+1,] <- fitness_plants[t,] * type_proportions_plants[t,] / sum(fitness_plants[t,] * type_proportions_plants[t,])
    # alternative replicator dynamics [generates chaotic regime]
    # type_proportions_humans[t+1,] <- type_proportions_humans[t,] + 0.1 * (type_proportions_humans[t,] * (fitness_humans - sum(fitness_humans * type_proportions_humans[t,])))
    # type_proportions_plants[t+1,] <- type_proportions_plants[t,] + 0.1 * (type_proportions_plants[t,] * (fitness_plants - sum(fitness_plants * type_proportions_plants[t,])))
    ### update populations --------------------------------------------------------- 
    humans[t+1] <- (1 + intrinsic_growth_rate_humans) * humans[t] - intrinsic_growth_rate_humans * (humans[t]^2 / carrying_capacity_humans[t])
    plants[t+1] <- (1 + intrinsic_growth_rate_plants) * plants[t] - intrinsic_growth_rate_plants * (plants[t]^2 / carrying_capacity_plants[t])
    ### update stats ---------------------------------------------------------------
    # increments (delta)
    population_change_humans[t] <- humans[t+1] - humans[t]
    population_change_plants[t] <- plants[t+1] - plants[t]
    # slope of the population distribution among types (degree of coevolution)
    coevolution_coefficient_humans[t] <- getCoevolutionCoefficient(type_proportions_humans[t,], type_indexes_humans)
    coevolution_coefficient_plants[t] <- getCoevolutionCoefficient(type_proportions_plants[t,], type_indexes_plants)
    # slope of the fitness function (degree of dependency)
    # if (all(fitness_humans[t,] == NA))
    # {
    #   depend.H[t] <- NA
    # }
    # else
    # {
      try(dependency_coefficient_humans[t] <- lm(fitness_humans[t,] ~ type_indexes_humans)$coefficients[2])
    # }
    # if (all(fitness_plants[t,] == NA))
    # {
    #   depend.P[t] <- NA
    # }
    # else
    # {
      try(dependency_coefficient_plants[t] <- lm(fitness_plants[t,] ~ type_indexes_plants)$coefficients[2])
    #}
    
    ### running plot --------------------------------------------------------------------
    if(plot_preview || plot_save) 
    {
        RESULTS$END <- list(
          time_end = t, 
          timing_humans = timing_humans, 
          timing_plants = timing_plants
          )
        
        RESULTS$TRAJECTORIES <- data.frame(
            humans = humans,
            plants = plants, 
            utility_plants_to_humans = utility_plants_to_humans,
            utility_humans_to_plants = utility_humans_to_plants,
            utility_other_to_humans = utility_other_to_humans,
            utility_other_to_plants = utility_other_to_plants,
            carrying_capacity_humans = carrying_capacity_humans,
            carrying_capacity_plants = carrying_capacity_plants,
            population_change_humans = population_change_humans,
            population_change_plants = population_change_plants,
            coevolution_coefficient_humans = coevolution_coefficient_humans,
            coevolution_coefficient_plants = coevolution_coefficient_plants,
            dependency_coefficient_humans = dependency_coefficient_humans,
            dependency_coefficient_plants = dependency_coefficient_plants
            )
        
        RESULTS$TYPES$type_proportions_humans <- type_proportions_humans
        RESULTS$TYPES$type_proportions_plants <- type_proportions_plants 
        RESULTS$TYPES$fitness_humans <- fitness_humans
        RESULTS$TYPES$fitness_plants <- fitness_plants
        
        if (plot_preview)
        {
          hpcModel.plot(RESULTS, device_sleep = plot_sleep)
        }
        
        if (plot_save && t %% plot_save_every == 1)
        {
          if (messages) { cat('saving plot...') }

          dir.create(file.path(plot_directory))

          tWithZeros <- paste0(paste(rep('0', nchar(max_iterations) - nchar(t)), collapse = ''), t)

          png(paste(plot_directory, plot_file_name, '_', tWithZeros, '.png', sep = ""),
              width = save_device_width, height = save_device_height)
          hpcModel.plot(RESULTS, device_sleep = plot_sleep)
          dev.off()

          if (messages) { cat('\n') }
        }
    }
    
    # break loop & check evolution -------------------------------------------------
    if (t > 2) 
    {
      # store the time step of change
      if (timing_humans == 0 && !is.na(coevolution_coefficient_humans[t]) && coevolution_coefficient_humans[t] > coevolution_threshold) 
      {timing_humans <- t}
      
      if (timing_plants == 0 && !is.na(coevolution_coefficient_plants[t]) &&  coevolution_coefficient_plants[t] > coevolution_threshold)
      {timing_plants <- t}
      
      # break loop (reltol method: from optim)
      evolutionStoppedHumans <- is.na(coevolution_coefficient_humans[t]) || abs(coevolution_coefficient_humans[t-2] - coevolution_coefficient_humans[t-1]) < 10^-reltol_exponential
      evolutionStoppedPlants <- is.na(coevolution_coefficient_plants[t]) || abs(coevolution_coefficient_plants[t-2] - coevolution_coefficient_plants[t-1]) < 10^-reltol_exponential
      evolutionStopped <- (evolutionStoppedHumans & evolutionStoppedPlants)
      
      populationsAreStable <- is.na(humans[t]) & is.na(plants[t]) || (abs(humans[t-2] - humans[t-1]) < 10^-reltol_exponential & abs(plants[t-2] - plants[t-1]) < 10^-reltol_exponential)
      
      if (evolutionStopped && populationsAreStable) {break}
    }
  }
  
  if (messages) { cat('done.\nloading results...') }
  
  # build results list -----------------------------------------------------------------
  
  RESULTS$END <- list(
    humans = humans[t],
    plants = plants[t],
    coevolution_coefficient_humans = coevolution_coefficient_humans[t], 
    coevolution_coefficient_plants = coevolution_coefficient_plants[t], 
    dependency_coefficient_humans = dependency_coefficient_humans[t], 
    dependency_coefficient_plants = dependency_coefficient_plants[t],
    timing_humans = timing_humans, 
    timing_plants = timing_plants, 
    time_end = t,
    adaptativeCost.H = sum(type_indexes_humans[type_indexes_humans < 0], na.rm = TRUE),
    adaptativeCost.P = sum(type_indexes_plants[type_indexes_plants < 0], na.rm = TRUE)
    )
  
  if (save_trajectories)
  {
    RESULTS$TRAJECTORIES <- data.frame(
      humans = humans, 
      plants = plants, 
      utility_plants_to_humans = utility_plants_to_humans,
      utility_humans_to_plants = utility_humans_to_plants,
      utility_other_to_humans = utility_other_to_humans,
      utility_other_to_plants = utility_other_to_plants,
      carrying_capacity_humans = carrying_capacity_humans,
      carrying_capacity_plants = carrying_capacity_plants,
      population_change_humans = population_change_humans,
      population_change_plants = population_change_plants,
      coevolution_coefficient_humans = coevolution_coefficient_humans, 
      coevolution_coefficient_plants = coevolution_coefficient_plants,
      dependency_coefficient_humans = dependency_coefficient_humans,
      dependency_coefficient_plants = dependency_coefficient_plants)
    RESULTS$TYPES$type_proportions_humans <- type_proportions_humans
    RESULTS$TYPES$type_proportions_plants <- type_proportions_plants 
    RESULTS$TYPES$fitness_humans <- fitness_humans
    RESULTS$TYPES$fitness_plants <- fitness_plants
  }
  
  if (messages) { cat('done.\n') }
  
  return(RESULTS)
}
