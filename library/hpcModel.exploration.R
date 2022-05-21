hpcModel.exploration <- function(
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
  # other options
  messages = TRUE
)
{
  if (messages) { cat('preparing experiments...') }
  #source('hpModel.run.R')
  
  # acumulabe 
  dataFrame <- data.frame()
  
  if (messages) { 
    i = 1 # experiment index for console messages
    N = (length(initial_population_humans) * length(initial_population_plants) * 
           length(number_types_humans) * length(number_types_plants) * 
           length(undirected_variation_humans) * length(undirected_variation_plants) * 
           length(intrinsic_growth_rate_humans) * length(intrinsic_growth_rate_plants) * 
           length(utility_per_capita_type_n_plants_to_humans) * length(utility_per_capita_type_n_humans_to_plants) *
           length(utility_per_capita_type_1_plants_to_humans) * length(utility_per_capita_type_1_humans_to_plants) * 
           length(utility_other_to_type_n_humans) * length(utility_other_to_type_n_plants) * 
           length(utility_other_to_type_1_humans) * length(utility_other_to_type_1_plants) * 
           length(max_area))
    cat(paste('done\niterating parameters values (total number of combinations = ', N, '\n'))
  }
  
  # iterate for all values of every parameter
for (initial_population_humans_i in initial_population_humans) {
  for (initial_population_plants_i in initial_population_plants) {
    for (number_types_humans_i in number_types_humans) {
      for (number_types_plants_i in number_types_plants) {
        for (undirected_variation_humans_i in undirected_variation_humans) {
          for (undirected_variation_plants_i in undirected_variation_plants) {
            for (intrinsic_growth_rate_humans_i in intrinsic_growth_rate_humans) {
              for (intrinsic_growth_rate_plants_i in intrinsic_growth_rate_plants) {
                for (utility_per_capita_type_n_plants_to_humans_i in utility_per_capita_type_n_plants_to_humans) {
                  for (utility_per_capita_type_n_humans_to_plants_i in utility_per_capita_type_n_humans_to_plants) {
                    for (utility_per_capita_type_1_plants_to_humans_i in utility_per_capita_type_1_plants_to_humans) {
                      for (utility_per_capita_type_1_humans_to_plants_i in utility_per_capita_type_1_humans_to_plants) {
                        for (utility_other_to_type_n_humans_i in utility_other_to_type_n_humans) {
                          for (utility_other_to_type_n_plants_i in utility_other_to_type_n_plants) {
                            for (utility_other_to_type_1_humans_i in utility_other_to_type_1_humans) {
                              for (utility_other_to_type_1_plants_i in utility_other_to_type_1_plants) {
                                for (max_area_i in max_area) {
                                    
                                    if (messages) { 
                                      cat(paste('===========================================\nexperiment ', i, ' of ', N, '\n'))
                                      i = i + 1
                                    }
                                    
                                    RUN <- hpcModel.run(
                                      initial_population_humans = initial_population_humans_i, 
                                      initial_population_plants = initial_population_plants_i,
                                      number_types_humans = number_types_humans_i, 
                                      number_types_plants = number_types_plants_i,
                                      undirected_variation_humans = undirected_variation_humans_i, 
                                      undirected_variation_plants = undirected_variation_plants_i,
                                      intrinsic_growth_rate_humans = intrinsic_growth_rate_humans_i, 
                                      intrinsic_growth_rate_plants = intrinsic_growth_rate_plants_i,
                                      utility_per_capita_type_n_plants_to_humans = utility_per_capita_type_n_plants_to_humans_i,
                                      utility_per_capita_type_n_humans_to_plants = utility_per_capita_type_n_humans_to_plants_i,
                                      utility_per_capita_type_1_plants_to_humans = utility_per_capita_type_1_plants_to_humans_i, 
                                      utility_per_capita_type_1_humans_to_plants = utility_per_capita_type_1_humans_to_plants_i,
                                      utility_other_to_type_n_humans = utility_other_to_type_n_humans_i, 
                                      utility_other_to_type_n_plants = utility_other_to_type_n_plants_i,
                                      utility_other_to_type_1_humans = utility_other_to_type_1_humans_i, 
                                      utility_other_to_type_1_plants = utility_other_to_type_1_plants_i,
                                      max_area = max_area_i,
                                      # fixed settings
                                      max_iterations = max_iterations,
                                      reltol_exponential = reltol_exponential,
                                      coevolution_threshold = coevolution_threshold,
                                      save_trajectories = FALSE,
                                      messages = messages,
                                      plot_preview = FALSE, 
                                      plot_save = FALSE
                                    )
                                    
                                    dataFrame <- rbind(dataFrame, c(RUN$PARAMETERS, RUN$END))
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  if (messages) { cat('experiments finished.') }
  
  return(dataFrame)
}
