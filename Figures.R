
# Figures

########################
# set up

## HPC model functions
##--------------------
source("library/hpcModel.run.R")
source("library/hpcModel.exploration.R")
source("library/hpcModel.plot.R")

## load packages
##-------------
require(ggplot2)
require(scales)
require(ggpubr)
require(randomForest)

## custom plot functions
##----------------------

source("library/plotting/variableImportancePlot.R")
source("library/plotting/plotTrajectoriesPerVariableGroup.R")
source("library/plotting/tripleRaster_fourParameters.R")

plotDirectory <- "plots/figures/"
dataDirectory <- "data/"

## Select output type (png or eps)
## ----------------------
outputType = "png"
#outputType = "eps"

## this is the 'default' parameter setting (end state is 'fast coevolution')
## ----------------------
# initial populations
initial_population_humans_default = 10
initial_population_plants_default = 10
# number of discrete types
number_types_humans_default = 30
number_types_plants_default = 30    
# undirected variation 
undirected_variation_humans_default = 0.15
undirected_variation_plants_default = 0.15
# intrinsic growth rate 
intrinsic_growth_rate_humans_default = 0.04
intrinsic_growth_rate_plants_default = 0.1
# Utility per capita of individuals of type N
utility_per_capita_type_n_plants_to_humans_default = 1.5
utility_per_capita_type_n_humans_to_plants_default = 1
# Utility per capita of individuals of type 1
utility_per_capita_type_1_plants_to_humans_default = 0.15                           
utility_per_capita_type_1_humans_to_plants_default = 0                               
# basic resources:
# population of type N that can be sustained by resources independent of HP relationship
utility_other_to_type_n_humans_default = 10                               
utility_other_to_type_n_plants_default = 20
# population of type 1 that can be sustained by resources independent of HP relationship
utility_other_to_type_1_humans_default = 80                               
utility_other_to_type_1_plants_default = 100                                 
# maximum local area to be used by populations (multiplier or scaling effect)
max_area_default = 200
### settings 
# simulation flow & data
max_iterations_default = 5000
relreltol_exponential_exponential_default = 6
coevolution_threshold_default = 0.5
save_trajectories_default = TRUE
messages_default = TRUE
#==================================================================
#########################
# Figure - Trajectories
# -----------------------

load(paste0(dataDirectory, "5_LHS_trajectories.RData"))

plotName = "Fig3"

if (outputType == "png")
{
  plotScale = 10
  png(paste0(plotDirectory, plotName, ".png"),
      width = 100 * plotScale, height = 50 * plotScale)
}
if (outputType == "eps")
{
  plotScale = 8
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0(plotDirectory, plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

plotTrajectoriesPerVariableGroup(exp_LHS_trajectories, 
                                 plot1_variables = c('humans', 'plants'),
                                 plot2_variables = c('coevolution_coefficient_humans', 'coevolution_coefficient_plants'),
                                 plot1_title = 'Populations', 
                                 plot2_title = 'Coevolution coefficients',
                                 plot1_colors = c('blue', 'red'),
                                 plot2_colors = c('blue', 'red'),
                                 plot_legend_labels = c('Humans', 'Humans', 'Plants', 'Plants'),
                                 plot_legend_line_x_positions = c(35, 39, 57, 61),
                                 plot_legend_label_x_positions = c(40, 40, 62, 62),
                                 plot1_mark_y_zero = FALSE,
                                 plot2_mark_y_zero = TRUE,
                                 plot_scale = plotScale)

dev.off()

#########################
# Figure - Run scenario: Coevolution do not occur
# -----------------------

run_noCoevolution <- hpcModel.run(
  initial_population_humans = initial_population_humans_default,
  initial_population_plants = initial_population_plants_default,
  number_types_humans = number_types_humans_default,         
  number_types_plants = number_types_plants_default,        
  undirected_variation_humans = undirected_variation_humans_default,
  undirected_variation_plants = undirected_variation_plants_default,
  intrinsic_growth_rate_humans = intrinsic_growth_rate_humans_default, 
  intrinsic_growth_rate_plants = intrinsic_growth_rate_plants_default, 
  utility_per_capita_type_n_plants_to_humans = utility_per_capita_type_n_plants_to_humans_default,
  utility_per_capita_type_n_humans_to_plants = utility_per_capita_type_n_humans_to_plants_default,
  utility_per_capita_type_1_plants_to_humans = 0, # utility of type 1 plants has utility zero                      
  utility_per_capita_type_1_humans_to_plants = utility_per_capita_type_1_humans_to_plants_default,                                   
  utility_other_to_type_n_humans = utility_other_to_type_n_humans_default,                                
  utility_other_to_type_n_plants = utility_other_to_type_n_plants_default, 
  utility_other_to_type_1_humans = utility_other_to_type_1_humans_default,                               
  utility_other_to_type_1_plants = utility_other_to_type_1_plants_default,                                 
  max_area = max_area_default,
  max_iterations = max_iterations_default,
  reltol_exponential = relreltol_exponential_exponential_default,
  coevolution_threshold = coevolution_threshold_default,
  save_trajectories = save_trajectories_default,
  messages = messages_default, 
  # plotting
  plot_preview = FALSE,
  plot_save = FALSE
)

plotName = "Fig4"

if (outputType == "png")
{
  png(paste0(plotDirectory, plotName, ".png"),
      width = 1000, height = 1000)
  
  cex_parameters = 0.8
}
if (outputType == "eps")
{
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0(plotDirectory, plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
  
  cex_parameters = 1
}

hpcModel.plot(run_noCoevolution, cex_parameters = cex_parameters)
dev.off()

#########################
# Figure - Run scenario: Coevolution occurs
# -----------------------

run_coevolution_sameTiming <- hpcModel.run(
  initial_population_humans = initial_population_humans_default,
  initial_population_plants = initial_population_plants_default,
  number_types_humans = number_types_humans_default,         
  number_types_plants = number_types_plants_default,        
  undirected_variation_humans = undirected_variation_humans_default,
  undirected_variation_plants = undirected_variation_plants_default,
  intrinsic_growth_rate_humans = intrinsic_growth_rate_humans_default, 
  intrinsic_growth_rate_plants = intrinsic_growth_rate_plants_default, 
  utility_per_capita_type_n_plants_to_humans = utility_per_capita_type_n_plants_to_humans_default,
  utility_per_capita_type_n_humans_to_plants = utility_per_capita_type_n_humans_to_plants_default,
  utility_per_capita_type_1_plants_to_humans = utility_per_capita_type_1_plants_to_humans_default,                                  
  utility_per_capita_type_1_humans_to_plants = utility_per_capita_type_1_humans_to_plants_default,                                   
  utility_other_to_type_n_humans = utility_other_to_type_n_humans_default,                                
  utility_other_to_type_n_plants = utility_other_to_type_n_plants_default, 
  utility_other_to_type_1_humans = utility_other_to_type_1_humans_default,                               
  utility_other_to_type_1_plants = utility_other_to_type_1_plants_default,                                 
  max_area = max_area_default,
  max_iterations = max_iterations_default,
  reltol_exponential = relreltol_exponential_exponential_default,
  coevolution_threshold = coevolution_threshold_default,
  save_trajectories = save_trajectories_default,
  messages = messages_default, 
  # plotting
  plot_preview = FALSE, 
  plot_save = FALSE
)

plotName = "Fig5"

if (outputType == "png")
{
  png(paste0(plotDirectory, plotName, ".png"),
      width = 1000, height = 1000)
  
  cex_parameters = 0.8
}
if (outputType == "eps")
{
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0(plotDirectory, plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
  
  cex_parameters = 1
}

hpcModel.plot(run_coevolution_sameTiming, cex_parameters = cex_parameters)
dev.off()

#########################
# Figure - Run scenario: Coevolution occurs partially
# -----------------------

run_partialCoevolution <- hpcModel.run(
  initial_population_humans = initial_population_humans_default,
  initial_population_plants = initial_population_plants_default,
  number_types_humans = number_types_humans_default,         
  number_types_plants = number_types_plants_default,        
  undirected_variation_humans = undirected_variation_humans_default,
  undirected_variation_plants = undirected_variation_plants_default,
  intrinsic_growth_rate_humans = intrinsic_growth_rate_humans_default, 
  intrinsic_growth_rate_plants = intrinsic_growth_rate_plants_default, 
  utility_per_capita_type_n_plants_to_humans = 0.5, # utility type n plant is the same than type 1 plants and lower than default
  utility_per_capita_type_n_humans_to_plants = utility_per_capita_type_n_humans_to_plants_default,
  utility_per_capita_type_1_plants_to_humans = 0.5, # utility type 1 plant is the same than type n plants and higher than default
  utility_per_capita_type_1_humans_to_plants = utility_per_capita_type_1_humans_to_plants_default,                                   
  utility_other_to_type_n_humans = utility_other_to_type_n_humans_default,                                
  utility_other_to_type_n_plants = utility_other_to_type_n_plants_default, 
  utility_other_to_type_1_humans = utility_other_to_type_1_humans_default,                               
  utility_other_to_type_1_plants = utility_other_to_type_1_plants_default,                                 
  max_area = max_area_default,
  max_iterations = max_iterations_default,
  reltol_exponential = relreltol_exponential_exponential_default,
  coevolution_threshold = coevolution_threshold_default,
  save_trajectories = save_trajectories_default,
  messages = messages_default, 
  # plotting
  plot_preview = FALSE,
  plot_save = FALSE
)

plotName = "Fig-partialCoevo"

if (outputType == "png")
{
  png(paste0(plotDirectory, plotName, ".png"),
      width = 1000, height = 1000)
  
  cex_parameters = 0.8
}
if (outputType == "eps")
{
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0(plotDirectory, plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
  
  cex_parameters = 1
}

hpcModel.plot(run_partialCoevolution, cex_parameters = cex_parameters)
dev.off()

#########################

run_partialCoevolution_oscillation2 <- hpcModel.run(
  initial_population_humans = initial_population_humans_default,
  initial_population_plants = initial_population_plants_default,
  number_types_humans = 10,         
  number_types_plants = 10,        
  undirected_variation_humans = undirected_variation_humans_default,
  undirected_variation_plants = undirected_variation_plants_default, 
  intrinsic_growth_rate_humans = 0.15,
  intrinsic_growth_rate_plants = 0.15, 
  utility_per_capita_type_n_plants_to_humans = 1,
  utility_per_capita_type_n_humans_to_plants = 0.5, # utility type n humans is the same than type 1
  utility_per_capita_type_1_plants_to_humans = 0.2,
  utility_per_capita_type_1_humans_to_plants = 0.5,
  utility_other_to_type_n_humans = 10,
  utility_other_to_type_n_plants = 10, 
  utility_other_to_type_1_humans = 100,
  utility_other_to_type_1_plants = 100,
  max_area = max_area_default, 
  max_iterations = 600, # limit iterations
  reltol_exponential = relreltol_exponential_exponential_default,
  coevolution_threshold = coevolution_threshold_default,
  save_trajectories = save_trajectories_default,
  messages = messages_default, 
  # plotting
  plot_preview = FALSE,
  plot_save = FALSE
)

plotName = "Fig6"#"Fig-partialCoevo-osc"

if (outputType == "png")
{
  png(paste0(plotDirectory, plotName, ".png"),
      width = 1000, height = 1000)
  
  cex_parameters = 0.8
}
if (outputType == "eps")
{
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0(plotDirectory, plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
  
  cex_parameters = 1
}

hpcModel.plot(run_partialCoevolution_oscillation2, cex_parameters = cex_parameters)
dev.off()

#########################
# Figure - importance plot (random forest)
# -----------------------

# set up color mapping for variable importance plots (variableImportancePlot())
# assuming order to be: n, v, r, mU, U.b and MaxArea
parameterColorMap <- c("black", rep("blue", 4), rep("green", 4), rep("red", 6))

# load
load(paste0(dataDirectory, "5_RF_coevolution_coefficient_humans.RData"))
load(paste0(dataDirectory, "5_RF_coevolution_coefficient_plants.RData"))

plotName = "Fig7"

if (outputType == "png")
{
  plotScale = 2
  png(paste0(plotDirectory, plotName, ".png"),
      width = 480 * plotScale, height = 760 * plotScale)
}
if (outputType == "eps")
{
  plotScale = 1.2
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0(plotDirectory, plotName, ".eps"),
                        pointsize = 10,
                        width = 4.8 * plotScale,
                        height = 7.6 * plotScale,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

variableImportancePlotPair(RF_coevolution_coefficient_humans, 
                           RF_coevolution_coefficient_plants, 
                           plot_scale = plotScale,
                           color_variable_map = parameterColorMap)

dev.off()

#########################
# Figure - mutual utility
# -----------------------

# load
load(paste0(dataDirectory, '4_exp_utilities_to_humans.RData'))

plotName = "Fig8"

if (outputType == "png")
{
  plotScale = 10
  png(paste0("plots/figures/", plotName, ".png"),
      width = 100 * plotScale, height = 100 * plotScale)
}
if (outputType == "eps")
{
  plotScale = 7
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0("plots/figures/", plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

tripleRaster_fourParameters(exp_utilities_to_humans, 
                            parameter_x = 'utility_other_to_type_1_humans', 
                            parameter_y = 'utility_other_to_type_n_humans', 
                            parameter_strip_x = 'utility_per_capita_type_1_plants_to_humans', 
                            parameter_strip_y = 'utility_per_capita_type_n_plants_to_humans', 
                            variable_1 = 'coevolution_coefficient_humans', 
                            variable_2 = 'coevolution_coefficient_plants',
                            variable_3 = 'time_end',
                            parameter_x_label = expression(paste('utility of other to type 1 humans (', U['bH1'], ')')),
                            parameter_y_label = expression(paste('utility of other to type n humans (', U['bHn'], ')')),
                            variable_1_label = expression('coevo'[H]),
                            variable_2_label = expression('coevo'[P]),
                            variable_3_label = expression(t['end']),
                            plot_scale = plotScale,
                            strip_parameter_name_max_length = 28, strip_text_size = 0.9)

dev.off()

#########################
# Figure - number of types (n) x undirected variation (v)
# -----------------------

# load
load(paste0(dataDirectory, '4_exp_types_number_and_variation.RData'))

plotName = "Fig-nxv"

if (outputType == "png")
{
  plotScale = 10
  png(paste0(plotDirectory, plotName, ".png"),
      width = 100 * plotScale, height = 100 * plotScale)
}
if (outputType == "eps")
{
  plotScale = 6
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0(plotDirectory, plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

tripleRaster_fourParameters(exp_types_number_and_variation, 
                            parameter_x = 'undirected_variation_humans', 
                            parameter_y = 'undirected_variation_plants', 
                            parameter_strip_x = 'number_types_humans', 
                            parameter_strip_y = 'number_types_plants', 
                            variable_1 = 'coevolution_coefficient_humans', 
                            variable_2 = 'coevolution_coefficient_plants', 
                            variable_3 = 'time_end',
                            parameter_x_label = expression(paste('undirected variation in humans (', v['H'], ')')),
                            parameter_y_label = expression(paste('undirected variation in plants ( ', v['P'], ')')),
                            variable_1_label = expression('coevo'[H]),
                            variable_2_label = expression('coevo'[P]),
                            variable_3_label = expression(t['end']),
                            plot_scale = plotScale)

dev.off()
