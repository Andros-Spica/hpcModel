# Human-Plant Coevolution model: Simulation UI
# by Andreas Angourakis (andros.spica@gmail.com)
# available at: https://github.com/Andros-Spica/hpcModel
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(
        HTML("<h1 style='text-align: center; color: white; background-color:darkslategrey;'>Human-Plant Coevolution model: Simulation GUI with Shiny</h1>")
        ),
    
    column(
        HTML("<h2 style='text-align: center; background-color:lightgrey;'>Controls</h2>"),
        width = 6,
        fluidRow(
            column(
                width = 2,
                conditionalPanel(
                    condition = "output.isRunning == 'FALSE'",
                    actionButton("runSim", HTML("<h2>Run</h2>"), width = "auto")
                ),
                conditionalPanel(
                    condition = "output.isRunning == 'TRUE'",
                    actionButton("runDisabled", "Running...", width = "100px")
                )
            ),
            column(
                width = 4,
                conditionalPanel(
                    condition = "output.isRunning == 'FALSE'",
                    actionButton("parsToDefault", HTML("<h3>set parameters to default</h3>"), width = "auto")
                )
            ),
            column(
                width = 6,
                conditionalPanel(
                    condition = "output.simulationDone == 'TRUE'",
                    uiOutput("timeStepSlider")
                )
            )
        ),
        
        HTML("<h2 style='background-color:lightgrey;'>Parameter setting</h2>"),
        navlistPanel(widths = c(5,7),
                     
                     tabPanel(
                         HTML("<b style='font-size:100%;'>initial_population_humans, initial_population_plants</b>"),
                         style = "font-size:100%",
                         id = "tab_initial_population",
                         sliderInput("initial_population_humans",
                                     "Initial population of humans",
                                     width = "100%",
                                     min = 0,
                                     max = 100,
                                     value = 10,
                                     step = 1),
                         sliderInput("initial_population_plants",
                                     "Initial population of plants",
                                     width = "100%",
                                     min = 0,
                                     max = 100,
                                     value = 10,
                                     step = 1),
                         imageOutput("diagram_ini")
                     ),
                     tabPanel(
                         HTML("<b style='font-size:100%;'>intrinsic_growth_rate_humans, intrinsic_growth_rate_plants</b>"),
                         style = "font-size:100%",
                         id = "tab_intrinsic_growth_rate",
                         sliderInput("intrinsic_growth_rate_humans",
                                     "Intrinsic growth rate of humans",
                                     width = "100%",
                                     min = 0.01,
                                     max = 0.25,
                                     value = 0.04,
                                     step = 0.01),
                         sliderInput("intrinsic_growth_rate_plants",
                                     "Intrinsic growth rate of plants",
                                     width = "100%",
                                     min = 0.01,
                                     max = 0.25,
                                     value = 0.1,
                                     step = 0.01),
                         imageOutput("diagram_r")
                     ),
                     tabPanel(
                         HTML("<b style='font-size:100%;'>number_types_humans, number_types_plants</b>"),
                         style = "font-size:100%",
                         id = "tab_number_types",
                         sliderInput("number_types_humans",
                                     "Number of human types",
                                     width = "100%",
                                     min = 2,
                                     max = 40,
                                     value = 30,
                                     step = 1),
                         sliderInput("number_types_plants",
                                     "Number of plant types",
                                     width = "100%",
                                     min = 2,
                                     max = 40,
                                     value = 30,
                                     step = 1),
                         imageOutput("diagram_n")
                     ),
                     tabPanel(
                         HTML("<b style='font-size:100%;'>undirected_variation_humans, undirected_variation_plants</b>"),
                         style = "font-size:100%",
                         id = "tab_undirected_variation",
                         sliderInput("undirected_variation_humans",
                                     "Undirected variation of humans",
                                     width = "100%",
                                     min = 0.05,
                                     max = 0.25,
                                     value = 0.15,
                                     step = 0.01),
                         sliderInput("undirected_variation_plants",
                                     "Undirected variation of plants",
                                     width = "100%",
                                     min = 0.05,
                                     max = 0.25,
                                     value = 0.15,
                                     step = 0.01),
                         imageOutput("diagram_v")
                     ),
                     tabPanel(
                         HTML("<b style='font-size:80%;'>utility_per_capita_type_1_humans_to_plants, utility_per_capita_type_n_humans_to_plants</b>"),
                         style = "font-size:100%",
                         id = "tab_utility_per_capita_humans_to_plants",
                         sliderInput("utility_per_capita_type_n_humans_to_plants",
                                     "Utility per capita of type-n humans",
                                     width = "100%",
                                     min = 0,
                                     max = 3,
                                     value = 1,
                                     step = 0.01),
                         sliderInput("utility_per_capita_type_1_humans_to_plants",
                                     "Utility per capita of type-1 humans",
                                     width = "100%",
                                     min = 0,
                                     max = 3,
                                     value = 0,
                                     step = 0.01),
                         imageOutput("diagram_mU.HP")
                     ),
                     tabPanel(
                         HTML("<b style='font-size:80%;'>utility_per_capita_type_1_plants_to_humans, utility_per_capita_type_n_plants_to_humans</b>"),
                         style = "font-size:100%",
                         id = "tab_utility_per_capita_plants_to_humans",
                         sliderInput("utility_per_capita_type_n_plants_to_humans",
                                     "Utility per capita of type-n plants to plants",
                                     width = "100%",
                                     min = 0,
                                     max = 3,
                                     value = 1.5,
                                     step = 0.01),
                         sliderInput("utility_per_capita_type_1_plants_to_humans",
                                     "Utility per capita of type-1 plants to humans",
                                     width = "100%",
                                     min = 0,
                                     max = 3,
                                     value = 0.15,
                                     step = 0.01),
                         imageOutput("diagram_mU.PH")
                     ),
                     tabPanel(
                         HTML("<b style='font-size:100%;'>utility_other_to_type_1_humans, utility_other_to_type_n_humans</b>"),
                         style = "font-size:100%",
                         id = "tab_utility_other_to_humans",
                         sliderInput("utility_other_to_type_n_humans",
                                     "Utility of other resources to type-n humans",
                                     width = "100%",
                                     min = 0,
                                     max = 500,
                                     value = 80,
                                     step = 1),
                         sliderInput("utility_other_to_type_1_humans",
                                     "Utility of other resources to type-1 humans",
                                     width = "100%",
                                     min = 0,
                                     max = 500,
                                     value = 10,
                                     step = 1),
                         imageOutput("diagram_U.bH")
                     ),
                     tabPanel(
                         HTML("<b style='font-size:100%;'>utility_other_to_type_1_plants, utility_other_to_type_n_plants</b>"),
                         style = "font-size:100%",
                         id = "tab_utility_other_to_plants",
                         sliderInput("utility_other_to_type_n_plants",
                                     "Utility of other resources to type-n plants",
                                     width = "100%",
                                     min = 0,
                                     max = 500,
                                     value = 100,
                                     step = 1),
                         sliderInput("utility_other_to_type_1_plants",
                                     "Utility of other resources to type-1 plants",
                                     width = "100%",
                                     min = 0,
                                     max = 500,
                                     value = 20,
                                     step = 1),
                         imageOutput("diagram_U.bP")
                     ),
                     tabPanel(
                         HTML("<b style='font-size:100%;'>max_area</b>"),
                         style = "font-size:100%",
                         id = "tab_max_area",
                         sliderInput("max_area",
                                     "Maximum number of plants fitting the area available",
                                     width = "100%",
                                     min = 0,
                                     max = 500,
                                     value = 200,
                                     step = 1),
                         imageOutput("diagram_MaxArea")
                     ),
                     tabPanel(
                         HTML("<b style='font-size:100%;'>Simulation flow and data</b>"),
                         style = "font-size:100%",
                         id = "tab_simulation",
                         sliderInput("max_iterations",
                                     "Maximum iterations",
                                     width = "100%",
                                     min = 500,
                                     max = 5000,
                                     value = 1000,
                                     step = 1),
                         sliderInput("reltol_exponential",
                                     "Float point precision of stop criterium (delta < 10^reltol_exponential)",
                                     width = "100%",
                                     min = 1,
                                     max = 10,
                                     value = 6,
                                     step = 1),
                         sliderInput("coevolution_threshold",
                                     "Coevolution coefficient considered to be effective change",
                                     width = "100%",
                                     min = 0,
                                     max = 1,
                                     value = 0.5,
                                     step = 0.01)
                     )
        )
    ),
    column(
        width = 6,
        conditionalPanel(
            condition = "output.simulationDone == 'FALSE'",
            br(),
            HTML("<i style='font-size:200%; text-align: centre'>Press 'Run' to run a simulation</i>")
        ),
        conditionalPanel(
            condition = "output.simulationDone == 'TRUE'",
            imageOutput("simulationRunPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    source("library/hpcModel.run.R")
    source("library/hpcModel.plot.R")
    
    simController <- reactiveValues(
        isRunning = FALSE,
        nclicks = 0,
        simResult = NULL,
        time_end = 1
    )
    
    ### UI related ###############################
    diagramSize = "400px"
    
    output$diagram_ini <- renderImage({
        
        list(src = "diagrams/diagram_initial_population.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_r <- renderImage({
        
        list(src = "diagrams/diagram_intrinsic_growth_rate.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_n <- renderImage({
        
        list(src = "diagrams/diagram_number_types.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_v <- renderImage({
        
        list(src = "diagrams/diagram_undirected_variation.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_mU.HP <- renderImage({
        
        list(src = "diagrams/diagram_utility_per_capita_humans_to_plants.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_mU.PH <- renderImage({
        
        list(src = "diagrams/diagram_utility_per_capita_plants_to_humans.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_U.bH <- renderImage({
        
        list(src = "diagrams/diagram_utility_other_to_humans.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_U.bP <- renderImage({
        
        list(src = "diagrams/diagram_utility_other_to_plants.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_MaxArea <- renderImage({
        
        list(src = "diagrams/diagram_max_area.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    observeEvent(input$parsToDefault,{
        updateSliderInput(session, "initial_population_humans", value = 10)
        updateSliderInput(session, "initial_population_plants", value = 10)
        updateSliderInput(session, "number_types_humans", value = 30)
        updateSliderInput(session, "number_types_plants", value = 30)
        updateSliderInput(session, "undirected_variation_humans", value = 0.15)
        updateSliderInput(session, "undirected_variation_plants", value = 0.15)
        updateSliderInput(session, "intrinsic_growth_rate_humans", value = 0.04)
        updateSliderInput(session, "intrinsic_growth_rate_plants", value = 0.1)
        updateSliderInput(session, "utility_per_capita_type_n_plants_to_humans", value = 1.5)
        updateSliderInput(session, "utility_per_capita_type_n_humans_to_plants", value = 1)
        updateSliderInput(session, "utility_per_capita_type_1_plants_to_humans", value = 0.15)
        updateSliderInput(session, "utility_per_capita_type_1_humans_to_plants", value = 0)
        updateSliderInput(session, "utility_other_to_type_n_humans", value = 10)
        updateSliderInput(session, "utility_other_to_type_n_plants", value = 20)                              
        updateSliderInput(session, "utility_other_to_type_1_humans", value = 80)
        updateSliderInput(session, "utility_other_to_type_1_plants", value = 100)                              
        updateSliderInput(session, "max_area", value = 200)                              
        updateSliderInput(session, "max_iterations", value = 1000)
        updateSliderInput(session, "reltol_exponential", value = 6)                              
        updateSliderInput(session, "coevolution_threshold", value = 0.5)
    })
    ### Simulation related ###############################
    
    observeEvent(input$runSim,{
        
        simController$simResult = NULL
        
        simController$isRunning = TRUE
        
        # Don't do anything if analysis is already being run
        if(simController$nclicks != 0){
            showNotification("Already running analysis")
            return(NULL)
        }
        
        # Increment clicks and prevent concurrent analyses
        simController$nclicks = simController$nclicks + 1
        
        withProgress(
            message = "Running simulation...",
            {
                simController$simResult <- hpcModel.run(
                    initial_population_humans = input$initial_population_humans,
                    initial_population_plants = input$initial_population_plants,
                    number_types_humans = input$number_types_humans,
                    number_types_plants = input$number_types_plants,
                    undirected_variation_humans = input$undirected_variation_humans,
                    undirected_variation_plants = input$undirected_variation_plants,
                    intrinsic_growth_rate_humans = input$intrinsic_growth_rate_humans,
                    intrinsic_growth_rate_plants = input$intrinsic_growth_rate_plants,
                    utility_per_capita_type_n_plants_to_humans = input$utility_per_capita_type_n_plants_to_humans,
                    utility_per_capita_type_n_humans_to_plants = input$utility_per_capita_type_n_humans_to_plants,
                    utility_per_capita_type_1_plants_to_humans = input$utility_per_capita_type_1_plants_to_humans,
                    utility_per_capita_type_1_humans_to_plants = input$utility_per_capita_type_1_humans_to_plants,
                    utility_other_to_type_n_humans = input$utility_other_to_type_n_humans,
                    utility_other_to_type_n_plants = input$utility_other_to_type_n_plants,
                    utility_other_to_type_1_humans = input$utility_other_to_type_1_humans,
                    utility_other_to_type_1_plants = input$utility_other_to_type_1_plants,
                    max_area = input$max_area,
                    max_iterations = input$max_iterations,
                    reltol_exponential = input$reltol_exponential,
                    coevolution_threshold = input$coevolution_threshold,
                    save_trajectories = TRUE,
                    messages = FALSE,
                    # plotting
                    plot_preview = FALSE,
                    plot_save = FALSE
                )
            }
        )
        
        simController$time_end <- simController$simResult$END$time_end
        if (is.null(simController$time_end)) { simController$time_end = 1 }
        
        output$timeStepSlider <- renderUI({
            
            #generate the slider
            sliderInput("time_step",label = "time step",
                        width = "100%",
                        min = 1, max = simController$time_end, 
                        value = simController$time_end )
        })
        
        simController$nclicks = 0
        simController$isRunning = FALSE
    })
    
    output$isRunning <- renderText(as.character(simController$isRunning))
    
    outputOptions(output, "isRunning", suspendWhenHidden = FALSE) 
    
    output$simulationDone <- renderText(as.character(!is.null(simController$simResult)))
    
    outputOptions(output, "simulationDone", suspendWhenHidden = FALSE) 
    
    # observeEvent(
    #     input$time_step,
    #     {
    #         simController$time_step = input$time_step
    #     }
    # )
    
    # reactive({
    #     simResult_temp <- simResult()
    #     simResult_temp$TRAJECTORIES <- simResult_temp$TRAJECTORIES[1:simController$time_step,]
    #     
    #     simResultToPlot(simResult_temp)
    # })
    
    output$simulationRunPlot <- renderPlot(
        height = 1000,
        {
            simResultToPlot <- simController$simResult
            simResultToPlot$END$time_end <- input$time_step
            
            hpcModel.plot(simResultToPlot,
                          scale_override = 1.8)
        }
    )
    
    #output$message <- renderText(as.character(simController$time_step))
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1080))
