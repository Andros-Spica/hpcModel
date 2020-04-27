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
    titlePanel("Human-Plant Coevolution model: Simulation UI"),
    
    column(
        5,
        "Controls",
        fluidRow(
            column(
                5,
                conditionalPanel(
                    condition = "output.isRunning == 'FALSE'",
                    actionButton("runSim", "Run", width = "100px")
                ),
                conditionalPanel(
                    condition = "output.isRunning == 'TRUE'",
                    actionButton("runDisabled", "Running...", width = "100px")
                ),
                conditionalPanel(
                    condition = "output.isRunning == 'FALSE'",
                    actionButton("parsToDefault", "Parameters to default", width = "200px")
                )
            ),
            column(
                5,
                conditionalPanel(
                    condition = "output.simulationDone == 'TRUE'",
                    uiOutput("timeStepSlider")
                )
            )
        ),
        
        "Parameter setting",
        navlistPanel(widths = c(4,6),
                     
                     tabPanel(
                         "iniH, iniP",
                         style = "font-size:80%",
                         id = "tab_ini",
                         sliderInput("iniH",
                                     "Initial population of humans (iniH)",
                                     min = 0,
                                     max = 100,
                                     value = 10,
                                     step = 1),
                         sliderInput("iniP",
                                     "Initial population of plants (iniP)",
                                     min = 0,
                                     max = 100,
                                     value = 10,
                                     step = 1),
                         imageOutput("diagram_ini")
                     ),
                     tabPanel(
                         "r.H, r.P",
                         style = "font-size:80%",
                         id = "tab_r",
                         sliderInput("r.H",
                                     "Intrinsic growth rate of humans (r.H)",
                                     min = 0.01,
                                     max = 0.25,
                                     value = 0.04,
                                     step = 0.01),
                         sliderInput("r.P",
                                     "Intrinsic growth rate of plants (r.P)",
                                     min = 0.01,
                                     max = 0.25,
                                     value = 0.1,
                                     step = 0.01),
                         imageOutput("diagram_r")
                     ),
                     tabPanel(
                         "n.H, n.P",
                         style = "font-size:80%",
                         id = "tab_n",
                         sliderInput("n.H",
                                     "Number of human types (n.H)",
                                     min = 2,
                                     max = 40,
                                     value = 30,
                                     step = 1),
                         sliderInput("n.P",
                                     "Number of plant types (n.P)",
                                     min = 2,
                                     max = 40,
                                     value = 30,
                                     step = 1),
                         imageOutput("diagram_n")
                     ),
                     tabPanel(
                         "v.H, v.P",
                         style = "font-size:80%",
                         id = "tab_v",
                         sliderInput("v.H",
                                     "Undirected variation of humans (v.H)",
                                     min = 0.05,
                                     max = 0.25,
                                     value = 0.15,
                                     step = 0.01),
                         sliderInput("v.P",
                                     "Undirected variation of plants (v.P)",
                                     min = 0.05,
                                     max = 0.25,
                                     value = 0.15,
                                     step = 0.01),
                         imageOutput("diagram_v")
                     ),
                     tabPanel(
                         "mU.H1P, mU.HnP",
                         style = "font-size:80%",
                         id = "tab_mU.HP",
                         sliderInput("mU.HnP",
                                     "Utility per capita of type-n humans (mU.HnP)",
                                     min = 0,
                                     max = 3,
                                     value = 1,
                                     step = 0.01),
                         sliderInput("mU.H1P",
                                     "Utility per capita of type-1 humans (mU.H1P)",
                                     min = 0,
                                     max = 3,
                                     value = 0,
                                     step = 0.01),
                         imageOutput("diagram_mU.HP")
                     ),
                     tabPanel(
                         "mU.P1H, mU.PnH",
                         style = "font-size:80%",
                         id = "tab_mU.PH",
                         sliderInput("mU.PnH",
                                     "Utility per capita of type-n plants (mU.PnH)",
                                     min = 0,
                                     max = 3,
                                     value = 1.5,
                                     step = 0.01),
                         sliderInput("mU.P1H",
                                     "Utility per capita of type-1 plants (mU.P1H)",
                                     min = 0,
                                     max = 3,
                                     value = 0.15,
                                     step = 0.01),
                         imageOutput("diagram_mU.PH")
                     ),
                     tabPanel(
                         "U.bH1, U.bHn",
                         style = "font-size:80%",
                         id = "tab_U.bH",
                         sliderInput("U.bHn",
                                     "Utility of other resources to type-n humans (U.bHn)",
                                     min = 0,
                                     max = 500,
                                     value = 80,
                                     step = 1),
                         sliderInput("U.bH1",
                                     "Utility of other resources to type-1 humans (U.bH1)",
                                     min = 0,
                                     max = 500,
                                     value = 10,
                                     step = 1),
                         imageOutput("diagram_U.bH")
                     ),
                     tabPanel(
                         "U.bP1, U.bPn",
                         id = "tab_U.bP",
                         sliderInput("U.bPn",
                                     "Utility of other resources to type-n plants (U.bPn)",
                                     min = 0,
                                     max = 500,
                                     value = 100,
                                     step = 1),
                         sliderInput("U.bP1",
                                     "Utility of other resources to type-1 plants (U.bP1)",
                                     min = 0,
                                     max = 500,
                                     value = 20,
                                     step = 1),
                         imageOutput("diagram_U.bP")
                     ),
                     tabPanel(
                         "MaxArea",
                         style = "font-size:80%",
                         id = "tab_MaxArea",
                         sliderInput("MaxArea",
                                     "Maximum number of plants fitting the area available (MaxArea)",
                                     min = 0,
                                     max = 500,
                                     value = 200,
                                     step = 1),
                         imageOutput("diagram_MaxArea")
                     ),
                     tabPanel(
                         "Simulation flow and data",
                         style = "font-size:80%",
                         id = "tab_Sim",
                         sliderInput("maxIt",
                                     "Maximum iterations (maxIt)",
                                     min = 500,
                                     max = 5000,
                                     value = 1000,
                                     step = 1),
                         sliderInput("tol",
                                     "Float point precision of stop criterium (delta < 10^tol)",
                                     min = 1,
                                     max = 10,
                                     value = 6,
                                     step = 1),
                         sliderInput("timing.threshold",
                                     "Coevolution coefficient considered to be effective change (timing.threshold)",
                                     min = 0,
                                     max = 1,
                                     value = 0.5,
                                     step = 0.01)
                     )
        )
    ),
    column(
        6,
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
        endTime = 1
    )
    
    ### UI related ###############################
    diagramSize = "300px"
    
    output$diagram_ini <- renderImage({
        
        list(src = "diagrams/diagram_ini.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_r <- renderImage({
        
        list(src = "diagrams/diagram_r.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_n <- renderImage({
        
        list(src = "diagrams/diagram_n.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_v <- renderImage({
        
        list(src = "diagrams/diagram_v.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_mU.HP <- renderImage({
        
        list(src = "diagrams/diagram_mU.HP.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_mU.PH <- renderImage({
        
        list(src = "diagrams/diagram_mU.PH.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_U.bH <- renderImage({
        
        list(src = "diagrams/diagram_U.bH.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_U.bP <- renderImage({
        
        list(src = "diagrams/diagram_U.bP.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    output$diagram_MaxArea <- renderImage({
        
        list(src = "diagrams/diagram_MaxArea.png",
             width = diagramSize,
             contentType = "image/png",
             alt = "")
        
    }, deleteFile = FALSE)
    
    observeEvent(input$parsToDefault,{
        updateSliderInput(session, "iniH", value = 10)
        updateSliderInput(session, "iniP", value = 10)
        updateSliderInput(session, "n.H", value = 30)
        updateSliderInput(session, "n.P", value = 30)
        updateSliderInput(session, "v.H", value = 0.15)
        updateSliderInput(session, "v.P", value = 0.15)
        updateSliderInput(session, "r.H", value = 0.04)
        updateSliderInput(session, "r.P", value = 0.1)
        updateSliderInput(session, "mU.PnH", value = 1.5)
        updateSliderInput(session, "mU.HnP", value = 1)
        updateSliderInput(session, "mU.P1H", value = 0.15)
        updateSliderInput(session, "mU.H1P", value = 0)
        updateSliderInput(session, "U.bHn", value = 10)
        updateSliderInput(session, "U.bPn", value = 20)                              
        updateSliderInput(session, "U.bH1", value = 80)
        updateSliderInput(session, "U.bP1", value = 100)                              
        updateSliderInput(session, "MaxArea", value = 200)                              
        updateSliderInput(session, "maxIt", value = 1000)
        updateSliderInput(session, "tol", value = 6)                              
        updateSliderInput(session, "timing.threshold", value = 0.5)
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
                    iniH = input$iniH,
                    iniP = input$iniP,
                    n.H = input$n.H,
                    n.P = input$n.P,
                    v.H = input$v.H,
                    v.P = input$v.P,
                    r.H = input$r.H,
                    r.P = input$r.P,
                    mU.PnH = input$mU.PnH,
                    mU.HnP = input$mU.HnP,
                    mU.P1H = input$mU.P1H,
                    mU.H1P = input$mU.H1P,
                    U.bHn = input$U.bHn,
                    U.bPn = input$U.bPn,
                    U.bH1 = input$U.bH1,
                    U.bP1 = input$U.bP1,
                    MaxArea = input$MaxArea,
                    maxIt = input$maxIt,
                    tol = input$tol,
                    timing.threshold = input$timing.threshold,
                    saveTrajectories = TRUE,
                    messages = FALSE,
                    # plotting
                    plot.preview = FALSE,
                    plot.save = FALSE
                )
            }
        )
        
        simController$endTime <- simController$simResult$END$time
        if (is.null(simController$endTime)) { simController$endTime = 1 }
        
        output$timeStepSlider <- renderUI({
            
            #generate the slider
            sliderInput("timeStep",label = "time step",
                        min = 1, max = simController$endTime, value = simController$endTime )
        })
        
        simController$nclicks = 0
        simController$isRunning = FALSE
    })
    
    output$isRunning <- renderText(as.character(simController$isRunning))
    
    outputOptions(output, "isRunning", suspendWhenHidden = FALSE) 
    
    output$simulationDone <- renderText(as.character(!is.null(simController$simResult)))
    
    outputOptions(output, "simulationDone", suspendWhenHidden = FALSE) 
    
    # observeEvent(
    #     input$timeStep,
    #     {
    #         simController$timeStep = input$timeStep
    #     }
    # )
    
    # reactive({
    #     simResult_temp <- simResult()
    #     simResult_temp$TRAJECTORIES <- simResult_temp$TRAJECTORIES[1:simController$timeStep,]
    #     
    #     simResultToPlot(simResult_temp)
    # })
    
    output$simulationRunPlot <- renderPlot(
        height = 600,
        {
            simResultToPlot <- simController$simResult
            simResultToPlot$END$time <- input$timeStep
            
            hpcModel.plot(simResultToPlot,
                          scale.override = 1)
        }
    )
    
    #output$message <- renderText(as.character(simController$timeStep))
}

# Run the application 
shinyApp(ui = ui, server = server)
