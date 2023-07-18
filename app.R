##This code runs the WEFSys Model App for simulating WEF nexus
##for any study location using user-input data. Results are visualized
##in graph and table formats for the years of simulation.
##The interface requires the suer to upload an input CSV file (sample file is provided along with this code)

##Load the required packages##
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(dplyr)
library(knitr)
library(plotly)
library(testthat)
library(kableExtra)
library(shinydashboard)
library(tippy)
library(shinycssloaders)
library(shinytest)
library(htmltools)
library(ggplot2)
library(htmlTable)
library(rstudioapi)
library(gridExtra)
library(grid)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(base64enc)
library(rmarkdown)

source("SFDModel_r.R")  #The script with WEF model function

##Build the UI with three tabs - Home, Background, and Model##
ui <- shinyUI(fluidPage(theme = shinytheme("sandstone"),
                        setBackgroundColor(
                          color = c("white", "lightgrey"),
                          gradient = "linear",
                          direction = "bottom"
                        ),
                        titlePanel("WEFSys - Dynamic systems modeling of the water-energy-food nexus"),
                        navbarPage("Welcome!",
                                   tabPanel("Home",
                                            tags$head(
                                              tags$style(
                                                HTML("
           .custom-button {
             color: black;
             font-weight: bold;
             border: 2px solid black;
             background-color: papayawhip;
             border-radius: 10px;
           }
          ")
                                              )
                                            ),
                                            fluidRow(column(
                                                            p("WATER",style="color:black;text-align:center"),
                                                            width=4,style="background-color:#CCFFFF;font-size: 25px"),
                                                     column(
                                                            p("ENERGY",style="color:black;text-align:center"),
                                                            width=4,style="background-color:#FFCCCC;font-size: 25px"),
                                                     column(
                                                            p("FOOD",style="color:black;text-align:center"),
                                                            width=4,style="background-color:#E5FFCC;font-size: 25px")),
                                            
                                            fluidRow(column(
                                              
                                              hr(),
                                              p("Welcome! The WEFSys tool is designed to run a dynamic systems model of the water-energy-food (WEF) nexus within a community or region, using data provided by the user. The tool helps visualize temporal changes in supply and demand 
                                     quantities of water, energy, and food, and various other indicators connecting the nexus. Users are also able to estimate indices associated with water, energy, and food securities. More information about the model and data requirements can be found in the 'Background' tab.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                              hr(),
                                              p("To learn more about our WEF Nexus Hub and to explore useful resources for WEF modeling,",
                                                br(),
                                                a(href="http://sites.psu.edu/wefnexusmodeling", "Click Here",target="_blank"),style="text-align:center;color:black"),
                                              width=12)),
                                            
                                            
                                            hr(),
                                            p(em("Interface developed by"),br("Dr. Femeena Pandara Valappil & Dr. Rachel Brennan"),("Department of Civil and Environmental Engineering,"),("The Pennsylvania State University, U.S.A"),
                                            br(),em("Model framework based on"), a(href="https://www.sciencedirect.com/science/article/abs/pii/S2352550920304553","Purwanto et al. (2021)"),style="text-align:center; font-family: times",width=2)),
                                   tabPanel("Background",
                                            tags$head(
                                              tags$style(HTML("
      .download-link {
        display: inline-block;
        background-color: black;
        color: white;
        padding: 10px 20px;
        text-decoration: none;
      }
    "))
                                            ),
                                            
                                            fluidRow(column(width=2),
                                                     column(
                                                       h3(p("WEFSys Modeling Tool",style="color:black;font-weight: bold;text-align:center")),
                                                       width=8,style="background-color:lavender;border-radius: 10px")
                                            ),
                                            br(),
                                            fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                                                     column(
                                                       p("The WEFSys modeling tool is a first-of-its kind free online interface that allow users to simulate the 
                                                         water-energy-food nexus using dynamic systems modeling approach. The interface provided here is primarily based on a WEF-Nexus framework developed by", 
                                                         a(href="https://www.sciencedirect.com/science/article/abs/pii/S2352550920304553",strong("Purwanto et al. (2021).")),
                                                       "With this model, users are able to simulate a community or region with  residential, industrial, and agricultural areas (including livestock and up to three staple crops).
                                                       Temporal changes in these areas and in the production of energy, water, and food are used as input to quantify interlinkages between the three sectors.", br(),br(), "Water, Energy, and Food security indices are calculated in the model using the same methodology used in
                                                        estimating the",a(href="https://www.rand.org/pubs/tools/TL165.html",
                                                       strong("Pardee RAND Food-Energy-Water Index.")),"The indices range from 1 to 5, with 5 indicating the most
                                                        secure status. Accessibility and availability of resources (water, energy, and food) are the main factors
                                                        driving these indices.",
                                                         style="color:black;text-align:justify"),
                                                       p("The model additionally determines two indicators for each sector : ", strong("(1) Availability per person")," (water, energy, or food
                                                       available per person) and ", strong("(2) Self-sufficiency level")," (which quantifies the extent to which local resource production
                                                         can meet the population demand without relying on imports). ",style="color:black;text-align:justify"),
                                                       width=8,style="background-color:lavender;border-radius: 10px")
                                            ),
                                            br(),
                                            fluidRow(column(12, align = "center", 
                                                            downloadLink("downloadDocx", "Click here to download the instructions manual",class = "download-link"))
                                            ),
                                            
                                            hr(),
                                            fluidRow(column(width =12,align = "center",
                                                     p("The original WEFSys model was initially built on Stella modeling platform with causal-loop
                                                       diagrams as shown below. Clicking on the image below will redirect you to the Stella model view, where
                                                       you may view the different componentsof the model using the zoom feature. To provide a more user-friendly experience with enhanced results and data inputs, 
                                                       the model has now been redesigned and published on this interface, which you will
                                                       find on the 'Model' tab on top.", style="color:black;
                                                       background-color:lightblue;border-radius: 10px"))),
                                            fluidRow(column(12,align = 'center',
                                            a(href = "https://exchange.iseesystems.com/models/player/femeena-pandara-valappil/wefsys-model", target = "_blank",
                                            img(src = "StellaModel.PNG", height = "400px", width = "500px")))
                                            
                                            
                                   )),
                                   
                                   tabPanel("Model",tags$style(HTML("


.box.box-solid.box-danger>.box-header {
  color:#000099;
  background:#CCFFFF
                    }

.box.box-solid.box-danger{
border-bottom-color:#CCFFFF;
border-left-color:#CCFFFF;
border-right-color:#CCFFFF;
border-top-color:#CCFFFF;
}

.box.box-danger{
border-bottom-color:#CCFFFF;
border-left-color:#CCFFFF;
border-right-color:#CCFFFF;
border-top-color:#CCFFFF;
background: #CCFFFF;
}

.box.box-solid.box-info>.box-header {
  color:#990000;
  background:#FFCCCC
                    }

.box.box-solid.box-info{
border-bottom-color:#FFCCCC;
border-left-color:#FFCCCC;
border-right-color:#FFCCCC;
border-top-color:#FFCCCC;
}

.box.box-info>.box-header {
  color:#FFCCCC; 
  background:#FFCCCC
                    }

.box.box-info{
border-bottom-color:#FFCCCC;
border-left-color:#FFCCCC;
border-right-color:#FFCCCC;
border-top-color:#FFCCCC;
background: #FFCCCC;
}

.box.box-solid.box-warning>.box-header {
  color:#336600;
  background:#E5FFCC
                    }

.box.box-solid.box-warning{
border-bottom-color:#E5FFCC;
border-left-color:#E5FFCC;
border-right-color:#E5FFCC;
border-top-color:#E5FFCC;
}

.box.box-warning>.box-header {
  color:#fff; 
  background:#E5FFCC
                    }

.box.box-warning{
border-bottom-color:#E5FFCC;
border-left-color:#E5FFCC;
border-right-color:#E5FFCC;
border-top-color:#E5FFCC;
background: #E5FFCC;
}



                                    ")),
                                            fluidPage(
                                              fluidRow(
                                                column(
                                                p("All the values are at an annual scale. Please refer to the
                                                  instruction manual for description of data requirements and examples. If all
                                                  the data are not readily available, users may use assumptions and estimates,
                                                  as described in the manual.",style="color:white;text-align:center"),
                                                width=12,style="background-color:black;font-size: 15px;border-radius: 10px")
                                              ),
                                              fluidRow(
                                                column(12,
                                                    fluidRow(
                                                      column(4,h2("Population"),
                                                             p(HTML("<b>Total Population</b>"), span(shiny::icon("info-circle"), id = "info_X"), numericInput('Population', NULL, 10000),
                                                               tippy::tippy_this(elementId = "info_X", tooltip = "Total population in the study area", placement = "right")
                                                             ),
                                                    fluidRow(
                                                             column(6,sliderInput("BirthRate", "Birth Rate ( per 100 people)", 0, 10, 0.016, step = 0.001)),
                                                             column(6,sliderInput("DeathRate", "Death Rate ( per 100 people)", 0, 10, 0.005, step = 0.001))
                                                      ),
                                                             column(6,sliderInput("ImmigrationRate", "Immigration Rate ( per 100 people)", 0, 10, 0.017, step = 0.001)),
                                                             column(6,sliderInput("OutmigrationRate", "Outmigration Rate ( per 100 people)", 0, 10, 0.01, step = 0.001))
                                                      ),
                                                    column(4, h2("Land"),
                                                           br(),
                                                           fluidRow(
                                                             column(6, numericInput('TotalArea', "Total Area (ha)", 200000))
                                                           ),
                                                           fluidRow(
                                                             column(4, numericInput('Crop1Area', "Crop-1 Area (ha)", 30000)),
                                                             column(4, numericInput('Crop2Area', "Crop-2 Area (ha)", 0)),
                                                             column(4, numericInput('Crop3Area', "Crop-3 Area (ha)", 0))
                                                           ),
                                                           fluidRow(
                                                             column(4, numericInput('LivestockArea', "Livestock Area (ha)", 40000)),
                                                             column(4, numericInput('ResidentialArea', "Residential Area (ha)", 110000)),
                                                             column(4, numericInput('IndustrialArea', "Industrial Area (ha)", 10000))
                                                           )
                                                    ),
                                                    column(4, 
                                                           br(),br(),br(),
                                                           h4("Annual land expansion/reduction rates (%)"),
                                                           br(),
                                                           #box("Annual land expansion/reduction rates (%)",
                                                               fluidRow(
                                                                 column(4, sliderInput("Crop1rate", "For Crop-1", -20, 20, 0.02, step = 0.001)),
                                                                 column(4, sliderInput("Crop2rate", "For Crop-2", -20, 20, 0, step = 0.001)),
                                                                 column(4, sliderInput("Crop3rate", "For Crop-3", -20, 20, 0, step = 0.001))
                                                               ),
                                                               fluidRow(
                                                                 column(4, sliderInput("Livestockrate", "For Livestock", -20, 20, -0.005, step = 0.001)),
                                                                 column(4, sliderInput("Residentialrate", "For Residential area", -20, 20, 0.05, step = 0.001)),
                                                                 column(4, sliderInput("Industrialrate", "For Industrial area", -20, 20, 0.04, step = 0.001)))
                                                           )
                                                    )

                                                
                                              )
                                              ),
                                              hr(),
                                              fluidRow(
                                                column(12,
                                                       p(HTML("<b>Upload Input Data File</b>"),span(shiny::icon("info-circle"), id = "info_X2"),fileInput("InputDataFile", "", accept = ".csv"),
                                                       tippy::tippy_this(elementId = "info_X2", tooltip = "Refer to the CSV file template in the manual", placement = "right")
                                                       
                                              ))),
                                              
                                              fluidRow(
                                                box(title = "Water", width = 4, solidHeader = T, status = "danger", height = 750,


                                                           fluidRow(
                                                             column(3, numericInput('Crop1WaterDemand', HTML(paste0("Crop-1 Water Demand (m",tags$sup("3"), '/ton)')), 1432)),
                                                             column(3, numericInput('Crop2WaterDemand', HTML(paste0("Crop-2 Water Demand (m",tags$sup("3"), '/ton)')), 0)),
                                                             column(3, numericInput('Crop3WaterDemand', HTML(paste0("Crop-3 Water Demand (m",tags$sup("3"), '/ton)')), 0))
                                                           ),
                                                           fluidRow(
                                                             column(3, numericInput('LSWaterDemand', HTML(paste0("Livestock Water Demand (m",tags$sup("3"),')')), 1000)),
                                                             column(3, numericInput('IndWaterDemand', HTML(paste0("Industrial Water Demand (m",tags$sup("3"),')')), 5000)),
                                                             column(3, numericInput('DomWaterDemand', HTML(paste0("Domestic Water Demand (m",tags$sup("3"), '/capita)')), 55))
                                                           ),
                                                           fluidRow(tags$head(tags$style(HTML('
.box {margin-top: 2px;margin-left: 5px; margin-right: 0px; margin-bottom:2px;padding:20px}'
                                                           ))),
                                                           
                                                           sliderInput("EWIntensity", HTML(paste0("Water for Energy (m",tags$sup("3"), '/MWh)')), 0, 100, 13, step = 0.1)),
                                                           fluidRow(
                                                               sliderInput("DrinkingWaterAccess", "Population fraction with access to drinking water", 0, 1, 0.44, step = 0.01))
                                                           
                                                    
                                                ),
                                                box(title = "Energy", width = 4, solidHeader = T, status = "info", height = 750,


                                                           fluidRow(
                                                             column(3, numericInput('Crop1EnergyDemand', "Crop-1 Energy Demand (kWh/ton)", 533)),
                                                             column(3, numericInput('Crop2EnergyDemand', "Crop-2 Energy Demand (kWh/ton)", 0)),
                                                             column(3, numericInput('Crop3EnergyDemand', "Crop-3 Energy Demand (kWh/ton)", 0))
                                                           ),
                                                           fluidRow(
                                                             column(3, numericInput('LSEnergyDemand', "Livestock Energy Demand (MWh)", 100)),
                                                             column(3, numericInput('IndEnergyDemand', "Industrial Energy Demand (MWh)", 200)),
                                                             column(3, numericInput('DomEnergyDemand', "Domestic Energy Demand (kWh/capita)", 634))
                                                           ),
                                                           fluidRow(
                                                             sliderInput("WEIntensity", HTML(paste0("Energy for Water (MWH/m",tags$sup("3"), ')')), 0, 10, 0.37, step = 0.01)),
                                                           fluidRow(
                                                             sliderInput("ElectricityAccess", "Population fraction with access to electricity", 0, 1, 0.99, step = 0.01)),
                                                           fluidRow(
                                                             sliderInput("HHFuelAccess", "Population fraction with access to household fuel", 0, 1, 0.90, step = 0.01))
                                                           
                                                    ),
                                                box(title = "Food", width = 4, solidHeader = T, status = "warning", height = 750,
                                                  

                                                    
                                                           fluidRow(
                                                             column(6, numericInput('DomFoodDemand', "Domestic Food Demand (kcal/capita)", 0.118))
                                                             
                                                           ),
                                                   
                                                           fluidRow(
                                                             sliderInput("Crop1coeff", "Crop-1 Coefficient (kcal/g)", 0, 10, 3.66, step = 0.01)),
                                                           fluidRow(
                                                             sliderInput("Crop2coeff", "Crop-2 Coefficient (kcal/g)", 0, 10, 0, step = 0.01)),
                                                           fluidRow(
                                                             sliderInput("Crop3coeff", "Crop-3 Coefficient (kcal/g)", 0, 10, 0, step = 0.01)),
                                                           fluidRow(
                                                             sliderInput("FoodAccess", "Population fraction with access to food", 0, 1, 0.90, step = 0.01))
                                                                                  
                                                    ),
                                              fluidRow(
                                                  box(title = tags$div(
                                                    style = "padding-left: 15px;",
                                                    "Simulation variables"),width = 12,
                                                      column(4, tags$div(
                                                        style = "padding-left: 15px;",
                                                        numericInput('YearStart', "Start Year", 2010))),
                                                      column(4, numericInput('YearEnd', "End Year", 2030)),
                                                      column(4, tags$div(
                                                        style = "padding-top: 28px;",
                                                        actionButton("btn_model", "Run Model", style = "background-color: teal;font-size: 18px; height: 40px; width: 200px;")))
                                                  # column(3, numericInput('result', "Result",""))
                                                ))
                                                
                                              ),
                                              fluidRow(
                                                box(width = 12, solidHeader = T, status = "primary",
                                                    column(4,selectInput("plot_select", "Select a variable to display", 
                                                                         choices = c("Total Water Demand", "Total Water Supply",
                                                                                     "Total Energy Demand", "Total Energy Supply",
                                                                                     "Total Food Demand", "Total Food Supply",
                                                                                     "Water Security Index","Energy Security Index","Food Security Index",
                                                                                     "Water-Energy-Food Security Index",
                                                                                     "Per Capita Water Availability",
                                                                                     "Per Capita Energy Availability",
                                                                                     "Per Capita Food Availability",
                                                                                     "Self-Sufficiency Level for Water",
                                                                                     "Self-Sufficiency Level for Energy",
                                                                                     "Self-Sufficiency Level for Food"
                                                                                     ))),
                                                    column(8,align = "left", tags$div(
                                                      
                                                      style = "padding-top: 20px;",
                                                      actionButton("savePDF", "Download results", style = "height: 40px; width: 200px"),
                                                      
                                                    )),
                                                
                                                    )),
                                              fluidRow(
                                                box(title = HTML("<span style='color: teal;'><b>RESULTS</b></span>"), width = 12, solidHeader = T, status = "primary",
                                                    uiOutput("plotlegend")),
                                                box(width =12,
                                                    column(6,plotOutput("SelectedPlot")
                                                           ),
                                                    column(6,tableOutput("SelectedTable")
                                                        )
                                                    )
                                              )

                                            )
                                   )
                        )
)
)

##Specify units for each output variable##
get_units <- function(variable_name) {
  units <- switch(variable_name,
                  "Total Water Demand" = paste0("(m", "\u00B3", ")"),
                  "Total Water Supply" = paste0("(m", "\u00B3", ")"),
                  "Total Energy Demand" = "(MWh)",
                  "Total Energy Supply" = "(MWh)",
                  "Total Food Demand" = "(kCal)",
                  "Total Food Supply" = "(kCal)",
                  "Water Security Index" = "",
                  "Energy Security Index" = "",
                  "Food Security Index" = "",
                  "Water-Energy-Food Security Index" = "",
                  "Per Capita Water Availability" = paste0("(m", "\u00B3", "/person)"),
                  "Per Capita Energy Availability" = "(MWh/person)",
                  "Per Capita Food Availability" = "(kCal/person)",
                  "Self-Sufficiency Level for Water" = "",
                  "Self-Sufficiency Level for Energy" = "",
                  "Self-Sufficiency Level for Food" = "")
  return(units)
}

##Function to generate the output plots##
generate_plot <- function(DataOut, converted_variable, variable_name) {
   units <- get_units(variable_name) 
   if (variable_name %in% c("Water Security Index", "Food Security Index", "Energy Security Index","Water-Energy-Food Security Index")) {
     # Add color variable based on value
     DataOut$color <- ifelse(DataOut[[converted_variable()]] <= 1, "#CC0000",
                             ifelse(DataOut[[converted_variable()]] <= 2, "#CE6937",
                                    ifelse(DataOut[[converted_variable()]] <= 3, "#CC6600",
                                           ifelse(DataOut[[converted_variable()]] <= 4, "#999900", "#336600"))))
     ggplot(DataOut, aes(x = Year, y = .data[[converted_variable()]], color = color)) +
       geom_point() + ylim(1, 5) +
       labs(title = paste(variable_name, units, sep = ""), 
            x = "Year", y = paste(variable_name, units, sep = ""))  +
       scale_color_identity() +  # Use specified colors
       theme_bw() +
       theme(plot.title = element_text(size = 20, face = "bold"),
             axis.title = element_text(size = 16),
             axis.text = element_text(size = 14)) 


     } else {
     # Set default color for other variable names
     DataOut$color <- "blue"
    ggplot(DataOut, aes(x = Year, y = .data[[converted_variable()]], color = color)) +
    geom_point() +
    labs(title = paste(variable_name, units, sep = " "), 
         x = "Year", y = paste(variable_name, units, sep = " "))  +
    scale_color_identity() +  # Use specified colors
    theme_bw() +
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))
     }
}

##Function to generate the output tables##
generate_table <- function(DataOut, converted_variable,variable_name) {
  colnames(DataOut)[2] <- paste(variable_name, get_units(variable_name), sep = "")
  
  DataOut <- DataOut[, c("Year", converted_variable())]
  colnames(DataOut)[2] <- paste(variable_name, get_units(variable_name), sep = "")
  DataOut
  
}

##Server Module##
server <- function(input, output, session) {
  
  ##Downloading the instruction manual##
  output$downloadDocx <- downloadHandler(
    filename = "WefSys_Model_Documentation.docx",
    content = function(file) {
      file.copy("www/WefSys_Model_Documentation.docx", file)
    }
  )

  ##Specify legend for WEF secuiry index plots##
  output$plotlegend <- renderUI({
    if (input$plot_select %in% c("Water Security Index", "Food Security Index", "Energy Security Index","Water-Energy-Food Security Index")) {
      plotPath <- "IndexLegend.png"  # Path to the legend image
      
      tagList(
        img(src = plotPath, width = "40%", height = "auto")
      )
    } else {
      NULL
    }
  })
  
  ##Read uploaded CSV file##
  InputDatafunc <- reactive({
    req(input$InputDataFile)
    read.csv(input$InputDataFile$datapath, header = TRUE)
  })
  
  ##Convert variable names for printing in the report##
  converted_variable <- reactive({
  switch(input$plot_select,
           "Total Water Demand" = "Total_water_demand",
           "Total Water Supply" = "Total_water_supply",
         "Total Energy Demand"= "Total_energy_demand",
         "Total Energy Supply" ="Total_energy_supply",
         "Total Food Demand" = "Total_food_demand",
         "Total Food Supply" ="Total_food_supply",
         "Water Security Index" = "Water_security",
         "Energy Security Index" = "Energy_security",
         "Food Security Index" = "Food_security",
         "Water-Energy-Food Security Index" = "WEF_security",
        "Per Capita Water Availability" = "APP_water",
        "Per Capita Energy Availability" = "APP_energy",
        "Per Capita Food Availability" = "APP_food",
        "Self-Sufficiency Level for Water" = "SSL_water",
        "Self-Sufficiency Level for Energy" = "SSL_energy",
        "Self-Sufficiency Level for Food" = "SSL_food")
  })
  
  ##Simulate and run the model##
  observeEvent(input$btn_model, {
    parms <- c(TotalArea = input$TotalArea,
               Crop1EnergyDemand = input$Crop1EnergyDemand, 
               Crop2EnergyDemand = input$Crop2EnergyDemand,
               Crop3EnergyDemand = input$Crop3EnergyDemand, 
               LSEnergyDemand = input$LSEnergyDemand, 
               DomEnergyDemand = input$DomEnergyDemand, 
               IndEnergyDemand = input$IndEnergyDemand, 
               Crop1WaterDemand =input$Crop1WaterDemand, 
               Crop2WaterDemand = input$Crop2WaterDemand,
               Crop3WaterDemand = input$Crop3WaterDemand,
               LSWaterDemand = input$LSWaterDemand,
               DomWaterDemand = input$DomWaterDemand,
               IndWaterDemand  =input$IndWaterDemand,
               DomFoodDemand = input$DomFoodDemand, 
               WEIntensity = input$WEIntensity,
               EWIntensity = input$EWIntensity,
               Crop1coeff = input$Crop1coeff, 
               Crop2coeff = input$Crop2coeff, 
               Crop3coeff = input$Crop3coeff,
               DrinkingWaterAccess= input$DrinkingWaterAccess,
               ElectricityAccess = input$ElectricityAccess, 
               FoodAccess = input$FoodAccess,
               HHFuelAccess  =input$HHFuelAccess,
               BirthRate =input$BirthRate,
               DeathRate =input$DeathRate,
               ImmigrationRate =input$ImmigrationRate,
               OutmigrationRate =input$OutmigrationRate,
               Crop1rate =input$Crop1rate,
               Crop2rate =input$Crop2rate,
               Crop3rate =input$Crop3rate,
               Livestockrate = input$Livestockrate,
               Residentialrate =input$Residentialrate,
               Industrialrate =input$Industrialrate,
               YearStart = input$YearStart,
               YearEnd =input$YearEnd)
    Y <- c(Population = input$Population,
           Crop1Area = input$Crop1Area, 
           Crop2Area = input$Crop2Area, 
           Crop3Area = input$Crop3Area, 
           LivestockArea = input$LivestockArea, 
           ResidentialArea = input$ResidentialArea,
           IndustrialArea = input$IndustrialArea, 
           Total_water_demand = 0, 
           Total_water_supply = 0,
           Total_energy_demand = 0,
           Total_energy_supply = 0,
           Total_food_demand = 0, 
           Total_food_supply = 0,
           Water_security = 0, 
           Energy_security = 0,
           Food_security = 0,
           WEF_security = 0,
           APP_water = 0, 
           APP_energy = 0, 
           APP_food = 0, 
           SSL_water = 0, 
           SSL_energy = 0, 
           SSL_food = 0)

    output$SelectedPlot <- renderPlot({
      req(InputDatafunc())
      InputData <- InputDatafunc()
      
      out <- WEFmodel(Y,parms, InputData)
      DataOut <- as.data.frame(out)
      years <- seq(input$YearStart, input$YearEnd, by = 1)
      DataOut <- data.frame(Year = years, DataOut)
      for (col in 8:ncol(DataOut)) {
        DataOut[[col]] <- round(DataOut[[col]], 2)
      }
      variable_name <- input$plot_select
      generate_plot(DataOut, converted_variable, variable_name)

  })
  output$SelectedTable <- renderTable({
    req(InputDatafunc())
    InputData <- InputDatafunc()
    out <- WEFmodel(Y,parms,InputData)

    DataOut <- as.data.frame(out)
    years <- seq(input$YearStart, input$YearEnd, by = 1)
    roundedYears <- round(years, digits = 0)
    DataOut <- data.frame(Year = roundedYears, DataOut)
    DataOut$Year <- format(DataOut$Year, scientific = FALSE, trim = TRUE)
    for (col in 8:ncol(DataOut)) {
      DataOut[[col]] <- round(DataOut[[col]], 2)
    }
    variable_name <- input$plot_select
    generate_table(DataOut, converted_variable,variable_name)
  }, bordered = TRUE)
  
  ##Downloading PDF report##
  observeEvent(input$savePDF, {
    showModal(
      modalDialog(
        textInput("filename", "Enter a filename:", value = "WEFSys_Results.pdf"),
        #textInput("folderpath", "Enter the folder path:"),
        easyClose = TRUE,
        footer = tagList(
          downloadButton("downloadConfirm", "Download")
        )
      )
    )
  })

  output$downloadConfirm <- downloadHandler(
    filename = function() {
      input$filename
    },
    content = function(file) {
        pdf(file, width = 8.5, height = 11)
        mainTitle <- "Customized Report for WEFSys Model"
        description <- "This report contains all the plots and tables generated from running the WEF nexus model."
        titleGrob <- textGrob(mainTitle, gp = gpar(fontsize = 18, fontface = "bold"))
        descriptionGrob <- textGrob(description, gp = gpar(fontsize = 12))
        grid.arrange(titleGrob, descriptionGrob, ncol = 1)
        req(InputDatafunc())
        InputData <- InputDatafunc()
        out <- WEFmodel(Y, parms, InputData)
        DataOut <- as.data.frame(out)
        years <- seq(input$YearStart, input$YearEnd, by = 1)
        roundedYears <- round(years, digits = 0)
        DataOut <- data.frame(Year = roundedYears, DataOut)
        DataOut$Year <- format(DataOut$Year, scientific = FALSE, trim = TRUE)
        DataOut$Year <- as.numeric(DataOut$Year)
        for (col in 8:ncol(DataOut)) {
          DataOut[[col]] <- round(DataOut[[col]], 2)
        }
        
        
        variableMapping <- list(
          "Total_water_demand" = list(name = paste0("Total Water Demand (m", "\u00B3", ")")),
          "Total_water_supply" = list(name = paste0("Total Water Supply (m", "\u00B3", ")")),
          "Total_energy_demand" = list(name = "Total Energy demand (MWh)"),
          "Total_energy_supply" = list(name = "Total Energy Supply (MWh)"),
          "Total_food_demand" = list(name = "Total Food Demand (kCal)"),
          "Total_food_supply" = list(name = "Total Food Supply (kCal)"),
          "Water_security" = list(name = "Water Security Index"),
          "Energy_security" = list(name = "Energy Security Index"),
          "Food_security" = list(name = "Food Security Index"),
          "WEF_security" = list(name = "Water-Energy-Food Security Index"),
          "APP_water" = list(name = paste0("Per Capita Water Availability (m", "\u00B3", "/person)")),
          "APP_energy" = list(name = "Per Capita Energy Availability (MWh/person)"),
          "APP_food" = list(name = "Per Capita Food Availability (kCal/person) "),
          "SSL_water" = list(name = "Self-Sufficiency Level for Water"),
          "SSL_energy" = list(name = "Self-Sufficiency Level for Energy"),
          "SSL_food" = list(name = "Self-Sufficiency Level for Food")
        )
        
        for (variable in colnames(DataOut[,9:24])) {
          if (variable %in% names(variableMapping)) {
            varName <- variableMapping[[variable]]
          } else {
            varName <- variable  # Default to the original variable name if not in the mapping
          }
          
          plotTitle <- paste(varName)
          yLabel <- paste(varName)
          plot <- ggplot(DataOut, aes(x = Year, y = .data[[variable]])) +
            geom_point(color = "blue") +
            labs(x = "Year", y = yLabel) +
            scale_x_continuous(breaks = seq(min(DataOut$Year), max(DataOut$Year), by = 2))+
            theme_bw() +
            theme(plot.title = element_text(size = 12, face = "bold"),
                  axis.title = element_text(size = 10),
                  axis.text = element_text(size = 8))
          table <- DataOut[, c("Year", variable)]
          plot_grob <- ggplotGrob(plot)
          table_grob <- tableGrob(table, theme = ttheme_default(
            core = list(fg_params = list(cex = 0.6)))) 
          plotTitleGrob <- textGrob(plotTitle, gp = gpar(fontsize = 14, fontface = "bold"))
          
          
          grid.arrange(plotTitleGrob,plot_grob,table_grob, ncol = 1, heights = c(0.1, 0.7,1))
        }
        dev.off()
        showModal(
          modalDialog(
            "Download Completed",
            easyClose = TRUE,
            footer = tagList(
              actionButton("closeModal", "Close")
            )
          )
        )
      
    })
  })
  observeEvent(input$closeModal, {
    removeModal()
  })
    
}

shinyApp(ui, server)