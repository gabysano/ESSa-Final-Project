#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This Shiny app is made to analyze authigenic carbonate and water samples that
# were collected from the Dinaric Alps in the summer of 2022. It allows the user 
# to choose from ten basins and choose whether to plot d18O or d13C against 
# elevation and distance. In addition, it also plots the relationship between
# d18O and dD in the water samples and the relationship between d18O and d13C 
# in the carbonates.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# My data is divided into two different csv files:
carbs <- read.csv("Carbonates.csv", header = T)
water <- read.csv("Water.csv", header = T)

# Changes the name of the Sample.ID col in the 
# water df to just ID to match with the carbs df
water %>%
  rename("ID" = "Sample.ID")

# Combines the individual isotope concentrations into one column and 
# makes a concentration column for its corresponding measurement
# i.e. makes a wide dataset into a narrow dataset
# also adds a column describing the data type so user can choose whether 
# to see just carbonates or just water 
carb_longer <- carbs %>%
  pivot_longer(cols = c("d18O", "d13C"),
               names_to = "Isotope",
               values_to = "Concentration") %>%
  mutate(dataType = "carb")

water_longer <- water %>%
  pivot_longer(cols = c("d18O", "dD"),
               names_to = "Isotope",
               values_to = "Concentration") %>%
  mutate(dataType = "water")


# Joins the two narrow datasets
all_data <- full_join(carb_longer, water_longer)

# Creates a list with all of our basins where we sampled carbonates
carbBasins <- c("Bugojno" = "B",
            "Bijele Vode" = "BV",
            "Greben" = "G",
            "Kupres" = "K",
            "Mandek" = "M",
            "Ostrožac" = "OZ",
            "Pag" = "PG",
            "Sinj" = "S",
            "Tušnica" = "TU")

# Creates a list with all of the places where we sampled water
waterSites <- c("Bugojno" = "BW",
                "Mandek" = "MW",
                "Ostrožac" = "OZW",
                "Pag" = "PGW",
                "Sinj" = "SW",
                "Kakanj" = "KAW",
                "Livno" = "LW")

# Defines UI for application that will show either carb or water analysis
ui <- fluidPage(

    # Application title
    titlePanel("Stable isotope composition of the Dinaric Alps"),

    # Sidebar with a selection of whether to show carbonate or water data 
    sidebarLayout(
        sidebarPanel(
            p("This Shiny app is made to analyze authigenic carbonate and water 
              samples that were collected from the Dinaric Alps in the summer 
              of 2022. It allows the user to choose from ten basins and choose 
              whether to plot d18O or d13C against elevation and distance. In 
              addition, it also plots the relationship between d18O and dD in 
              the water samples and the relationship between d18O and d13C in
              the carbonates."),
            
            selectInput(inputId = "dataType", label = "Data Type",
                        c(Carbonate = "carb", 
                          Water = "water")),
            
    # Only shows this panel if carb data is selected        
            conditionalPanel(
              condition = "input.dataType == 'carb'",
              
              checkboxGroupInput(inputId = "ID", label = "Basin:",
                          choices = carbBasins,
                          selected = carbBasins),
              
              selectInput(inputId = "Isotope", label = "Isotope",
                        choices = c("d18O", "d13C"))
              ),
    
    # Only shows this panel if water data is selected
            conditionalPanel(
              condition = "input.dataType == 'water'",
              
              checkboxGroupInput(inputId = "Sample.ID", label = "Site:",
                                 choices = waterSites,
                                 selected = waterSites),
              
              selectInput(inputId = "Isotope", label = "Isotope",
                          choices = c("d18O", "dD"))
              )
    ),
            
        # Show a plot of the chosen basin
        mainPanel(
          plotOutput("relationshipPlot"),
          plotOutput("elevationPlot"),
          plotOutput("distancePlot")
        )
  )
)

# Define server logic required to draw various plots based on conditions
server <- function(input, output) {
  
  # first plot will be a scatter plot of d13C and d18O (if carb is chosen or
  # of d18O and dD (if water is chosen)
  output$relationshipPlot <- renderPlot({
    if (input$dataType == "carb") {
      
      filteredCarb <- carbs %>% 
        filter(ID == input$ID)

      ggplot(filteredCarb, aes(x = d13C, y = d18O)) +
        geom_point(aes(bg = ID), shape = 21, size = 5) +
        ylab(expression(paste(delta^13,"C (‰, VPDB)"))) +
        xlab(expression(paste(delta^18,"O (‰, VPDB)"))) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              panel.border = element_rect(fill=NA,color = "black"),
              axis.text = element_text(size=16),
              axis.title = element_text(size=20))
    } else {
      
      filteredWater <- water %>%
        filter(Sample.ID == input$Sample.ID)
      
      ggplot(filteredWater, aes(x = d18O, y = dD)) +
        geom_point(aes(bg = Sample.ID), shape = 21, size = 5) +
        ylab(expression(paste(delta,"D (‰, VSMOW)"))) +
        xlab(expression(paste(delta^18,"O (‰, VSMOW)"))) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              panel.border = element_rect(fill=NA,color = "black"),
              axis.text = element_text(size=16),
              axis.title = element_text(size=20))
    }
  }
  )
  
  # second  plot will be a scatter plot of d13C or d18O (if carb is chosen) 
  # against elevation or of d18O or dD (if water is chosen) against elevation
  output$elevationPlot <- renderPlot({
    if (input$dataType == "carb") {
      
      filteredCarbElev <- carb_longer %>%
        filter(ID == input$ID,
               Isotope == input$Isotope)
      
      ggplot(filteredCarbElev, aes(x = Elevation, y = Concentration)) +
        geom_point(aes(bg = ID), shape = 21, size = 5) +
        ylab(ifelse(input$Isotope == "d18O",
                    expression(paste(delta^18, "O (‰, VPDB)")),
                    expression(paste(delta^13, "C (‰, VPDB)")))) +
        xlab(expression("Elevation (m)")) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              panel.border = element_rect(fill=NA,color = "black"),
              axis.text = element_text(size=16),
              axis.title = element_text(size=20))
    } else {
      
      filteredWaterElev <- water_longer %>%
        filter(Sample.ID == input$Sample.ID,
               Isotope == input$Isotope)
      
      ggplot(filteredWaterElev, aes(x = Elevation, y = Concentration)) +
        geom_point(aes(bg = Sample.ID), shape = 21, size = 5) +
        ylab(ifelse(input$Isotope == "d18O",
                    expression(paste(delta^18, "O (‰, VSMOW)")),
                    expression(paste(delta, "D (‰, VSMOW)")))) +
        xlab(expression("Elevation (m)")) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              panel.border = element_rect(fill=NA,color = "black"),
              axis.text = element_text(size=16),
              axis.title = element_text(size=20))
    }
  }
  )
  
  # third  plot will be a scatter plot of d13C or d18O (if carb is chosen) 
  # against distance from coast or of d18O or dD (if water is chosen) against
  # distance from coast
  output$distancePlot <- renderPlot({
    if (input$dataType == "carb") {
      
      filteredCarbDist <- carb_longer %>%
        filter(ID == input$ID,
               Isotope == input$Isotope)
      
      ggplot(filteredCarbDist, aes(x = Distance.to.coast, y = Concentration)) +
        geom_point(aes(bg = ID), shape = 21, size = 5) +
        ylab(ifelse(input$Isotope == "d18O",
                    expression(paste(delta^18, "O (‰, VPDB)")),
                    expression(paste(delta^13, "C (‰, VPDB)")))) +
        xlab(expression("Distance to coast (km)")) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              panel.border = element_rect(fill=NA,color = "black"),
              axis.text = element_text(size=16),
              axis.title = element_text(size=20))
    } else {
      
      filteredWaterDist <- water_longer %>%
        filter(Sample.ID == input$Sample.ID,
               Isotope == input$Isotope)
      
      ggplot(filteredWaterDist, aes(x = Distance.to.coast, y = Concentration)) +
        geom_point(aes(bg = Sample.ID), shape = 21, size = 5) +
        ylab(ifelse(input$Isotope == "d18O",
                    expression(paste(delta^18, "O (‰, VSMOW)")),
                    expression(paste(delta, "D (‰, VSMOW)")))) +
        xlab(expression("Distance to coast (km)")) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              panel.border = element_rect(fill=NA,color = "black"),
              axis.text = element_text(size=16),
              axis.title = element_text(size=20))
      }
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)

