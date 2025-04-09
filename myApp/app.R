#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(sf)
library(ggplot2)
data_placette <- read.csv("data_plots_2008_2022_all_v2.csv", header = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mistletoe in french forests"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        selectInput(
          "ssp",
          "Choose the mistletoe subspecies",
          choices = c("album", "abietis", "austriacum")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  select_data <- reactive({
    if(input$ssp == "album"){
      subdata <- data_placette[!(data_placette$u_txguialbum %in% c("X", "0")), c("npp", "xl", "yl", "u_txguialbum")]
      names(subdata)[4] <- "tx_gui"
      subdata_sf <- st_as_sf(subdata, coords = c("xl", "yl"), crs = 27572)
    }else if(input$ssp == "abietis"){
      subdata <- data_placette[!(data_placette$u_txguiabiet %in% c("X", "0")), c("npp", "xl", "yl", "u_txguiabiet")]
      names(subdata)[4] <- "tx_gui"
      subdata_sf <- st_as_sf(subdata, coords = c("xl", "yl"), crs = 27572)
    }else{
      subdata <- data_placette[!(data_placette$u_txguiaustr %in% c("X", "0")), c("npp", "xl", "yl", "u_txguiaustr")]
      names(subdata)[4] <- "tx_gui"
      subdata_sf <- st_as_sf(subdata, coords = c("xl", "yl"), crs = 27572)
    }
    return(subdata_sf)
  })
  
    output$distPlot <- renderPlot({
        data <- select_data()
        data$tx_gui <- as.numeric(data$tx_gui)
        # draw the histogram with the specified number of bins
        ggplot(data, aes(x=tx_gui))+
          geom_bar()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
