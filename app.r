library(shiny)
library(visNetwork)
library(shinydashboard)
###
# Deploy with concsole command:
# rsconnect::deployApp('.')
###

#Import Data
traffic <- read.csv("csvs/sim-ROADS-N100-hv500at5_0.5_0.0_3_0-av500at0_1_1000_0.csv")
traffic <-  subset(traffic,select = -c(1))
randomSeed = 0
                    
# Shiny Server
server <- function(input, output) {
    output$network <- renderVisNetwork({

        nodes <- data.frame(id = 1:9,
                    # add labels on nodes
                    label = paste("Node", 1:9))

        edges <- data.frame(from = c(1,2,1,2,3,4,5,4,5,6,7,8), to = c(2,3,4,5,6,5,6,7,8,9,8,9), width = as.numeric(traffic[input$day,])/30)

        visNetwork(nodes, edges, height = "500px", width = "100%") %>% visLayout(randomSeed = 0)
    })
}

# Shiny UI
ui <- dashboardPage(
    dashboardHeader(title = "Traffic On Each Day"),
    dashboardSidebar(sliderInput("day", "Day:",
                                 min = 1, max = 500,
                                 value = 1,
                                 step = 1,
                                 animate = animationOptions(interval = 250, loop = TRUE)
                                 ),
                     menuItem("Return to Site", icon = icon("file-code-o"), 
                              href = "https://tubestubes.github.io")
                     ),
    dashboardBody(visNetworkOutput("network"))
    )
    
# Shiny App    
shinyApp(ui = ui, server = server)


# Non-Dashbaord UI 
#ui <- fluidPage(
#    sliderInput("day", "Day:",
#                min = 1, max = 500,
#                value = 1,
#                step = 1,
#                animate = animationOptions(interval = 250, loop = TRUE)),
#    visNetworkOutput("network")






