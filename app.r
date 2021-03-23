library(shiny)
library(visNetwork)
library(shinydashboard)
###
# Deploy with concsole command:
# rsconnect::deployApp('.')
###

# Params
params = list(hv = 500,
               av = 500,
               N = 100,
               hv_err = 5,
               hv_theta = .5,
               hv_beta = .5,
               hv_len = 3 ,
               hv_atis_bais = 0 ,
               av_err = 0,
               av_theta = 1,
               av_len = 1000 , 
               av_atis_bias = 0
            )
randomSeed = 0
                    
# Shiny Server
server <- function(input, output) {
    
    output$network <- renderVisNetwork({
        
        
        data = paste("csvs/sim-ROADS-N",params$N,"-hv",params$hv,"at",params$hv_err,"_",params$hv_theta,"_",params$hv_beta,"_",params$hv_len,"_",params$hv_atis_bais,"-av",params$av,"at",params$av_err,"_",params$av_theta,"_",params$av_len,"_",params$av_atis_bias,".csv", sep = "")
        traffic <- read.csv(data) 
        traffic <-  subset(traffic,select = -c(1))

        nodes <- data.frame(id = 1:9,
                    label = paste("Node", 1:9))

        edges <- data.frame(from = c(1,2,1,2,3,4,5,4,5,6,7,8), to = c(2,3,4,5,6,5,6,7,8,9,8,9), width = as.numeric(traffic[input$day,])/30)

        visNetwork(nodes, edges, height = "500px", width = "100%") %>% visLayout(randomSeed = 0)
        })
    
    output$selection <- renderUI({
        radioButtons("vals", "Select Data",
                 choices = switch(input$expr,
                                  "HV/AV Ratio" = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
                                  "HV Info" = c("(0.1,0)", "(0,0)" , "(0,0.5)" ,"(0.5,0)", "(0.5,0.5)"),
                                  "HV Theta" = c(0.01, 0.05, 0.1, .2, .3, .4, .5, 1, 2, 3),
                                  "HV Memory" = c(1, 2, 3, 4, 10, 30, 50, 70, 100, 250),
                                  "AV Theta" = c(0.01, 0.05, 0.1, .2, .3, .4, .5, 1, 2, 3),
                                  "AV Memory" =c(1, 2, 3, 4, 10, 30, 50, 70, 100, 250)
                                  ),
                 selected = 0)
        })

}

# Shiny UI
ui <- dashboardPage(
    dashboardHeader(title = "Traffic On Each Day"),
    dashboardSidebar(menuItem("Return to Site", icon = icon("file-code-o"), 
                              href = "https://tubestubes.github.io")
                     ),
    dashboardBody(selectInput(inputId = "expr", label = strong("Select Experiment"),
                              choices = c("HV/AV Ratio","HV Info", "HV Theta", "HV Memory", "AV Theta", "AV Memory"),
                              selected = "HV/AV Ratio"),
                  uiOutput("selection"),
                  visNetworkOutput("network"),
                  sliderInput("day", "Day:",
                             min = 1, max = 500,
                             value = 1,
                             step = 1,
                             animate = animationOptions(interval = 250, loop = TRUE)
                     )
                 )
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






