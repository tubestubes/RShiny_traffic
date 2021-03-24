library(shiny)
library(visNetwork)
library(shinydashboard)
###
# Deploy with concsole command:
# rsconnect::deployApp('.')
###
randomSeed = 0
                    
# Shiny Server
server <- function(input, output) {
    
    exprLables <- reactive({switch(input$expr,
                                   "HV/AV Ratio" = "#HV - #AV",
                                   "ATIS Bias" = "(HV ATIS, AV ATIS)",
                                   "HV Rationality" = "AV Theta",
                                   "HV Memory" = "HV Memory Length",
                                   "AV Rationality" = "AV Theta",
                                   "AV Memory" = "AV HV Memory Length"
                                 )})
    exprNames <- reactive({switch(input$expr,
                             "HV/AV Ratio" = c("HV 0 - AV 1000", "HV 100 - AV 900", "HV 200 - AV 800", "HV 300 - AV 700", "HV 400 - AV 600", "HV 500 - AV 500", "HV 600 - AV 400", "HV 700 - AV 300", "HV 800 - AV 200", "HV 900 - AV 100", "HV 1000 - AV 0"),
                             "ATIS Bias" = c("(0.1,0)", "(0,0)" , "(0,0.5)" ,"(0.5,0)", "(0.5,0.5)"),
                             "HV Rationality" = c(0.01, 0.05, 0.1, .2, .3, .4, .5, 1, 2, 3),
                             "HV Memory" = c(1, 2, 3, 4, 10, 30, 50, 70, 100, 250),
                             "AV Rationality" = c(0.01, 0.05, 0.1, .2, .3, .4, .5, 1, 2, 3),
                             "AV Memory" =c(1, 2, 3, 4, 10, 30, 50, 70, 100, 250)
                            )})
    
    output$selection <- renderUI({
        selectInput("vals", paste("Select Data:", exprLables()),
                 choices = exprNames(),
                 selected = 0)
        })

    params = reactive({
        switch(input$expr,
                "HV/AV Ratio" = list(
                    hv = switch(input$vals,
                                "HV 0 - AV 1000" = 0,
                                "HV 100 - AV 900" = 100, 
                                "HV 200 - AV 800" = 200, 
                                "HV 300 - AV 700" = 300, 
                                "HV 400 - AV 600" = 400, 
                                "HV 500 - AV 500" = 500, 
                                "HV 600 - AV 400" = 600, 
                                "HV 700 - AV 300" = 700,
                                "HV 800 - AV 200" = 800,
                                "HV 900 - AV 100" = 900, 
                                "HV 1000 - AV 0" = 1000),
                     av = switch(input$vals,
                               "HV 0 - AV 1000"= 1000,
                               "HV 100 - AV 900"= 900, 
                               "HV 200 - AV 800"= 800, 
                               "HV 300 - AV 700"= 700, 
                               "HV 400 - AV 600"= 600, 
                               "HV 500 - AV 500"= 500, 
                               "HV 600 - AV 400"= 400, 
                               "HV 700 - AV 300"= 300,
                               "HV 800 - AV 200"= 200,
                               "HV 900 - AV 100"= 100, 
                               "HV 1000 - AV 0" = 0),
                    N = 500,
                    hv_err = 5,
                    hv_theta = .5,
                    hv_beta = .5,
                    hv_len = 3 ,
                    hv_atis_bais = 0 ,
                    av_err = 0,
                    av_theta = 1,
                    av_len = 1000 , 
                    av_atis_bias = 0
                    ),
                "ATIS Bias" = list(
                    hv = 500,
                    av = 500,
                    N = 500,
                    hv_err = 5,
                    hv_theta = .5,
                    hv_beta = .5,
                    hv_len = 3 ,
                    hv_atis_bais = switch(input$vals,
                                          "(0.1,0)" = .1,
                                          "(0,0)" = 0,
                                          "(0,0.5)" = 0,
                                          "(0.5,0)" = .5, 
                                          "(0.5,0.5)" = .5,
                                          ),
                    av_err = 0,
                    av_theta = 1,
                    av_len = 1000 , 
                    av_atis_bias = switch(input$vals,
                                          "(0.1,0)" = 0,
                                          "(0,0)" = 0,
                                          "(0,0.5)" = 0.5,
                                          "(0.5,0)" = 0, 
                                          "(0.5,0.5)" = .5,
                                           )
                    ),
                "HV Rationality" = list(
                    hv = 500,
                    av = 500,
                    N = 500,
                    hv_err = 5,
                    hv_theta = input$vals,
                    hv_beta = .5,
                    hv_len = 3 ,
                    hv_atis_bais = 0 ,
                    av_err = 0,
                    av_theta = 1,
                    av_len = 1000 , 
                    av_atis_bias = 0
                    ),
                "HV Memory" =list(
                    hv = 500,
                    av = 500,
                    N = 100,
                    hv_err = 5,
                    hv_theta = .5,
                    hv_beta = .5,
                    hv_len = input$vals ,
                    hv_atis_bais = 0 ,
                    av_err = 0,
                    av_theta = 1,
                    av_len = 1000 , 
                    av_atis_bias = 0
                    ),
                "AV Rationality" =list(
                    hv = 500,
                    av = 500,
                    N = 500,
                    hv_err = 5,
                    hv_theta = .5,
                    hv_beta = .5,
                    hv_len = 3 ,
                    hv_atis_bais = 0 ,
                    av_err = 0,
                    av_theta = input$vals,
                    av_len = 1000 , 
                    av_atis_bias = 0
                    ) ,
                "AV Memory" = list(
                    hv = 500,
                    av = 500,
                    N = 500,
                    hv_err = 5,
                    hv_theta = .5,
                    hv_beta = .5,
                    hv_len = 3 ,
                    hv_atis_bais = 0 ,
                    av_err = 0,
                    av_theta = 1,
                    av_len = input$vals , 
                    av_atis_bias = 0
                    )
                )
        })
    
    output$network <- renderVisNetwork({
        
        data = paste("csvs/sim-ROADS-N",params()$N,"-hv",params()$hv,"at",params()$hv_err,"_",params()$hv_theta,"_",params()$hv_beta,"_",params()$hv_len,"_",params()$hv_atis_bais,"-av",params()$av,"at",params()$av_err,"_",params()$av_theta,"_",params()$av_len,"_",params()$av_atis_bias,".csv", sep = "")
        traffic <- read.csv(data) 
        traffic <-  subset(traffic,select = -c(1))
        
        nodes <- data.frame(id = 1:9,
                            label = paste("Node", 1:9))
        
        edges <- data.frame(from = c(1,2,1,2,3,4,5,4,5,6,7,8), to = c(2,3,4,5,6,5,6,7,8,9,8,9), width = as.numeric(traffic[input$day,])/30)
        
        visNetwork(nodes, edges, height = "500px", width = "100%") %>% visLayout(randomSeed = 0)
    })
}

# Shiny UI
ui <- dashboardPage(
    dashboardHeader(title = "Master's Dissertation Data"),
    dashboardSidebar(menuItem("Return to Site", icon = icon("file-code-o"), 
                              href = "https://tubestubes.github.io")
                     ),
    
    dashboardBody(selectInput(inputId = "expr", label = strong("Select Experiment"),
                              choices = c("HV/AV Ratio","ATIS Bias", "HV Rationality", "HV Memory", "AV Rationality", "AV Memory"),
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






