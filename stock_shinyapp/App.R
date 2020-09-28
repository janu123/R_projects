library(shiny)
library(TTR)
library(quantmod)
library(caret)
library(fscaret)
library(kernlab)
library(gmodels)
library(nnet)
library(NeuralNetTools)
library(neuralnet)
#source("getFullData.R")
source("modTrain.R")
source("helpers.R")
Logged = FALSE;
my_username <- "test"
my_password <- "test"

ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2 <- function(){tagList(tabPanel("Test"))}

ui = (htmlOutput("page"))
server = (function(input, output,session) {
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        navbarPage("Stock Market Trend Prediction and Analysis Tool",
                   tabPanel("Stock Prediction",
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Select a stock to examine. Information will be collected 
                                         from yahoo finance, wikitrend, google, and so forth."),
                                textInput(inputId = "symb", label = "Please enter the name of stock:", value = "AAPL"),
                                textInput(inputId = "related", label = "Please enter related terms:", value = "iPhone, iPad, Macbook"),
                                dateInput(inputId = "date", label = "Please choose the date: ", value = Sys.Date()),
                                actionButton(inputId = "go", label = "Update")
                                ),
                              mainPanel(
                                h3(textOutput("text")),
                                h1(textOutput("result")),
                                h3(helpText("\nHistorical Data from Yahoo Finance")),
                                plotOutput("tempPlotYahoo"),
                                h4(helpText("We have trained 4 different kernels of Supported Vector Machine, 
                                    based on the result of our paper. 
                                    And the best model we chose for this prediction is as follows:")),
                                verbatimTextOutput("modSummary")
                              )
                            )
                   ),
                   tabPanel("Yahoo Finance Trend",
                            plotOutput("yahooPlot"),
                            verbatimTextOutput("yahooSummary")),
                   tabPanel("Neural Network",
                            verbatimTextOutput("nnresult"),
                            verbatimTextOutput("nnresult1")
                  
                   )
                   
        ) 
        })
    }
      })
      print(ui)
      yahooData = eventReactive(input$go, {
        getSymbols(input$symb, src = "yahoo", from = input$date - 730, to = input$date, auto.assign = FALSE)
      })
      
      output$tempPlotYahoo = renderPlot({
        chartSeries(yahooData(), type = "line")
      })
      output$yahooPlot = renderPlot({
        chartSeries(yahooData(), type = "line")
      })
      output$yahooSummary = renderPrint({
        tail(yahooData())
      })
     
      output$modSummary = renderPrint({
        bestMod = getBestModel(yahooData())
        print(bestMod)
      })
      
      output$text = renderText({
        paste("We predict the price of ", input$symb, " on the next trading day would be\n", sep = "")
      })
      output$result = renderPrint({
        getPred(yahooData())
      })
      output$nnresult = renderPrint({
        model=getNNmodel(yahooData())
        print(model)
      })
      output$nnresult1 =renderPrint({
        model=getNNmodel1(yahooData())
        print(model)
      })
})    





shinyApp(ui = ui, server = server)
