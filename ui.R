# ui

rm(list=ls())

library(shiny)
library(datasets)
library(shinydashboard)
library(rsconnect)

header <- dashboardHeader(
  title= "Optimal Stock Return"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Input Data", tabName= "input", icon= icon("hourglass-start")),
    menuItem("Expected Returns", tabName= "results1", icon= icon("check-square")),
    menuItem("Investment Results", tabName= "results2", icon= icon("check-square")),
    menuItem("Interpreting Results", tabName= "interpret", icon= icon("question")),
    menuItem("About", tabName= "about", icon= icon("address-card"))
  )
)

body <- dashboardBody(
  tabItems(
    # 1st tab: input data
    tabItem(tabName= "input",
            fluidRow(
              box(
                height= 150,
                textInput("coOne", "Enter Company's Ticker Symbol to Compare to Default SPY", 
                          value= "Enter symbol here...", width= NULL, placeholder= NULL),
                actionButton("refresh", "Refresh"),
                "Please wait, data takes awhile to load"
              ),
              box(
                "Instructions",
                height= 260,
                #textOutput("instructions"),
                tags$br(),
                "1. Enter the symbol of a company that you would like to compare against the SP500",
                tags$br(),
                "2. Click Refresh and then the Expected Returns or the Investment Results tabs to load the results",
                tags$br(), tags$br(),
                "Notes:", tags$br(),
                "> Default index is set to S&P500", tags$br(),
                "> Results are generated from data going back twenty years from today's date",
                tags$br(), tags$br(),
                "Last updated: 06/06/2018 7:43:05 PM" #manual change
                )
              )
            ),
    tabItem(tabName= "results1",
            fluidRow(
              box(title= "Returns from Market Data",
                  tableOutput("text2"), height= 200),
              box(title= "Returns from Monte Carlo Simulations",
                  tableOutput("textMC"), height= 200),
              box(plotOutput("plot1"), height= 450),
              #box(plotOutput("plot3"), height= 450),
              #box(plotOutput("plot2"), height= 450)
              tabBox(
                title= "RS vs FMK Model Fits", height= "450px",
                tabPanel(" ", plotOutput("plot3")),
                tabPanel("S&P500", plotOutput("plot2"))
                
              )
            )
            ),
    tabItem(tabName= "results2",
            fluidRow(
              #box(title= "Kelly Criterion Results",
              #    tableOutput("kellycrit"), height= 200),
              valueBoxOutput("kelly1"),
              valueBoxOutput("kelly2")
            )
    ),
    tabItem(tabName= "interpret",
            fluidRow(
              box(
                title= "Expected Returns",
                "This tab displays expected returns utilizing general lambda distribution techniques using 20 years of historical data and 
                Monte Carlo Simulations."
                
              ),
              box(
                title= "Investment Results",
                "This tab displays the distribution of wealth an individual should invest their wealth in based on the given
                stock and the S&P500 based on the Kelly Criterion methods. Each box displays a percentage to invest and underneath displays
                the related stock."
              ),
              box(
                title= "Returns from Market Data",
                "This table displays the expected return, variance, skewness, and kurtosis of the selected stock and the S&P500.
                The numbers were generated using historical data going back 20 years from now."
              ),
              box(
                title= "Returns from Monte Carlo Simulations",
                "This table displays the expected return, variance, skewness, and kurtosis of the selected stock and the S&P500.
                The numbers were generated from 10000000 simulations based on estimates created using the FMKL method with the historical data."
              ),
              box(
                title= "Fig 1: Adjusted Returns",
                "This graph displays the adjusted returns for the selected stock against the S&P500. It uses the adjusted returns given on 
                Yahoo finance and displays the logged and differenced returns."
              ),
              box(
                title= "Fig 2: RS vs FMKL",
                "There are two graphs that the user can toggle back and forth to look at it. Fig 2.1 displays the model fit for
                the RS and FMKL methods for the selected stock and Fig 2.2 displays the same information but with S&P500. These graphs
                show that FMKL for the most part, is the better fit and is therefore utilized in the Monte Carlo simulations of expected
                returns."
              ),
              box(
                title= "RS",
                "RS is a method of calculating expected returns using a generalized lambda distribution. RS is the abbreviation
                for the last names of the researchers who created this method - Ramberg and Schmeiser (1974)."
              ),
              box(
                title= "FMKL",
                "FMKL is a method of calculating expected returns using a generalized lambda distribution. FMKL is the abbreviation
                for the last names of the researchers who created this method - Freimer, Mudholkar, Kollia, and Lin (1988)."
              )
            )),
    tabItem(tabName= "about",
            fluidRow(
              box(
                "Project by Brandon Lofgren, Jonny Woolley, & Vania Tso for
                course GSE 544 under the direction of Professor Zambrano",
                tags$br(), tags$br(),
                "Cal Poly MSE Spring 2018"
                
              ),
              box("Additional documentation and source code can be found", tags$a(href= "https://github.com/vtso/zambrano", "here.")),
              box(
                title= "References:",
                "> On the Kelly Criterion: > https://www.investopedia.com/articles/trading/04/091504.asp", tags$br(),
                "> Background on RS and FMKL: > https://www.jstatsoft.org/article/view/v021i09/v21i09.pdf ", tags$br(),
                "> On Building the R Shiny App:", tags$br(),
                ">> https://rstudio.github.io/shinydashboard/structure.html", tags$br(),
                ">> https://fontawesome.com/icons?d=gallery", tags$br(),
                ">> https://shiny.rstudio.com/", tags$br(),
                ">> https://shiny.rstudio.com/articles/shinyapps.html"
              )
              
            ))
  )
)


# put it all together
ui <- dashboardPage(header, sidebar, body)

# test run app
#shinyApp(ui, server)

# deploy app
#deployApp("~/Desktop/RShiny-GSE544/")
