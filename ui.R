
library(shiny)
shinyUI(fluidPage(
  tags$head(tags$style(".shiny-progress {position: absolute; top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px !important; color: blue;font-size: 20px;font-style: italic;}")),
  headerPanel(
    h1('Flight Delay Predictive Machine Learning Model')
    ),
  sidebarPanel(
    numericInput('dayOfWeek', 'Day of Week: 1 = Monday, 7 = Sunday', 1, min = 1, max = 7, step = 1),
    selectInput('carrier', 'Carrier', choices = list("AA", "B6", "DL", "EV", "F9", "MQ", "NK", "OO", "UA", "US", "VX", "WN")),
    selectInput('dest', 'Destination Airport', choices = list("ATL", "CLT", "DFW", "JFK", "LAS", "LAX", "ORD", "PHX", "SFO")),
    selectInput('origin', 'Origin Airport', choices = list("ATL", "CLT", "DFW", "JFK", "LAS", "LAX", "ORD", "PHX", "SFO")),
    selectInput('departTime', 'Departure Time Block', choices = list("0001-0559", "0600-0659", "0700-0759", "0800-0859", "0900-0959", "1000-1059", "1100-1159", "1200-1259", "1300-1359", "1400-1459", "1500-1559", "1600-1659", "1700-1759", "1800-1859", "1900-1959", "2000-2059", "2100-2159", "2200-2259", "2300-2359")),
    actionButton('lookup', 'Submit')
  ),
  mainPanel(
   p('The following is using a machine learning model to predict flight delay.  The ML model is using linear regression and has been trained with data from United States Depart of Transportation '),
   h4('Training Model predicted:'),
   verbatimTextOutput("prediction"),
   h4('input 7, AA, JFK, ORD, 1900-1959 should return Delayed'),
   h4('input 7, AA, JFK, ORD, 1200-1259 should return On Time'),
   h4('Please be patient as the trained model needs to load after hitting Submit')
  )
))