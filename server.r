
pkg <- c("shiny", "caret", "htmltools")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}


library(shiny)
shinyServer(
  function(input, output) {
    output$prediction <- renderPrint({
      
      if (input$lookup == 0) {
        return()
      }
      
      #disable('input$loopup')
      
      withProgress(message = "Loading Machine Learning Model"
                   , value = 0
                   , detail = 'Downloading training data', {
                     
                    # Load the trained model
                     load("flightDelayLogisticRegModel.RData")
                     library(caret)
                     
                     #   Predict using trained model against test data
                     
                     prediction <- function(DAY_OF_WEEK, CARRIER, DEST, ORIGIN, DEP_TIME_BLK) {
                       predDF <- data.frame(cbind(DAY_OF_WEEK, CARRIER, DEST, ORIGIN, DEP_TIME_BLK))
                       intPred <- predict(logisticRegModel, predDF)
                       if (intPred == '1.00') {
                         "DELAY OVER 15 MINS"
                       }
                       else {
                         "ON TIME"
                       }
                     }
                     prediction(input$dayOfWeek
                                , input$carrier
                                , input$dest
                                , input$origin
                                , input$departTime)
                     
                   })
      
    })
  }
)