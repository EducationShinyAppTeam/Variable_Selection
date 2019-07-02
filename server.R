library(shiny)
library(png)
library(shinyBS)
library(shinyDND)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(simstudy)
library(lubridate)
library(shinyalert)
library(shinyWidgets)
library(leaps)


#read in dataset

exid <- read.csv("VariableSelection.csv")
best <- regsubsets(Y~., data=exid, nbest=3) 
null <- lm(Y~1, data=exid)
full <- lm(Y~., data=exid)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  ##########################action buttons##################################### 
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = " Choose different methods to understand variable selection.",
      type = "info"
    )
  })
  
  observeEvent(input$hint,{
    sendSweetAlert(
      session = session,
      title = "Hints:",
      text = " Each layer is a model which contain all the variable black block represent. The deeper color the more precise this model is.",
      type = "info"
    )
  })
  
  observeEvent(input$pre,{
    updateTabItems(session,"tabs","prereq")
  })
    
  observeEvent(input$go,{
    updateTabItems(session,"tabs","explore")
  })
  
  observeEvent(input$go1,{
    updateTabItems(session,"tabs","explore")
  })

  # observeEvent(input$game1,{
  #   updateTabItems(session,"tabs","game")
  # })
  
  observeEvent(input$info2,{
    toggle("wow")
  })
  
  ############################Gray out buttons###############################
  
  observeEvent(input$start2, {
    updateButton(session, "answer", disabled = TRUE)
  })
  
  observeEvent(input$challenge, {
    updateButton(session, "answer", disabled = FALSE)
  })
  
  observeEvent(input$answer, {
    updateButton(session, "answer", disabled=TRUE)
  })
  
  observeEvent(input$begin, {
    updateButton(session, "submit", disabled = TRUE)
  })

####################### next question buttom ##############################
  
  observeEvent(input$nextq, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = TRUE)
    updateSelectInput(session,"answer","",c('Cp criterion','Adjusted R-Squared','bic criterion','Forward Selection','Backward Selection','Stepwise Selection'))
    output$result <- renderUI({
      
      h3("Choose the distribution from the list to match the given text, then click 'Submit' to check your answer.")
    })
    
    index_list$list=index_list$list[-1]   
    value$index <- index_list$list[1]
  })

########################### exploring #####################################
  
  output$plots <- renderPlot({   
    if(input$model== "Adjusted R-Squared criterion"){
      plot(best, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(input$model == "Cp criterion"){
      plot(best, scale="Cp", main = "Cp criterion")
    }
    else if(input$model == "BIC criterion"){
      plot(best, scale="bic", main = "BIC criterion")
    }
    else if(input$model == "Forward Selection"){
      plot(step(null, scope=list(lower=null, upper=full), direction="forward"))
    }
    else if(input$model == "Backward Selection"){
      plot(step(full, data=hw6, direction="backward"))
    }
    else if(input$model == "Stepwise Selection"){
      plot(step(null, scope = list(upper=full), data=hw6, direction="both"))
    }
  })
    
  output$download_data <- downloadHandler(
    filename = function() {
      paste("VariableSelection.csv")
    },
    
    content = function(file) { 
        write.csv(exid,file) 
      }
    )
  
  output$interpretation <- renderText({
    if(input$model== "Adjusted R-Squared criterion"){
      paste("test1")
    }
    else if(input$model== "Cp criterion"){
      paste("test2")
    }
    else if(input$model== "BIC criterion"){
      paste("test3")
    }
    else if(input$model== "Forward Selection"){
      paste("test4")
    }
    else if(input$model== "Backward Selection"){
      paste("test5")
    }
    else if(input$model== "Stepwise Selection"){
      paste("test6")
    }
  })
  
############################### Gaming #######################################

  
  
  
###closing for SERVER DON'T DELET####      
})