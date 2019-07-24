library(shiny)
library(png)
library(shinyBS)
library(shinyDND)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(leaps)

# Little tips for R 
# Ctrl + F ------------ quick way to find the key word
# Ctrl + shif + C ----- convenient way to block out the code you don't need

value <- reactiveValues(index = 1)
c<-reactiveValues(list=3)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
##########################action buttons##################################### 
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      type = NULL,
      closeOnClickOutside = TRUE,
      text = " Choose different methods to understand variable selection."
    )
   })
  
  observeEvent(input$hint,{
    sendSweetAlert(
      session = session,
      title = "Hints:",
      type = NULL,
      closeOnClickOutside = TRUE,
      text = " Each layer is a model which contain all the variable black blocks represent. The deeper color the more precise this model is."
      
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

#################################Gray out buttons###############################
  
  observeEvent(input$restart, {
      value$index <- 1
      if(input$nfactor == 3){
        c$list = sample(1:2,1,replace = TRUE)
      }
      else if(input$nfactor == 4){
        c$list = sample(3:4,1,replace = TRUE)
      }
      else if(input$nfactor == 5){
        c$list = sample(5:6,1,replace = TRUE)
      }
      else if(input$nfactor == 6){
        c$list = sample(7:8,1,replace = TRUE)
      }
      else if(input$nfactor == 7){
        c$list = sample(9:10,1,replace = FALSE)
      }
      else if(input$nfactor == 8){
        c$list = 11
      }
  })
  
#################################### exploring #####################################
  # hi, can you find out a way to simplify the code below? 
  # For example, when I put in a new data, the code will automaticlly change the plot and corresponding answer?
  # Thank you so much!

  
  #####################input#########################
  observeEvent(input$refresh, {
    
    x1 <- rnorm(200,3,1.5)
    x2 <- rnorm(200,10,2)
    x3 <- rnorm(200,0,.2)
    x4 <- rnorm(200,100,40)
    x5 <- rnorm(200,0,1)
    x6 <- rnorm(200,54,27)
    x7 <- rnorm(200,25,7.5)
    x8 <- rnorm(200,10,4)
    Y  <- 0.2*x1**0.6+0.3*x2**0.8+0.5*x3**0.3+0.05*x8+50
    
    data1 <- data.frame(Y,x1,x2,x3)
    data2 <- data.frame(Y,x3,x4,x8)
    data3 <- data.frame(Y,x2,x5,x6,x7)
    data4 <- data.frame(Y,x1,x5,x7,x8)
    data5 <- data.frame(Y,x2,x3,x4,x5,x6)
    data6 <- data.frame(Y,x1,x3,x4,x7,x8)
    data7 <- data.frame(Y,x1,x3,x4,x6,x7,x8)
    data8 <- data.frame(Y,x1,x2,x3,x4,x5,x6)
    data9 <- data.frame(Y,x2,x3,x4,x5,x6,x7,x8)
    data10 <- data.frame(Y,x1,x2,x4,x5,x6,x7,x8)
    data11 <- data.frame(Y,x1,x2,x3,x4,x5,x6,x7,x8)
    
    
    best1 <- regsubsets(Y~., data=data1, nbest=3)
    null1 <- lm(Y~1, data=data1)
    full1 <- lm(Y~., data=data1)
    best2 <- regsubsets(Y~., data=data2, nbest=3)
    null2 <- lm(Y~1, data=data2)
    full2 <- lm(Y~., data=data2)
    best3 <- regsubsets(Y~., data=data3, nbest=3)
    null3 <- lm(Y~1, data=data3)
    full3 <- lm(Y~., data=data3)
    best4 <- regsubsets(Y~., data=data4, nbest=3)
    null4 <- lm(Y~1, data=data4)
    full4 <- lm(Y~., data=data4)
    best5 <- regsubsets(Y~., data=data5, nbest=3)
    null5 <- lm(Y~1, data=data5)
    full5 <- lm(Y~., data=data5)
    best6 <- regsubsets(Y~., data=data6, nbest=3)
    null6 <- lm(Y~1, data=data6)
    full6 <- lm(Y~., data=data6)
    best7 <- regsubsets(Y~., data=data7, nbest=3)
    null7 <- lm(Y~1, data=data7)
    full7 <- lm(Y~., data=data7)
    best8 <- regsubsets(Y~., data=data8, nbest=3)
    null8 <- lm(Y~1, data=data8)
    full8 <- lm(Y~., data=data8)
    best9 <- regsubsets(Y~., data=data9, nbest=3)
    null9 <- lm(Y~1, data=data9)
    full9 <- lm(Y~., data=data9)
    best10 <- regsubsets(Y~., data=data10, nbest=3)
    null10 <- lm(Y~1, data=data10)
    full10 <- lm(Y~., data=data10)
    best11 <- regsubsets(Y~., data=data11, nbest=3)
    null11 <- lm(Y~1, data=data11)
    full11 <- lm(Y~., data=data11)

  output$plots <- renderPlot({
    if(c$list == 1){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best1, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best1, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best1, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null1, scope=list(lower=null1, upper=full1), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full1, data=data1, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null1, scope = list(upper=full1), data=data1, direction="both"))
      }
    }
    
    if(c$list == 2){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best2, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best2, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best2, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null2, scope=list(lower=null2, upper=full2), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full2, data=data2, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null2, scope = list(upper=full2), data=data2, direction="both"))
      }
    }
    
    if(c$list == 3){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best3, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best3, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best3, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null3, scope=list(lower=null3, upper=full3), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full3, data=data3, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null3, scope = list(upper=full3), data=data3, direction="both"))
      }
    }
    
    if(c$list == 4){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best4, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best4, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best4, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null4, scope=list(lower=null4, upper=full4), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full4, data=data4, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null4, scope = list(upper=full4), data=data4, direction="both"))
      }
    }
    
    if(c$list == 5){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best5, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best5, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best5, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null5, scope=list(lower=null5, upper=full5), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full5, data=data5, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null5, scope = list(upper=full5), data=data5, direction="both"))
      }
    }
    
    if(c$list == 6){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best6, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best6, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best6, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null6, scope=list(lower=null6, upper=full6), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full6, data=data6, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null6, scope = list(upper=full6), data=data6, direction="both"))
      }
    }
    
    if(c$list == 7){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best7, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best7, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best7, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null7, scope=list(lower=null7, upper=full7), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full7, data=data7, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null7, scope = list(upper=full7), data=data7, direction="both"))
      }
    }
    
    if(c$list == 8){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best8, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best8, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best8, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null8, scope=list(lower=null8, upper=full8), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full8, data=data6, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null8, scope = list(upper=full8), data=data8, direction="both"))
      }
    }
    
    if(c$list == 9){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best9, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best9, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best9, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null9, scope=list(lower=null9, upper=full9), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full9, data=data9, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null9, scope = list(upper=full9), data=data9, direction="both"))
      }
    }
    
    if(c$list == 10){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best10, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best10, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best10, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null10, scope=list(lower=null10, upper=full10), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full10, data=data10, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null10, scope = list(upper=full10), data=data10, direction="both"))
      }
    }
    
    if(c$list == 11){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best11, scale="adjr2",main = "Adjusted R-Squared",xlab="adjustment R-squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best11, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best11, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null11, scope=list(lower=null11, upper=full11), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full11, data=data11, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null11, scope = list(upper=full11), data=data11, direction="both"))
      }
    }
    
    })
  
  })

  output$answer <- renderText({
    if(c$list == 1){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 2){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 3){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 4){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1,X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 5){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 6){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 7){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 8){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 9){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 10){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1,X2,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1,X2,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1,X2,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 11){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1,X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1,X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1,X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1,X2,X3,X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
  })
###closing for SERVER DON'T DELET####      
})