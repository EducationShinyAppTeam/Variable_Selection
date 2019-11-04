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
      text = " Choose different methods to understand variable selection.
               If the 'change'button does not work well, please keep pressing the button."
    )
   })
  
  observeEvent(input$hint,{
    sendSweetAlert(
      session = session,
      title = "Hints:",
      type = NULL,
      closeOnClickOutside = TRUE,
      text = " Each row designates a model which contains all of the variables shown in black blocks.The deeper the color, the more precise this model is.[adjr2 is the abbreviation of adjusted R-squared]"
      
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
        c$list = sample(1:10,1,replace = TRUE)
      }
      else if(input$nfactor == 4){
        c$list = sample(11:20,1,replace = TRUE)
      }
      else if(input$nfactor == 5){
        c$list = sample(21:30,1,replace = TRUE)
      }
      else if(input$nfactor == 6){
        c$list = sample(31:40,1,replace = TRUE)
      }
      else if(input$nfactor == 7){
        c$list = sample(41:48,1,replace = FALSE)
      }
      else if(input$nfactor == 8){
        c$list = 49
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
    
    ################# model: 3 variable ######################   
    data1 <- data.frame(Y,x1,x2,x3)
    data2 <- data.frame(Y,x3,x4,x8)
    data3 <- data.frame(Y,x1,x2,x4)
    data4 <- data.frame(Y,x2,x3,x8)
    data5 <- data.frame(Y,x2,x3,x6)
    data6 <- data.frame(Y,x3,x4,x7)
    data7 <- data.frame(Y,x3,x5,x6)
    data8 <- data.frame(Y,x2,x4,x6)
    data9 <- data.frame(Y,x1,x5,x8)
    data10 <- data.frame(Y,x1,x5,x7)
    
    ################# model: 4 variable ######################      
    data11 <- data.frame(Y,x2,x5,x6,x7)
    data12 <- data.frame(Y,x1,x5,x7,x8)
    data13 <- data.frame(Y,x1,x2,x3,x8)
    data14 <- data.frame(Y,x1,x4,x6,x7)
    data15 <- data.frame(Y,x2,x3,x7,x8)
    data16 <- data.frame(Y,x2,x4,x5,x6)
    data17 <- data.frame(Y,x1,x2,x6,x8)
    data18 <- data.frame(Y,x2,x3,x4,x5)
    data19 <- data.frame(Y,x3,x6,x7,x8)
    data20 <- data.frame(Y,x3,x4,x5,x6)
    
    ################# model: 5 variable ######################   
    data21 <- data.frame(Y,x2,x3,x4,x5,x6)
    data22 <- data.frame(Y,x1,x3,x4,x7,x8)
    data23 <- data.frame(Y,x1,x2,x3,x6,x8)
    data24 <- data.frame(Y,x1,x3,x5,x6,x7)
    data25 <- data.frame(Y,x2,x3,x4,x5,x6)
    data26 <- data.frame(Y,x2,x5,x6,x7,x8)
    data27 <- data.frame(Y,x3,x4,x5,x7,x8)
    data28 <- data.frame(Y,x3,x5,x6,x7,x8)
    data29 <- data.frame(Y,x3,x4,x6,x7,x8)
    data30 <- data.frame(Y,x1,x2,x4,x5,x6)
    
    ################# model: 6 variable ######################   
    data31 <- data.frame(Y,x1,x3,x4,x6,x7,x8)
    data32 <- data.frame(Y,x1,x2,x3,x4,x5,x6)
    data33 <- data.frame(Y,x1,x2,x4,x5,x6,x7)
    data34 <- data.frame(Y,x1,x3,x4,x5,x7,x8)
    data35 <- data.frame(Y,x1,x3,x4,x5,x6,x7)
    data36 <- data.frame(Y,x2,x3,x4,x6,x7,x8)
    data37 <- data.frame(Y,x2,x4,x5,x6,x7,x8)
    data38 <- data.frame(Y,x2,x3,x5,x6,x7,x8)
    data39 <- data.frame(Y,x2,x3,x4,x5,x6,x7)
    data40 <- data.frame(Y,x3,x4,x5,x6,x7,x8)
    
    ################# model: 7 variable ######################   
    data41 <- data.frame(Y,x2,x3,x4,x5,x6,x7,x8)
    data42 <- data.frame(Y,x1,x3,x4,x5,x6,x7,x8)
    data43 <- data.frame(Y,x1,x2,x4,x5,x6,x7,x8)
    data44 <- data.frame(Y,x1,x2,x3,x5,x6,x7,x8)
    data45 <- data.frame(Y,x1,x2,x3,x4,x6,x7,x8)
    data46 <- data.frame(Y,x1,x2,x3,x4,x5,x7,x8)
    data47 <- data.frame(Y,x1,x2,x3,x4,x5,x7,x8)
    data48 <- data.frame(Y,x1,x2,x3,x4,x5,x6,x7)

    ################# model: 8 variable ######################   
    data49 <- data.frame(Y,x1,x2,x3,x4,x5,x6,x7,x8)
    
    
    ################# regression: 3 variable ######################    
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
    ################# regression: 4 variable ###################### 
    best11 <- regsubsets(Y~., data=data11, nbest=3)
    null11 <- lm(Y~1, data=data11)
    full11 <- lm(Y~., data=data11)
    best12 <- regsubsets(Y~., data=data12, nbest=3)
    null12 <- lm(Y~1, data=data12)
    full12 <- lm(Y~., data=data12)
    best13 <- regsubsets(Y~., data=data13, nbest=3)
    null13 <- lm(Y~1, data=data13)
    full13 <- lm(Y~., data=data13)
    best14 <- regsubsets(Y~., data=data14, nbest=3)
    null14 <- lm(Y~1, data=data14)
    full14 <- lm(Y~., data=data14)
    best15 <- regsubsets(Y~., data=data15, nbest=3)
    null15 <- lm(Y~1, data=data15)
    full15 <- lm(Y~., data=data15)
    best16 <- regsubsets(Y~., data=data16, nbest=3)
    null16 <- lm(Y~1, data=data16)
    full16 <- lm(Y~., data=data16)
    best17 <- regsubsets(Y~., data=data17, nbest=3)
    null17 <- lm(Y~1, data=data17)
    full17 <- lm(Y~., data=data17)
    best18 <- regsubsets(Y~., data=data18, nbest=3)
    null18 <- lm(Y~1, data=data18)
    full18 <- lm(Y~., data=data18)
    best19 <- regsubsets(Y~., data=data19, nbest=3)
    null19 <- lm(Y~1, data=data19)
    full19 <- lm(Y~., data=data19)    
    best20 <- regsubsets(Y~., data=data20, nbest=3)
    null20 <- lm(Y~1, data=data20)
    full20 <- lm(Y~., data=data20)
    ################# regression: 5 variable ###################### 
    best21 <- regsubsets(Y~., data=data21, nbest=3)
    null21 <- lm(Y~1, data=data21)
    full21 <- lm(Y~., data=data21)
    best22 <- regsubsets(Y~., data=data22, nbest=3)
    null22 <- lm(Y~1, data=data22)
    full22 <- lm(Y~., data=data22)
    best23 <- regsubsets(Y~., data=data23, nbest=3)
    null23 <- lm(Y~1, data=data23)
    full23 <- lm(Y~., data=data23)
    best24 <- regsubsets(Y~., data=data24, nbest=3)
    null24 <- lm(Y~1, data=data24)
    full24 <- lm(Y~., data=data24)
    best25 <- regsubsets(Y~., data=data25, nbest=3)
    null25 <- lm(Y~1, data=data25)
    full25 <- lm(Y~., data=data25)
    best26 <- regsubsets(Y~., data=data26, nbest=3)
    null26 <- lm(Y~1, data=data26)
    full26 <- lm(Y~., data=data26)
    best27 <- regsubsets(Y~., data=data27, nbest=3)
    null27 <- lm(Y~1, data=data27)
    full27 <- lm(Y~., data=data27)
    best28 <- regsubsets(Y~., data=data28, nbest=3)
    null28 <- lm(Y~1, data=data28)
    full28 <- lm(Y~., data=data28)
    best29 <- regsubsets(Y~., data=data29, nbest=3)
    null29 <- lm(Y~1, data=data29)
    full29 <- lm(Y~., data=data29)
    best30 <- regsubsets(Y~., data=data30, nbest=3)
    null30 <- lm(Y~1, data=data30)
    full30 <- lm(Y~., data=data30)
    ################# regression: 6 variable ###################### 
    best31 <- regsubsets(Y~., data=data31, nbest=3)
    null31 <- lm(Y~1, data=data31)
    full31 <- lm(Y~., data=data31)
    best32 <- regsubsets(Y~., data=data32, nbest=3)
    null32 <- lm(Y~1, data=data32)
    full32 <- lm(Y~., data=data32)
    best33 <- regsubsets(Y~., data=data33, nbest=3)
    null33 <- lm(Y~1, data=data33)
    full33 <- lm(Y~., data=data33)
    best34 <- regsubsets(Y~., data=data34, nbest=3)
    null34 <- lm(Y~1, data=data34)
    full34 <- lm(Y~., data=data34)
    best35 <- regsubsets(Y~., data=data35, nbest=3)
    null35 <- lm(Y~1, data=data35)
    full35 <- lm(Y~., data=data35)
    best36 <- regsubsets(Y~., data=data36, nbest=3)
    null36 <- lm(Y~1, data=data36)
    full36 <- lm(Y~., data=data36)
    best37 <- regsubsets(Y~., data=data37, nbest=3)
    null37 <- lm(Y~1, data=data37)
    full37 <- lm(Y~., data=data37)
    best38 <- regsubsets(Y~., data=data38, nbest=3)
    null38 <- lm(Y~1, data=data38)
    full38 <- lm(Y~., data=data38)
    best39 <- regsubsets(Y~., data=data39, nbest=3)
    null39 <- lm(Y~1, data=data39)
    full39 <- lm(Y~., data=data39)
    best40 <- regsubsets(Y~., data=data40, nbest=3)
    null40 <- lm(Y~1, data=data40)
    full40 <- lm(Y~., data=data40)
    ################# regression: 7 variable ###################### 
    best41 <- regsubsets(Y~., data=data41, nbest=3)
    null41 <- lm(Y~1, data=data41)
    full41 <- lm(Y~., data=data41)
    best42 <- regsubsets(Y~., data=data42, nbest=3)
    null42 <- lm(Y~1, data=data42)
    full42 <- lm(Y~., data=data42)
    best43 <- regsubsets(Y~., data=data43, nbest=3)
    null43 <- lm(Y~1, data=data43)
    full43 <- lm(Y~., data=data43)
    best44 <- regsubsets(Y~., data=data44, nbest=3)
    null44 <- lm(Y~1, data=data44)
    full44 <- lm(Y~., data=data44)
    best45 <- regsubsets(Y~., data=data45, nbest=3)
    null45 <- lm(Y~1, data=data45)
    full45 <- lm(Y~., data=data45)
    best46 <- regsubsets(Y~., data=data46, nbest=3)
    null46 <- lm(Y~1, data=data46)
    full46 <- lm(Y~., data=data46)
    best47 <- regsubsets(Y~., data=data47, nbest=3)
    null47 <- lm(Y~1, data=data47)
    full47 <- lm(Y~., data=data47)
    best48 <- regsubsets(Y~., data=data48, nbest=3)
    null48 <- lm(Y~1, data=data48)
    full48 <- lm(Y~., data=data48)
    ################# regression: 8 variable ###################### 
    best49 <- regsubsets(Y~., data=data49, nbest=3)
    null49 <- lm(Y~1, data=data49)
    full49 <- lm(Y~., data=data49)

  output$plots <- renderPlot({
    
    ################# plot: 3 variable ###################### 
    if(c$list == 1){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best1,scale="adjr2",main = "Adjusted R-Squared")
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
        plot(best3, scale="adjr2",main = "Adjusted R-Squared")
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
        plot(best4, scale="adjr2",main = "Adjusted R-Squared")
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
        plot(best5, scale="adjr2",main = "Adjusted R-Squared")
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
        plot(best6, scale="adjr2",main = "Adjusted R-Squared")
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
        plot(best7, scale="adjr2",main = "Adjusted R-Squared")
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
        plot(best8, scale="adjr2",main = "Adjusted R-Squared")
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
        plot(step(full8, data=data8, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null8, scope = list(upper=full8), data=data8, direction="both"))
      }
    }
    
    if(c$list == 9){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best9, scale="adjr2",main = "Adjusted R-Squared")
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
        plot(best10, scale="adjr2",main = "Adjusted R-Squared")
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
    
    ################# plot: 4 variable ###################### 
    if(c$list == 11){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best11, scale="adjr2",main = "Adjusted R-Squared")
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
    
    if(c$list == 12){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best12, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best12, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best12, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null12, scope=list(lower=null12, upper=full12), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full12, data=data12, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null12, scope = list(upper=full12), data=data12, direction="both"))
      }
    }
    
    if(c$list == 13){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best13, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best13, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best13, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null13, scope=list(lower=null13, upper=full13), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full13, data=data13, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null13, scope = list(upper=full13), data=data13, direction="both"))
      }
    }
    
    if(c$list == 14){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best14, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best14, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best14, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null14, scope=list(lower=null14, upper=full14), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full14, data=data14, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null14, scope = list(upper=full14), data=data14, direction="both"))
      }
    }
    
    if(c$list == 15){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best15, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best15, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best15, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null15, scope=list(lower=null15, upper=full15), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full15, data=data15, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null15, scope = list(upper=full15), data=data15, direction="both"))
      }
    }
    
    if(c$list == 16){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best16, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best16, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best16, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null16, scope=list(lower=null16, upper=full16), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full16, data=data16, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null16, scope = list(upper=full16), data=data16, direction="both"))
      }
    }
    
    if(c$list == 17){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best17, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best17, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best17, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null17, scope=list(lower=null17, upper=full17), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full17, data=data17, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null17, scope = list(upper=full17), data=data17, direction="both"))
      }
    }
    
    if(c$list == 18){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best18, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best18, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best18, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null18, scope=list(lower=null18, upper=full18), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full18, data=data18, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null18, scope = list(upper=full18), data=data18, direction="both"))
      }
    }
    
    if(c$list == 19){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best19, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best19, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best19, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null19, scope=list(lower=null19, upper=full19), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full19, data=data19, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null19, scope = list(upper=full19), data=data19, direction="both"))
      }
    }
    
    if(c$list == 20){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best20, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best20, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best20, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null20, scope=list(lower=null20, upper=full20), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full20, data=data20, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null20, scope = list(upper=full20), data=data20, direction="both"))
      }
    }
    
    ################# plot: 5 variable ###################### 
    if(c$list == 21){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best21, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best21, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best21, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null21, scope=list(lower=null21, upper=full21), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full21, data=data21, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null21, scope = list(upper=full21), data=data21, direction="both"))
      }
    }
    
    if(c$list == 22){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best22, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best22, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best22, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null22, scope=list(lower=null22, upper=full22), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full22, data=data22, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null22, scope = list(upper=full22), data=data22, direction="both"))
      }
    }
    
    if(c$list == 23){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best23, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best23, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best23, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null23, scope=list(lower=null23, upper=full23), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full23, data=data23, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null23, scope = list(upper=full23), data=data23, direction="both"))
      }
    }
    
    if(c$list == 24){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best24, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best24, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best24, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null24, scope=list(lower=null24, upper=full24), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full24, data=data24, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null24, scope = list(upper=full24), data=data24, direction="both"))
      }
    }
    
    if(c$list == 25){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best25, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best25, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best25, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null25, scope=list(lower=null25, upper=full25), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full25, data=data25, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null25, scope = list(upper=full25), data=data25, direction="both"))
      }
    }
    
    if(c$list == 26){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best26, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best26, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best26, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null26, scope=list(lower=null26, upper=full26), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full26, data=data26, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null26, scope = list(upper=full26), data=data26, direction="both"))
      }
    }
    
    if(c$list == 27){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best27, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best27, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best27, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null27, scope=list(lower=null27, upper=full27), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full27, data=data27, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null27, scope = list(upper=full27), data=data27, direction="both"))
      }
    }
    
    if(c$list == 28){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best28, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best28, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best28, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null28, scope=list(lower=null28, upper=full28), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full28, data=data28, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null28, scope = list(upper=full28), data=data28, direction="both"))
      }
    }
    
    if(c$list == 29){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best29, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best29, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best29, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null29, scope=list(lower=null29, upper=full29), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full29, data=data29, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null29, scope = list(upper=full29), data=data29, direction="both"))
      }
    }
    
    if(c$list == 30){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best30, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best30, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best30, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null30, scope=list(lower=null30, upper=full30), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full30, data=data30, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null30, scope = list(upper=full30), data=data30, direction="both"))
      }
    }
    
    ################# plot: 6 variable ###################### 
    if(c$list == 31){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best31, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best31, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best31, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null31, scope=list(lower=null31, upper=full31), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full31, data=data31, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null31, scope = list(upper=full31), data=data31, direction="both"))
      }
    }
    
    if(c$list == 32){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best32, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best32, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best32, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null32, scope=list(lower=null32, upper=full32), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full32, data=data32, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null32, scope = list(upper=full32), data=data32, direction="both"))
      }
    }
    
    if(c$list == 33){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best33, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best33, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best33, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null33, scope=list(lower=null33, upper=full33), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full33, data=data33, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null33, scope = list(upper=full33), data=data33, direction="both"))
      }
    }
    
    if(c$list == 34){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best34, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best34, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best34, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null34, scope=list(lower=null34, upper=full34), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full34, data=data34, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null34, scope = list(upper=full34), data=data34, direction="both"))
      }
    }
    
    if(c$list == 35){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best35, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best35, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best35, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null35, scope=list(lower=null35, upper=full35), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full35, data=data35, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null35, scope = list(upper=full35), data=data35, direction="both"))
      }
    }
    
    if(c$list == 36){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best36, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best36, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best36, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null36, scope=list(lower=null36, upper=full36), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full36, data=data36, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null36, scope = list(upper=full36), data=data36, direction="both"))
      }
    }
    
    if(c$list == 37){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best37, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best37, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best37, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null37, scope=list(lower=null37, upper=full37), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full37, data=data37, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null37, scope = list(upper=full37), data=data37, direction="both"))
      }
    }
    
    if(c$list == 38){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best38, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best38, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best38, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null38, scope=list(lower=null38, upper=full38), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full38, data=data38, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null38, scope = list(upper=full38), data=data38, direction="both"))
      }
    }
    
    if(c$list == 39){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best39, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best39, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best39, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null39, scope=list(lower=null39, upper=full39), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full39, data=data39, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null39, scope = list(upper=full39), data=data39, direction="both"))
      }
    }
    
    if(c$list == 40){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best40, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best40, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best40, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null40, scope=list(lower=null40, upper=full40), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full40, data=data40, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null40, scope = list(upper=full40), data=data40, direction="both"))
      }
    }
    
    ################# plot: 7 variable ###################### 
    if(c$list == 41){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best41, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best41, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best41, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null41, scope=list(lower=null41, upper=full41), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full41, data=data41, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null41, scope = list(upper=full41), data=data41, direction="both"))
      }
    }
    
    if(c$list == 42){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best42, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best42, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best42, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null42, scope=list(lower=null42, upper=full42), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full42, data=data42, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null42, scope = list(upper=full42), data=data42, direction="both"))
      }
    }
    
    if(c$list == 43){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best43, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best43, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best43, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null43, scope=list(lower=null43, upper=full43), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full43, data=data43, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null43, scope = list(upper=full43), data=data43, direction="both"))
      }
    }
    
    if(c$list == 44){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best44, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best44, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best44, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null44, scope=list(lower=null44, upper=full44), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full44, data=data44, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null44, scope = list(upper=full44), data=data44, direction="both"))
      }
    }
    
    if(c$list == 45){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best45, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best45, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best45, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null45, scope=list(lower=null45, upper=full45), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full45, data=data45, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null45, scope = list(upper=full45), data=data45, direction="both"))
      }
    }
    
    if(c$list == 46){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best46, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best46, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best46, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null46, scope=list(lower=null46, upper=full46), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full46, data=data46, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null46, scope = list(upper=full46), data=data46, direction="both"))
      }
    }
    
    if(c$list == 47){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best47, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best47, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best47, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null47, scope=list(lower=null47, upper=full47), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full47, data=data47, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null47, scope = list(upper=full47), data=data47, direction="both"))
      }
    }
    
    if(c$list == 48){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best48, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best48, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best48, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null48, scope=list(lower=null48, upper=full48), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full48, data=data48, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null48, scope = list(upper=full48), data=data48, direction="both"))
      }
    }
    
    ################# plot: 8 variable ###################### 
    if(c$list == 49){
      if(input$model== "Adjusted R-Squared criterion"){
        plot(best49, scale="adjr2",main = "Adjusted R-Squared")
      }
      else if(input$model == "Cp criterion"){
        plot(best49, scale="Cp", main = "Cp criterion")
      }
      else if(input$model == "BIC criterion"){
        plot(best49, scale="bic", main = "BIC criterion")
      }
      else if(input$model == "Forward Selection"){
        plot(step(null49, scope=list(lower=null49, upper=full49), direction="forward"))
      }
      else if(input$model == "Backward Selection"){
        plot(step(full49, data=data49, direction="backward"))
      }
      else if(input$model == "Stepwise Selection"){
        plot(step(null49, scope = list(upper=full49), data=data49, direction="both"))
      }
    }
    
    })
  
  })

  output$answer <- renderText({
    ############################ True Answer text: 3 variables####################################
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
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 4){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 5){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 6){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 7){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 8){
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
    
    else if(c$list == 9){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 10){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    ############################ True Answer text: 4 variables####################################
    else if(c$list == 11){
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
    
    else if(c$list == 12){
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
    
    else if(c$list == 13){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1,X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1,X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1,X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1,X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1,X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1,X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 14){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 15){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 16){
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
    
    else if(c$list == 17){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 18){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 19){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 20){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X3 . You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X3 . You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X3 . You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X3 . You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X3 . You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    ############################ True Answer text: 5 variables####################################
    else if(c$list == 21){
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
    
    else if(c$list == 22){
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
    
    else if(c$list == 23){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 24){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 25){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 26){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 27){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 28){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 29){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 30){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    ############################ True Answer text: 6 variables####################################
    else if(c$list == 31){
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
    
    else if(c$list == 32){
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
    
    else if(c$list == 33){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1 and X2. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 34){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 35){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1 and X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 36){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 37){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 38){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 39){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
    } 
    
    else if(c$list == 40){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    ############################ True Answer text: 7 variables####################################
    else if(c$list == 41){
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
    
    else if(c$list == 42){
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
    
    else if(c$list == 43){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1, X2 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 44){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 45){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 46){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 47){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1, X2, X3 and X8. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    else if(c$list == 48){
      if(input$model== "Adjusted R-Squared criterion"){
        paste("The true model for this data set is the model which contains X1, X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Cp criterion"){
        paste("The true model for this data set is the model which contains X1, X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "BIC criterion"){ 
        paste("The true model for this data set is the model which contains X1, X2 and X3. You can compare the true model with the result that you interpret from graph. ")
      }
      else if(input$model== "Forward Selection"){
        paste("The true model for this data set is the model which contains X1, X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Backward Selection"){
        paste("The true model for this data set is the model which contains X1, X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
      else if(input$model== "Stepwise Selection"){
        paste("The true model for this data set is the model which contains X1, X2 and X3. You can compare the true model with the result that you interpret from graph.")
      }
    }
    
    ############################ True Answer text: 8 variables####################################
    else if(c$list == 49){
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