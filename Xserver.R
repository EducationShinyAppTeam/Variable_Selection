library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaps)


# Little tips for R 
# Ctrl + F ------------ quick way to find the key word
# Ctrl + shif + C ----- convenient way to block out the code you don't need

value <- reactiveValues(index = 1)
c<-reactiveValues(list = 3)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
##########################action buttons##################################### 
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      type = NULL,
      closeOnClickOutside = TRUE,
      text = " Choose the optimality criteria from the dropdown list of different methods.
Make your own choice about which model you think is best before showing the true model."
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
  
  observeEvent(input$prereqs,{
    print("test")
    updateTabItems(session=session,inputId = "pages", selected = "prereq")
    
  })
    
  observeEvent(input$explore,{
    updateTabItems(session=session,inputId = "pages",selected = "explore1")
    
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
  
  #reactive for how the user chooses the dataset they want to use
  
  
 
  
  #read in the dataset that they want to use
  # if statements for which they chose and then if statements for type of file if they are inputing their own
  
  
  
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
    Y  <- 0.2*x1 + 0.3*x2 + 0.5*x3 + 0.05*x8 + 50
    
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
  
    
  
    
  output$Aplot <- renderPlot({
    
    ################# plot: 3 variable ###################### 
    if(c$list == 1){
      plot(best1,scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 2){
      plot(best2, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 3){
      plot(best3, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 4){
      plot(best4, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 5){
      plot(best5, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 6){
      plot(best6, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 7){
      plot(best7, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 8){
      plot(best8, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 9){
      plot(best9, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 10){
      plot(best10, scale="adjr2",main = "Adjusted R-Squared")
    }
    
    ################# plot: 4 variable ###################### 
    else if(c$list == 11){
      plot(best11,scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 12){
      plot(best12, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 13){
      plot(best13, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 14){
      plot(best14, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 15){
      plot(best15, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 16){
      plot(best16, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 17){
      plot(best17, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 18){
      plot(best18, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 19){
      plot(best19, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 20){
      plot(best20, scale="adjr2",main = "Adjusted R-Squared")
    }
    
    ################# plot: 5 variable ######################
    else if(c$list == 21){
      plot(best21,scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 22){
      plot(best22, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 23){
      plot(best23, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 24){
      plot(best24, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 25){
      plot(best25, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 26){
      plot(best26, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 27){
      plot(best27, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 28){
      plot(best28, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 29){
      plot(best29, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 30){
      plot(best30, scale="adjr2",main = "Adjusted R-Squared")
    }
    
    ################# plot: 6 variable ######################
    else if(c$list == 31){
      plot(best31,scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 32){
      plot(best32, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 33){
      plot(best33, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 34){
      plot(best34, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 35){
      plot(best35, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 36){
      plot(best36, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 37){
      plot(best37, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 38){
      plot(best38, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 39){
      plot(best39, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 40){
      plot(best40, scale="adjr2",main = "Adjusted R-Squared")
    }
    
    ################# plot: 7 variable ######################
    else if(c$list == 41){
      plot(best41,scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 42){
      plot(best42, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 43){
      plot(best43, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 44){
      plot(best44, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 45){
      plot(best45, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 46){
      plot(best46, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 47){
      plot(best47, scale="adjr2",main = "Adjusted R-Squared")
    }
    else if(c$list == 48){
      plot(best48, scale="adjr2",main = "Adjusted R-Squared")
    }
    
    ################# plot: 8 variable ######################
    else if(c$list == 49){
      plot(best49, scale="adjr2",main = "Adjusted R-Squared")
    }
    
  })
  
  
  output$Bplot <- renderPlot({
    
    ################# plot: 3 variable ###################### 
    if(c$list == 1){
      plot(best1,scale="bic",main = "BIC criterion")
    }
    else if(c$list == 2){
      plot(best2, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 3){
      plot(best3, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 4){
      plot(best4, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 5){
      plot(best5, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 6){
      plot(best6, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 7){
      plot(best7, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 8){
      plot(best8, scale="adjr2",main = "BIC criterion")
    }
    else if(c$list == 9){
      plot(best9, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 10){
      plot(best10, scale="bic",main = "BIC criterion")
    }
    
    ################# plot: 4 variable ###################### 
    else if(c$list == 11){
      plot(best11,scale="bic",main = "BIC criterion")
    }
    else if(c$list == 12){
      plot(best12, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 13){
      plot(best13, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 14){
      plot(best14, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 15){
      plot(best15, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 16){
      plot(best16, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 17){
      plot(best17, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 18){
      plot(best18, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 19){
      plot(best19, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 20){
      plot(best20, scale="bic",main = "BIC criterion")
    }
    
    ################# plot: 5 variable ######################
    else if(c$list == 21){
      plot(best21,scale="bic",main = "BIC criterion")
    }
    else if(c$list == 22){
      plot(best22, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 23){
      plot(best23, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 24){
      plot(best24, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 25){
      plot(best25, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 26){
      plot(best26, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 27){
      plot(best27, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 28){
      plot(best28, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 29){
      plot(best29, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 30){
      plot(best30, scale="bic",main = "BIC criterion")
    }
    
    ################# plot: 6 variable ######################
    else if(c$list == 31){
      plot(best31,scale="bic",main = "BIC criterion")
    }
    else if(c$list == 32){
      plot(best32, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 33){
      plot(best33, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 34){
      plot(best34, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 35){
      plot(best35, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 36){
      plot(best36, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 37){
      plot(best37, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 38){
      plot(best38, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 39){
      plot(best39, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 40){
      plot(best40, scale="bic",main = "BIC criterion")
    }
    
    ################# plot: 7 variable ######################
    else if(c$list == 41){
      plot(best41,scale="bic",main = "BIC criterion")
    }
    else if(c$list == 42){
      plot(best42, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 43){
      plot(best43, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 44){
      plot(best44, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 45){
      plot(best45, scale="bic",main ="BIC criterion")
    }
    else if(c$list == 46){
      plot(best46, scale="bic",main = "BIC criterion")
    }
    else if(c$list == 47){
      plot(best47, scale="bic",main ="BIC criterion")
    }
    else if(c$list == 48){
      plot(best48, scale="bic",main ="BIC criterion")
    }
    
    ################# plot: 8 variable ######################
    else if(c$list == 49){
      plot(best49, scale="bic",main = "BIC criterion")
    }
    
  })
  
  
  output$Cplot <- renderPlot({
    
    ################# plot: 3 variable ###################### 
    if(c$list == 1){
      plot(best1, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 2){
      plot(best2, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 3){
      plot(best3, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 4){
      plot(best4, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 5){
      plot(best5, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 6){
      plot(best6, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 7){
      plot(best7, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 8){
      plot(best8, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 9){
      plot(best9, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 10){
      plot(best10, scale="Cp", main = "Cp criterion")
    }
    
    ################# plot: 4 variable ###################### 
    else if(c$list == 11){
      plot(best11, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 12){
      plot(best12, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 13){
      plot(best13, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 14){
      plot(best14, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 15){
      plot(best15, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 16){
      plot(best16, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 17){
      plot(best17, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 18){
      plot(best18, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 19){
      plot(best19, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 20){
      plot(best20, scale="Cp", main = "Cp criterion")
    }
    
    ################# plot: 5 variable ######################
    else if(c$list == 21){
      plot(best21, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 22){
      plot(best22, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 23){
      plot(best23, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 24){
      plot(best24, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 25){
      plot(best25, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 26){
      plot(best26, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 27){
      plot(best27, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 28){
      plot(best28, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 29){
      plot(best29, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 30){
      plot(best30, scale="Cp", main = "Cp criterion")
    }
    
    ################# plot: 6 variable ######################
    else if(c$list == 31){
      plot(best31, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 32){
      plot(best32, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 33){
      plot(best33, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 34){
      plot(best34, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 35){
      plot(best35, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 36){
      plot(best36, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 37){
      plot(best37, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 38){
      plot(best38, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 39){
      plot(best39, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 40){
      plot(best40, scale="Cp", main = "Cp criterion")
    }
    
    ################# plot: 7 variable ######################
    else if(c$list == 41){
    }
    else if(c$list == 42){
      plot(best42, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 43){
      plot(best43, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 44){
      plot(best44, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 45){
      plot(best45, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 46){
      plot(best46, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 47){
      plot(best47, scale="Cp", main = "Cp criterion")
    }
    else if(c$list == 48){
      plot(best48, scale="Cp", main = "Cp criterion")
    }
    
    ################# plot: 8 variable ######################
    else if(c$list == 49){
      plot(best49, scale="Cp", main = "Cp criterion")
    }
    
  })#close of Cplot
  })#close of data simulation
  
 
  
  output$answer <- renderText({
    ############################ True Answer text: 3 variables####################################
    if(c$list == 1){
      paste("The best model for this data set using the given criterion is the model which contains X1,X2,X3.")
    }
    else if(c$list == 2){
      paste("The best model for this data set using the given criterion is the model which contains X3,X8. ")
    }
    else if(c$list == 3){
      paste("The best model for this data set using the given criterion is the model which contains X1 and X2. ")
    }
    else if(c$list == 4){
      paste("The best model for this data set using the given criterion is the model which contains X2, X3 and X8.")
    }
    else if(c$list == 5){
      paste("The best model for this data set using the given criterion is the model which contains X2 and X3.")
    }
    else if(c$list == 6){
      paste("The best model for this data set using the given criterion is the model which contains X3.")
    }
    else if(c$list == 7){
      paste("The best model for this data set using the given criterion is the model which contains X3.")
    }
    else if(c$list == 8){
      paste("The best model for this data set using the given criterion is the model which contains X2.")
    }
    else if(c$list == 9){
      paste("The best model for this data set using the given criterion is the model which contains X1 and X8.")
    }
    else if(c$list == 10){
      paste("The best model for this data set using the given criterion is the model which contains X1.")
    }
    
    ############################ True Answer text: 4 variables####################################
    else if(c$list == 11){
      paste("The best model for this data set using the given criterion is the model which contains X2. ")
    }
    else if(c$list == 12){
      paste("The best model for this data set using the given criterion is the model which contains X1,X8.")
    }
    else if(c$list == 13){
      paste("The best model for this data set using the given criterion is the model which contains X1,X2 and X8.")
    }
    else if(c$list == 14){
      paste("The best model for this data set using the given criterion is the model which contains X1.")
    }
    else if(c$list == 15){
      paste("The best model for this data set using the given criterion is the model which contains X2, X3 and X8.")
    }
    else if(c$list == 16){
      paste("The best model for this data set using the given criterion is the model which contains X2.")
    }
    else if(c$list == 17){
      paste("The best model for this data set using the given criterion is the model which contains X1, X2 and X8. ")
    }
    else if(c$list == 18){
      paste("The best model for this data set using the given criterion is the model which contains X2 and X3.")
    }
    else if(c$list == 19){
      paste("The best model for this data set using the given criterion is the model which contains X3 and X8.")
    }
    else if(c$list == 20){
      paste("The best model for this data set using the given criterion is the model which contains X3.")
    }
    
    ############################ True Answer text: 5 variables####################################
    else if(c$list == 21){
      paste("The best model for this data set using the given criterion is the model which contains X2,X3.")
    }
    else if(c$list == 22){
      paste("The best model for this data set using the given criterion is the model which contains X1,X3,X8.")
    }
    else if(c$list == 23){
      paste("The best model for this data set using the given criterion is the model which contains X1, X2, X3 and X8.")
    }
    else if(c$list == 24){
      paste("The best model for this data set using the given criterion is the model which contains X1 and X3.")
    }
    else if(c$list == 25){
      paste("The best model for this data set using the given criterion is the model which contains X2 and X3.")
    }
    else if(c$list == 26){
      paste("The best model for this data set using the given criterion is the model which contains X2 and X8.")
    }
    else if(c$list == 27){
      paste("The best model for this data set using the given criterion is the model which contains X3 and X8.")
    }
    else if(c$list == 28){
      paste("The best model for this data set using the given criterion is the model which contains X3 and X8.")
    }
    else if(c$list == 29){
      paste("The best model for this data set using the given criterion is the model which contains X3 and X8.")
    }
    else if(c$list == 30){
      paste("The best model for this data set using the given criterion is the model which contains X1 and X2.")
    }
    
    ############################ True Answer text: 6 variables####################################
    else if(c$list == 31){
      paste("The best model for this data set using the given criterion is the model which contains X1,X3,X8.")
    }
    else if(c$list == 32){
      paste("The best model for this data set using the given criterion is the model which contains X1,X2,X3. ")
    }
    else if(c$list == 33){
      paste("The best model for this data set using the given criterion is the model which contains X1 and X2. ")
    }
    else if(c$list == 34){
      paste("The best model for this data set using the given criterion is the model which contains X1, X3 and X8. ")
    }
    else if(c$list == 35){
      paste("The best model for this data set using the given criterion is the model which contains X1 and X3. ")
    }
    else if(c$list == 36){
      paste("The best model for this data set using the given criterion is the model which contains X2, X3 and X8.")
    }
    else if(c$list == 37){
      paste("The best model for this data set using the given criterion is the model which contains X2 and X8.")
    }
    else if(c$list == 38){
      paste("The best model for this data set using the given criterion is the model which contains X2, X3 and X8.")
    }
    else if(c$list == 39){
      paste("The best model for this data set using the given criterion is the model which contains X2 and X3. ")
    } 
    else if(c$list == 40){
      paste("The best model for this data set using the given criterion is the model which contains X3 and X8. ")
    }
    
    ############################ True Answer text: 7 variables####################################
    else if(c$list == 41){
      paste("The best model for this data set using the given criterion is the model which contains X2,X3,X8. ")
    }
    else if(c$list == 42){
      paste("The best model for this data set using the given criterion is the model which contains X1,X2,X8. ")
    }
    else if(c$list == 43){
      paste("The best model for this data set using the given criterion is the model which contains X1, X2 and X8.")
    }
    else if(c$list == 44){
      paste("The best model for this data set using the given criterion is the model which contains X1, X2, X3 and X8. ")
    }
    else if(c$list == 45){
      paste("The best model for this data set using the given criterion is the model which contains X1, X2, X3 and X8. ")
    }
    else if(c$list == 46){
      paste("The best model for this data set using the given criterion is the model which contains X1, X2, X3 and X8. ")
    }
    else if(c$list == 47){
      paste("The best model for this data set using the given criterion is the model which contains X1, X2, X3 and X8. ")
    }
    else if(c$list == 48){
      paste("The best model for this data set using the given criterion is the model which contains X1, X2 and X3.")
    }
    
    ############################ True Answer text: 8 variables####################################
    else if(c$list == 49){
      paste("The best model for this data set using the given criterion is the model which contains X1,X2,X3,X8.")
    }
    
  })
  
  output$best <- renderText({
    paste0("Which is the best estimated model under the given criterion?")
  })
  
  full.model <- lm(Fertility ~., data = swiss)
  best.model <- regsubsets(Fertility~., data=swiss, nbest=3)
  null.model <- lm(Fertility~1, data=swiss)

  output$aPlot <- renderPlot({
    plot(best.model, scale="adjr2",main = "Adjusted R-Squared")
  })
  
  output$bPlot <- renderPlot({
    plot(best.model,scale="bic",main = "BIC criterion")
  })
  
  output$cPlot <- renderPlot({
    plot(best.model, scale="Cp", main = "Cp criterion")
  })
  
  output$feedback <- renderPrint({
   
    validate(need(!is.null(input$question1) & !is.null(input$question2) & !is.null(input$question3) & !is.null(input$question4),'Please answer all questions.'))
    if((input$question1 == "Examination" | input$question1 == "examination")
       &(input$question2 == 'y')
       &(input$question3 == 'y')
       &(input$question4 == 'y')
       &(input$question5 == 'y')
       &(input$question6 == 'y')){
      cat('All correct. Great Job!')
    }
    
    #Render pic1
    if (input$question1!=''){
      # output$pic1 = renderUI({
      #   
      #   if(input$question1 == "Examination" || input$question1 == "examination"){
      #     img(src = "check.png", alt = "it means the answer is correct", width = 25)
      #   }
      #   else{
      #     img(src = "cross.png", alt = "it means the answer is wrong", width = 25)
      #   }
      # })}
      # The below code replaces the above code
    output$pic1 <- boastUtils::renderIcon(
      icon = ifelse(input$question1 == "Examination" || input$question1 == "examination",
                    "correct",
                    "incorrect"),
      width = 25)
    }
    
    # To clear the grade mark
    # output$pic1 <- boastUtils::renderIcon()
    #The same as
    # output$pic1 <- renderUI({
    # img(src = NULL)
    # })
    
    #Render pic2
    if (input$question2!='null'){
      output$pic2 <- boastUtils::renderIcon(
      icon = ifelse(input$question2 == 'y',
                    "correct",
                    "incorrect"),
      width = 25)
  }
    #Render pic3
    if (input$question3!='null'){
      output$pic3 <- boastUtils::renderIcon(
        icon = ifelse(input$question3 == "y",
                      "correct",
                      "incorrect"),
        width = 25)
    }
    
    
    #Render pic4
    if (input$question4!='null'){
      output$pic4 <- boastUtils::renderIcon(
        icon = ifelse(input$question4 == 'y',
                      "correct",
                      "incorrect"),
        width = 25)
    }
    
    
    #Render pic5
    if (input$question5!='null'){
      output$pic5 <- boastUtils::renderIcon(
        icon = ifelse(input$question5 == 'y',
                      "correct",
                      "incorrect"),
        width = 25)
    }
    
     
    
    #Render pic6
    if (input$question6!='null'){
      output$pic6 <- boastUtils::renderIcon(
        icon = ifelse(input$question6 == 'y',
                      "correct",
                      "incorrect"),
        width = 25)
    }
    
    
    #Render pic7
    if (input$question7!=''){
      output$pic2 <- boastUtils::renderIcon(
        icon = ifelse(input$question7 == 'null',
                      "correct",
                      "incorrect"),
        width = 25)
    }
  })
  

  ###closing for SERVER DON'T DELET####      
})