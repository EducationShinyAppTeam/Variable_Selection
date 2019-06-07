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

#read in dataset




# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  ##########################action buttons##################################### 
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = " Move the sliders to see their effect on the diagnostic plots.",
      type = "info"
    )
  })
  
  observeEvent(input$pre,{
    updateTabItems(session,"tabs","prereq")
  })
  
  observeEvent(input$start1,{
    updateTabItems(session,"tabs","instruction")
  })
    
  observeEvent(input$go,{
    updateTabItems(session,"tabs","explore")
  })

  observeEvent(input$game,{
    updateTabItems(session,"tabs","game")
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

  
# 
#   hw6 = read.table('HW6.csv', sep=',', header=T)
#   #Best subset routine
#   best=regsubsets(Y~., data=hw6, nbest=3) 
#   plot(best, scale="adjr2")
#   plot(best, scale="Cp")
#   plot(best, scale="bic")
#   
#   #Automatic selection - Forward
#   null=lm(Y~1, data=hw6) #intercept only model
#   #full model- Regresses y on all variables in dataset
#   full=lm(Y~., data=hw6)#full model- Regresses y on all variables in dataset
#   step(null, scope=list(lower=null, upper=full), 
#        direction="forward")
#   
#   #Automatic selection - backward
#   step(full, data=hw6, direction="backward")
#   
#   #Automatic selection - Stepwise
#   step(null, scope = list(upper=full), data=hw6, 
#        direction="both")  
#   
  
  
  
  
  
  
  
  
  
  
  
  
#exploring
  
  
#check answer
#
# 
  
###closing for SERVER DON'T DELET####      
})