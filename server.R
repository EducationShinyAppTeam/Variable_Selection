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

  observeEvent(input$game1,{
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

####################### next question buttom ##############################
  
  observeEvent(input$nextq, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = TRUE)
    updateSelectInput(session,"answer","",c('Select methods','Cp criterion','Adjusted R-Squared','bic criterion','Forward Selection','Backward Selection','Stepwise Selection'))
    output$result <- renderUI({
      
      h3("Choose the distribution from the list to match the given text, then click 'Submit' to check your answer.")
    })
    
    index_list$list=index_list$list[-1]   
    value$index <- index_list$list[1]
  })

########################### exploring #####################################
  
# R code details
  output$design = renderUI({
    if(input$designcheckbox)
    {
      h4("A researcher plans to take a random sample of size n students to do a survey about their experiences in studying at the University Park campus of Penn State University. However, she worries that sample results could be biased because the students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the percentage of Penn State Students who are Pennsylvania residents based on her study and compares it to the mean of 59.5% for the population of all Penn State University Park students. This app shows  how confidence intervals of that type would come out when there is no bias.")
    }
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
  
  
############################### Gaming #######################################

  
  
  
###closing for SERVER DON'T DELET####      
})