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

# insert "convertMentItem" function

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

# Creat the UI
shinyUI(fluidPage(
       dashboardPage(skin = "black",
                    
                    #header 
                    dashboardHeader(title = "Variable selection",
                                    titleWidth = 180,
                                    tags$li(class = "dropdown", tags$a(href='https://shinyapps.science.psu.edu/',icon("home"))),
                                    tags$li(class = "dropdown", actionLink("info",icon("info",class="myClass"))),
                                    tags$li(class = "dropdown", actionLink("hint",icon("question",class="myClass")))
                                    ),
          
                    #menu bar
                    dashboardSidebar(width = 180,
                                     sidebarMenu(id='tabs',style='font-size:13px;',
                                                 convertMenuItem(menuItem("Overview",tabName = "instruction", icon = icon("dashboard"),
                                                                 menuSubItem("Pre-requisites", tabName= "prereq", icon=icon("book"))),'instruction'),
                                                 menuItem("Explore",tabName = "explore", icon = icon("wpexplorer"))
                                                 )
                                     ),
                    
                    #change the color,bacground color & word styles of buttons, icons & words
                    dashboardBody(
                                  #change header font
                                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")),
                                
                                  #change icon color
                                  tags$head(tags$style(".fa-home {color:#FFFFFF}"),
                                            tags$style(".fa-info {color:#FFFFFF}"),
                                            tags$style(".fa-question {color:#FFFFFF}")
                                            ),
                                        
                                  #change button color
                                  tags$head(tags$style(HTML('#start1{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#go{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#go1{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#pre{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#download_data{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#info2{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#restart{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#refresh{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ffb6c1}")),
                                            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {border-color: #ffb6c1"))
                                  ),
                                  
                                  #change header color
                                  tags$style(HTML('
                                                  .skin-black .main-header>.navbar {
                                                  background-color: #ffb6c1 ;
                                                  }
                                                  
                                                  .content-wrapper,.right-side {
                                                  background-color: white;
                                                  }
                                                  
                                                  .skin-black .main-header .logo {
                                                  background-color: #ffb6c1  ;
                                                  color: white;
                                                  }
                                                  .skin-black .main-header .logo:hover {
                                                  background-color: #ffb6c1;
                                                  }
                                                  .skin-black .main-header .navbar .sidebar-toggle:hover{
                                                  background-color: #ffb6c1;
                                                  }')),

                                   tabItems(      
                                           # instruction page
                                           tabItem(tabName = "instruction",
                                                   tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                                                   br(),br(),br(),
                                                   h3(strong("About:")),
                                                   h4('This app introduces the concept of variable selection.'),
                                                   br(),
                                                   div(style = "text-align: center",
                                                       actionButton("pre","Pre-requisite",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow")),
                                                   br(),
                                                   h3(strong('Instructions:')),
                                                   h4(tags$li('Click Go button to enter the explore page. ')),
                                                   h4(tags$li('Change the options to see how each method performs.')),
                                                   h4(tags$li('Click Refresh button if you want to generate a new dataset.')),
                                                   h4(tags$li('You can use "i" button for instruction')),
                                                   br(),
                                                   div(style = "text-align: center",
                                                       actionButton("go","G O !",icon("bolt"),class="circle grow")),
                                                   br(),
                                                   h3(strong('Acknowledgements:')),
                                                   h4("This app was developed and coded by Zhiruo Wang.")
                                                  ),
                         
                                           # pre-requisite page
                                           tabItem(tabName="prereq",
                                                   h3(strong('Background')),
                                                   h3('What is variable selection:'),
                                                   h4('Variable selection is the task of selecting the best statistical regression model for analysis from a set of potential influenial factors,'),
                                                   br(),
                                                   tags$li('The adjusted R-squared compares the explanatory power of regression models that contain different numbers of predictors. The model with the highest adjusted R-squared is preferred.'),
                                                   br(),
                                                   tags$li('The Mallow Cp criterion is used to assess the fit of a regression model that has been estimated using ordinary least squares. The model with the lowest Cp is preferred..                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       '),
                                                   br(),
                                                   tags$li('The Bayesian Information Criterion (BIC) is a criterion for model selection among a finite set of models. The model with the lowest BIC is preferred. '),
                                                   br(),
                                                   tags$li('Forward selection is a type of stepwise regression which begins with an empty model and adds in variables one by one. In each forward step, you add the one variable that gives the single best improvement to your model. '),
                                                   br(),
                                                   tags$li('Backward elimination is a type of stepwise regression which involves starting with all candidate variables, testing the deletion of each variable using a chosen model fit criterion, deleting the variable (if any) whose loss gives the most statistically insignificant deterioration of the model fit, and repeating this process until no further variables can be deleted without a statistically significant loss of fit. '),
                                                   br(),
                                                   tags$li('The Stepwise selection is the combination of forward selection and backward elimination. '),
                                                   br(),
                                 
                                                   div(style = "text-align: center",
                                                       actionButton("go1","G O !",icon("bolt"),class="circle grow"))
                                                  ),
                                           
                                           
                                           # explore page
                                           tabItem(tabName="explore",
                                                   sidebarLayout(sidebarPanel(
                                                                              sliderInput("nfactor", h4(strong("The number of potential factors that influence Y:")),min = 3 , max = 8, value = 4, step= 1),
                                                                              bsButton('refresh', "Refresh data", disabled =FALSE),
                                                                              br(),
                                                                              br(),
                                                                              bsButton('restart', "Genereate New Model", disabled =FALSE),
                                                                              br(),
                                                                              br(),
                                                                              selectInput("model", h4(strong("Select one method of variable selection:")), 
                                                                                          choices = c('Adjusted R-Squared criterion','Cp criterion','BIC criterion','Forward Selection','Backward Selection','Stepwise Selection')
                                                                                          ),
                                                                              prettyCheckbox(inputId = "button", label = "Show me the true answer", icon = icon("check")),
                                                                              conditionalPanel("input.button != 0",textOutput("answer")),
                                                                              width = 4
                                                                              ),
                                                                 mainPanel( 
                                                                           width = 8,
                                                                           plotOutput("plots")
                                                                 ))
                                                   )
                                           
                    
                    )#close tabItems            
      )#close dashboardbody                                   
                      
###closing for SERVER DON'T DELET####          
)))
