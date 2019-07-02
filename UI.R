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
                                                 # menuItem("Game",tabName = "game", icon = icon("gamepad"))
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
                                            # tags$style(HTML('#game1{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#download_data{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#info2{color:white;background-color: #ffb6c1}'))
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
                                                   h4(tags$li('Change the options to see how each method perform.')),
                                                   h4(tags$li('Click download button if you want the database.')),
                                                   h4(tags$li('After working with the explore section, you can start the game to test your understanding of the concepts.')),
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
                                                   tags$li('The adjusted R-squared compares the explanatory power of regression models that contain different numbers of predictors.The model with the highest adjusted R-squared is preferred.'),
                                                   br(),
                                                   tags$li('The Mallow Cp criterion is used to assess the fit of a regression model that has been estimated using ordinary least squares. The model with the lowest Cp is preferred..                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       '),
                                                   br(),
                                                   tags$li('The BIC criterion is a criterion for model selection among a finite set of models. The model with the lowest BIC is preferred. '),
                                                   br(),
                                                   tags$li('The Forward selection . If the equal variance assumption is true, you should expect a roughly horizontal line with the dots showing equal spread.'),
                                                   br(),
                                                   tags$li('The Backward selection checks for equal spread of residual the residuals. If the equal variance assumption is true, you should expect a roughly horizontal line with the dots showing equal spread.'),
                                                   br(),
                                                   tags$li('The Stepwise selection checks for influential outliers. Outliers with high leverage will appear outside the dashed line range.'),
                                                   br(),
                                 
                                                   div(style = "text-align: center",
                                                       actionButton("go1","G O !",icon("bolt"),class="circle grow"))
                                                  ),
                                           
                                           
                                           # explore page
                                           tabItem(tabName="explore",
                                                   sidebarLayout(sidebarPanel(
                                                                              selectInput("model", h4(strong("Select one method of variable selection:")), 
                                                                                          choices = c('Adjusted R-Squared criterion','Cp criterion','BIC criterion','Forward Selection','Backward Selection','Stepwise Selection')
                                                                                          ),
                                                                              column(6,downloadButton("download_data", "Download data")),
                                                                              column(6,actionButton("info2","Interpretation")),
                                                                              br(),
                                                                              br(),
                                                                              br(),
                                                                              conditionalPanel(condition = 'input.info2',
                                                                                               hidden(div(id="wow",textOutput("interpretation")))),
                                                                              
                                                                              width = 4
                                                                              ),
                                                                 mainPanel( 
                                                                           width = 8,
                                                                           plotOutput("plots")
                                                                           # div(style = "text-align: center",
                                                                           #     actionButton("game1","Game",icon("bolt"),class="circle grow"))
                                                                 ))
                                                   )
                                           
                                           #game page
                                           # tabItem(tabName="game",
                                           #         titlePanel("Matching the text with the distribution"),
                                           #         sidebarLayout(
                                           #                       sidebarPanel(
                                           #                                    wellPanel(uiOutput("question"),
                                           #                                              tags$style(type='text/css', '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}','.well { padding: 12px; margin-bottom: 15px; max-width: 1000px; }')
                                           #                                              ),
                                           #                                    br(),
                                           #                                    
                                           #                                    fluidRow( 
                                           #                                             h3("Identify the distribution of given text:")
                                           #                                             ),
                                           #                                    
                                           #                                    fluidRow(uiOutput('answerbox'),selectInput('answer',"",c('Select Distribution','Bernoulli','Binomial','Continuous Uniform','Discrete Uniform','Exponential','Gamma','Geometric','Negative Binomial',
                                           #                                                                                             'Normal','Poisson'), width='100%'),
                                           #                                             uiOutput('mark')),
                                           #                                    
                                           #                                    br(),
                                           #                                    br(),
                                           #                                    br(),
                                           #                                    
                                           #                                    tags$head(tags$style(HTML("#result {font-size: 17px;background-color:#EAF2F8}"))),
                                           #                                    
                                           #                                    
                                           #                                    width = 6
                                           #                                    
                                           #                                    ),
                                           #                       mainPanel(
                                           #                                 br(),
                                           #                                 width = 6,
                                           #                                 br(),
                                           #                                 br(),
                                           #                                 br(),
                                           #                                    
                                           #                                 fluidRow(
                                           #                                          column(3, offset=2,
                                           #                                                 bsButton('nextq', "Next Question", size ="large", style="success",disabled=TRUE)),
                                           #                                          column(3,
                                           #                                                 bsButton('submit', "Submit", size= "large", style ="warning", disabled =FALSE))
                                           #                                          )
                                           #                                 
                                           #                                 ),
                                           #                       
                                           #                       
                                           #                                 position ="left"
                                           #                       )
                                           #         )
                                                                          
                                                                          
   
                    
                    )#close tabItems            
      )#close dashboardbody                                   
                      
###closing for SERVER DON'T DELET####          
)))