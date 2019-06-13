library(shiny)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(shinycssloaders)
library(shinydashboard)
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
                                    tags$li(class = "dropdown", actionLink("info",icon("info",class="myClass")))
                    ),
          
                    #menu bar
                    dashboardSidebar(width = 180,
                                     sidebarMenu(id='tabs',style='font-size:13px;',
                                                 convertMenuItem(menuItem("Overview",tabName = "instruction", icon = icon("dashboard"),
                                                                 menuSubItem("Pre-requisites", tabName= "prereq", icon=icon("book"))),'instruction'),
                                                 menuItem("Explore",tabName = "explore", icon = icon("wpexplorer")),
                                                 menuItem("Game",tabName = "game", icon = icon("gamepad"))
                                                 )
                                     ),
                    
                    #change the color,bacground color & word styles of buttons, icons & words
                    dashboardBody(
                                  #change header font
                                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")),
                                
                                  #change icon color
                                  tags$head(tags$style(".fa-home {color:#FFFFFF}"),
                                            tags$style(".fa-info {color:#FFFFFF}")),
                                        
                                  #change button color
                                  tags$head(tags$style(HTML('#start1{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#go{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#pre{color:white;background-color: #ffb6c1}')),
                                            tags$style(HTML('#game1{color:white;background-color: #ffb6c1}'))
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
                                        #           tags$style(HTML('#pre2{color:white;background-color: #ffb6c1}')),
                                        #           tags$style(HTML('#submitA{color:white;background-color: #ffb6c1}')),
                                        #           tags$style(HTML('#new{color:white;background-color: #ffb6c1}')),
                                        #           tags$style(HTML('#start_timer{color:white;background-color: #ffb6c1}')),
                                        #           tags$style(HTML('#set{color:white;background-color: #ffb6c1}')),
                                        #           tags$style(HTML('#reset{color:white;background-color: #ffb6c1}')))
                    
                                  tabItems(      
                                           # instruction page
                                           tabItem(tabName = "instruction",
                                                   tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                                                   br(),br(),br(),
                                                   h3(strong("About:")),
                                                   h4('This app introduces the concept of variable selection.'),
                                                   br(),
                    
                                                                                  div(style = "text-align: center",
                                                       bsButton("pre","Pre-requisite",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow")),
                                                   br(),
                                                   h3(strong('Instructions:')),
                                                   h4(tags$li('Click Go button to enter the explore page. ')),
                                                   h4(tags$li('Use the radio buttons to select different variables and see the changes in the interaction plot. Or use slider bars to change the parameters. ')),
                                                   h4(tags$li('After working with the explore section, you can start the matching game to test your understanding of the concepts. You can use "i" button for instruction and "?" for hints.')),
                               
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
                                                   h4('Variable selection is the task of selecting a statistical model from a set of canditadate models,'),
                                                   br(),
                                                   h4('Model checking is a critical part of an analysis. You need to understand the diagnostic plot s like these four:',
                                                   br(),
                                                   tags$li('The Residuals vs Fitted plot checks linear pattern of residuals. If the liner model is correct, you should expect a roughly horizontal line.'),
                                                   br(),
                                                   tags$li('The Normal Q-Q plot checks normality. If the normality assumption is true, you should expect the dots roughly follow a straight line.'),
                                                   br(),
                                                   tags$li('The Scale-Location plot checks for equal spread of residual the residuals. If the equal variance assumption is true, you should expect a roughly horizontal line with the dots showing equal spread.'),
                                                   br(),
                                                   tags$li('The Residual vs Leverage plot checks for influential outliers. Outliers with high leverage will appear outside the dashed line range.')),
                                                   br(),
                                 
                                                   div(style = "text-align: center",
                                                       actionButton("start1","Go to the overview",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow"))
                                                  ),
                                           
                                           # explore page
                                           tabItem(tabName="explore",
                                                   sidebarLayout(sidebarPanel(
                                                                              selectInput("model", "Select one method of variable selection:", 
                                                                                          choices= c('Select methods','Adjusted R-Squared','Cp criterion','bic criterion','Forward Selection','Backward Selection','Stepwise Selection')
                                                                                          ),
                                                                              checkboxInput("designcheckbox","Show design info:", TRUE),
                                                                              uiOutput("design"),
                                                                              width = 4
                                                                              ),
                                                                 mainPanel( 
                                                                           width = 8,
                                                                           fluidRow(
                                                                                    uiOutput("correct", align = 'center')
                                                                                    ),
                                                                           br(),
                                                                           br(),
                                                                           
                                                                           div(style = "text-align: center",
                                                                               actionButton("game1","Game",icon("bolt"),class="circle grow"))
                                                                 ))
                                                   ),
                                           
                                           #game page
                                           tabItem(tabName="game",
                                                   titlePanel("Matching the text with the distribution"),
                                                   sidebarLayout(
                                                                 sidebarPanel(
                                                                              wellPanel(style = "background-color: #EAF2F8",
                                                                                        uiOutput("question"),
                                                                                        tags$style(type='text/css', '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}','.well { padding: 12px; margin-bottom: 15px; max-width: 1000px; }')
                                                                                        ),
                                                                              
                                                                              fluidRow( 
                                                                                       h3("Identify the distribution of given text:")
                                                                                       ),
                                                                              
                                                                              div(style="display: inline-block;vertical-align:top;",
                                                                                  circleButton("hint",icon = icon("question"), status = "myClass",size = "xs")
                                                                                  ),
                                                                              
                                                                              fluidRow(uiOutput('answerbox'),selectInput('answer',"",c('Select Distribution','Bernoulli','Binomial','Continuous Uniform','Discrete Uniform','Exponential','Gamma','Geometric','Negative Binomial',
                                                                                                                                       'Normal','Poisson'), width='100%'),
                                                                                       uiOutput('mark')),
                                                                              
                                                                              
                                                                              br(),
                                                                              br(),
                                                                              br(),
                                                                              
                                                                              tags$head(tags$style(HTML("#result {font-size: 17px;background-color:#EAF2F8}"))),
                                                                              
                                                                              
                                                                              width = 6
                                                                              
                                                                              ),
                                                                 mainPanel(
                                                                           br(),
                                                                           width = 6,
                                                                              
                                                                           fluidRow(
                                                                                    uiOutput("correct", align = 'center')
                                                                                    ),
                                                                              
                                                                           br(),
                                                                           br(),
                                                                              
                                                                           fluidRow(
                                                                                    uiOutput("distPlot", align = 'center')
                                                                                    ),
                                                                           br(),
                                                                           br(),
                                                                           br(),
                                                                              
                                                                           fluidRow(
                                                                                    column(3, offset=2,
                                                                                           bsButton('nextq', "Next Question", size ="large", style="success",disabled=TRUE)),
                                                                                    column(3,
                                                                                           bsButton('submit', "Submit", size= "large", style ="warning", disabled =FALSE))
                                                                                    )
                                                                           
                                                                           ),
                                                                 
                                                                 
                                                                           position ="left"
                                                                 )
                                                   )
                                                                          
                                                                          
   
                    
                    )#close tabItems            
      )#close dashboardbody                                   
                      
###closing for SERVER DON'T DELET####          
)))