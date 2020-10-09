library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Variable Selection"
APP_DESCP  <<- paste(
  "This app introduces the concept of variable selection according to different criterion."
)
## End App Meta Data------------------------------------------------------------

# Creat the UI
dashboardPage(
  skin = "black",
  #header 
  dashboardHeader(
    
    title = "Variable Selection",
    titleWidth = 250,
    tags$li(class="dropdown",
            actionLink("info", icon("info"), class="myClass")),
    tags$li(
      class = "dropdown",
      tags$a(target = "_blank", icon("comments"),
             href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Variable_Selection"
      )
    ),
    tags$li(class = "dropdown", tags$a(href='https://shinyapps.science.psu.edu/',icon("home")))
  ),
  #menu bar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Overview", tabName = "over", icon = icon("dashboard")),
      menuItem("Prerequisites", tabName= "prereq", icon = icon("book")),
      menuItem("Explore Criteria", tabName = "explore1", icon = icon("wpexplorer")),
      menuItem("Explore Real Data", tabName = "explore2", icon = icon("wpexplorer")),
      menuItem("References", tabName = "Ref", icon = icon("leanpub"))
    ), 
    #PSU Logo
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ), 
  #change the color,bacground color & word styles of buttons, icons & words
  dashboardBody(
    #change header font
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href="https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    ),
    tabItems(      
      # instruction page
      tabItem(
        tabName = "over",
        h1("Variable Selection"),
        p("This app introduces the concept of variable selection."),
        br(),
        div(style = "text-align: center",
            bsButton(inputId = "Jolin","Prerequisites",icon("bolt"),size = "large",class="circle grow")),
        br(),
        h2("Instructions"),
        tags$ol(
          tags$li("Click explore button to enter the explore page."),
          tags$li("Change the options to see how each method performs when exploring stimulation page."),
          tags$li("Click Refresh button if you want to generate a new dataset."),
          tags$li("See how each method works when exploring real date set page.")),
        br(),
        div(style = "text-align: center", 
            bsButton(inputId = "explore", "Explore", icon("bolt"), size = "large",class = "circle grow")),
        br(),
        h2("Acknowledgements"),
        p("This app was developed and programmed by Ziruo Wang and updated by Zhuolin Luo in 2020.",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 07/29/2020 by ZL.")
        )
      ),
    tabItem(  
      tabName="prereq",
      titlePanel("What is variable selection:"),
      p("Variable selection is the task of selecting the best statistical 
        regression model for analysis from a set of potential influenial factors,"),
      br(),
      tags$ul(
        tags$li('The adjusted R-squared compares the explanatory power of 
        regression models that contain different numbers of predictors. 
                The model with the highest adjusted R-squared is preferred.'),
        tags$li('The Mallow Cp criterion is used to assess the fit of a 
        regression model that has been estimated using ordinary least squares. 
                The model with the lowest Cp is preferred..                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       '),
        tags$li('The Bayesian Information Criterion (BIC) is a criterion for model 
        selection among a finite set of models. 
                The model with the lowest BIC is preferred. '),
        tags$li('The Akaike Information Criterion (AIC) is a criterion for model 
        selection, dealing with both the risk of overfitting and the risk of underfitting. 
                The model with the lowest AIC is preferred. '),
        tags$li('Forward selection is a type of stepwise regression which begins 
        with an empty model and adds in variables one by one. 
                In each forward step, you add the one variable that gives the 
                single best improvement to your model. '),
        tags$li('Backward elimination is a type of stepwise regression which 
                involves starting with all candidate variables, testing the 
                deletion of each variable using a chosen model fit criterion, 
                deleting the variable (if any) whose loss gives the most statistically 
                insignificant deterioration of the model fit, and repeating this 
                process until no further variables can be deleted without a statistically 
                significant loss of fit. '),
        tags$li('Stepwise selection is the combination of forward selection and 
                backward elimination. ')
      )
    ),
    
    # explore page
    tabItem(
      tabName="explore1",
      h2("Exploring Variable Selection Criteria"),
      sliderInput(inputId = "nfactor", label = "Number of Xs in Data Set", 
                  min = 3 , max = 8, value = 4, step= 1),
      br(),
      bsButton('refresh', "Refresh data", disabled =FALSE),
      bsButton('restart', "Genereate New Model", disabled =FALSE),
      br(),br(),
      tabsetPanel(
        id = "criteria",
        type = "tabs",
        tabPanel(
          title = "Adjusted R-Squared",
          br(),
          br(),
          plotOutput("Aplot"),
          tags$script(HTML(
            "$(document).ready(function() {
            document.getElementById('Aplot').setAttribute('aria-label',
            `model selection plot under the criterion adjusted r-squared`)
            })"
          ))
        ),
        tabPanel(
          title = "BIC Criterion",
          br(),
          br(),
          plotOutput("Bplot"),
          tags$script(HTML(
            "$(document).ready(function() {
            document.getElementById('Bplot').setAttribute('aria-label',
            `model selection plot under the criterion BIC criterion`)
            })"
          ))
        ),
        tabPanel(
          title = "Mallow's Cp",
          br(),
          br(),
          plotOutput("Cplot"),
          tags$script(HTML(
            "$(document).ready(function() {
            document.getElementById('Cplot').setAttribute('aria-label',
            `model selection plot under the criterion Mallow's Cp criterion`)
            })"
          ))
        )
      ),
      textOutput("best"),
      br(),
      prettyCheckbox(inputId = "button", label = "Describe the Best Estimated Model", icon = icon("check")),
      conditionalPanel("input.button != 0",textOutput("answer"))
    ),
   
    tabItem(
      # Exploration Tab ------------------------------------------------------
      tabName = "explore2",
      h2("Exploring Real Data"),
      p("The example here using the Swiss Fertility and Socioeconomic Indicators Data, 
        helping us explore the past scene in field of Agriculture"),
      box(
        title = strong("Background Information"), # This is the header of the box. Consider using "Story Context"
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        width = '100%', 
        "Standardized fertility measure and socio-economic indicators for each 
        of 47 French-speaking provinces of Switzerland at about 1888. 
        Fertility: Ig, 'common standardized fertility measure’; Agriculture: 
        % of males involved in agriculture as occupation; Examination:
        % draftees receiving highest mark on army examination; Education: 
        % education beyond primary school for draftees; Catholic: % ‘catholic’ (as opposed to ‘protestant’); 
        Infant Mortality: live births who live less than 1 year.
        "
      ),
      br(),
      tabsetPanel(
        id = "criteria",
        type = "tabs",
        tabPanel(
          title = "Model Selection",
          br(),
          br(),
          fluidRow(
            column(4,
                   plotOutput("aPlot"),
                   tags$script(HTML(
                     "$(document).ready(function() {
                     document.getElementById('aPlot').setAttribute('aria-label',
                     `real data model selection plot under the criterion adjusted r-squared`)
                     })"
                   ))
                   ),
            column(4,
                   plotOutput("bPlot"),
                   tags$script(HTML(
                     "$(document).ready(function() {
                     document.getElementById('bPlot').setAttribute('aria-label',
                     `real data model selection plot under the criterion BIC`)
                     })"
                   ))
                   ),
            column(4,
                   plotOutput("cPlot"),
                   tags$script(HTML(
                     "$(document).ready(function() {
                     document.getElementById('cPlot').setAttribute('aria-label',
                     `real data model selection plot under the criterion Mallow's Cp`)
                     })"
                   ))
                   )
          )
        ),
        tabPanel(
          title = "Check Yourself",
          br(),
          fluidRow(
            column(10, offset = 1,
              h3("Questions: "), br(),
              # question1
              div(style="display:inline-block",
                  textInput("question1", 
                            "Which variable(s) are not included in the top model for any of the three criteria?", width='13cm',"")),
              div(style="display:inline-block", uiOutput('pic1')) #Use uiOutput instead of htmlOutput
              )),
              
              # question2
          fluidRow(
            column(10, offset = 1,
                   div(style="display:inline-block", selectInput("question2", "Which model is at or near the optimum for all three criteria?",
                                                            c(" " = "null",
                                                              "Full model" = "n1",
                                                              "Model without Examination" = "y",
                                                              "Model without Examination and Agriculture" = "n2"
                                                              ), width='13cm',selected = "null")),
                   div(style="display:inline-block",htmlOutput('pic2'))
          )),
              
              # question3
          fluidRow(
            column(10, offset = 1,
              div(style="display:inline-block",selectInput("question3", "For Adjusted R-squared criterion, which model does the best using only three of the five variables?",
                                                           c(" " = "null",
                                                             "Model with Agriculture, Education, and Catholic" = "n1",
                                                             "Model with Examination, Education, and Infant Mortality" = "n2",
                                                             "Model with Education, Catholic, and Infant Mortality" = "y",
                                                             "Model with Agriculture, Examination, and Infant Mortality" = "n3"
                                                           ), width='13cm',selected = "null")),
              div(style="display:inline-block",htmlOutput('pic3'))
            )),
          
              # question4
          fluidRow(
            column(10, offset = 1,
              div(style="display:inline-block",selectInput("question4", "For BIC criterion, which model does the best using only three of the five variables?",
                                                           c(" " = "null",
                                                             "Model with Agriculture, Education, and Catholic" = "n1",
                                                             "Model with Education, Catholic, and Infant Mortality" = "y",
                                                             "Model with Examination, Education, and Infant Mortality" = "n2",
                                                             "Model with Agriculture, Examination, and Infant Mortality" = "n3"
                                                           ), width = '13cm',selected = "null")),
              div(style="display:inline-block",htmlOutput('pic4'))
            )),
          
              # question5
          fluidRow(
            column(10, offset = 1,
              div(style="display:inline-block",selectInput("question5", "For Mallow's Cp criterion, which model does the best using only three of the five variables?",
                                                           c(" " = "null",
                                                             "Model with Agriculture, Education, and Catholic" = "n1",
                                                             "Model with Examination, Education, and Infant Mortality" = "n2",
                                                             "Model with Agriculture, Examination, and Infant Mortality" = "n3",
                                                             "Model with Education, Catholic, and Infant Mortality" = "y"
                                                           ), width='13cm',selected = "null")),
              div(style="display:inline-block",htmlOutput('pic5'))
            )),
          
              # question6
          fluidRow(
            column(10, offset = 1,
              div(style="display:inline-block",selectInput("question6", "Is there any model using only three of the five variables that does well for all of the criteria?",
                                                           c("Yes, there is such a model." = "y",
                                                             "No, it does not exist." = "n",
                                                             " " = "null"), width='13cm',selected = "null")),
              div(style="display:inline-block",htmlOutput('pic6'))
            )),
          textOutput("feedback"), br(),
              # question7
          fluidRow(
            column(10, offset = 1,
              div(style="display:inline-block",textInput("question7","Which model makes the most sense to you in terms of the Swiss fertility context?", width='13cm',"")),
              div(style="display:inline-block",htmlOutput('pic7'))
            ))
        )
      )
    ),
    
    tabItem(
      tabName = "Ref",
      h2("References"),
      p(class = "hangingindent",
        "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, R package. 
                    Available from https://CRAN.R-project.org/package=shinyBS"),
      p(class = "hangingindent",
        "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: 
                    Create dashboards with 'Shiny', R Package. Available from 
                    https://CRAN.R-project.org/package=shinydashboard"),
      p(class = "hangingindent",
        "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019), 
                    shiny: Web application framework for R, R Package. 
                    Available from https://CRAN.R-project.org/package=shiny"),
      p(class = "hangingindent",
        "Lumley, T. (2020), leaps: Regression subset selection, R Package. Available 
        from https://cran.r-project.org/web/packages/leaps/index.html"),
      p(class = "hangingindent",
        "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom inputs
        widgets for shiny, R Package. Available from https://cran.r-project.org/web/packages/shinyWidgets/index.html")

    )
    
    
    )#close tabItems            
  )#close dashboardbody  
  )                                 
                      
