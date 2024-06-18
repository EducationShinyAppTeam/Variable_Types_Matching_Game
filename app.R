# Load Required Packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinyalert)
library(shinyWidgets)
library(boastUtils)
library(dplyr)

# Load Question Banks and Choices ----
bankA <- read.csv(file = "questionBankA.csv", stringsAsFactors = FALSE)
bankB <- read.csv(file = "questionBankB.csv", stringsAsFactors = FALSE)
bankC <- read.csv(file = "questionBankC.csv", stringsAsFactors = FALSE)
bankD <- read.csv(file = "questionBankD.csv", stringsAsFactors = FALSE)
level1Choices <- c("Select One", "Quantitative", "Qualitative")
level1Choicesp2 <- c("Select One", "Discrete", "Continuous", "Nominal", "Ordinal")
level2Choices <- c("Select One", "Quantitative and Discrete" = "QuanDiscrete", 
                   "Quantitative and Continuous" = "QuanContinuous",
                   "Qualitative and Nominal" = "QualNominal",
                   "Qualitative and Ordinal" = "QualOrdinal")

# Define UI ----
ui <- list(
  dashboardPage(
    skin = "red",
    ## Header ----
    dashboardHeader(
      title = "Variable Types",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Variable_Types_Matching_Game")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = "https://shinyapps.science.psu.edu/",
          icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisite", icon = icon("book")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        tabItem(
          ### Overview ----
          tabName = "overview",
          h1("Variable Types Matching Game"),
          p("Identify variable types by nature of measurement (quantitative (numeric) 
            discrete, quantitative continuous, qualitative (categorical)
            nominal, and qualitative ordinal variables) on the first two levels.
            Then, identify variable types by role in the analysis (explanatory, 
            response, and confounding) for the last two levels."),
          h2("Instructions"),
          tags$ol(
            tags$li("View prerequisites as needed on the 'Prerequisites' tab."),
            tags$li("Then, continue to the 'Game' tab to begin the game."),
            tags$li("Submit your answer only after finishing all of the questions."),
            tags$li("You may go to the next level once all of your answers are correct 
                    for levels 1 and 2."),
            tags$li("For levels 3 and 4, you must get 5 correct 
                    problems on each level to move on.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToGame",
              label = "Game!",
              icon = icon("bolt"),
              size = "large"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Yuxin Zhang, Luxin Wang, &
            Thomas McIntyre. Special thanks to Robert P. Carey III and
            Alex Chen for help on some programming issues. We'd also like to
            thank Mike Fleck for help with the qualitative and quantitaive 
            variables flow diagram. This app was updated in 2023 by Taryn McHugh
            and in June 2024 by Nathan Pechulis.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 6/11/2024 by NP.")
          ),
        ),
        ### Prerequisites ----
        tabItem(
          tabName = "prerequisite",
          withMathJax(),
          h2("Types of Variables"),
          br(),
          box(
            title = strong("Quantitative vs Qualitative Variables"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            tags$ul(
              tags$li("Nominal Variables are qualitative (categorical) variables 
                      that do not require a specific order or rank."),
              tags$li("Ordinal Variables are qualitative variables that require 
                      a specific order or rank."),
              tags$li("Discrete Variables are quantitative (numerical) variables 
                      where you can make a fixed list of possible variables."),
              tags$li("Continuous Variables are quantitative variables that can take on
                      an unlimited number of values within a range, they do not need 
                      to be fixed.")
            ),
            br(),
            tags$figure(
              class = "center-figure",
              tags$img(
                src = "variableFlowChart.png",
                width = "100%",
                alt = "Flow chart with three tiers. The top is variables and it 
                goes into quantitaive (which can be can be discrete or continuous) 
                and qualitative (which can be nominal or ordinal) variables."
              )
            )
          ),
          box(
            title = strong("Explanatory, Response, and Confounding Variables"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = "100%",          
            tags$ul(
              tags$li("Explanatory/Independent Variables are what might explain 
                      changes in the response variable."),
              tags$li("Response/Dependent Variables are what is the focus of the study."),
              tags$li("Confounding Variables are variables that are not in an experiment, 
                      but impact the relationship between explanatory and response.")
            ),
            br(),
            p("In the figure below you can see that it looks like ice cream sales 
              impacts the number of shark attacks. This is because the confounding variable 
              has an impact on both the explanatory and response, making it look 
              like there is a relationship."),
            tags$figure(
              class = "center-figure",
              tags$img(
                src = "ercChart.png",
                width = "100%",
                alt = "flow chart that describes explanatory, response, and confounding
                  variables"
              )
            ) 
          )
        ),
        ### Game ----
        tabItem(
          tabName = "game",
          tabsetPanel(
            id = "levels",
            type = "tabs",
            #### Level 1 ----
            tabPanel(
              title = "Level 1",
              value = "b",
              titlePanel("Matching Qualitative and Quantitative Variables"),
              p("To move onto the next level, you need to match all 12 variables to their
                correct variable types and categories. To submit your answers, you must answer all questions. 
                After submitting, icons will appear to show your results for each question; red means both parts
                of your answer are wrong, yellow means it is partially correct, and 
                green means it is entirely correct.
                If you get any wrong, click 'Retry' to try again."),
              hr(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ1"),
                    selectInput(
                      inputId = "lvl1Q1",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q1p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A1")
                  )
                ),
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ2"),
                    selectInput(
                      inputId = "lvl1Q2",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q2p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A2")
                  )
                ),
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ3"),
                    selectInput(
                      inputId = "lvl1Q3",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q3p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A3")
                  )
                )
              ), 
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ4"),
                    selectInput(
                      inputId = "lvl1Q4",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q4p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A4")
                  )
                ),
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ5"),
                    selectInput(
                      inputId = "lvl1Q5",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q5p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A5")
                  )
                ),
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ6"),
                    selectInput(
                      inputId = "lvl1Q6",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q6p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A6")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ7"),
                    selectInput(
                      inputId = "lvl1Q7",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q7p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A7")
                  )
                ),
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ8"),
                    selectInput(
                      inputId = "lvl1Q8",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q8p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A8")
                  )
                ),
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ9"),
                    selectInput(
                      inputId = "lvl1Q9",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q9p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A9")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ10"),
                    selectInput(
                      inputId = "lvl1Q10",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q10p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A10")
                  )
                ),
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ11"),
                    selectInput(
                      inputId = "lvl1Q11",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q11p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A11")
                  )
                ),
                column(
                  width = 4,
                  wellPanel(
                    uiOutput("titleQ12"),
                    selectInput(
                      inputId = "lvl1Q12",
                      label = "Quantitative or Qualitative",
                      choices = level1Choices
                    ),
                    selectInput(
                      inputId = "lvl1Q12p2",
                      label = "Choose a Category",
                      choices = level1Choicesp2
                    ),
                    uiOutput(outputId = "lvl1A12")
                  )
                )
              ),
              br(),
              ##### Buttons ----
              fluidRow(
                column(
                  width = 1,
                  offset = 5,
                  conditionalPanel(
                    "(input.lvl1Q1 != 'Select One') & (input.lvl1Q1p2 != 'Select One') &
                    (input.lvl1Q2 != 'Select One') & (input.lvl1Q2p2 != 'Select One') &
                    (input.lvl1Q3 != 'Select One') & (input.lvl1Q3p2 != 'Select One') &
                    (input.lvl1Q4 != 'Select One') & (input.lvl1Q4p2 != 'Select One') &
                    (input.lvl1Q5 != 'Select One') & (input.lvl1Q5p2 != 'Select One') &
                    (input.lvl1Q6 != 'Select One') & (input.lvl1Q6p2 != 'Select One') &
                    (input.lvl1Q7 != 'Select One') & (input.lvl1Q7p2 != 'Select One') &
                    (input.lvl1Q8 != 'Select One') & (input.lvl1Q8p2 != 'Select One') &
                    (input.lvl1Q9 != 'Select One') & (input.lvl1Q9p2 != 'Select One') &
                    (input.lvl1Q10 != 'Select One') & (input.lvl1Q10p2 != 'Select One') &
                    (input.lvl1Q11 != 'Select One') & (input.lvl1Q11p2 != 'Select One') &
                    (input.lvl1Q12 != 'Select One') & (input.lvl1Q12p2 != 'Select One')",
                    bsButton(
                      inputId = "retryA",
                      label = "Retry"
                    )
                  )
                ),
                column(
                  width = 1,
                  conditionalPanel(
                    "(input.lvl1Q1 != 'Select One') & (input.lvl1Q1p2 != 'Select One') &
                    (input.lvl1Q2 != 'Select One') & (input.lvl1Q2p2 != 'Select One') &
                    (input.lvl1Q3 != 'Select One') & (input.lvl1Q3p2 != 'Select One') &
                    (input.lvl1Q4 != 'Select One') & (input.lvl1Q4p2 != 'Select One') &
                    (input.lvl1Q5 != 'Select One') & (input.lvl1Q5p2 != 'Select One') &
                    (input.lvl1Q6 != 'Select One') & (input.lvl1Q6p2 != 'Select One') &
                    (input.lvl1Q7 != 'Select One') & (input.lvl1Q7p2 != 'Select One') &
                    (input.lvl1Q8 != 'Select One') & (input.lvl1Q8p2 != 'Select One') &
                    (input.lvl1Q9 != 'Select One') & (input.lvl1Q9p2 != 'Select One') &
                    (input.lvl1Q10 != 'Select One') & (input.lvl1Q10p2 != 'Select One') &
                    (input.lvl1Q11 != 'Select One') & (input.lvl1Q11p2 != 'Select One') &
                    (input.lvl1Q12 != 'Select One') & (input.lvl1Q12p2 != 'Select One')",
                    bsButton(
                      inputId = "submitA", 
                      label = "Submit")
                  )
                ),
                column(
                  width = 1,
                  offset = 3,
                  bsButton(
                    inputId = "toLvl2",
                    label = "Next Level",
                    disabled = TRUE
                  )
                )
              ),
              hr(),
              conditionalPanel(
                "input.submitA != 0",
                fluidRow(
                  wellPanel(
                    p(tags$strong("You must score 30 points to move onto the next level.")),
                    textOutput("scoreA")
                  )
                )
              )
            ),
            #### Level 2 ----
            tabPanel(
              title = "Level 2",
              value = "c",
              h2("Quantitative and Qualitative Variables in Plots"),
              p("Match the variable defined in the instructions of each plot
                   to the variable type until you get all 4 correct. 
                   To submit, you must answer all questions. If you get one wrong, 
                   click 'Retry' to try again."),
              hr(),
              fluidRow(
                column(
                  width = 8,
                  p(textOutput("imgQ1")),
                    uiOutput("image1")
                ),
                column(
                  width = 4,
                  wellPanel(
                    selectInput(
                      inputId = "lvl2Q1",
                      label = "Graph 1",
                      choices = level2Choices
                    ),
                    uiOutput(outputId = "lvl2A1")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 8,
                  p(textOutput("imgQ2")),
                  uiOutput("image2")
                ),
                column(
                  width = 4,
                  wellPanel(
                    selectInput(
                      inputId = "lvl2Q2",
                      label = "Graph 2",
                      choices = level2Choices
                    ),
                    uiOutput(outputId = "lvl2A2")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 8,
                  p(textOutput("imgQ3")),
                  uiOutput("image3")
                ),
                column(
                  width = 4,
                  wellPanel(
                    selectInput(
                      inputId = "lvl2Q3",
                      label = "Graph 3",
                      choices = level2Choices
                    ),
                    uiOutput(outputId = "lvl2A3")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 8,
                  p(textOutput("imgQ4")),
                  uiOutput("image4")
                ),
                column(
                  width = 4,
                  wellPanel(
                    selectInput(
                      inputId = "lvl2Q4",
                      label = "Graph 4",
                      choices = level2Choices
                    ),
                    uiOutput(outputId = "lvl2A4")
                  )
                )
              ),
              br(),
              ##### Buttons ----
              fluidRow(
                column(
                  width = 1,
                  offset = 1,
                  bsButton(
                    inputId = "prevLvl1",
                    label = "Previous Level"
                  )
                ),
                column(
                  width = 1,
                  offset = 3,
                  conditionalPanel(
                    "(input.lvl2Q1!='Select One') & (input.lvl2Q1!= null) &
                    (input.lvl2Q2!='Select One') & (input.lvl2Q2!=null) &
                    (input.lvl2Q3!='Select One') & (input.lvl2Q3!=null) &
                    (input.lvl2Q4!='Select One') & (input.lvl2Q4!=null)",
                    bsButton(
                      inputId = "retryB",
                      label = "Retry"
                    )
                  )
                ),
                column(
                  width = 1,
                  conditionalPanel(
                    "(input.lvl2Q1!='Select One') & (input.lvl2Q1!= null) &
                    (input.lvl2Q2!='Select One') & (input.lvl2Q2!=null) &
                    (input.lvl2Q3!='Select One') & (input.lvl2Q3!=null) &
                    (input.lvl2Q4!='Select One') & (input.lvl2Q4!=null)",
                    bsButton(
                      inputId = "submitB", 
                      label = "Submit")
                  )
                ),
                column(
                  width = 1,
                  offset = 3,
                  bsButton(
                    inputId = "toBtwnLvls",
                    label = "Next Level",
                    disabled = TRUE 
                  )
                )
              ),
              hr(),
              conditionalPanel(
                "input.submitB != 0",
                fluidRow(
                  wellPanel(
                    p(tags$strong("You must score 20 points to move onto the next level.")),
                    textOutput("scoreB")
                  )
                )
              )
            ),
            #### Page In Between Concepts ----
            tabPanel(
              title = "Concept Seperator",
              value = "d",
              titlePanel("Congrats on completing levels 1 and 2!"),
              p("Levels 1 and 2 were all about qualitative and quantitative variables. 
                  There are two more levels after this that go over explanatory,
                  response, and confounding variables. If you feel ready, press 
                  'Next Level' to continue onto Level 3 and 4. If not, press 'Finish' 
                  to go back to the overview page."),
              br(),
              fluidRow(
                wellPanel(
                  h3("Results"),
                  tags$ul(
                    tags$li(textOutput("level1ScoreResults1")),
                    tags$li(textOutput("level2ScoreResults1"))
                  )
                )
              ),
              br(),
              br(),
              fluidRow(
                column(
                  width = 2, 
                  offset = 2,
                  bsButton(
                    inputId = "prevLvl2",
                    label = "Previous Level"
                  )
                ),
                column(
                  width = 1,
                  offset = 1,
                  bsButton(
                    inputId = "btwnToFinish",
                    label = "Finish"
                  )
                ),
                column(
                  width = 1,
                  offset = 1,
                  bsButton(
                    inputId = "toLvl3",
                    label = "Next Level"
                  )
                )
              )
            ),
            #### Level 3 ----
            tabPanel(
              title = "Level 3",
              value = "e",
              titlePanel("Explanatory and Response Variables"),
              p("Correctly match each variable to the variable type depending on
                  the context given. You must get both answers correct to earn 1 point and get 5 points 
                  before moving to the next level. Once you have made your choices hit 'Submit', then 
                  click 'New Question' for the next question. You have 16 attempts, if all 16 attempts are 
                  used, follow directions and retry the level."),
              hr(),
              wellPanel(
                fluidRow(
                  uiOutput("questionC"), 
                  br()
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 3,
                  offset = 1,
                  selectInput(
                    inputId = "explC",
                    label = uiOutput("varEXP"),
                    c("", "Neither", "Explanatory", "Response")
                  ), 
                  uiOutput("markc1")
                ),
                column(
                  width = 3,
                  offset = 3,
                  selectInput(
                    inputId = "respC",
                    label = uiOutput("varRES"),
                    c("", "Neither", "Explanatory", "Response")
                  ), 
                  uiOutput("markc2")
                )
              ),
              br(),
              fluidRow(
                column(width = 4, offset = 3, textOutput("NumTries"))
              ),
              br(),
              ##### Buttons ----
              conditionalPanel(
                "input.toBtwnLvls != 0",
                fluidRow(
                  column(
                    width = 1, 
                    offset = 1,
                    bsButton(
                      inputId = "prevBtwnLvls",
                      label = "Previous Level"
                    )
                  ),
                  column(
                    width = 1, 
                    offset = 3,
                    conditionalPanel(
                      "(input.explC!='') & (input.respC!='')",
                      bsButton(
                        inputId = "submitC",
                        label = "Submit"
                      )
                    )
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "newQLvl3",
                      label = "New Question"
                    )
                  ),
                  column(
                    width = 1,
                    offset = 3,
                    bsButton(
                      inputId = "toLvl4",
                      label = "Next Level",
                      disabled = TRUE
                    )
                  )
                )
              ),
              hr(),
              fluidRow(
                progressBar(
                  id = "barLevel3",
                  value = 0,
                  display_pct = TRUE
                )
              )
            ),
            #### Level 4 ----
            tabPanel(
              title = "Level 4",
              value = "f",
              titlePanel("Explanatory, Response, and Confounding Variables"),
              p("Correctly match each variable to the variable type 
                  depending on the context given. You must get all three answers 
                  correct to earn 1 point and get 5 points before moving to the 
                  next level. Once you have made your choices hit 'Submit', then 
                  click 'New Question' for the next question. You have 8 attempts, 
                  if all 8 attempts are used, follow directions and retry the level."),
              hr(),
              wellPanel(
                fluidRow(uiOutput("questionD"))
              ),
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  selectInput(
                    inputId = "resp", 
                    label = uiOutput("varRESD"), 
                    c( "", "Explanatory", "Response", "Confounding",
                       "None of the above")
                  ),
                  uiOutput("markd2")
                ),
                column( 
                  width = 4,
                  offset = 1,
                  selectInput(
                    inputId = "conf", 
                    label = uiOutput("varCOND"), 
                    c("", "Explanatory", "Response", "Confounding",
                      "None of the above")
                  ), 
                  uiOutput("markd3")
                ),
                column(
                  width = 4,
                  offset = 1,
                  selectInput(
                    inputId = "expla",
                    label = uiOutput("varEXPD"),
                    c("", "Explanatory", "Response", "Confounding",
                      "None of the above")
                  ), 
                  uiOutput("markd1")
                )
              ),
              fluidRow(
                column(width = 3, offset = 3, textOutput("NumTriesLvl4"))
              ),
              br(),
              ##### Buttons ----
              conditionalPanel(
                "input.toLvl4 != 0",
                fluidRow(
                  column(
                    width = 1, 
                    offset = 1,
                    bsButton(
                      inputId = "prevLvl3",
                      label = "Previous Level"
                    )
                  ),
                  column(
                    width = 1, 
                    offset = 3, 
                    conditionalPanel(
                      "(input.expla!='') & (input.resp!='') & (input.conf!='')",
                      bsButton(inputId = "submitD", label = "Submit")
                    )
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "newQLvl4",
                      label = "New Question"
                    )
                  ),
                  column(
                    width = 1,
                    offset = 3,
                    bsButton(
                      inputId = "finish",
                      label = "Results",
                      disabled = TRUE
                    )
                  )
                ),
                hr()
              ),
              fluidRow(
                progressBar(
                  id = "barLevel4",
                  value = 0,
                  display_pct = TRUE
                )
              )
            ),
            #### Results ----
            tabPanel(
              title = "Results",
              value = "g",
              titlePanel("Congratulations! You finished the game."),
              br(),
              fluidRow(
                wellPanel(
                  h3("Results"),
                  tags$ul(
                    tags$li(textOutput("level1ScoreResults2")),
                    tags$li(textOutput("level2ScoreResults2")),
                    tags$li(textOutput("level3Score")),
                    tags$li(textOutput("level4Score"))
                  )
                )
              )
            )
          )
        ),
        tabItem(
          ### References ----
          tabName = "references",
          h2("References"),
          p(
            class = "hangingindent",
            "Attali, D. (2021). shinyjs: Easily Improve the User Experience of Your 
            Shiny Apps in Seconds. (v2.1). [R Package]. Avaliable from
            https://cran.r-project.org/package=shinyjs"
          ),
          p(
            class = "hangingindent",
            "Attali, D (2021). shintalert: Easily create pretty popup messages (modals),
            in Shiny. (v3.0). [R Package]. Avaliable from
            https://cran.r-project.org/web/packages/shinyalert/index.html"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N.J. (2023). boastUtils: BOAST utilities. 
            (v0.1.11.2). [R Package]. Avaliable from 
            https://github.com/EducationShinyappTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio (2021). shinydashboard: Create dashboards
            with 'Shiny.' (v0.7.2). Avaliable from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, 
            McPherson J, Dipert A, Borges B (2023). shiny: Web Application Framework 
            for R. R package version 1.7.4.9002. Avaliable from 
            https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D. (2023) shinyWidgets: Custom Input
            Widgets for Shiny. (v0.7.6). Avaliable from 
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K., and Vaughan, D.
            (2023). dplyr: A grammar of data manipulation. (v1.1.2). [R Package].
            Available from https://CRAN.R-project.org/package=dplyr"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define Server ----
server <- function(input, output, session) {
  ## Buttons  ----
  observeEvent(input$goToGame, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "game"
    )
  })
  
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Go through each level to demonstrate your proficiency in distinguishing
        between different variable types."
      )
    }
  )

  observeEvent(
    eventExpr = input$prevLvl1,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "levels",
        selected = 'b'
      )
    }
  )
  
  observeEvent(
    eventExpr = input$toLvl2,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "levels",
        selected = 'c'
      )
    }
  )
  
  observeEvent(
    eventExpr = input$prevLvl2,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "levels",
        selected = 'c'
      )
    }
  )
  
  observeEvent(
    eventExpr = input$toBtwnLvls,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "levels",
        selected = 'd'
      )
    }
  )
  
  observeEvent(
    eventExpr = input$toLvl3,
    handlerExpr = {
      summationC$correct1 <- c(0)
      updateProgressBar(
        id = "barLevel3",
        value = 0
      )
      updateTabsetPanel(
        session = session,
        inputId = "levels",
        selected = 'e'
      )
    }
  )
  
  observeEvent(
    eventExpr = input$btwnToFinish,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "pages",
        selected = "overview"
      )
    }
  )
  
  observeEvent(
    eventExpr = input$toLvl4,
    handlerExpr = {
      summationD$correct1D <- c(0)
      updateProgressBar(
        id = "barLevel4",
        value = 0
      )
      updateTabsetPanel(
        session = session,
        inputId = "levels",
        selected = 'f'
      )
      
    }
  )
  
  observeEvent(
    eventExpr = input$finish, 
    handlerExpr = {
      
      stmt <- boastUtils::generateStatement(
        session,
        verb = "completed",
        object = "shiny-tab-challenge",
        description = "Challenge completed",
        completion = TRUE
      )
      
      boastUtils::storeStatement(session, stmt)
      
      updateTabsetPanel(
        session = session, 
        inputId = "levels",
        selected = "g")
    }
  )
  
  ## Submit Observers ----
  observeEvent(
    eventExpr = input$submitA, 
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "submitA",
        disabled = TRUE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$retryA, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "submitA", 
        disabled = FALSE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$submitB, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "submitB", 
        disabled = TRUE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$retryB, 
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "submitB",
        disabled = FALSE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$submitC, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "submitC", 
        disabled = TRUE
      )
    }
  )
  
  observe({
    if (length(index_list$listc) == 1) {
      updateButton(
        session = session, 
        inputId = "newQLvl3",
        disabled = TRUE
      )
      updateButton(
        session = session, 
        inputId = "submitC",
        disabled = TRUE
      )
      shinyalert(
        title = "Oops!",
        text = "You have used up all 16 tries. Please click 'Previous Level' then
        click 'Next Level' to re-enter this level to try again",
        type = "error")
    }
  })
  
  observe({
    if (length(index_listD$listD) == 1) {
      updateButton(
        session = session, 
        inputId = "newQLvl4",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "submitD", 
        disabled = TRUE
      )
      shinyalert(
        title = "Oops!",
        text = "You have used up all 8 tries. Please click 'Previous Level'
        then click 'Next Level' to re-enter this level to try again", 
        type = "error")
    }
  })
  
  observeEvent(
    eventExpr = input$submitC, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "newQLvl3", 
        disabled = FALSE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$prevBtwnLvls, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "submitC", 
        disabled = FALSE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$newQLvl3,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "submitC",
        disabled = FALSE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$newQLvl3,
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "newQLvl3",
        disabled = TRUE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$submitD,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "submitD", 
        disabled = TRUE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$submitD, 
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "newQLvl4",
        disabled = FALSE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$prevLvl3, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "submitD",
        disabled = FALSE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$newQLvl4, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "submitD", 
        disabled = FALSE
      )
    }
  )
  
  observeEvent(
    eventExpr = input$newQLvl4, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "newQLvl4",
        disabled = TRUE
      ) 
    }
  )
  
  ## Level 1 ----
  scoreLevelA <- reactiveVal(0)
  
  subsetBankA <- reactiveVal(
    value = {
      subsetBankA <- bankA %>%
        group_by(Type,Category) %>%
        slice_sample(n = 4)
      
      randOrderL1 <- sample(x = 1:16, size = 16, replace = FALSE)
      subsetBankA[randOrderL1,]
    }
  )
  ### Labels ----
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q1"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q2"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q3"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q4"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q5"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q6"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q7"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q8"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q9"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q10"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q11"
      )
    }
  )
  
  observeEvent(
    eventExpr = c(input$retryA),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "lvl1Q12"
      )
    }
  )
  
  output$titleQ1 <- renderUI(
    expr = {
      tags$strong(paste("1. ", subsetBankA()$Variable[1]))
    }
  )
  
  output$titleQ2 <- renderUI(
    expr = {
      tags$strong(paste("2. ", subsetBankA()$Variable[2]))
    }
  )
  
  output$titleQ3 <- renderUI(
    expr = {
      tags$strong(paste("3. ", subsetBankA()$Variable[3]))
    }
  )
  
  output$titleQ4 <- renderUI(
    expr = {
      tags$strong(paste("4. ", subsetBankA()$Variable[4]))
    }
  )
  
  output$titleQ5 <- renderUI(
    expr = {
      tags$strong(paste("5. ", subsetBankA()$Variable[5]))
    }
  )
  
  output$titleQ6 <- renderUI(
    expr = {
      tags$strong(paste("6. ", subsetBankA()$Variable[6]))
    }
  )
  
  output$titleQ7 <- renderUI(
    expr = {
      tags$strong(paste("7. ", subsetBankA()$Variable[7]))
    }
  )
  
  output$titleQ8 <- renderUI(
    expr = {
      tags$strong(paste("8. ", subsetBankA()$Variable[8]))
    }
  )
  
  output$titleQ9 <- renderUI(
    expr = {
      tags$strong(paste("9. ", subsetBankA()$Variable[9]))
    }
  )
  
  output$titleQ10 <- renderUI(
    expr = {
      tags$strong(paste("10. ", subsetBankA()$Variable[10]))
    }
  )
  
  output$titleQ11 <- renderUI(
    expr = {
      tags$strong(paste("11. ", subsetBankA()$Variable[11]))
    }
  )
  
  output$titleQ12 <- renderUI(
    expr = {
      tags$strong(paste("12. ", subsetBankA()$Variable[12]))
    }
  )
  
  ### Validation ----
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      attempts$level1 <- attempts$level1 + 1
      if (!is.null(input$lvl1Q1)) {
        validType <- any(trimws(input$lvl1Q1) == subsetBankA()$Type[1])
        validCat <- any(trimws(input$lvl1Q1p2) == subsetBankA()$Category[1])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A1 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A1 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A1 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q2)) {
        validType <- any(trimws(input$lvl1Q2) == subsetBankA()$Type[2])
        validCat <- any(trimws(input$lvl1Q2p2) == subsetBankA()$Category[2])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A2 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A2 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A2 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q3)) {
        validType <- any(trimws(input$lvl1Q3) == subsetBankA()$Type[3])
        validCat <- any(trimws(input$lvl1Q3p2) == subsetBankA()$Category[3])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A3 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A3 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A3 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q4)) {
        validType <- any(trimws(input$lvl1Q4) == subsetBankA()$Type[4])
        validCat <- any(trimws(input$lvl1Q4p2) == subsetBankA()$Category[4])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A4 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A4 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A4 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q5)) {
        validType <- any(trimws(input$lvl1Q5) == subsetBankA()$Type[5])
        validCat <- any(trimws(input$lvl1Q5p2) == subsetBankA()$Category[5])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A5 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A5 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A5 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q6)) {
        validType <- any(trimws(input$lvl1Q6) == subsetBankA()$Type[6])
        validCat <- any(trimws(input$lvl1Q6p2) == subsetBankA()$Category[6])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A6 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A6 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A6 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q7)) {
        validType <- any(trimws(input$lvl1Q7) == subsetBankA()$Type[7])
        validCat <- any(trimws(input$lvl1Q7p2) == subsetBankA()$Category[7])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A7 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A7 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A7 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q8)) {
        validType <- any(trimws(input$lvl1Q8) == subsetBankA()$Type[8])
        validCat <- any(trimws(input$lvl1Q8p2) == subsetBankA()$Category[8])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A8 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A8 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A8 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q9)) {
        validType <- any(trimws(input$lvl1Q9) == subsetBankA()$Type[9])
        validCat <- any(trimws(input$lvl1Q9p2) == subsetBankA()$Category[9])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A9 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A9 <- renderIcon(icon = "partial", width = 30)
        }else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A9 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q10)) {
        validType <- any(trimws(input$lvl1Q10) == subsetBankA()$Type[10])
        validCat <- any(trimws(input$lvl1Q10p2) == subsetBankA()$Category[10])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A10 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A10 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A10 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q11)) {
        validType <- any(trimws(input$lvl1Q11) == subsetBankA()$Type[11])
        validCat <- any(trimws(input$lvl1Q11p2) == subsetBankA()$Category[11])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A11 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A11 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A11 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (!is.null(input$lvl1Q12)) {
        validType <- any(trimws(input$lvl1Q12) == subsetBankA()$Type[12])
        validCat <- any(trimws(input$lvl1Q12p2) == subsetBankA()$Category[12])
        if (validType & validCat) {
          scoreLevelA(scoreLevelA() + 2.5)
          output$lvl1A12 <- renderIcon(icon = "correct", width = 30)
        } else if (validType | validCat) {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A12 <- renderIcon(icon = "partial", width = 30)
        } else {
          scoreLevelA(scoreLevelA() + 0)
          output$lvl1A12 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  ### Scoring and Update Buttons ----
  observeEvent(
    eventExpr = input$retryA, 
    handlerExpr = {
      output$lvl1A1 <- renderIcon()
      output$lvl1A2 <- renderIcon()
      output$lvl1A3 <- renderIcon()
      output$lvl1A4 <- renderIcon()
      output$lvl1A5 <- renderIcon()
      output$lvl1A6 <- renderIcon()
      output$lvl1A7 <- renderIcon()
      output$lvl1A8 <- renderIcon()
      output$lvl1A9 <- renderIcon()
      output$lvl1A10 <- renderIcon()
      output$lvl1A11 <- renderIcon()
      output$lvl1A12 <- renderIcon()
      scoreLevelA(0)
      updateButton(
        session = session,
        inputId = "submitA",
        disabled = FALSE
      )
    }
  )
  
  output$scoreA <- renderText(
    expr = {
      paste("You have", scoreLevelA(), "points.")
    }
  )
  
  observeEvent(
    eventExpr = input$submitA,
    handlerExpr = {
      if (scoreLevelA() >= 30) {
        updateButton(
          session = session,
          inputId = "toLvl2",
          disabled = FALSE
        )
      }
    }
  )
  
  ## Level 2 ----
  scoreLevelB <- reactiveVal(0)
  
  subsetBankB <- reactiveVal(
    value = {
      subsetBankB <- bankB %>%
        group_by(Type) %>%
        slice_sample(n = 1)
      
      randOrderL2 <- sample(x = 1:4, size = 4, replace = FALSE)
      subsetBankB[randOrderL2,]
    }
  )
  
  ### Labels/Images ----
  output$imgQ1 <- renderText(
    expr = {
      paste("1.", subsetBankB()$Question[1])
    }
  )
  output$image1 <- renderUI(
    expr = {
      img(src = subsetBankB()$Variable[1],
          alt = subsetBankB()$Alt[1],
          width = "85%",
          #height = "95%", 
          style = "text-align: center;")
    }
  )
  
  output$imgQ2 <- renderText(
    expr = {
      paste("2.", subsetBankB()$Question[2])
    }
  )
  output$image2 <- renderUI(
    expr = {
      img(src = subsetBankB()$Variable[2],
          alt = subsetBankB()$Alt[2],
          width = "85%",
          #height = "95%", 
          style = "text-align: center;")
    }
  )
  
  output$imgQ3 <- renderText(
    expr = {
      paste("3.", subsetBankB()$Question[3])
    }
  )
  output$image3 <- renderUI(
    expr = {
      img(src = subsetBankB()$Variable[3],
          alt = subsetBankB()$Alt[3],
          width = "85%",
          #height = "95%", 
          style = "text-align: center;")
    }
  )
  
  output$imgQ4 <- renderText(
    expr = {
      paste("4.", subsetBankB()$Question[4])
    }
  )
  output$image4 <- renderUI(
    expr = {
      img(src = subsetBankB()$Variable[4],
          alt = subsetBankB()$Alt[4],
          width = "85%",
          #height = "95%", 
          style = "text-align: center;")
    }
  )
  
  ### Validation ----
  observeEvent(
    eventExpr = input$submitB,
    handlerExpr = {
      attempts$level2 <- attempts$level2 + 1
      if (!is.null(input$lvl2Q1)) {
        valid <- any(trimws(input$lvl2Q1) == subsetBankB()$Type[1])
        if (valid) {
          scoreLevelB(scoreLevelB() + 5)
          output$lvl2A1 <- renderIcon(icon = "correct", width = 30)
        } else {
          scoreLevelB(scoreLevelB() + 0)
          output$lvl2A1 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitB,
    handlerExpr = {
      if (!is.null(input$lvl2Q2)) {
        valid <- any(trimws(input$lvl2Q2) == subsetBankB()$Type[2])
        if (valid) {
          scoreLevelB(scoreLevelB() + 5)
          output$lvl2A2 <- renderIcon(icon = "correct", width = 30)
        } else {
          scoreLevelB(scoreLevelB() + 0)
          output$lvl2A2 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitB,
    handlerExpr = {
      if (!is.null(input$lvl2Q3)) {
        valid <- any(trimws(input$lvl2Q3) == subsetBankB()$Type[3])
        if (valid) {
          scoreLevelB(scoreLevelB() + 5)
          output$lvl2A3 <- renderIcon(icon = "correct", width = 30)
        } else {
          scoreLevelB(scoreLevelB() + 0)
          output$lvl2A3 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$submitB,
    handlerExpr = {
      if (!is.null(input$lvl2Q4)) {
        valid <- any(trimws(input$lvl2Q4) == subsetBankB()$Type[4])
        if (valid) {
          scoreLevelB(scoreLevelB() + 5)
          output$lvl2A4 <- renderIcon(icon = "correct", width = 30)
        } else {
          scoreLevelB(scoreLevelB() + 0)
          output$lvl2A4 <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    }
  )
  
  ### Scoring and Update Buttons----
  observeEvent(
    eventExpr = input$retryB,
    handlerExpr = {
      output$lvl2A1 <- renderIcon()
      output$lvl2A2 <- renderIcon()
      output$lvl2A3 <- renderIcon()
      output$lvl2A4 <- renderIcon()
      scoreLevelB(0)
    }
  )
  
  observeEvent(
    eventExpr = input$submitB, 
    handlerExpr = {
      if (scoreLevelB() >= 20) {
        updateButton(
          session = session, 
          inputId = "toBtwnLvls",
          disabled = FALSE
        )
      }
      else {
        updateButton(
          session = session, 
          inputId = "toBtwnLvls", 
          disabled = TRUE
        )
      }
    }
  )
  
  output$scoreB <- renderText(
    expr = {
      paste("You have", scoreLevelB(), "points.")
    }
  )

  ## Level 3 ----
  index <- reactiveValues(index = 18)
  
  index_list <- reactiveValues(listc = sample(1:17, 17, replace = FALSE))
  
  observeEvent(
    eventExpr = input$prevBtwnLvls, 
    handlerExpr = {
      updateTabsetPanel(
        session = session, 
        inputId = "levels", 
        selected = "d")
      if (length(index_list$listc) < 17) {
        index_list$listc <- c(index_list$listc, sample(1:17, 17, replace = FALSE))
      } 
      index_list$listc <- index_list$listc[1:17]
    })
  
  observeEvent(
    eventExpr = input$toBtwnLvls,
    handlerExpr = {
      index$index <- 18
      index$exp_index <- 2 * index$index - 1
      index$res_index <- 2 * index$index
    }
  )
  
  observeEvent(
    eventExpr = input$newQLvl3,
    handlerExpr = {
      index_list$listc <- index_list$listc[-1]
      index$index <- index_list$listc[1]
      index$exp_index <- 2 * index$index - 1
      index$res_index <- 2 * index$index
      updateSelectInput(
        inputId = "explC",
        selected = ""
      )
      updateSelectInput(
        inputId = "respC",
        selected = ""
      )
    }
  )
  
  key1 <- as.matrix(bankC[1:36, 1])
  ### Labels ----
  output$questionC <- renderUI(
    expr = {
      if (index$index == 1) {
        p(bankC[1, 5])
      } else if (index$index == 2) {
        p(bankC[3, 5])
      } else if (index$index == 3) {
        p(bankC[5, 5])
      } else if (index$index == 4) {
        p(bankC[7, 5])
      }
      else if (index$index == 5) {
        p(bankC[9, 5])
      } else if (index$index == 6) {
        p(bankC[11, 5])
      } else if (index$index == 7) {
        p(bankC[13, 5])
      } else if (index$index == 8) {
        p(bankC[15, 5])
      }
      else if (index$index == 9) {
        p(bankC[17, 5])
      } else if (index$index == 10) {
        p(bankC[19, 5])
      } else if (index$index == 11) {
        p(bankC[21, 5])
      } else if (index$index == 12) {
        p(bankC[23, 5])
      } else if (index$index == 13) {
        p(bankC[25, 5])
      } else if (index$index == 14) {
        p(bankC[27, 5])
      } else if (index$index == 15) {
        p(bankC[29, 5])
      } else if (index$index == 16) {
        p(bankC[31, 5])
      } else if (index$index == 17) {
        p(bankC[33, 5])
      } else if (index$index == 18) {
        p(bankC[35, 5])
      }
    }
  )
  
  output$varEXP <- renderUI(
    expr = {
      if (index$index == 1) {
        p(tags$strong(bankC[1, 4]))
      } else if (index$index == 2) {
        p(tags$strong(bankC[3, 4]))
      } else if (index$index == 3) {
        p(tags$strong(bankC[5, 4]))
      } else if (index$index == 4) {
        p(tags$strong(bankC[7, 4]))
      }
      else if (index$index == 5) {
        p(tags$strong(bankC[9, 4]))
      } else if (index$index == 6) {
        p(tags$strong(bankC[11, 4]))
      } else if (index$index == 7) {
        p(tags$strong(bankC[13, 4]))
      } else if (index$index == 8) {
        p(tags$strong(bankC[15, 4]))
      }
      else if (index$index == 9) {
        p(tags$strong(bankC[17, 4]))
      } else if (index$index == 10) {
        p(tags$strong(bankC[19, 4]))
      } else if (index$index == 11) {
        p(tags$strong(bankC[21, 4]))
      } else if (index$index == 12) {
        p(tags$strong(bankC[23, 4]))
      }
      else if (index$index == 13) {
        p(tags$strong(bankC[25, 4]))
      } else if (index$index == 14) {
        p(tags$strong(bankC[27, 4]))
      } else if (index$index == 15) {
        p(tags$strong(bankC[29, 4]))
      } else if (index$index == 16) {
        p(tags$strong(bankC[31, 4]))
      }
      else if (index$index == 17) {
        p(tags$strong(bankC[33, 4]))
      } else if (index$index == 18) {
        p(tags$strong(bankC[35, 4]))
      }
    }
  )
  
  output$varRES <- renderUI(
    expr = {
      if (index$index == 1) {
        p(tags$strong(bankC[2, 4]))
      } else if (index$index == 2) {
        p(tags$strong(bankC[4, 4]))
      } else if (index$index == 3) {
        p(tags$strong(bankC[6, 4]))
      } else if (index$index == 4) {
        p(tags$strong(bankC[8, 4]))
      }
      else if (index$index == 5) {
        p(tags$strong(bankC[10, 4]))
      } else if (index$index == 6) {
        p(tags$strong(bankC[12, 4]))
      } else if (index$index == 7) {
        p(tags$strong(bankC[14, 4]))
      } else if (index$index == 8) {
        p(tags$strong(bankC[16, 4]))
      }
      else if (index$index == 9) {
        p(tags$strong(bankC[18, 4]))
      } else if (index$index == 10) {
        p(tags$strong(bankC[20, 4]))
      } else if (index$index == 11) {
        p(tags$strong(bankC[22, 4]))
      } else if (index$index == 12) {
        p(tags$strong(bankC[24, 4]))
      }
      else if (index$index == 13) {
        p(tags$strong(bankC[26, 4]))
      } else if (index$index == 14) {
        p(tags$strong(bankC[28, 4]))
      } else if (index$index == 15) {
        p(tags$strong(bankC[30, 4]))
      } else if (index$index == 16) {
        p(tags$strong(bankC[32, 4]))
      }
      else if (index$index == 17) {
        p(tags$strong(bankC[34, 4]))
      } else if (index$index == 18) {
        p(tags$strong(bankC[36, 4]))
      }
    }
  )
  ### Validate ----
  observeEvent(
    eventExpr = input$submitC,
    handlerExpr = {
      attempts$level3 <- attempts$level3 + 1
      observeEvent(
        eventExpr = input$newQLvl3,
        handlerExpr = {
          output$markc1 <- renderUI(
            img(src = NULL,width = 30)
          )
        }
      )
      observe({
        eventExpr = output$markc1 <- renderUI(expr = {
          if (!is.null(input$explC)) {
            if (any(input$explC == key1[index$exp_index,1])) {
              renderIcon(icon = "correct", width = 30)
            } else {
              renderIcon(icon = "incorrect", width = 30)
            }
          }
        })
      })
    })
  
  observeEvent(
    eventExpr = input$submitC,
    handlerExpr = {
      observeEvent(
        eventExpr = input$newQLvl3,
        handlerExpr = {
          output$markc2 <- renderUI(
            img(src = NULL,width = 30)
          )
        })
      observe({
        eventExpr = output$markc2 <- renderUI(expr = {
          if (!is.null(input$respC)) {
            if (any(input$respC == key1[index$res_index,1])) {
              renderIcon(icon = "correct", width = 30)
            } else {
              renderIcon(icon = "incorrect", width = 30)
            }
          }
        })
      })
    })
  
  observeEvent(
    eventExpr = input$newQLvl3,
    handlerExpr = {
      reset(id = "expl3")
      reset(id = "resp3")
      reset(id = "submit")
    }
  )
  
  ### Scoring and Update Buttons ----
  summation <- reactiveValues( summationC = c(rep(0, 20)), summationD = c(rep(0, 20)))
  summationC <- reactiveValues(correct1 = c(0), started = FALSE)
  
  observeEvent(
    eventExpr = input$toBtwnLvls, 
    handlerExpr = {
      summationC$started <- TRUE
    }
  )
  
  observeEvent(
    eventExpr = input$newQLvl3, 
    handlerExpr = {
      summationC$started <- TRUE
    }
  )
  
  observeEvent(
    eventExpr = input$submitC, 
    handlerExpr = {
      summationC$started <- TRUE
    }
  )
  
  observeEvent(
    eventExpr = input$submitC,
    handlerExpr = {
      success <- FALSE
      for (i in c(input$explC)) {
        success <- any(input$explC == key1[index$exp_index, 1]) & any(input$respC == key1[index$res_index, 1])
        if (success) {
          summationC$correct1 <- c(summationC$correct1, 1)
        } else {
          summationC$correct1 <- c(summationC$correct1, 0)
        }
      }
      
      total <- sum(c(summationC$correct1))
      
      response <- list(
        "Explanatory" = input$explC,
        "Response" = input$respC
      )
      
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "level3",
        description = "Explanatory and Response Variables",
        interactionType = "choice",
        response = jsonlite::toJSON(response),
        success = success
      )
      
      boastUtils::storeStatement(session, stmt)
      
      summation$summationC[input$submitC] <- total
    })
  
  output$NumTries <- renderPrint(
    expr = {
      tries_left = length(index_list$listc) - 1
      cat("You have", tries_left, "tries left")
    }
  )
  
  observeEvent(
    eventExpr = input$submitC,
    handlerExpr = {
      if (summation$summationC[input$submitC] >= 5) {
        updateButton(
          session = session, 
          inputId = "toLvl4", 
          disabled = FALSE)
        updateButton(
          session = session,
          inputId = "newQLvl3",
          disabled = FALSE)
      }
    }
  )
  
  ### Progress Bar ----
  observe(
    if (sum(c(summationC$correct1)) == 1) {
      output$barLevel3 <- updateProgressBar(
        id = "barLevel3",
        value = 20
      )
    }
    else if (sum(c(summationC$correct1)) == 2) {
      output$barLevel3 <- updateProgressBar(
        id = "barLevel3",
        value = 40
      )
    }
    else if (sum(c(summationC$correct1)) == 3) {
      output$barLevel3 <- updateProgressBar(
        id = "barLevel3",
        value = 60
      )
    }
    else if (sum(c(summationC$correct1)) == 4) {
      output$barLevel3 <- updateProgressBar(
        id = "barLevel3",
        value = "80"
      )
    }
    else if (sum(c(summationC$correct1)) == 5) {
      output$barLevel3 <- updateProgressBar(
        id = "barLevel3",
        value = 100
      )
    }
  )
  ## Level 4 ----
  index2 <- reactiveValues(index2 = 10)
  
  index_listD <- reactiveValues(listD = sample(1:9, 9, replace = FALSE))
  
  observeEvent(
    eventExpr = input$prevLvl3, 
    handlerExpr = {
      updateTabsetPanel(
        session = session, 
        inputId = "levels", 
        selected = "e")
      if (length(index_listD$listD) < 9) {
        index_listD$listD <- c(index_listD$listD, sample(1:9, 9, replace = FALSE)) 
      }
      index_listD$listD <- index_listD$listD[1:9]
    })
  
  ### Labels ----
  observeEvent(
    eventExpr = input$toLvl4,
    handlerExpr = {
      index2$index2 <- 9
      index2$explan <- 3 * index2$index2 - 2
      index2$respon <- 3 * index2$index2 - 1
      index2$confou <- 3 * index2$index2
    }
  )
  
  observeEvent(
    eventExpr = input$newQLvl4,
    handlerExpr = {
      index_listD$listD <- index_listD$listD[-1]
      index2$index2 <- index_listD$listD[1]
      index2$explan <- 3 * index2$index2 - 2
      index2$respon <- 3 * index2$index2 - 1
      index2$confou <- 3 * index2$index2
      updateSelectInput(
        inputId = "resp",
        selected = ""
      )
      updateSelectInput(
        inputId = "conf",
        selected = ""
      )
      updateSelectInput(
        inputId = "expla",
        selected = ""
      )
    }
  )
  
  key2 <- as.matrix(bankD[1:27, 1])
  
  output$questionD <- renderUI(
    expr = {
      if (index2$index2 == 1) {
        p(bankD[1, 4])
      } else if (index2$index2 == 2) {
        p(bankD[4, 4])
      } else if (index2$index2 == 3) {
        p(bankD[7, 4])
      } else if (index2$index2 == 4) {
        p(bankD[10, 4])
      }
      else if (index2$index2 == 5) {
        p(bankD[13, 4])
      }
      else if (index2$index2 == 6) {
        p(bankD[16, 4])
      }
      else if (index2$index2 == 7) {
        p(bankD[19, 4])
      }
      else if (index2$index2 == 8) {
        p(bankD[22, 4])
      }
      else if (index2$index2 == 9) {
        p(bankD[25, 4])
      }
    }
  )
  
  output$varEXPD <- renderUI(
    expr = {
      if (index2$index2 == 1) {
        p(tags$strong(bankD[1, 3]))
      } else if (index2$index2 == 2) {
        p(tags$strong(bankD[4, 3]))
      } else if (index2$index2 == 3) {
        p(tags$strong(bankD[7, 3]))
      } else if (index2$index2 == 4) {
        p(tags$strong(bankD[10, 3]))
      }
      else if (index2$index2 == 5) {
        p(tags$strong(bankD[13, 3]))
      }
      else if (index2$index2 == 6) {
        p(tags$strong(bankD[16, 3]))
      }
      else if (index2$index2 == 7) {
        p(tags$strong(bankD[19, 3]))
      }
      else if (index2$index2 == 8) {
        p(tags$strong(bankD[22, 3]))
      }
      else if (index2$index2 == 9) {
        p(tags$strong(bankD[25, 3]))
      }
    }
  )
  
  output$varRESD <- renderUI(
    expr = {
      if (index2$index2 == 1) {
        p(tags$strong(bankD[2, 3]))
      } else if (index2$index2 == 2) {
        p(tags$strong(bankD[5, 3]))
      } else if (index2$index2 == 3) {
        p(tags$strong(bankD[8, 3]))
      } else if (index2$index2 == 4) {
        p(tags$strong(bankD[11, 3]))
      }
      else if (index2$index2 == 5) {
        p(tags$strong(bankD[14, 3]))
      }
      else if (index2$index2 == 6) {
        p(tags$strong(bankD[17, 3]))
      }
      else if (index2$index2 == 7) {
        p(tags$strong(bankD[20, 3]))
      }
      else if (index2$index2 == 8) {
        p(tags$strong(bankD[23, 3]))
      }
      else if (index2$index2 == 9) {
        p(tags$strong(bankD[26, 3]))
      }
    }
  )
  
  output$varCOND <- renderUI(
    expr = {
      if (index2$index2 == 1) {
        p(tags$strong(bankD[3, 3]))
      } else if (index2$index2 == 2) {
        p(tags$strong(bankD[6, 3]))
      } else if (index2$index2 == 3) {
        p(tags$strong(bankD[9, 3]))
      } else if (index2$index2 == 4) {
        p(tags$strong(bankD[12, 3]))
      }
      else if (index2$index2 == 5) {
        p(tags$strong(bankD[15, 3]))
      }
      else if (index2$index2 == 6) {
        p(tags$strong(bankD[18, 3]))
      }
      else if (index2$index2 == 7) {
        p(tags$strong(bankD[21, 3]))
      }
      else if (index2$index2 == 8) {
        p(tags$strong(bankD[24, 3]))
      }
      else if (index2$index2 == 9) {
        p(tags$strong(bankD[27, 3]))
      }
    }
  )
  
  ### Validate  ----
  observeEvent(
    eventExpr = input$submitD,
    handlerExpr = {
      attempts$level4 <- attempts$level4 + 1
      observeEvent(
        eventExpr = input$newQLvl4,
        handlerExpr = {
          output$markd1 <- renderUI(
            expr = {
              img(src = NULL, width = 30)
            }
          )
        }
      )
      observe({
        output$markd1 <- renderUI(
          expr = {
            if (!is.null(input$expla)) {
              if (any(input$expla == key2[index2$explan, 1])) {
                renderIcon(icon = "correct", width = 30)
              } else {
                renderIcon(icon = "incorrect", width = 30)
              }
            }
          }
        )
      })
    })
  
  observeEvent(
    eventExpr = input$submitD, 
    handlerExpr = {
      observeEvent(input$newQLvl4, {
        output$markd2 <- renderUI(
          expr = {
            img(src = NULL, width = 30)
          }
        )
      }
      )
      observe({
        output$markd2 <- renderUI(
          expr = {
            if (!is.null(input$resp)) {
              if (any(input$resp == key2[index2$respon, 1])) {
                renderIcon(icon = "correct", width = 30)
              } else {
                renderIcon(icon = "incorrect", width = 30)
              }
            }
          }
        )
      })
    })
  
  observeEvent(
    eventExpr = input$submitD,
    handlerExpr = {
      observeEvent(
        eventExpr = input$newQLvl4,
        handlerExpr = {
          output$markd3 <- renderUI(
            expr = {
              img(src = NULL, width = 30)
            }
          )
        }
      )
      observe({
        output$markd3 <- renderUI(
          expr = {
            if (!is.null(input$conf)) {
              if (any(input$conf == key2[index2$confou, 1])) {
                renderIcon(icon = "correct", width = 30)
              } else {
                renderIcon(icon = "incorrect", width = 30)
              }
            }
          }
        )
      })
    })
  
  observeEvent(
    eventExpr = input$newQLvl4,
    handlerExpr = {
      reset(id = "expla")
      reset(id = "resp")
      reset(id = "conf")
    }
  )
  
  ### Scoring and Update Buttons ----
  summationD <- reactiveValues(correct1D = c(0), started = FALSE)
  test <- reactiveValues(A = FALSE, B = FALSE, C = TRUE)
  
  observeEvent(
    eventExpr = input$submitD, 
    handlerExpr = {
      success <- FALSE
      for (x in c(input$expla)) {
        success <- (any(input$expla == key2[index2$explan,1]) & any(input$resp == key2[index2$respon,1]) & any(input$conf == key2[index2$confou,1]))
        if (success) {
          summationD$correct1D <- c(summationD$correct1D, 1)
        } else {
          summationD$correct1D <- c(summationD$correct1D, 0)
        }
      }
      
      total <- sum(c(summationD$correct1D))
      
      ## TODO: FIX INPUT$CONF & INPUT$RESP VALUES ARE SWITCHED
      response <- list(
        "Explanatory" = input$expla,
        "Response" = input$conf,
        "Confounding" = input$resp
      )
      
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "level4",
        description = "This level will add in the concepts of confounding variables.",
        interactionType = "choice",
        response = jsonlite::toJSON(response),
        success = success
      )
      
      boastUtils::storeStatement(session, stmt)
    })
  
  observeEvent(
    eventExpr = input$submitD, 
    handlerExpr = {
      if (sum(c(summationD$correct1D)) >= 5) {
        updateButton(
          session = session, 
          inputId = "finish",
          disabled = FALSE
        )
        updateButton(
          session = session, 
          inputId = "newQLvl4", 
          disabled = FALSE)
      }
    }
  )
  
  output$NumTriesLvl4 <- renderPrint(
    expr = {
      tries_left = length(index_listD$listD) - 1
      cat("You have", tries_left, "tries left")
    }
  )
  
  
  ### Progress Bar ----
  observe(
    if (sum(c(summationD$correct1D)) == 1) {
      output$barLevel4 <- updateProgressBar(
        id = "barLevel4",
        value = 20
      )
    }
    else if (sum(c(summationD$correct1D)) == 2) {
      output$barLevel4 <- updateProgressBar(
        id = "barLevel4",
        value = 40
      )
    }
    else if (sum(c(summationD$correct1D)) == 3) {
      output$barLevel4 <- updateProgressBar(
        id = "barLevel4",
        value = 60
      )
    }
    else if (sum(c(summationD$correct1D)) == 4) {
      output$barLevel4 <- updateProgressBar(
        id = "barLevel4",
        value = 80
      )
    }
    else if (sum(c(summationD$correct1D)) == 5) {
      output$barLevel4 <- updateProgressBar(
        id = "barLevel4",
        value = 100
      )
    }
  )
  
  ## Results ----
  attempts <- reactiveValues(
    level1 = 0,
    level2 = 0,
    level3 = 0,
    level4 = 0
  )
  
  ### Results 1
  output$level1ScoreResults1 <- renderPrint(
    expr = {
      cat("It took", max(attempts$level1), "attempts to complete level 1.")
    }
  )
  output$level2ScoreResults1 <- renderPrint(
    expr = {
      cat("It took", max(attempts$level2), "attempts to complete level 2.")
    }
  )
  
  ### Results 2
  output$level1ScoreResults2 <- renderPrint(
    {
      cat("It took", max(attempts$level1), "attempts to complete level 1.")
    }
  )
  output$level2ScoreResults2 <- renderPrint(
    expr = {
      cat("It took", max(attempts$level2), "attempts to complete level 2.")
    }
  )
  output$level3Score <- renderPrint(
    expr = {
      cat("It took", max(attempts$level3), "questions to complete level 3.")
    }
  )
  output$level4Score <- renderPrint(
    expr = {
      cat("It took", max(attempts$level4), "questions to complete level 4.")
    }
  )
}

boastUtils::boastApp(ui = ui, server = server)
