# Load Required Packages
library(shiny)
library(shinydashboard)
library(shinyDND)
library(shinyjs)
library(shinyBS)
library(shinyalert)
library(shinyWidgets)
library(boastUtils)

source("variableFlowChart.R")

# Load Question Banks ----
bank <- read.csv(file = "questionBank.csv", stringsAsFactors = FALSE)
bankB <- read.csv(file = "questionBankB.csv", stringsAsFactors = FALSE)
bankC <- read.csv(file = "questionBankC.csv", stringsAsFactors = FALSE)
bankD <- read.csv(file = "questionBankD.csv", stringsAsFactors = FALSE)

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
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
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
          p("Identify variable types by nature of measurement [Quantitative (numeric) 
            discrete variables, Quantitative continuous variables, Qualitative (categorical)
            nominal variables, and Qualitative ordinal variables] and by role in 
            the analysis [explanatory versus response versus confounding]."),
          h2("Instructions"),
          tags$ol(
            tags$li("View prerequisites as needed on the prerequsities tab."),
            tags$li("Go to the game tab to complete levels of variable deciphering"),
            tags$li("Submit your answer only after finishing all the questions."),
            tags$li("You may go to the next level once all of your answers are correct 
                    for level 1 and 2. For level 3 and 4 you must get 5 correct 
                    problems on each level to finish the levels.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToGame",
              label = "Game!",
              icon = icon("bolt"),
              size = "large",
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Yuxin Zhang, Luxin Wang, &
            Thomas McIntyre. Special thanks to Robert P. Carey III and
            Alex Chen for help on some programming issues. We'd also like to
            thank Mike Fleck for help with the flow diagram.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 6/15/2021 by TM.")
          )
        ),
        ### Prerequisites ----
        tabItem(
          tabName = "prerequisite",
          withMathJax(),
          box(
            title = strong("Types of Variables"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            tags$ul(
              tags$li("Nominal Variables are qualitative variables that do not 
                      require a specific ordering"),
              tags$li("Ordinal Variables are qualitative variables that require 
                      a specific ordering "),
              tags$li("Discrete Variables are quantitative variables that are whole 
                      numbers."),
              tags$li("Continuous Variables are quantitative variables that are 
                      are continuous, they do not need to be fixed values.")
            ),
            div(
              style = "text-align: center;",
              variableFlowChart
            ),
          ),
          box(
            title = strong("Types of Variables"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",          
            tags$ul(
              tags$li("Explanatory/Independent Variables are what is changed in 
                      a relationship to see its effects on the response variable"),
              tags$li("Response/Dependent Variables are what is being measured."),
              tags$li("Confounding Variables are variables not in an experiment, 
                      but impacts the relationship between explanatory and response.")
            ),
            br(),
            p("In the figure below you can see that it looks like ice cream sales 
              is impact the number of shark attacks. This is because the confounding variable 
              is making it look like there is a relationship."),
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
          h2("Level 1"),
          tabsetPanel(
            id = "levels",
            #type = "hidden",
            #### Level 1 ----
            tabPanel(
              title ="Level 1",
               value = "b",
              p("Correctly identify the variable type of each variable."),
              hr(),
              fluidRow(
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group1",
                    label = textOutput("disName1"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                ),
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group2",
                    label = textOutput("disName2"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                ),
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group3",
                    label = textOutput("nomName1"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group4",
                    label = textOutput("contName1"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                ),
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group5",
                    label = textOutput("disName3"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                ),
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group6",
                    label = textOutput("contName2"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group7",
                    label = textOutput("nomName2"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                ),
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group8",
                    label = textOutput("ordName1"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                ),
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group9",
                    label = textOutput("contName3"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group10",
                    label = textOutput("ordName2"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                ),
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group11",
                    label = textOutput("nomName3"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                ),
                column(
                  width = 4,
                  radioGroupButtons(
                    inputId = "group12",
                    label = textOutput("ordName3"),
                    choices = c("Qualitative and Ordinal","Qualitative and 
                                       Nominal", "Quantitative and Discrete", "
                                       Quantitative and Continuous"),
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon")
                    )
                  )
                )
              ),
              hr(),
              ##### Buttons ----
              fluidRow(
                column(
                  width = 1,
                  bsButton(
                    inputId = "Reset1",
                    label = "Reset"
                  )
                ),
                column(
                  width = 1,
                  offset = 4,
                  conditionalPanel(
                    "(input.group1!='') & (input.group2!='') & (input.group3!='')
                           & (input.group4!='') & (input.group5!='') & (input.group6!='')
                           & (input.group7!='') & (input.group8!='') & (input.group9!='')",
                    bsButton(
                      inputId = "submitA",
                      label = "Submit"
                    )
                  )
                ),
                column(
                  width = 1,
                  offset = 4,
                  bsButton(
                    inputId = "next2",
                    label = "Next Level",
                    disabled = FALSE
                  )
                ),
                br()
              )
            ),
            #### Level 2 ----
            tabPanel(
              title = "Level 2",
              value = "c",
              titlePanel("Identify in Plots"),
              p("Match the variable defined in the instructions of each plot
                       to the variable type."),
              hr(),
              fluidRow(
                wellPanel(div(style = "text-align:center", h4(textOutput("imgQ1"))),
                          uiOutput("image1", class = "picSize"),
                          div(style = "position: relative; top:-15px;"),
                          class = "col-lg-6 col-md-12 wellBorder"
                ),
                wellPanel(div(style = "text-align:center", h4(textOutput("imgQ2"))),
                          uiOutput("image2", class = "picSize"),
                          div(style = "position: relative; top:-15px;"),
                          class = "col-lg-6 col-md-12 wellBorder"
                )
              ),
              br(),
              wellPanel(
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      inputId = "match1",
                      label = "Quantitative and Discrete",
                      choices = c("A", "B", "C", "D")
                    ),
                    uiOutput(outputId = "answer17")
                  ),
                  column(
                    width = 6,
                    selectInput(
                      inputId = "match2",
                      label = "Quantitative and Continuous",
                      choices = c("A", "B", "C", "D")
                    ),
                    uiOutput(outputId = "answer18")
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      inputId = "match3",
                      label = "Qualitative and Nominal",
                      choices = c("A", "B", "C", "D")
                    ),
                    uiOutput(outputId = "answer19")
                  ),
                  column(
                    width = 6,
                    selectInput(
                      inputId = "match4",
                      label = "Qualitative and Ordinal",
                      choices = c("A", "B", "C", "D")
                    ),
                    uiOutput(outputId = "answer20")
                  )
                )
              ),
              br(),
              fluidRow(
                wellPanel(div(style = "position: relative; top:-5px;"),
                          div(style = "position:relative; text-align:center; top: -15px;", h4(textOutput("imgQ3"))),
                          div(style = "position:relative; top: -15px;", uiOutput("image3", class = "picSize")),
                          class = "col-lg-6 col-md-12 wellBorder"
                ),
                wellPanel(div(style = "position: relative; top:-5px;"),
                          div(style = "position:relative; text-align:center; top: -15px;", h4(textOutput("imgQ4"))),
                          div(style = "position:relative; top: -15px;", uiOutput("image4", class = "picSize")),
                          class = "col-lg-6 col-md-12 wellBorder"
                )
              ),
              ##### Buttons ----
              fluidRow(
                column(
                  width = 1,
                  bsButton(
                    inputId = "previous2",
                    label = "Previous Level"
                  )
                ),
                column(
                  width = 1,
                  offset = 2,
                  bsButton(
                    inputId = "clearB",
                    label = "Retry"
                  )
                ),
                column(
                  width = 1, 
                  offset = 2, 
                  conditionalPanel(
                    "(input.match1!='') & (input.match2!='') & (input.match3!='') & (input.match4!='')",
                    bsButton(
                      inputId = "submitB", 
                      label = "Submit Answer")
                  )
                ),
                column(
                  width = 1,
                  offset = 4,
                  bsButton(
                    inputId = "next3",
                    label = "Next Level",
                    disabled = TRUE 
                  )
                )
              ),
              hr(),
              conditionalPanel(
                "input.submitB != 0",
                wellPanel(h3("Full score is 20 for level B."),
                          verbatimTextOutput("scoreB"),
                          class = "wellTransparent col-lg-4"
                )
              )
              ),
            #### Level 3 ----
            tabPanel(
              title = "Level 3",
              value = "e",
              titlePanel("Explanatory and Response Variables"),
              fluidRow(h4("You must get both answers correct to earn 1 point and get 5 points before moving to the next level"), style = "margin-left:15px"),
              fluidRow(h4("Once you have made your choices hit submit answer, then click new question for the next question"), style = "margin-left:15px"),
              hr(),
              wellPanel(
                fluidRow(uiOutput("questionC"), br())
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
                column(width = 4, offset = 3, textOutput("correctC"))
              ),
              br(),
              ##### Buttons ----
              conditionalPanel(
                "input.next3 != 0",
                fluidRow(
                  column(
                    width = 1, 
                    offset = 1,
                    bsButton(
                      inputId = "previous4",
                      label = "Previous Level",
                    )
                  ),
                  column(
                    width = 1, 
                    offset = 1,
                    conditionalPanel(
                      "(input.explC!='') & (input.respC!='')",
                      bsButton(
                        inputId = "submit3",
                        label = "Submit",
                      )
                    )),
                  column(
                    width = 1,
                    offset = 2,
                    bsButton(
                      inputId = "new",
                      label = "New Question"
                    )
                  ),
                  column(
                    width = 1,
                    offset = 2,
                    bsButton(
                      inputId = "next4",
                      label = "Next Level",
                      disabled = TRUE
                    )
                  )
                ),
                hr()
              ),
              fluidRow(
                column(width = 3, offset = 4, uiOutput("train1"))
              )
            ),
            #### Level 4 ----
            tabPanel(
              title = "Level 4",
              value = "f",
              titlePanel(h1("This level will add in the concepts of confounding variables")),
              fluidRow(h4("You must answer 5 correct choices before completing the level"), style = "margin-left:15px"),
              fluidRow(h4("Once you have made your choices hit submit answer, then click new question for the next question"), style = "margin-left:15px"),
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
                column(width = 3, offset = 3, textOutput("correctD"))
              ),
              br(),
              ##### Buttons ----
              conditionalPanel(
                "input.next4 != 0",
                fluidRow(
                  column(
                    width = 1, 
                    offset = 1,
                    bsButton(
                      inputId = "previous5",
                      label = "Previous Level",
                    )
                  ),
                  column(
                    width = 1, 
                    offset = 1, 
                    conditionalPanel(
                      "(input.expla!='') & (input.resp!='') & (input.conf!='')",
                      bsButton(inputId = "submitD", label = "Submit")
                    )
                  ),
                  column(
                    width = 1,
                    offset = 2,
                    bsButton(
                      inputId = "new2",
                      label = "New Question"
                    )
                  ),
                  column(
                    width = 1,
                    offset = 2,
                    bsButton(
                      inputId = "finish",
                      label = "Results"
                    )
                  )
                ),
                hr()
              ),
              fluidRow(
                column(width = 3, offset = 4, uiOutput("trainB"))
              )
            ),
            #### Results ----
            tabPanel(
              title = "Results",
              value = "d",
              titlePanel(h1("Congratulations! You finished the game.")),
              fluidRow(column(3, offset = 9, textOutput("timer5"))), br(), br(),
              fluidPage(
                fluidRow(h3("Your scores:")),
                fluidRow(
                  wellPanel(verbatimTextOutput("init"), class = "wellScore col-lg-4 col-md-6 col-sm-12"),
                  wellPanel(verbatimTextOutput("end"), class = "wellScore col-lg-4 col-md-6 col-sm-12"),
                  wellPanel(verbatimTextOutput("totalScore"), class = "wellScore col-lg-4 col-md-6 col-sm-12")
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

  ## Set timer with start, stop, restart, stop, and termination; and show the timer
  time <- reactiveValues(inc = 0, timer = reactiveTimer(1000), started = FALSE)

  ## Setup questions and begin timer
  startGame <- function() {

    # Check if game has already been started
    if (!time$started) {

      # Bank A
      initBankA()

      # Bank B
      initBankB()

      # Start timer
      time$started <- TRUE
    }
  }

  ## Go button ----
  observeEvent(input$goToGame, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "game"
    )
  })
  
  ##Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Go through each level to apply examples of various variables."
      )
    }
  )

  ## Reset Button ----
  observeEvent(input$reset_button, {
    js$reset()
  })

  ## Init Bank A ----
  numbers <- reactiveValues(dis = c(), cont = c(), nom = c(), ord = c())
  initBankA <- function() {

    numbers$dis <- sample(1:10, 4)
    numbers$cont <- sample(11:36, 4)
    numbers$nom <- sample(37:56, 4)
    numbers$ord <- sample(57:71, 4)

    output$disID1 <- renderText({
      bank[numbers$dis[1], 2]
    })

    output$disID2 <- renderText({
      bank[numbers$dis[2], 2]
    })

    output$disID3 <- renderText({
      bank[numbers$dis[3], 2]
    })

    output$disName1 <- renderText({
      bank[numbers$dis[1], 3]
    })

    output$disName2 <- renderText({
      bank[numbers$dis[2], 3]
    })

    output$disName3 <- renderText({
      bank[numbers$dis[3], 3]
    })


    output$contID1 <- renderText({
      bank[numbers$cont[1], 2]
    })

    output$contID2 <- renderText({
      bank[numbers$cont[2], 2]
    })

    output$contID3 <- renderText({
      bank[numbers$cont[3], 2]
    })

    output$contName1 <- renderText({
      bank[numbers$cont[1], 3]
    })

    output$contName2 <- renderText({
      bank[numbers$cont[2], 3]
    })

    output$contName3 <- renderText({
      bank[numbers$cont[3], 3]
    })

    output$nomID1 <- renderText({
      bank[numbers$nom[1], 2]
    })

    output$nomID2 <- renderText({
      bank[numbers$nom[2], 2]
    })

    output$nomID3 <- renderText({
      bank[numbers$nom[3], 2]
    })

    output$nomName1 <- renderText({
      bank[numbers$nom[1], 3]
    })

    output$nomName2 <- renderText({
      bank[numbers$nom[2], 3]
    })

    output$nomName3 <- renderText({
      bank[numbers$nom[3], 3]
    })

    output$ordID1 <- renderText({
      bank[numbers$ord[1], 2]
    })

    output$ordID2 <- renderText({
      bank[numbers$ord[2], 2]
    })

    output$ordID3 <- renderText({
      bank[numbers$ord[3], 2]
    })

    output$ordName1 <- renderText({
      bank[numbers$ord[1], 3]
    })

    output$ordName2 <- renderText({
      bank[numbers$ord[2], 3]
    })

    output$ordName3 <- renderText({
      bank[numbers$ord[3], 3]
    })
  }

  ## Init Bank B ----
  numbersB <- reactiveValues(disB = c(), contB = c(), nomB = c(), ordB = c(), indexB = c(), questionB = data.frame())
  initBankB <- function() {
    numbersB$disB <- sample(1:13, 1)
    numbersB$contB <- sample(14:39, 1)
    numbersB$nomB <- sample(40:58, 1)
    numbersB$ordB <- sample(59:74, 1)

    numbersB$indexB <- sample(c("A", "B", "C", "D"), 4)
    numbersB$questionB <- cbind(bankB[c(numbersB$disB, numbersB$contB, numbersB$nomB, numbersB$ordB), ], numbersB$indexB)

    output$imgQ1 <- renderText({
      paste("A.", numbersB$questionB[numbersB$questionB[5] == "A", 4])
    })

    output$image1 <- renderUI({
      img(src = numbersB$questionB[numbersB$questionB[5] == "A", 3], width = "95%", height = "95%", style = "text-align: center")
    })

    output$imgQ2 <- renderText({
      paste("B.", numbersB$questionB[numbersB$questionB[5] == "B", 4])
    })

    output$image2 <- renderUI({
      img(src = numbersB$questionB[numbersB$questionB[5] == "B", 3], width = "95%", height = "95%")
    })

    output$imgQ3 <- renderText({
      paste("C.", numbersB$questionB[numbersB$questionB[5] == "C", 4])
    })

    output$image3 <- renderUI({
      img(src = numbersB$questionB[numbersB$questionB[5] == "C", 3], width = "95%", height = "95%")
    })

    output$imgQ4 <- renderText({
      paste("D.", numbersB$questionB[numbersB$questionB[5] == "D", 4])
    })

    output$image4 <- renderUI({
      img(src = numbersB$questionB[numbersB$questionB[5] == "D", 3], width = "95%", height = "95%")
    })
  }

  ## Init Bank C ----
  index <- reactiveValues(index = 18)

  index_list <- reactiveValues(listc = sample(1:17, 17, replace = FALSE))

  observeEvent(
    eventExpr = input$previous4, 
    handlerExpr = {
    updateTabsetPanel(
      session = session, 
      inputId = "levels", 
      selected = "e")
    index_list$listc <- c(index_list$listc, sample(1:17, 17, replace = FALSE))
  })

  observeEvent(
    eventExpr = input$next3,
    handlerExpr = {
      index$index <- 18
      index$exp_index <- 2 * index$index - 1
      index$res_index <- 2 * index$index
    })

  observeEvent(
    eventExpr = input$new,
    handlerExpr = {
      index_list$listc <- index_list$listc[-1]
      index$index <- index_list$listc[1]
      index$exp_index <- 2 * index$index - 1
      index$res_index <- 2 * index$index
    })

  key1 <- as.matrix(bankC[1:36, 1])

  output$questionC <- renderUI({
    if (index$index == 1) {
      h3(bankC[1, 5])
    } else if (index$index == 2) {
      h3(bankC[3, 5])
    } else if (index$index == 3) {
      h3(bankC[5, 5])
    } else if (index$index == 4) {
      h3(bankC[7, 5])
    }
    else if (index$index == 5) {
      h3(bankC[9, 5])
    } else if (index$index == 6) {
      h3(bankC[11, 5])
    } else if (index$index == 7) {
      h3(bankC[13, 5])
    } else if (index$index == 8) {
      h3(bankC[15, 5])
    }
    else if (index$index == 9) {
      h3(bankC[17, 5])
    } else if (index$index == 10) {
      h3(bankC[19, 5])
    } else if (index$index == 11) {
      h3(bankC[21, 5])
    } else if (index$index == 12) {
      h3(bankC[23, 5])
    } else if (index$index == 13) {
      h3(bankC[25, 5])
    } else if (index$index == 14) {
      h3(bankC[27, 5])
    } else if (index$index == 15) {
      h3(bankC[29, 5])
    } else if (index$index == 16) {
      h3(bankC[31, 5])
    } else if (index$index == 17) {
      h3(bankC[33, 5])
    } else if (index$index == 18) {
      h3(bankC[35, 5])
    }
  })

  output$varEXP <- renderUI({
    if (index$index == 1) {
      h3(bankC[1, 4])
    } else if (index$index == 2) {
      h3(bankC[3, 4])
    } else if (index$index == 3) {
      h3(bankC[5, 4])
    } else if (index$index == 4) {
      h3(bankC[7, 4])
    }
    else if (index$index == 5) {
      h3(bankC[9, 4])
    } else if (index$index == 6) {
      h3(bankC[11, 4])
    } else if (index$index == 7) {
      h3(bankC[13, 4])
    } else if (index$index == 8) {
      h3(bankC[15, 4])
    }
    else if (index$index == 9) {
      h3(bankC[17, 4])
    } else if (index$index == 10) {
      h3(bankC[19, 4])
    } else if (index$index == 11) {
      h3(bankC[21, 4])
    } else if (index$index == 12) {
      h3(bankC[23, 4])
    }
    else if (index$index == 13) {
      h3(bankC[25, 4])
    } else if (index$index == 14) {
      h3(bankC[27, 4])
    } else if (index$index == 15) {
      h3(bankC[29, 4])
    } else if (index$index == 16) {
      h3(bankC[31, 4])
    }
    else if (index$index == 17) {
      h3(bankC[33, 4])
    } else if (index$index == 18) {
      h3(bankC[35, 4])
    }
  })

  output$varRES <- renderUI({
    if (index$index == 1) {
      h3(bankC[2, 4])
    } else if (index$index == 2) {
      h3(bankC[4, 4])
    } else if (index$index == 3) {
      h3(bankC[6, 4])
    } else if (index$index == 4) {
      h3(bankC[8, 4])
    }
    else if (index$index == 5) {
      h3(bankC[10, 4])
    } else if (index$index == 6) {
      h3(bankC[12, 4])
    } else if (index$index == 7) {
      h3(bankC[14, 4])
    } else if (index$index == 8) {
      h3(bankC[16, 4])
    }
    else if (index$index == 9) {
      h3(bankC[18, 4])
    } else if (index$index == 10) {
      h3(bankC[20, 4])
    } else if (index$index == 11) {
      h3(bankC[22, 4])
    } else if (index$index == 12) {
      h3(bankC[24, 4])
    }
    else if (index$index == 13) {
      h3(bankC[26, 4])
    } else if (index$index == 14) {
      h3(bankC[28, 4])
    } else if (index$index == 15) {
      h3(bankC[30, 4])
    } else if (index$index == 16) {
      h3(bankC[32, 4])
    }
    else if (index$index == 17) {
      h3(bankC[34, 4])
    } else if (index$index == 18) {
      h3(bankC[36, 4])
    }
  })

  ## Init Bank D ----
  index2 <- reactiveValues(index2 = 9)

  index_listD <- reactiveValues(listD = sample(1:8, 8, replace = FALSE))

  observeEvent(
    eventExpr = input$previous5, 
    handlerExpr = {
      updateTabsetPanel(
        session = session, 
        inputId = "levels", 
        selected = "f")
      index_listD$listD <- c(index_listD$listD, sample(1:8, 8, replace = FALSE))
    })

  observeEvent(
    eventExpr = input$next4,
    handlerExpr = {
      index2$index2 <- 9
      index2$explan <- 3 * index2$index2 - 2
      index2$respon <- 3 * index2$index2 - 1
      index2$confou <- 3 * index2$index2
    })

  observeEvent(
    eventExpr = input$new2,
    handlerExpr = {
      index_listD$listD <- index_listD$listD[-1]
      index2$index2 <- index_listD$listD[1]
      index2$explan <- 3 * index2$index2 - 2
      index2$respon <- 3 * index2$index2 - 1
      index2$confou <- 3 * index2$index2
    })

  key2 <- as.matrix(bankD[1:27, 1])

  output$questionD <- renderUI({
    if (index2$index2 == 1) {
      h3(bankD[1, 4])
    } else if (index2$index2 == 2) {
      h3(bankD[4, 4])
    } else if (index2$index2 == 3) {
      h3(bankD[7, 4])
    } else if (index2$index2 == 4) {
      h3(bankD[10, 4])
    }
    else if (index2$index2 == 5) {
      h3(bankD[13, 4])
    }
    else if (index2$index2 == 6) {
      h3(bankD[16, 4])
    }
    else if (index2$index2 == 7) {
      h3(bankD[19, 4])
    }
    else if (index2$index2 == 8) {
      h3(bankD[22, 4])
    }
    else if (index2$index2 == 9) {
      h3(bankD[25, 4])
    }
  })

  output$varEXPD <- renderUI({
    if (index2$index2 == 1) {
      h3(bankD[1, 3])
    } else if (index2$index2 == 2) {
      h3(bankD[4, 3])
    } else if (index2$index2 == 3) {
      h3(bankD[7, 3])
    } else if (index2$index2 == 4) {
      h3(bankD[10, 3])
    }
    else if (index2$index2 == 5) {
      h3(bankD[13, 3])
    }
    else if (index2$index2 == 6) {
      h3(bankD[16, 3])
    }
    else if (index2$index2 == 7) {
      h3(bankD[19, 3])
    }
    else if (index2$index2 == 8) {
      h3(bankD[22, 3])
    }
    else if (index2$index2 == 9) {
      h3(bankD[25, 3])
    }
  })

  output$varRESD <- renderUI({
    if (index2$index2 == 1) {
      h3(bankD[2, 3])
    } else if (index2$index2 == 2) {
      h3(bankD[5, 3])
    } else if (index2$index2 == 3) {
      h3(bankD[8, 3])
    } else if (index2$index2 == 4) {
      h3(bankD[11, 3])
    }
    else if (index2$index2 == 5) {
      h3(bankD[14, 3])
    }
    else if (index2$index2 == 6) {
      h3(bankD[17, 3])
    }
    else if (index2$index2 == 7) {
      h3(bankD[20, 3])
    }
    else if (index2$index2 == 8) {
      h3(bankD[23, 3])
    }
    else if (index2$index2 == 9) {
      h3(bankD[26, 3])
    }
  })

  output$varCOND <- renderUI({
    if (index2$index2 == 1) {
      h3(bankD[3, 3])
    } else if (index2$index2 == 2) {
      h3(bankD[6, 3])
    } else if (index2$index2 == 3) {
      h3(bankD[9, 3])
    } else if (index2$index2 == 4) {
      h3(bankD[12, 3])
    }
    else if (index2$index2 == 5) {
      h3(bankD[15, 3])
    }
    else if (index2$index2 == 6) {
      h3(bankD[18, 3])
    }
    else if (index2$index2 == 7) {
      h3(bankD[21, 3])
    }
    else if (index2$index2 == 8) {
      h3(bankD[24, 3])
    }
    else if (index2$index2 == 9) {
      h3(bankD[27, 3])
    }
  })

  ## Submit Observers ----
  observeEvent(input$submitA, {
    updateButton(session, "submitA", disabled = TRUE)
  })

  observeEvent(input$clear, {
    updateButton(session, "submitA", disabled = FALSE)
  })

  observeEvent(input$submitB, {
    updateButton(session, "submitB", disabled = TRUE)
  })

  observeEvent(input$clearB, {
    updateButton(session, "submitB", disabled = FALSE)
  })

  observeEvent(input$submitC, {
    updateButton(session, "submitC", disabled = TRUE)
  })

  observe({
    if (length(index_list$listc) == 1) {
      updateButton(session, "new", disabled = TRUE)
      updateButton(session, "submitC", disabled = TRUE)
      shinyalert("Oops!", "You have used up all the tries. Please click 'previous' then click 'next' to re-enter this level to try again", type = "error")
    }
  })

  observe({
    if (length(index_listD$listD) == 1) {
      updateButton(session, "new2", disabled = TRUE)
      updateButton(session, "submitD", disabled = TRUE)
      shinyalert("Oops!", "You have used up all the tries. Please click 'previous' then click 'next' to re-enter this level to try again", type = "error")
    }
  })

  observeEvent(input$submitC, {
    updateButton(session, "new", disabled = FALSE)
  })

  observeEvent(input$previous4, {
    updateButton(session, "submitC", disabled = FALSE)
  })

  observeEvent(input$new, {
    updateButton(session, "submitC", disabled = FALSE)
  })

  observeEvent(input$new, {
    updateButton(session, "new", disabled = TRUE)
  })

  observeEvent(input$submitD, {
    updateButton(session, "submitD", disabled = TRUE)
  })

  observeEvent(input$submitD, {
    updateButton(session, "new2", disabled = FALSE)
  })

  observeEvent(input$previous5, {
    updateButton(session, "submitD", disabled = FALSE)
  })

  observeEvent(input$new2, {
    updateButton(session, "submitD", disabled = FALSE)
  })

  observeEvent(input$new2, {
    updateButton(session, "new2", disabled = TRUE)
  })

  ## Begin Validation ----
  ### Validate Level 1 ----
  observeEvent(input$submitA, {
    observeEvent(input$clear, {
      output$answer1 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer1 <- renderUI({
        if (!is.null(input$group1)) {
          valid <- any(trimws(input$group1) == bank[c(1:10), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })

  observeEvent(input$submitA, {
    observeEvent(input$clear, {
      output$answer2 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer2 <- renderUI({
        if (!is.null(input$group2)) {
          valid <- any(trimws(input$group2) == bank[c(1:10), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA, {
    observeEvent(input$clear, {
      output$answer3 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer3 <- renderUI({
        if (!is.null(input$group3)) {
          valid <- any(trimws(input$group3) == bank[c(1:10), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA, {
    observeEvent(input$clear, {
      output$answer4 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer4 <- renderUI({
        if (!is.null(input$group4)) {
          valid <- any(trimws(input$group4) == bank[c(1:10), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA, {
    observeEvent(input$clear, {
      output$answer5 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer5 <- renderUI({
        if (!is.null(input$group5)) {
          valid <- any(trimws(input$group5) == bank[c(11:36), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA, {
    observeEvent(input$clear, {
      output$answer6 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer6 <- renderUI({
        if (!is.null(input$group6)) {
          valid <- any(trimws(input$group6) == bank[c(11:36), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA, {
    observeEvent(input$clear, {
      output$answer7 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer7 <- renderUI({
        if (!is.null(input$group7)) {
          valid <- any(trimws(input$group7) == bank[c(11:36), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA, {
    observeEvent(input$clear, {
      output$answer8 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer8 <- renderUI({
        if (!is.null(input$group8)) {
          valid <- any(trimws(input$group8) == bank[c(11:36), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA, {
    observeEvent(input$clear, {
      output$answer9 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer9 <- renderUI({
        if (!is.null(input$group9)) {
          valid <- any(trimws(input$group9) == bank[c(37:56), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
    observe({
      output$answer10 <- renderUI({
        if (!is.null(input$group10)) {
          valid <- any(trimws(input$group10) == bank[c(37:56), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
    observe({
      output$answer11 <- renderUI({
        if (!is.null(input$group11)) {
          valid <- any(trimws(input$group11) == bank[c(37:56), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
    observe({
      output$answer12 <- renderUI({
        if (!is.null(input$group12)) {
          valid <- any(trimws(input$group12) == bank[c(37:56), 3])
          if (valid) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })

  ### Validate Level 2 ----
  observeEvent(input$submitB, {
    observeEvent(input$clearB, {
      output$answer17 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer17 <- renderUI({
        if (!is.null(input$match1)) {
          if (input$match1 == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete", 5]) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB, {
    observeEvent(input$clearB, {
      output$answer18 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer18 <- renderUI({
        if (!is.null(input$match2)) {
          if (input$match2 == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous", 5]) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB, {
    observeEvent(input$clearB, {
      output$answer19 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer19 <- renderUI({
        if (!is.null(input$match3)) {
          if (input$match3 == numbersB$questionB[numbersB$questionB[1] == "QualNominal", 5]) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB, {
    observeEvent(input$clearB, {
      output$answer20 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer20 <- renderUI({
        if (!is.null(input$match4)) {
          if (input$match4 == numbersB$questionB[numbersB$questionB[1] == "QualOrdinal", 5]) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })

  summation <- reactiveValues(summationA = c(rep(0, 20)), summationB = c(rep(0, 20)), summationC = c(rep(0, 20)), summationD = c(rep(0, 20)), summationScore = c(rep(0, 20)))

  observeEvent(input$submitA, {

    score1 <- c()
    score2 <- c()
    score3 <- c()
    score4 <- c()

    for (i in c(input$group1, input$group2, input$group3, input$group4)) {
      if (any(trimws(i) == bank[c(1:10), 3])) {
        score1 <- c(score1, 2.5)
      } else {
        score1 <- c(score1, -1.5)
      }
    }
    for (i in c(input$group5, input$group6, input$group7, input$group8)) {
      if (any(trimws(i) == bank[c(11:36), 3])) {
        score2 <- c(score2, 2.5)
      } else {
        score2 <- c(score2, -1.5)
      }
    }
    for (i in c(input$group9)) {
      if (any(trimws(i) == bank[c(37:56), 3])) {
        score3 <- c(score3, 2.5)
      } else {
        score3 <- c(score3, -1.5)
      }
    }


    total <- sum(c(score1, score2, score3, score4))

    response <- list(
      "Quantitative_Discrete" = c(trimws(input$group1), trimws(input$group2), trimws(input$group3)),
      "Quantitative_Continuous" = c(trimws(input$group4), trimws(input$group5)),
      "Qualitative_Nominal" = c(trimws(input$group6), trimws(input$group7)),
      "Qualitative_Ordinal" = c(trimws(input$group8), trimws(input$group9))
    )

    stmt <- boastUtils::generateStatement(
      session,
      verb = "answered",
      object = "level1",
      description = "Drag the variables into the categories they belong to.",
      interactionType = "matching",
      response = jsonlite::toJSON(response),
      success = total == 40
    )

    boastUtils::storeStatement(session, stmt)

    summation$summationA[input$submitA] <- total
  })

  observeEvent(input$submitB, {
    image1 <- numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete", 5]
    image2 <- numbersB$questionB[numbersB$questionB[1] == "QuanContinuous", 5]
    image3 <- numbersB$questionB[numbersB$questionB[1] == "QualNominal", 5]
    image4 <- numbersB$questionB[numbersB$questionB[1] == "QualOrdinal", 5]

    score5 <- c()

    for (i in input$match1) {
      if (i == image1) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$match2) {
      if (i == image2) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$match3) {
      if (i == image3) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$match5) {
      if (i == image4) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }

    total <- sum(score5)

    response <- list(
      "Quantitative_Discrete" = c(image1, input$match1),
      "Quantitative_Continuous" = c(image2, input$match2),
      "Qualitative_Nominal" = c(image3, input$match3),
      "Qualitative_Ordinal" = c(image4, input$match5)
    )

    stmt <- boastUtils::generateStatement(
      session,
      verb = "answered",
      object = "level2",
      description = "Identify in Plots",
      interactionType = "matching",
      response = jsonlite::toJSON(response),
      success = total == 20
    )

    boastUtils::storeStatement(session, stmt)

    summation$summationB[input$submitB] <- total
  })
  values <- reactiveValues(
    count = 0
  )
  observeEvent(input$submitA, {
    if (summation$summationA[input$submitA] == 40) {
      updateButton(session, "next2", disabled = FALSE)
    }
  })
  observeEvent(input$submitB, {
    if (summation$summationB[input$submitB] == 20) {
      updateButton(session, "next3", disabled = FALSE)
    }
    else {
      updateButton(session, "next3", disabled = TRUE)
    }
  })

  output$scoreA <- renderPrint({
    cat("Current score of this level is", summation$summationA[input$submitA])
  })

  output$scoreB <- renderPrint({
    cat("Current score of this level is", max(summation$summationB))
  })

  ### Validate Level 3 ----
  observeEvent(
    eventExpr = input$submitC,
    handlerExpr = {
      observeEvent(
        eventExpr = input$new,
        handlerExpr = {
          output$markc1 <- renderUI(
            img(src = NULL,width = 30)
          )
        })
      observe({
        eventExpr = output$markc1 <- renderUI(expr = {
          if (!is.null(input$explC)) {
            if (any(input$explC == key1[index$exp_index,1])) {
              img(src = "check.PNG", width = 30)
            } else {
              img(src = "cross.PNG", width = 30)
            }
          }
        })
      })
    })
  
  observeEvent(
    eventExpr = input$submitC,
    handlerExpr = {
      observeEvent(
        eventExpr = input$new,
        handlerExpr = {
          output$markc2 <- renderUI(
            img(src = NULL,width = 30)
          )
        })
      observe({
        eventExpr = output$markc2 <- renderUI(expr = {
          if (!is.null(input$respC)) {
            if (any(input$respC == key1[index$res_index,1])) {
              img(src = "check.PNG", width = 30)
            } else {
              img(src = "cross.PNG", width = 30)
            }
          }
        })
      })
    })
  
  observeEvent(
    eventExpr = input$new3,
    handlerExpr = {
      reset(id = "expl3")
      reset(id = "resp3")
      reset(id ="submit")
    }
  )

  ##### Scoring 
  summationC <- reactiveValues(correct1 = c(0), started = FALSE)

  observeEvent(
    eventExpr = input$next3, 
    handlerExpr = {
      summationC$started <- TRUE
    }
  )

  observeEvent(
    eventExpr = input$new, 
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

  output$correctC <- renderPrint({
    if (sum(c(summationC$correct1)) == 0) {
      cat("You have earned 0 points")
    }
    else {
      cat("You have earned", summation$summationC[input$submitC], "points")
    }
  })

  observeEvent(
    eventExpr = input$submitC,
    handlerExpr = {
      if (summation$summationC[input$submitC] >= 5) {
        updateButton(
          session = session, 
          inputId = "next4", 
          disabled = FALSE)
        updateButton(
          session = session,
          inputId = "new",
          disabled = TRUE)
      }
    })

  ### Validate Level 4 ----
  observeEvent(
    eventExpr = input$submitD,
    handlerExpr = {
      observeEvent(
        eventExpr = input$new2,
        handlerExpr = {
          output$markd1 <- renderUI(
            img(src = NULL, width = 30)
          )
        })
    observe({
      eventExpr = output$markd1 <- renderUI(expr = {
        if (!is.null(input$expla)) {
          if (any(input$expa == key2[index$explan,1])) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })

  observeEvent(
    eventExpr = input$submitD,
    handlerExpr = {
      observeEvent(
        eventExpr = input$new2,
        handlerExpr = {
          output$markd2 <- renderUI(
            img(src = NULL,width = 30)
          )
        })
      observe({
        eventExpr = output$markd2 <- renderUI(expr = {
          if (!is.null(input$resp)) {
            if (any(input$resp == key2[index$respon,1])) {
              img(src = "check.PNG", width = 30)
            } else {
              img(src = "cross.PNG", width = 30)
            }
          }
        })
      })
    })
  
  observeEvent(
    eventExpr = input$submitD,
    handlerExpr = {
      observeEvent(
        eventExpr = input$new2,
        handlerExpr = {
          output$markd3 <- renderUI(
            img(src = NULL,width =30)
          )
        })
      observe({
        eventExpr = output$markd3 <- renderUI(expr = {
          if (!is.null(input$conf)) {
            if (any(input$conf == key2[index$confou,1])) {
              img(src = "check.PNG", width = 30)
            } else {
              img(src = "cross.PNG", width = 30)
            }
          }
        })
      })
    })
  


  observeEvent(input$new2, {
    reset("expla")
    reset("resp")
    reset("conf")
  })

  summationD <- reactiveValues(correct1D = c(0), started = FALSE)
  test <- reactiveValues(A = FALSE, B = FALSE, C = TRUE)

  observeEvent(input$next4, {
    time$started <- TRUE
  })

  observeEvent(input$new2, {
    time$started <- TRUE
  })

  observeEvent(input$submitD, {
    time$started <- TRUE
  })

  observeEvent(input$submitD, {
    success <- FALSE
    for (x in c(input$expla)) {

      success <- (any(input$expla == key2[index2$explan,1]) & any(input$resp== key2[index2$respon,1])&any(input$conf== key2[index2$confou,1]))

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

  output$correctD <- renderPrint({
    if (sum(c(summationD$correct1D)) == 0) {
      cat("You have earned 0 points")
    }
    else {
      cat("You have earned", sum(c(summationD$correct1D)), "points")
    }
  })

  observeEvent(input$submitD, {
    if (sum(c(summationD$correct1D)) >= 5) {
      updateButton(session, "finish", disabled = FALSE)
      updateButton(session, "new2", disabled = FALSE)
    }
  })

  observeEvent(input$finish, {
    summation$summationA[which(summation$summationA == 0)] <- summation$summationA[input$submitA]
    summation$summationB[which(summation$summationB == 0)] <- summation$summationB[input$submitB]
    summation$summationScore <- summation$summationA + summation$summationB + 40
  })

  output$init <- renderPrint({
    if (any(summation$summationA != 0) & any(summation$summationB != 0)) {
      initialScore <- summation$summationScore[which(summation$summationScore != 0)][1]
    } else {
      initialScore <- 0
    }

    cat("Initial", "\n", initialScore)
  })

  final <- reactiveValues(final = 0)

  observeEvent(input$finish, {

    score1 <- c()
    score2 <- c()
    score3 <- c()
    score4 <- c()
    score5 <- c()

    for (i in c(input$match1, input$drp2, input$drp3, input$drp4)) {
      if (any(trimws(i) == bank[c(1:10), 3])) {
        score1 <- c(score1, 2.5)
      } else {
        score1 <- c(score1, -1.5)
      }
    }
    for (i in c(input$drp5, input$drp6, input$drp7, input$drp8)) {
      if (any(trimws(i) == bank[c(11:36), 3])) {
        score2 <- c(score2, 2.5)
      } else {
        score2 <- c(score2, -1.5)
      }
    }
    for (i in c(input$drp9, input$match10, input$match11, input$match12)) {
      if (any(trimws(i) == bank[c(37:56), 3])) {
        score3 <- c(score3, 2.5)
      } else {
        score3 <- c(score3, -1.5)
      }
    }
    for (i in c(input$match13, input$match14, input$match15, input$match16)) {
      if (any(trimws(i) == bank[c(57:71), 3])) {
        score4 <- c(score4, 2.5)
      } else {
        score4 <- c(score4, -1.5)
      }
    }
    for (i in input$match1) {
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete", 5]) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$match2) {
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous", 5]) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$match3) {
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualNominal", 5]) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$match5) {
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualOrdinal", 5]) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }

    final$final <- sum(c(score1, score2, score3, score4, score5)) + 40
  })

  output$end <- renderPrint({
    cat("Improved", "\n", final$final)
  })

  output$totalScore <- renderPrint({
    cat("Total", "\n", round(as.numeric(summation$summationScore[1]) * (2 / 3) + as.numeric(final$final) * (1 / 3), digits = 1))
  })

  ##### Train 1 ----
  observe(
    if (sum(c(summationC$correct1)) == 1) {
      output$train1 <- renderUI({
        img(src = "train1.PNG", width = "20%", height = "20%")
      })
    }
    else if (sum(c(summationC$correct1)) == 2) {
      output$train1 <- renderUI({
        img(src = "train2.gif", width = "40%", height = "40%")
      })
    }
    else if (sum(c(summationC$correct1)) == 3) {
      output$train1 <- renderUI({
        img(src = "train3.gif", width = "60%", height = "60%")
      })
    }
    else if (sum(c(summationC$correct1)) == 4) {
      output$train1 <- renderUI({
        img(src = "train4.gif", width = "80%", height = "80%")
      })
    }
    else if (sum(c(summationC$correct1)) == 5) {
      output$train1 <- renderUI({
        img(src = "train_f_02.PNG", width = "110%", height = "110%")
      })
    }
  )

  ##### Train 2 ----
  observe(
    if (sum(c(summationD$correct1D)) == 1) {
      output$trainB <- renderUI({
        img(src = "train1B.PNG", width = "20%", height = "20%")
      })
    }
    else if (sum(c(summationD$correct1D)) == 2) {
      output$trainB <- renderUI({
        img(src = "train2B.gif", width = "40%", height = "40%")
      })
    }
    else if (sum(c(summationD$correct1D)) == 3) {
      output$trainB <- renderUI({
        img(src = "train3B.gif", width = "60%", height = "60%")
      })
    }
    else if (sum(c(summationD$correct1D)) == 4) {
      output$trainB <- renderUI({
        img(src = "train4B.gif", width = "80%", height = "80%")
      })
    }
    else if (sum(c(summationD$correct1D)) == 5) {
      output$trainB <- renderUI({
        img(src = "train_f_02.PNG", width = "110%", height = "110%")
      })
    }
  )
  #### End Validation ----

  # Check button
  observeEvent(input$check, {
    updateButton(session, "check", disabled = TRUE)
  })

  # Listen for game start events
  observeEvent(input$pages, {
    if (input$pages == "game") {
      startGame()
    }
  })
}

boastUtils::boastApp(ui = ui, server = server)
