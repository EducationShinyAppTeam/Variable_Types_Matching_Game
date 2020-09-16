# Load Required Packages
library(shiny)
library(shinyDND)
library(shinyjs)
library(shinyBS)
library(shinyalert)
library(boastUtils)

# App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Variable Types Matching Game"
APP_DESCP <<- paste(
  "Identify variable types by nature of measurement [Quantitative (numeric) discrete variables, 
  Quantitative continuous variables, Qualitative (categorical) nominal variables, and Qualitative
  ordinal variables] and by role in the analysis [explanatory versus response versus confounding]."
)
# End App Meta Data------------------------------------------------------------

# Load Question Banks ----
bank <- read.csv(file = "questionBank.csv", stringsAsFactors = FALSE)
bankB <- read.csv(file = "questionBankB.csv", stringsAsFactors = FALSE)
bankC <- read.csv(file = "questionBankC.csv", stringsAsFactors = FALSE)
bankD <- read.csv(file = "questionBankD.csv", stringsAsFactors = FALSE)

# Define UI ----
ui <- list(
  useShinyalert(),
  useShinyjs(),
  dashboardPage(
    skin = "red",
    ## Header ----
    dashboardHeader(
      title = "Variable Types",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
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
        id = "tabs",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Challenge", tabName = "challenge", icon = icon("cog")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        tabItem(
          ### Overview ----
          tabName = "overview",
          h1("Variable Types Matching Game"),
          p("Level 1 and 2: Identify four different variable types: Quantitative (numeric) discrete variables, Quantitative continuous variables,
          Qualitative (categorical) nominal variables, and Qualitative ordinal variables."),
          img(src = "variable-types.PNG", alt = "Variable types decision tree"),
          p("Level 3 and 4: Identify the explanatory and response variables in level 3. Then in level 4 you will also
          be required to identify the confounding variable."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("Click GO! button to start the game."),
            tags$li("Drag the variable names to the boxes by the variable types."),
            tags$li("Submit your answer only after finishing all the questions."),
            tags$li("You may go to the next level only when you correct your answers for level 1 and 2. For level 3 and 4 you must get 5 correct problems on each level to finish the game and get your final score."),
            tags$li("The score you get after the first trial and the revised score you get after correct all answers will be weighted to generate your final score.")
          ),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go",
              label = "Go to Challenge",
              icon = icon("bolt"),
              size = "large",
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Yuxin Zhang, Luxin Wang, & Thomas McIntyre. Special thanks to Robert P. Carey III and Alex Chen for help on some programming issues.",
            div(class = "updated", "Last Update: 9/14/2020 by RPC.")
          )
        ),
        tabItem(
          ### Challenge ----
          tabName = "challenge",
          h2("Challenge Yourself!"),
          fluidPage(
            tabsetPanel(
              id = "levels",
              type = "hidden",
              #### Level 1 ----
              tabPanel("Level 1",
                value = "b",
                fluidPage(
                  theme = "bootstrap.css",
                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "themestyle.css")),
                  titlePanel("Drag the variables into the categories they belong to. "),
                  fluidRow(
                    column(
                      3,
                      div(
                        style = "display: inline-block;vertical-align:top;",
                        tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 30)),
                        bsButton("ins1", "", icon = icon("info", class = "iconi fa-fw"), type = "toggle", class = "butt"),
                        bsButton("bq1", "", icon = icon("question", class = "iconq fa-fw"), type = "toggle", class = "butt"),
                        bsButton("bt1", "", icon = icon("time", lib = "glyphicon", class = "icont fa-fw"), type = "toggle", class = "butt")
                      ),
                      div(
                        id = "plot-container",
                        conditionalPanel(
                          "input.bq1 != 0",
                          tags$img(
                            src = "variable-types.PNG",
                            id = "hint"
                          )
                        )
                      ),
                      div(
                        style = "display: inline-block;vertical-align:top;",
                        conditionalPanel(
                          "input.ins1 != 0",
                          box(
                            title = "Instruction:", status = "danger", solidHeader = TRUE, width = 12,
                            "Drag variable names to correct variable type."
                          )
                        )
                      )
                    ),
                    column(3,
                      offset = 6,
                      hidden(div(id = "timer1h", textOutput("timer1")))
                    )
                  ),
                  br(),
                    # Set up all dragUIs which are randomly chosen from the question bank
                    fluidRow(
                      wellPanel(dragUI(textOutput("disID1"), textOutput("disName1"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("disID2"), textOutput("disName2"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("nomID1"), textOutput("nomName1"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("contID1"), textOutput("contName1"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),

                      wellPanel(dragUI(textOutput("disID3"), textOutput("disName3"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("contID2"), textOutput("contName2"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("nomID2"), textOutput("nomName2"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("ordID1"), textOutput("ordName1"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),

                      wellPanel(dragUI(textOutput("contID3"), textOutput("contName3"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("ordID2"), textOutput("ordName2"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("nomID3"), textOutput("nomName3"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("contID4"), textOutput("contName4"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),

                      wellPanel(dragUI(textOutput("ordID3"), textOutput("ordName3"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-2 drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("disID4"), textOutput("disName4"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-2 drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("ordID4"), textOutput("ordName4"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-2 drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                      wellPanel(dragUI(textOutput("nomID4"), textOutput("nomName4"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-2 drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2")
                    ),
                    hr(),
                    # Set up all dropUIs and check/cross boxes
                    fluidRow(
                      h4("Quantitative & Discrete:", class = "col-sm-12 col-md-12 col-lg-3"),
                      bsPopover(
                        id = "drp1", title = "Quantitative & Discrete", content = "Countable Number/Whole Number",
                        placement = "top", trigger = "hover", options = NULL
                      ),
                      wellPanel(dropUI("drp1", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer1")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp2", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer2")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp3", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer3")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp4", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer4")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      )
                    ),
                    fluidRow(
                      h4("Quantitative & Continuous:", class = "col-sm-12 col-md-12 col-lg-3"),
                      bsPopover(
                        id = "drp5", title = "Quantitative & Continuous", content = "Noncountable Number / Decimals",
                        placement = "top", trigger = "hover", options = NULL
                      ),
                      wellPanel(dropUI("drp5", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer5")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp6", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer6")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp7", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer7")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp8", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer8")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      )
                    ),
                    fluidRow(
                      h4("Qualitative & Nominal:", class = "col-sm-12 col-md-12 col-lg-3"),
                      bsPopover(
                        id = "drp9", title = "Qualitative & Nominal", content = "Unordered Categories",
                        placement = "top", trigger = "hover", options = NULL
                      ),
                      wellPanel(dropUI("drp9", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer9")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp10", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer10")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp11", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer11")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp12", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer12")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      )
                    ),
                    fluidRow(
                      h4("Qualitative & Ordinal:", class = "col-sm-12 col-md-12 col-lg-3"),
                      bsPopover(
                        id = "drp13", title = "Qualitative & Ordinal", content = "Ordered Categories",
                        placement = "top", trigger = "hover", options = NULL
                      ),
                      wellPanel(dropUI("drp13", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer13")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp14", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer14")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp15", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer15")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      ),
                      wellPanel(dropUI("drp16", class = "dropelement"),
                        div(style = "position:absolute;top: 10%;right:2%;", htmlOutput("answer16")),
                        class = "wellTransparent col-sm-12 col-md-6 col-lg-2"
                      )
                    ),
                    hr(),
                    # Submit button and pagination button
                    fluidRow(
                      column(1, bsButton("previous3", "<< Previous", style = "primary", size = "small")),
                      column(1,
                        offset = 4,
                        conditionalPanel(
                          "(input.drp1!='') & (input.drp2!='') & (input.drp3!='') & (input.drp4!='') &
                           (input.drp5!='') & (input.drp6!='') & (input.drp7!='') & (input.drp8!='') &
                           (input.drp9!='') & (input.drp10!='') & (input.drp11!='') & (input.drp12!='') &
                           (input.drp13!='') & (input.drp14!='') & (input.drp15!='') & (input.drp16!='')",
                          bsButton("submitA", "Submit Answer", style = "primary", size = "small", class = "grow")
                        )
                      ),
                      column(1, offset = 5, bsButton("next2", "Next >>", style = "primary", size = "small", disabled = TRUE))
                    ),
                  br(),
                  conditionalPanel("input.submitA != 0",
                                   wellPanel(
                    fluidPage(
                      fluidRow(
                        wellPanel(
                          h4("Please drag the wrong answers into this PENALTY box and click the CLEAR button to restart."),
                          dropUI("home1", class = "dropelement dropelementHome", col_n = 3),
                          class = "wellTransparent col-lg-8"
                        ),
                        wellPanel(h3("Full score is 40 for level A."),
                          div(style = "position:absolute; top:8em; right:2em", bsButton("clear", "CLEAR", style = "danger")),
                          verbatimTextOutput("scoreA"),
                          class = "wellTransparent col-lg-4"
                        )
                      )
                    )
                  )
                  )
                )
              ),
              #### Level 2 ----
              tabPanel("Level 2",
                value = "c",
                titlePanel("Identify in Plots"),
                fluidRow(
                  column(
                    3,
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 30)),
                      bsButton("ins2", "", icon = icon("info", class = "iconi fa-fw"), type = "toggle", class = "butt"),
                      bsButton("bq2", "", icon = icon("question", class = "iconq fa-fw"), type = "toggle", class = "butt"),
                      bsButton("bt2", "", icon = icon("time", lib = "glyphicon", class = "icont fa-fw"), type = "toggle", class = "butt")
                    ),
                    div(
                      id = "plot-container2",
                      conditionalPanel(
                        "input.bq2 != 0",
                        tags$img(
                          src = "variable-types.PNG",
                          id = "hint"
                        )
                      )
                    ),
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      conditionalPanel(
                        "input.ins2 != 0",
                        box(
                          title = "Instruction:", status = "danger", solidHeader = TRUE, width = 12,
                          "Drag letters below graphs to correct variable type."
                        )
                      )
                    )
                  ),
                  column(3,
                    offset = 6,
                    hidden(div(id = "timer2h", textOutput("timer2")))
                  )
                ),
                br(),
                conditionalPanel(
                  "input.next2 != 0",
                  fluidRow(
                    wellPanel(div(style = "text-align:center", h4(textOutput("imgQ1"))),
                      uiOutput("image1", class = "picSize"),
                      div(style = "position: relative; top:-15px;", dragUI("img1", "A", style = "width: 100px; height: 40px;", class = "drag dragelement dragelement2")),
                      class = "col-lg-6 col-md-12 wellBorder"
                    ),
                    wellPanel(div(style = "text-align:center", h4(textOutput("imgQ2"))),
                      uiOutput("image2", class = "picSize"),
                      div(style = "position: relative; top:-15px;", dragUI("img2", "B", style = "width: 100px; height: 40px;", class = "drag dragelement dragelement2")),
                      class = "col-lg-6 col-md-12 wellBorder"
                    )
                  ),
                  fluidRow(
                    fluidRow(
                      wellPanel(
                        dropUI("drop1", class = "dropelement dropelement2"),
                        div(style = "position: absolute; top:0;left:1em", h5("Quantitative & Discrete")),
                        div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer17")),
                        class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"
                      ),
                      wellPanel(
                        dropUI("drop2", class = "dropelement dropelement2"),
                        div(style = "position: absolute; top:0;left:1em", h5("Quantitative & Continuous")),
                        div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer18")),
                        class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"
                      ),
                      wellPanel(
                        dropUI("drop3", class = "dropelement dropelement2"),
                        div(style = "position: absolute; top:0; left:1em", h5("Qualitative & Nominal")),
                        div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer19")),
                        class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"
                      ),
                      wellPanel(
                        dropUI("drop4", class = "dropelement dropelement2"),
                        div(style = "position: absolute; top:0; left:1em", h5("Qualitative & Ordinal")),
                        div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer20")),
                        class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"
                      )
                    )
                  ),
                  br(),
                  fluidRow(
                    wellPanel(div(style = "position: relative; top:-5px;", dragUI("img3", "C", style = "width: 100px; height: 40px;", class = "drag dragelement dragelement2")),
                      div(style = "position:relative; text-align:center; top: -15px;", h4(textOutput("imgQ3"))),
                      div(style = "position:relative; top: -15px;", uiOutput("image3", class = "picSize")),
                      class = "col-lg-6 col-md-12 wellBorder"
                    ),
                    wellPanel(div(style = "position: relative; top:-5px;", dragUI("img4", "D", style = "width: 100px; height: 40px;", class = "drag dragelement dragelement2")),
                      div(style = "position:relative; text-align:center; top: -15px;", h4(textOutput("imgQ4"))),
                      div(style = "position:relative; top: -15px;", uiOutput("image4", class = "picSize")),
                      class = "col-lg-6 col-md-12 wellBorder"
                    )
                  ),
                  fluidRow(
                    column(1, bsButton("previous2", "<< Previous", style = "primary", size = "small")),
                    column(1, offset = 4, conditionalPanel(
                      "(input.drop1!='') & (input.drop2!='') & (input.drop3!='') & (input.drop4!='') & (input.drop5!='')",
                      bsButton("submitB", "Submit Answer", style = "primary", class = "grow", size = "small")
                    )),
                    column(1, offset = 5, bsButton("next3", "Next >>", style = "primary", size = "small", disabled = TRUE))
                  ),
                  hr(),
                  conditionalPanel("input.submitB != 0",
                                   wellPanel(
                    fluidPage(
                      fluidRow(
                        wellPanel(
                          div(style = "position:absolute;top:9em; left:1em", h4("Please drag the wrong answers into this box and click the CLEAR to restart.")),
                          dropUI("home2", class = "dropelement dropelementHome2", row_n = 2, col_n = 2),
                          div(style = "position:absolute; top:8em; right:3em", bsButton("clearB", "CLEAR", style = "danger")),
                          class = "wellTransparent col-lg-8"
                        ),
                        wellPanel(h3("Full score is 20 for level B."),
                          verbatimTextOutput("scoreB"),
                          class = "wellTransparent col-lg-4"
                        )
                      )
                    )
                  )
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
                fluidRow(
                  column(
                    3,
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 30)),
                      bsButton("ins3", "", icon = icon("info", class = "iconi fa-fw"), type = "toggle", class = "butt"),
                      bsButton("bq3", "", icon = icon("question", class = "iconq fa-fw"), type = "toggle", class = "butt"),
                      bsButton("bt3", "", icon = icon("time", lib = "glyphicon", class = "icont fa-fw"), type = "toggle", class = "butt")
                    ),
                    div(
                      id = "plot-container3",
                      conditionalPanel(
                        "input.bq3 != 0",
                        tags$img(
                          src = "HINT3.PNG",
                          id = "hint3"
                        )
                      )
                    ),
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      conditionalPanel(
                        "input.ins3 != 0",
                        box(
                          title = "Instruction:", status = "danger", solidHeader = TRUE, width = 12,
                          "Choose variable types from dropdown menus."
                        )
                      )
                    )
                  ),
                  column(3,
                    offset = 6,
                    hidden(div(id = "timer3h", textOutput("timer3")))
                  ),
                  br()
                ),
                br(),
                wellPanel(
                  fluidRow(uiOutput("questionC"), br())
                ),
                hr(),
                fluidRow(
                  column(3,
                    offset = 1,
                    selectInput("explC", uiOutput("varEXP"), c(
                      "", "Neither", "Explanatory",
                      "Response"
                    )), uiOutput("markc1")
                  ),
                  column(3,
                    offset = 3,
                    selectInput("respC", uiOutput("varRES"), c(
                      "", "Neither", "Explanatory",
                      "Response"
                    )), uiOutput("markc2")
                  )
                ),
                br(),
                fluidRow(
                  column(4, offset = 3, textOutput("correctC"))
                ),
                br(),
                conditionalPanel(
                  "input.next3 != 0",
                  fluidRow(
                    column(1, offset = 1, bsButton("previous4", "<< Previous", style = "primary", size = "small")),
                    column(1, offset = 1, conditionalPanel(
                      "(input.explC!='') & (input.respC!='')",
                      bsButton("submitC", "Submit Answer", style = "primary", class = "grow", size = "small")
                    )),
                    column(1, offset = 2, bsButton("new", "New Question", size = "small", style = "primary", disabled = TRUE)),
                    column(1, offset = 2, bsButton("next4", "Next >>", size = "small", style = "primary", disabled = TRUE))
                  ),
                  hr()
                ),
                fluidRow(
                  column(3, offset = 4, uiOutput("train1"))
                )
              ),
              #### Level 4 ----
              tabPanel(
                title = "Level 4",
                value = "f",
                titlePanel(h1("This level will add in the concepts of confounding variables")),
                fluidRow(h4("You must answer 5 correct choices before completing the level"), style = "margin-left:15px"),
                fluidRow(h4("Once you have made your choices hit submit answer, then click new question for the next question"), style = "margin-left:15px"),
                fluidRow(
                  column(
                    3,
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 30)),
                      bsButton("ins4", "", icon = icon("info", class = "iconi fa-fw"), type = "toggle", class = "butt"),
                      bsButton("bq4", "", icon = icon("question", class = "iconq fa-fw"), type = "toggle", class = "butt"),
                      bsButton("bt4", "", icon = icon("time", lib = "glyphicon", class = "icont fa-fw"), type = "toggle", class = "butt")
                    ),
                    div(
                      id = "plot-container4",
                      conditionalPanel(
                        "input.bq4 != 0",
                        tags$img(
                          src = "HINT4.PNG",
                          id = "hint4"
                        )
                      )
                    ),
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      conditionalPanel(
                        "input.ins4 != 0",
                        box(
                          title = "Instruction:", status = "danger", solidHeader = TRUE, width = 12,
                          "Choose variable types from dropdown menus."
                        )
                      )
                    )
                  ),
                  column(3,
                    offset = 6,
                    hidden(div(id = "timer4h", textOutput("timer4")))
                  ), br() # print the timer)
                ),
                hr(),
                wellPanel(
                  fluidRow(uiOutput("questionD"))
                ),
                fluidRow(
                  column(5,
                    offset = 1,
                    selectInput("resp", uiOutput("varRESD"), c(
                      "", "Explanatory",
                      "Response",
                      "Confounding",
                      "None of the above"
                    )), uiOutput("markd2")
                  ),
                  column(4,
                    offset = 1,
                    selectInput("conf", uiOutput("varCOND"), c(
                      "", "Explanatory",
                      "Response",
                      "Confounding",
                      "None of the above"
                    )), uiOutput("markd3")
                  ),
                  column(4,
                    offset = 1,
                    selectInput("expla", uiOutput("varEXPD"), c(
                      "", "Explanatory",
                      "Response",
                      "Confounding",
                      "None of the above"
                    )), uiOutput("markd1")
                  )
                ),
                fluidRow(
                  column(3, offset = 3, textOutput("correctD"))
                ),
                br(),
                conditionalPanel(
                  "input.next4 != 0",
                  fluidRow(
                    column(1, offset = 1, bsButton("previous5", "<< Previous", style = "primary", size = "small")),
                    column(1, offset = 1, conditionalPanel(
                      "(input.expla!='') & (input.resp!='') & (input.conf!='')",
                      bsButton("submitD", "Submit Answer", style = "primary", class = "grow", size = "small")
                    )),
                    column(1, offset = 2, bsButton("new2", "New Question", size = "small", style = "primary", disabled = TRUE)),
                    column(1, offset = 2, bsButton("finish", "Stop >>", style = "danger", disabled = TRUE, size = "small"))
                  ),
                  hr()
                ),
                fluidRow(
                  column(3, offset = 4, uiOutput("trainB"))
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
          )
        ),
        tabItem(
          ### References ----
          tabName = "references",
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter Bootstrap Components
                      for Shiny. R package version 0.61. Available from
                      https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils:
                      BOAST Utilities. R package version 0.1.6.3. Available from
                      https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard:
                      Create Dashboards with 'Shiny'. R package version 0.7.1.
                      Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and
                      McPherson, J. (2020). shiny: Web Application Framework for
                      R. R package version 1.5.0. Available from
                      https://CRAN.R-project.org/package=shiny"
          )
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
    if(!time$started) {
      
      # Bank A
      initBankA()
      
      # Bank B
      initBankB()
      
      # Start timer
      time$started <- TRUE
    }
  }

  ## Go button ----
  observeEvent(input$go, {
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "challenge"
    )
  })

  ## Reset Button ----
  observeEvent(input$reset_button, {
    js$reset()
  })
  
  ## Time Tracking ----
  observeEvent(input$submitA, {
    time$started <- FALSE
  })
  
  observeEvent(input$previous2, {
    updateTabsetPanel(session, "levels", selected = "b")
  })
  
  observeEvent(input$next2, {
    time$started <- TRUE
    updateTabsetPanel(session, "levels", selected = "c")
  })
  
  observeEvent(input$submitB, {
    time$started <- FALSE
  })
  
  observeEvent(input$previous3, {
    updateTabsetPanel(session, "levels", selected = "c")
  })
  
  observeEvent(input$next3, {
    time$started <- TRUE
    updateTabsetPanel(session, "levels", selected = "e")
  })
  
  observeEvent(input$new, {
    time$started <- TRUE
  })
  
  observeEvent(input$submitC, {
    time$started <- FALSE
  })
  
  observeEvent(input$next4, {
    time$started <- TRUE
    updateTabsetPanel(session, "levels", selected = "f")
  })
  
  observeEvent(input$finish, {
    
    stmt <- boastUtils::generateStatement(
      session,
      verb = "completed",
      object = "shiny-tab-challenge",
      description = "Challenge completed",
      completion = TRUE
    )
    
    boastUtils::storeStatement(session, stmt)
    
    time$timer <- reactiveTimer(Inf)
    updateTabsetPanel(session, "levels", selected = "d")
  })

  observe({
    time$timer()
    if (isolate(time$started)) {
      time$inc <- isolate(time$inc) + 1
    }
  })
  
  observeEvent(input$bt1 == TRUE, {
    toggle("timer1h")
    output$timer1 <- renderPrint({
      cat("you have used:", time$inc, "secs")
    })
  })

  observeEvent(input$bt2 == TRUE, {
    toggle("timer2h")
    output$timer2 <- renderPrint({
      cat("you have used:", time$inc, "secs")
    })
  })

  observeEvent(input$bt3 == TRUE, {
    toggle("timer3h")
    output$timer3 <- renderPrint({
      cat("you have used:", time$inc, "secs")
    })
  })

  observeEvent(input$bt4 == TRUE, {
    toggle("timer4h")
    output$timer4 <- renderPrint({
      cat("you have used:", time$inc, "secs")
    })
  })

  observeEvent(input$bt5 == TRUE, {
    toggle("timer5")
    output$timer5 <- renderPrint({
      cat("you have used:", time$inc, "secs")
    })
  })

  output$timer1 <- renderPrint({
    cat("you have used:", time$inc, "secs")
  })

  output$timer2 <- renderPrint({
    cat("you have used:", time$inc, "secs")
  })

  output$timer3 <- renderPrint({
    cat("you have used:", time$inc, "secs")
  })

  output$timer4 <- renderPrint({
    cat("you have used:", time$inc, "secs")
  })

  output$timer5 <- renderPrint({
    cat("you have used:", time$inc, "secs")
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
    
    output$disID4 <- renderText({
      bank[numbers$dis[4], 2]
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
    
    output$disName4 <- renderText({
      bank[numbers$dis[4], 3]
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
    
    output$contID4 <- renderText({
      bank[numbers$cont[4], 2]
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
    
    output$contName4 <- renderText({
      bank[numbers$cont[4], 3]
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
    
    output$nomID4 <- renderText({
      bank[numbers$nom[4], 2]
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
    
    output$nomName4 <- renderText({
      bank[numbers$nom[4], 3]
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
    
    output$ordID4 <- renderText({
      bank[numbers$ord[4], 2]
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
    
    output$ordName4 <- renderText({
      bank[numbers$ord[4], 3]
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

  observeEvent(input$previous4, {
    updateTabsetPanel(session, "levels", selected = "e")
    index_list$listc <- c(index_list$listc, sample(1:17, 17, replace = FALSE))
  })

  observeEvent(input$next3, {
    index$index <- 18
    index$exp_index <- 2 * index$index - 1
    index$res_index <- 2 * index$index
  })

  observeEvent(input$new, {
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

  observeEvent(input$previous5, {
    updateTabsetPanel(session, "levels", selected = "f")
    index_listD$listD <- c(index_listD$listD, sample(1:8, 8, replace = FALSE))
  })

  observeEvent(input$next4, {
    index2$index2 <- 9
    index2$explan <- 3 * index2$index2 - 2
    index2$respon <- 3 * index2$index2 - 1
    index2$confou <- 3 * index2$index2
  })

  observeEvent(input$new2, {
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
        if (!is.null(input$drp1)) {
          valid <- any(trimws(input$drp1) == bank[c(1:10), 3])
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
        if (!is.null(input$drp2)) {
          valid <- any(trimws(input$drp2) == bank[c(1:10), 3])
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
        if (!is.null(input$drp3)) {
          valid <- any(trimws(input$drp3) == bank[c(1:10), 3])
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
        if (!is.null(input$drp4)) {
          valid <- any(trimws(input$drp4) == bank[c(1:10), 3])
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
        if (!is.null(input$drp5)) {
          valid <- any(trimws(input$drp5) == bank[c(11:36), 3])
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
        if (!is.null(input$drp6)) {
          valid <- any(trimws(input$drp6) == bank[c(11:36), 3])
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
        if (!is.null(input$drp7)) {
          valid <- any(trimws(input$drp7) == bank[c(11:36), 3])
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
        if (!is.null(input$drp8)) {
          valid <- any(trimws(input$drp8) == bank[c(11:36), 3])
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
        if (!is.null(input$drp9)) {
          valid <- any(trimws(input$drp9) == bank[c(37:56), 3])
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
      output$answer10 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer10 <- renderUI({
        if (!is.null(input$drp10)) {
          valid <- any(trimws(input$drp10) == bank[c(37:56), 3])
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
      output$answer11 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer11 <- renderUI({
        if (!is.null(input$drp11)) {
          valid <- any(trimws(input$drp11) == bank[c(37:56), 3])
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
      output$answer12 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer12 <- renderUI({
        if (!is.null(input$drp12)) {
          valid <- any(trimws(input$drp12) == bank[c(37:56), 3])
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
      output$answer13 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer13 <- renderUI({
        if (!is.null(input$drp13)) {
          valid <- any(trimws(input$drp13) == bank[c(57:71), 3])
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
      output$answer14 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer14 <- renderUI({
        if (!is.null(input$drp14)) {
          valid <- any(trimws(input$drp14) == bank[c(57:71), 3])
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
      output$answer15 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer15 <- renderUI({
        if (!is.null(input$drp15)) {
          valid <- any(trimws(input$drp15) == bank[c(57:71), 3])
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
      output$answer16 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$answer16 <- renderUI({
        if (!is.null(input$drp16)) {
          valid <- any(trimws(input$drp16) == bank[c(57:71), 3])
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
        if (!is.null(input$drop1)) {
          if (input$drop1 == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete", 5]) {
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
        if (!is.null(input$drop2)) {
          if (input$drop2 == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous", 5]) {
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
        if (!is.null(input$drop3)) {
          if (input$drop3 == numbersB$questionB[numbersB$questionB[1] == "QualNominal", 5]) {
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
        if (!is.null(input$drop4)) {
          if (input$drop4 == numbersB$questionB[numbersB$questionB[1] == "QualOrdinal", 5]) {
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
    
    for (i in c(input$drp1, input$drp2, input$drp3, input$drp4)) {
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
    for (i in c(input$drp9, input$drp10, input$drp11, input$drp12)) {
      if (any(trimws(i) == bank[c(37:56), 3])) {
        score3 <- c(score3, 2.5)
      } else {
        score3 <- c(score3, -1.5)
      }
    }
    for (i in c(input$drp13, input$drp14, input$drp15, input$drp16)) {
      if (any(trimws(i) == bank[c(57:71), 3])) {
        score4 <- c(score4, 2.5)
      } else {
        score4 <- c(score4, -1.5)
      }
    }
    
    total <- sum(c(score1, score2, score3, score4))
    
    response <- list(
      "Quantitative_Discrete" = c(trimws(input$drp1), trimws(input$drp2), trimws(input$drp3), trimws(input$drp4)),
      "Quantitative_Continuous" = c(trimws(input$drp5), trimws(input$drp6), trimws(input$drp7), trimws(input$drp8)),
      "Qualitative_Nominal" = c(trimws(input$drp9), trimws(input$drp10), trimws(input$drp11), trimws(input$drp12)),
      "Qualitative_Ordinal" = c(trimws(input$drp13), trimws(input$drp14), trimws(input$drp15), trimws(input$drp16))
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
    
    for (i in input$drop1) {
      if (i == image1) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$drop2) {
      if (i == image2) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$drop3) {
      if (i == image3) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$drop4) {
      if (i == image4) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    
    total <- sum(score5)
    
    response <- list(
      "Quantitative_Discrete" = c(image1, input$drop1),
      "Quantitative_Continuous" = c(image2, input$drop2),
      "Qualitative_Nominal" = c(image3, input$drop3),
      "Qualitative_Ordinal" = c(image4, input$drop4)
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
  observeEvent(input$submitC, {
    observeEvent(input$new, {
      output$markc1 <- renderUI({
        img(src = NULL, width = 30)
      })
    })

    observe({
      output$markc1 <- renderUI({
        if (!is.null(input$explC)) {
          if (any(input$explC == key1[index$exp_index, 1])) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })

  observeEvent(input$submitC, {
    observeEvent(input$new, {
      output$markc2 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$markc2 <- renderUI({
        if (!is.null(input$respC)) {
          if (any(input$respC == key1[index$res_index, 1])) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })

  observeEvent(input$new, {
    reset("explC")
    reset("respC")
    reset("submit")
  })
  
  summationC <- reactiveValues(correct1 = c(0), started = FALSE)
  
  observeEvent(input$next3, {
    summationC$started <- TRUE
  })
  
  observeEvent(input$new, {
    summationC$started <- TRUE
  })
  
  observeEvent(input$submitC, {
    summationC$started <- TRUE
  })
  
  observeEvent(input$submitC, {
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
  
  observeEvent(input$submitC, {
    if (summation$summationC[input$submitC] >= 5) {
      updateButton(session, "next4", disabled = FALSE)
      updateButton(session, "new", disabled = TRUE)
    }
  })

  ### Validate Level 4 ----
  observeEvent(input$submitD, {
    observeEvent(input$new2, {
      output$markd1 <- renderUI({
        img(src = NULL, width = 30)
      })
    })

    observe({
      output$markd1 <- renderUI({
        if (!is.null(input$expla)) {
          if (any(input$expla == key2[index2$explan, 1])) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })

  observeEvent(input$submitD, {
    observeEvent(input$new2, {
      output$markd2 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$markd2 <- renderUI({
        if (!is.null(input$resp)) {
          if (any(input$resp == key2[index2$respon, 1])) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })

  observeEvent(input$submitD, {
    observeEvent(input$new2, {
      output$markd3 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$markd3 <- renderUI({
        if (!is.null(input$conf)) {
          if (any(input$conf == key2[index2$confou, 1])) {
            img(src = "check.PNG", width = 30)
          } else {
            img(src = "cross.PNG", width = 30)
          }
        }
      })
    })
  })

  observeEvent(input$submitD, {
    observeEvent(input$new2, {
      output$markd3 <- renderUI({
        img(src = NULL, width = 30)
      })
    })
    observe({
      output$markd3 <- renderUI({
        if (!is.null(input$conf)) {
          if (any(input$conf == key2[index2$confou, 1])) {
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

      success <- (any(input$expla == key2[index2$explan,1])& any(input$resp== key2[index2$respon,1])&any(input$conf== key2[index2$confou,1]))
      
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
    
    for (i in c(input$drp1, input$drp2, input$drp3, input$drp4)) {
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
    for (i in c(input$drp9, input$drp10, input$drp11, input$drp12)) {
      if (any(trimws(i) == bank[c(37:56), 3])) {
        score3 <- c(score3, 2.5)
      } else {
        score3 <- c(score3, -1.5)
      }
    }
    for (i in c(input$drp13, input$drp14, input$drp15, input$drp16)) {
      if (any(trimws(i) == bank[c(57:71), 3])) {
        score4 <- c(score4, 2.5)
      } else {
        score4 <- c(score4, -1.5)
      }
    }
    for (i in input$drop1) {
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete", 5]) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$drop2) {
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous", 5]) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$drop3) {
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualNominal", 5]) {
        score5 <- c(score5, 5)
      } else {
        score5 <- c(score5, -3)
      }
    }
    for (i in input$drop4) {
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
  observeEvent(input$tabs, {
    if(input$tabs == "challenge") {
      startGame()
    }
  })
}

boastUtils::boastApp(ui = ui, server = server)
