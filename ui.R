library(shiny)
library(shinyDND)
library(shinyjs)
library(shinyBS)
library(V8)
library(grid)
library(RUnit)
library(shinyalert)
library(shinydashboard)

data <- read.csv("questionBank.csv")



#####remember to add back the disable=TRUE to the next button

shinyUI(tagList(
  useShinyalert(),
  useShinyjs(),
  navbarPage(title = "Variable Types",id = "navMain", #Give an id to use the updateNavPage() later
             tabPanel(title = "Home",value = "a", ##Give a value to use the updateNavPage() later
                      tags$a(href='http://stat.psu.edu/',tags$img(src='logo.PNG', align = "left", width = 180)),
                      br(),br(),br(),
                      fluidPage(
                        tags$style(type='text/css', '#scoreA {font-size: 25px; font-weight: bold;font family:Sans-serif; height: 140px}'),
                        tags$style(type='text/css', '#scoreB {font-size: 25px; font-weight: bold;font family:Sans-serif; height: 140px}'),
                        #tags$style(type='text/css', '#scoreC {font-size: 60px; font-weight: bold;font family:Sans-serif; height: 140px}'),
                        #tags$style(type='text/css', '#scoreD {font-size: 60px; font-weight: bold;font family:Sans-serif; height: 140px}'),
                        tags$style(type='text/css', '#timer1 {background-color:black; font-size: 30px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'),
                        tags$style(type='text/css', '#timer2 {background-color:#2C3E50; font-size: 30px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'),
                        tags$style(type='text/css', '#timer3 {background-color:#2C3E50; font-size: 30px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'),
                        tags$style(type='text/css', '#timer4 {background-color:#2C3E50; font-size: 30px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'),
                        tags$style(type='text/css', '#timer5 {background-color:#2C3E50; font-size: 30px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'),
                        tags$style(type='text/css', '#correctC {background-color:#c11919; font-size: 25px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'),
                        tags$style(type='text/css', '#correctD {background-color:#c11919; font-size: 25px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'),
                          
                        h3(strong("About:")),
                        h4(tags$li("Level 1 and 2: Identify four different variable types: Quantitative (numeric) discrete variables, Quantitative continuous variables,
          Qualitative (categorical) nominal variables, and Qualitative ordinal variables.")),
                        uiOutput("about2"),br(),br(),br(),
                        h4(tags$li("Level 3 and 4: Identify the explanatory and response variables in level 3. Then in level 4 you will also
          be required to identify the confounding variable.")),
                        br(),
                        h3(strong("Instructions:")),
                        h4(tags$li("Click GO! button to start the game.")),
                        h4(tags$li("Drag the variable names to the boxes by the variable types.")),
                        h4(tags$li("Submit your answer only after finishing all the questions.")),
                        h4(tags$li("You may go to the next level only when you correct your answers for level 1 and 2. For level 3 and 4 you must get 5 correct problems on each level to finish the game and get your final score.")),
                        h4(tags$li("The score you get after the first trial and the revised score you get after correct all answers will be weighted to
          generate your final score. Keep in mind that both the final score and consumed time will determine whether you will be on the leaderboard!")),
                        div(style = "text-align: center",
                            bsButton("go","G O !" ,style = "danger",size = "medium",class = "circle grow")),br(),
                        h3(strong("Acknowledgements:")),
                        h4("This app was developed and coded by Yuxin Zhang, Luxin Wang, & Thomas McIntyre. Special thanks to Robert P. Carey III and Alex Chen for help on some programming issues.")
                        )                                   
                        ),
             
             tabPanel("Level 1",value = "b",
                      fluidPage(theme = "bootstrap.css", #css theme
                                
                                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "themestyle.css")), #link to your own css file
                                #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "webkit.css")),
                                titlePanel("Drag the variables into the categories they belong to. "),
                                fluidRow(
                                  column(3, 
                                         div(style="display: inline-block;vertical-align:top;",
                                             tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 30)),
                                             bsButton('ins1', '', icon = icon('info',class = "iconi fa-fw"),type = 'toggle', class = 'butt'),
                                             bsButton('bq1', '',icon = icon('question',class = "iconq fa-fw"),type = 'toggle', class = 'butt'),
                                             bsButton('bt1', '',icon = icon('time', lib = 'glyphicon',class = "icont fa-fw"),type = 'toggle', class = 'butt')
                                         ),
                                         
                                         
                                         div(id = "plot-container",
                                             conditionalPanel("input.bq1 != 0",
                                                              tags$img(src = "STAT.PNG",
                                                                       id = "hint"))
                                         ),
                                         div(style="display: inline-block;vertical-align:top;",
                                             conditionalPanel("input.ins1 != 0",
                                                              box(title = "Instruction:", status = "danger", solidHeader = TRUE, width = 12, 
                                                                "Drag variable names to correct variable type."))
                                             )
                                  ),
                                  column(3,offset = 6,
                                         hidden(div(id='timer1h',textOutput("timer1"))
                                         ))),
                                
                                br(), #print the timer
                                
                                conditionalPanel("input.go != 0", #Show everything only after the GO button is clicked
                                                 #Set up all dragUIs which are randomly chosen from the question bank 
                                                 fluidRow(
                                                   wellPanel(dragUI(textOutput("disID1"),textOutput("disName1"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("disID2"),textOutput("disName2"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("nomID1"),textOutput("nomName1"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("contID1"),textOutput("contName1"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   
                                                   wellPanel(dragUI(textOutput("disID3"),textOutput("disName3"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("contID2"),textOutput("contName2"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("nomID2"), textOutput("nomName2"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("ordID1"), textOutput("ordName1"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   
                                                   wellPanel(dragUI(textOutput("contID3"),textOutput("contName3"),class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("ordID2"),textOutput("ordName2"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("nomID3"),textOutput("nomName3"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("contID4"),textOutput("contName4"), class = "drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   
                                                   wellPanel(dragUI(textOutput("ordID3"),textOutput("ordName3"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-2 drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("disID4"),textOutput("disName4"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-2 drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("ordID4"),textOutput("ordName4"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-2 drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dragUI(textOutput("nomID4"),textOutput("nomName4"), class = "col-xs-12 col-sm-12 col-md-6 col-lg-2 drag dragelement"), class = "wellTransparent col-sm-12 col-md-6 col-lg-2")
                                                 ),hr(),
                                                 
                                                 #Set up all dropUIs and check/cross boxes
                                                 fluidRow(
                                                   h4("Quantitative & Discrete:", class = "col-sm-12 col-md-12 col-lg-3"),
                                                   bsPopover(id= "drp1",title= "Quantitative & Discrete", content="Countable Number/Whole Number",
                                                             placement= "top", trigger = "hover", options = NULL),
                                                   wellPanel(dropUI("drp1", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer1")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp2", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer2")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp3", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer3")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp4", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer4")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2")
                                                 ), 
                                                 fluidRow(
                                                   h4("Quantitative & Continuous:", class = "col-sm-12 col-md-12 col-lg-3"),
                                                   bsPopover(id= "drp5",title= "Quantitative & Continuous", content="Noncountable Number / Decimals",
                                                             placement= "top", trigger = "hover", options = NULL),
                                                   wellPanel(dropUI("drp5", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer5")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp6", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer6")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp7", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer7")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp8", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer8")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2")
                                                 ),
                                                 fluidRow(
                                                   h4("Qualitative & Nominal:", class = "col-sm-12 col-md-12 col-lg-3"),
                                                   bsPopover(id= "drp9",title= "Qualitative & Nominal", content="Unordered Categories",
                                                             placement= "top", trigger = "hover", options = NULL),
                                                   wellPanel(dropUI("drp9", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer9")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp10", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer10")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp11", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer11")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp12", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer12")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2")
                                                 ),
                                                 fluidRow(
                                                   h4("Qualitative & Ordinal:", class = "col-sm-12 col-md-12 col-lg-3"),
                                                   bsPopover(id= "drp13",title= "Qualitative & Ordinal", content="Ordered Categories",
                                                             placement= "top", trigger = "hover", options = NULL),
                                                   wellPanel(dropUI("drp13", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer13")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp14", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer14")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp15", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer15")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2"),
                                                   wellPanel(dropUI("drp16", class = "dropelement"),
                                                             div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer16")), class = "wellTransparent col-sm-12 col-md-6 col-lg-2")
                                                 ),hr(), 
                                                 
                                                 
                                                 #Submit button and pagination button
                                                 fluidRow(
                                                   column(1,bsButton("previous3","<<Previous", style = "primary",size = "small")),
                                                   column(1,offset = 4, 
                                                          conditionalPanel("(input.drp1!='') & (input.drp2!='') & (input.drp3!='') & (input.drp4!='') &
                                                                           (input.drp5!='') & (input.drp6!='') & (input.drp7!='') & (input.drp8!='') &
                                                                           (input.drp9!='') & (input.drp10!='') & (input.drp11!='') & (input.drp12!='') &
                                                                           (input.drp13!='') & (input.drp14!='') & (input.drp15!='') & (input.drp16!='')",
                                                                           bsButton("submitA", "Submit Answer", style = "primary",size = "small",class = "grow")
                                                          )
                                                   ),
                                                   column(1,offset = 5,bsButton("next2","Next>>",style = "primary", size = "small",disabled=TRUE))
                                                   ),br()
                                                   ),
                                
                                conditionalPanel("input.submitA != 0",wellPanel(
                                  fluidPage(
                                    fluidRow(
                                      wellPanel(
                                        #div(style = "position:absolute; top;50em; left:1em",
                                        h4("Please drag the wrong answers into this PENALTY box and click the CLEAR button to restart."),
                                        #   ),
                                        dropUI("home1",class = "dropelement dropelementHome", col_n = 3),
                                        
                                        class = "wellTransparent col-lg-8"),
                                      wellPanel(h3("Full score is 40 for level A."),
                                                div(style = "position:absolute; top:8em; right:2em",bsButton("clear","CLEAR",style = "danger")),
                                                verbatimTextOutput("scoreA"),class = "wellTransparent col-lg-4")
                                    )
                                  )
                                ))
                                                 )
                      #############################################################################################             
                                ),
             tabPanel("Level 2", value = "c",
                      titlePanel("Identify in Plots"),
                      fluidRow(
                        column(3, 
                               div(style="display: inline-block;vertical-align:top;",
                                   tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 30)),
                                   bsButton('ins2', '', icon = icon('info',class = "iconi fa-fw"),type = 'toggle', class = 'butt'),
                                   bsButton('bq2', '',icon = icon('question',class = "iconq fa-fw"),type = 'toggle', class = 'butt'),
                                   bsButton('bt2', '',icon = icon('time', lib = 'glyphicon',class = "icont fa-fw"),type = 'toggle', class = 'butt')
                               ),
                               
                               
                               div(id = "plot-container2",
                                   conditionalPanel("input.bq2 != 0",
                                                    tags$img(src = "STAT.PNG",
                                                             id = "hint"))
                               ),
                               div(style="display: inline-block;vertical-align:top;",
                                   conditionalPanel("input.ins2 != 0",
                                                    box(title = "Instruction:", status = "danger", solidHeader = TRUE, width = 12, 
                                                        "Drag letters below graphs to correct variable type."))
                               )
                               
                        ),
                        
                        column(3,offset = 6,
                               hidden(div(id='timer2h',textOutput("timer2"))
                               ))),br(), #print the timer
                      
                      
                      conditionalPanel("input.next2 != 0",
                                       fluidRow(wellPanel(div(style = "text-align:center", h4(textOutput("imgQ1"))),
                                                          uiOutput("image1", class = "picSize"),
                                                          div(style = 'position: relative; top:-15px;', dragUI("img1","A", style = "width: 100px; height: 40px;", class = 'drag dragelement dragelement2'))
                                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                                
                                                wellPanel(div(style = "text-align:center", h4(textOutput("imgQ2"))),
                                                          uiOutput("image2", class = "picSize"),
                                                          div(style = 'position: relative; top:-15px;', dragUI("img2","B", style = "width: 100px; height: 40px;", class = 'drag dragelement dragelement2'))
                                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                                       fluidRow(
                                         fluidRow(wellPanel(
                                           dropUI("drop1",class = "dropelement dropelement2"),
                                           div(style = "position: absolute; top:0;left:1em",h5("Quantitative & Discrete")),
                                           div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer17")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                                           wellPanel(
                                             dropUI("drop2",class = "dropelement dropelement2"),
                                             div(style = "position: absolute; top:0;left:1em",h5("Quantitative & Continuous")),
                                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer18")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                                           wellPanel(
                                             dropUI("drop3",class = "dropelement dropelement2"),
                                             div(style = "position: absolute; top:0; left:1em",h5("Qualitative & Nominal")),
                                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer19")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                                           wellPanel(
                                             dropUI("drop4",class = "dropelement dropelement2"),
                                             div(style = "position: absolute; top:0; left:1em",h5("Qualitative & Ordinal")),
                                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer20")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12")
                                         )
                                       ),br(),
                                       
                                       fluidRow(wellPanel(div(style = 'position: relative; top:-5px;', dragUI("img3","C",style = "width: 100px; height: 40px;", class = 'drag dragelement dragelement2')),
                                                          div(style = "position:relative; text-align:center; top: -15px;", h4(textOutput("imgQ3"))),
                                                          div(style = "position:relative; top: -15px;",uiOutput("image3", class = "picSize"))
                                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                                wellPanel(div(style = 'position: relative; top:-5px;', dragUI("img4","D",style = "width: 100px; height: 40px;", class = 'drag dragelement dragelement2')),
                                                          div(style = "position:relative; text-align:center; top: -15px;",h4(textOutput("imgQ4"))),
                                                          div(style = "position:relative; top: -15px;",uiOutput("image4", class = "picSize"))
                                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                                       
                                       fluidRow(column(1,bsButton("previous2","<<Previous",style = "primary", size = "small")),
                                                column(1,offset = 4,conditionalPanel("(input.drop1!='') & (input.drop2!='') & (input.drop3!='') & (input.drop4!='') & (input.drop5!='')",
                                                                                     bsButton("submitB","Submit Answer", style = "primary", class = "grow", size = "small"))),
                                                column(1,offset = 5,bsButton("next3","Next>>", style = "primary",size = "small",disabled=TRUE)))
                                       ,hr(),
                                       conditionalPanel("input.submitB != 0",wellPanel(
                                         fluidPage(
                                           fluidRow(
                                             wellPanel(
                                               div(style = "position:absolute;top:9em; left:1em",h4("Please drag the wrong answers into this box and click the CLEAR to restart.")),
                                               dropUI("home2",class = "dropelement dropelementHome2", row_n = 2, col_n = 2),
                                               div(style = "position:absolute; top:8em; right:3em",bsButton("clearB","CLEAR",style = "danger")),class = "wellTransparent col-lg-8"),
                                             wellPanel(h3("Full score is 20 for level B."),
                                                       verbatimTextOutput("scoreB"),class = "wellTransparent col-lg-4")
                                           )
                                         )
                                       ))
                      )
                      
                      
             ),
             
             
             
             ##################Adding level 3 game
             tabPanel(title= "Level 3", value= "e",
                      titlePanel("Explanatory and Response Variables"),
                      fluidRow(h4("You must get both answers correct to earn 1 point and get 5 points before moving to the next level"),style='margin-left:15px'),
                      fluidRow(h4("Once you have made your choices hit submit answer, then click new question for the next question"),style='margin-left:15px'),
                      fluidRow(
                        column(3, 
                               div(style="display: inline-block;vertical-align:top;",
                                   tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 30)),
                                   bsButton('ins3', '', icon = icon('info',class = "iconi fa-fw"),type = 'toggle', class = 'butt'),
                                   bsButton('bq3', '',icon = icon('question',class = "iconq fa-fw"),type = 'toggle', class = 'butt'),
                                   bsButton('bt3', '',icon = icon('time', lib = 'glyphicon',class = "icont fa-fw"),type = 'toggle', class = 'butt')
                               ),
                               
                               
                               div(id = "plot-container3",
                                   conditionalPanel("input.bq3 != 0",
                                                    tags$img(src = "HINT3.PNG",
                                                             id = "hint3"))
                               ),
                               div(style="display: inline-block;vertical-align:top;",
                                   conditionalPanel("input.ins3 != 0",
                                                    box(title = "Instruction:", status = "danger", solidHeader = TRUE, width = 12, 
                                                        "Choose variable types from dropdown menus."))
                               )
                        ),
                        column(3,offset = 6,
                               hidden(div(id='timer3h',textOutput("timer3"))
                               )),br() #print the timer
                        
                      ),br(),
                      wellPanel(
                        fluidRow(uiOutput("questionC"),br()
                                 
                                 
                        )),hr(),
                      
                      
                      fluidRow(
                        column(3, offset = 1,
                               selectInput('explC', uiOutput('varEXP'), c('','Neither','Explanatory',
                                                                          'Response')),uiOutput('markc1')
                        ),
                        column(3, offset= 3,
                               selectInput('respC', uiOutput('varRES'), c('','Neither','Explanatory',
                                                                          'Response'
                               )),uiOutput('markc2') 
                        )),br(),
                      
                      
                      fluidRow(
                        column(4, offset=3,textOutput('correctC'))),br(),
                      
                      
                      conditionalPanel("input.next3 != 0",
                                       fluidRow(column(1,offset=1,bsButton("previous4","<<Previous",style = "primary", size = "small")),
                                                column(1,offset = 1,conditionalPanel("(input.explC!='') & (input.respC!='')",
                                                                                     bsButton("submitC","Submit Answer", style = "primary", class = "grow", size = "small"))),
                                                column(1, offset=2,bsButton("new","New Question", size = "small",style='primary',disabled=TRUE)),
                                                column(1,offset =2,bsButton("next4","Next>>", size = "small",style='primary',disabled =TRUE)))
                                       ,hr()
                                       
                      ),
                      fluidRow(
                        column(3,offset=4,uiOutput('train1'))
                      )
             ),
             
             
             #################Adding level 4 game 
             
             tabPanel(title= "Level 4", value= "f",
                      titlePanel(h1("This level will add in the concepts of confounding variables")),
                      fluidRow(h4("You must answer 5 correct choices before completing the level"),style='margin-left:15px'),
                      fluidRow(h4("Once you have made your choices hit submit answer, then click new question for the next question"),style='margin-left:15px'),
                      fluidRow(
                        column(3, 
                               div(style="display: inline-block;vertical-align:top;",
                                   tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 30)),
                                   bsButton('ins4', '', icon = icon('info',class = "iconi fa-fw"),type = 'toggle', class = 'butt'),
                                   bsButton('bq4', '',icon = icon('question',class = "iconq fa-fw"),type = 'toggle', class = 'butt'),
                                   bsButton('bt4', '',icon = icon('time', lib = 'glyphicon',class = "icont fa-fw"),type = 'toggle', class = 'butt')
                               ),
                               
                               
                               div(id = "plot-container4",
                                   conditionalPanel("input.bq4 != 0",
                                                    tags$img(src = "HINT4.PNG",
                                                             id = "hint4"))
                               ),
                               div(style="display: inline-block;vertical-align:top;",
                                   conditionalPanel("input.ins4 != 0",
                                                    box(title = "Instruction:", status = "danger", solidHeader = TRUE, width = 12, 
                                                        "Choose variable types from dropdown menus."))
                               )
                               
                               
                        ),
                        column(3,offset = 6,
                               hidden(div(id='timer4h',textOutput("timer4"))
                               )),br() #print the timer)
                        
                      ),hr(),
                      
                      
                      
                      wellPanel(
                        fluidRow(uiOutput("questionD"))
                        
                        
                      ),
                      
                      fluidRow(
                        column(5, offset = 1,
                               selectInput('resp', uiOutput('varRESD'), c('','Explanatory',
                                                                          'Response',
                                                                          'Confounding',
                                                                          'None of the above'
                               )),uiOutput('markd2')
                               
                        ),
                        column(4, offset= 1,
                               selectInput('conf', uiOutput('varCOND'),c('','Explanatory',
                                                                         'Response',
                                                                         'Confounding', 
                                                                         'None of the above'
                               )), uiOutput('markd3')
                        ),
                        
                        column(4, offset= 1,
                               selectInput('expla',uiOutput('varEXPD') , c('','Explanatory',
                                                                           'Response',
                                                                           'Confounding',
                                                                           'None of the above'
                               )),uiOutput('markd1')
                        )
                      ),
                      
                      fluidRow(
                        column(3, offset=3,textOutput('correctD'))
                        
                      ),
                      br(),
                      
                      conditionalPanel("input.next4 != 0",
                                       
                                       
                                       fluidRow(column(1, offset =1, bsButton("previous5","<<Previous",style = "primary", size = "small")),
                                                column(1,offset = 1,conditionalPanel("(input.expla!='') & (input.resp!='') & (input.conf!='')",
                                                                                     bsButton("submitD","Submit Answer", style = "primary", class = "grow", size = "small"))),
                                                column(1, offset = 2,bsButton("new2","New Question", size = "small",style='primary',disabled=TRUE)),
                                                column(1,offset = 2,bsButton("finish","Stop>>", style = "danger", disabled = TRUE, size = "small")))
                                       ,hr()
                                       
                      ),
                      
                      fluidRow(
                        column(3,offset=4,uiOutput('trainB'))
                      )
                      
             ),
             
             
             
             
             #####score page 
             tabPanel(title = "Score", value = "d",
                      titlePanel(h1("Congratulations! You finished the game.")),
                      fluidRow(column(3,offset = 9,textOutput("timer5"))),br(),br(),
                      fluidPage(
                        fluidRow(h3("Your scores:")),
                        fluidRow(
                          wellPanel(verbatimTextOutput("init"), class = "wellScore col-lg-4 col-md-6 col-sm-12"),
                          wellPanel(verbatimTextOutput("end"), class = "wellScore col-lg-4 col-md-6 col-sm-12"),
                          wellPanel(verbatimTextOutput("totalScore"), class = "wellScore col-lg-4 col-md-6 col-sm-12")
                        ),br(),
                        conditionalPanel("input.finish != 0",
                                         fluidRow(
                                           wellPanel(
                                             wellPanel(textInput("name",h4("Please type in your nickname to submit the score:"),placeholder = "Nickname",width = 600),
                                                       textOutput("checkName"),class = "col-lg-8 col-md-9 col-sm-10 col-xs-9"),
                                             
                                             wellPanel(div(style = "position:absolute; top:60px",bsButton("check","Submit",style = "danger",size = "large")),class = "col-lg-2 col-md-2 col-sm-1 col-xs-1"),style = "height:200px")
                                         ),
                                         
                                         
                                         #numericInput("time","Time",value = 1),
                                         
                                         #verbatimTextOutput("initial"),
                                         
                                         
                                         
                                         conditionalPanel("input.check != 0", dataTableOutput("highscore")),
                                         actionButton("weekhigh", "Show Weekly High Scores"),
                                         actionButton("totalhigh", "Show All-Time High Scores"))
                        
                      ))
             
                      )
             )
  )

