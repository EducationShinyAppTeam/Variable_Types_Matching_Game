library(shiny)
library(shinyDND)
library(shinyjs)
library(shinyBS)
library(V8)
library(shinyalert)

bank <- read.csv("questionBank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

bankB = read.csv("questionBankB.csv")
bankB = data.frame(lapply(bankB, as.character), stringsAsFactors = FALSE)

bankC = read.csv("questionBankC.csv")
bankC = data.frame(lapply(bankC, as.character), stringsAsFactors = FALSE)

bankD=read.csv("questionbankD.csv")
bankD= data.frame(lapply(bankD, as.character), stringsAsFactors = FALSE)
shinyServer(function(input, output, session) {
  ####################################Hide Menu bar###############################################
  observe({
    if(input$go == 0){
      hide(selector = "#navMain li a[data-value=b]")
    }
    else {
      show(selector = "#navMain li a[data-value=b]")
    }
  })
  observe({
    hide(selector = "#navMain li a[data-value=c]")
  })
  observeEvent(input$next2, {
    show(selector = "#navMain li a[data-value=c]")
  })
  observe({
    hide(selector = "#navMain li a[data-value=e]")
  })
  observeEvent(input$next3, {
    show(selector = "#navMain li a[data-value=e]")
  })
  observe({
    hide(selector = "#navMain li a[data-value=f]")
  })
  observeEvent(input$next4, {
    show(selector = "#navMain li a[data-value=f]")
  })
  observe({
    hide(selector = "#navMain li a[data-value=d]")
  })
  observeEvent(input$finish, {
    show(selector = "#navMain li a[data-value=d]")
  })
  #picture in the home page
  output$about2 <- renderUI(
    img(src = "STAT.PNG", width = 380)
  )
  
  #reload the app when the reset button is clicked 
  observeEvent(input$reset_button, {js$reset()}) 
  
  #Create six pagers
  observeEvent(input$go,{
    updateNavbarPage(session = session,"navMain", selected = "b")
  })
  observeEvent(input$next2,{
    updateNavbarPage(session = session,"navMain", selected = "c")
  })
  observeEvent(input$next3,{
    updateNavbarPage(session = session,"navMain", selected = "e")
  })
  observeEvent(input$next4,{
    updateNavbarPage(session = session,"navMain", selected = "f")
  })
  observeEvent(input$finish,{
    updateNavbarPage(session = session,"navMain", selected = "d")
  })
  observeEvent(input$previous1,{
    updateNavbarPage(session = session,"navMain", selected = "c")
  })
  observeEvent(input$previous2,{
    updateNavbarPage(session = session,"navMain", selected = "b")
  })
  
  observeEvent(input$previous3,{
    updateNavbarPage(session = session,"navMain", selected = "a")
  })
  observeEvent(input$previous4,{
    updateNavbarPage(session = session,"navMain", selected = "c")
  })
  observeEvent(input$previous5,{
    updateNavbarPage(session = session,"navMain", selected = "e")
  })
  
  ##Set timer with start, stop, restart, stop, and termination; and show the timer
  time<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=FALSE)
  
  observeEvent(input$go, {time$started<-TRUE})
  observeEvent(input$submitA, {time$started <- FALSE})
  observeEvent(input$next2, {time$started <- TRUE})
  observeEvent(input$submitB, {time$started <- FALSE})
  observeEvent(input$next3, {time$started <- TRUE})
  observeEvent(input$new, {time$started <- TRUE})
  observeEvent(input$submitC, {time$started <- FALSE})
  observeEvent(input$next4, {time$started <- TRUE})
  observeEvent(input$finish, {time$timer<-reactiveTimer(Inf)})
  
  observe({
    time$timer()
    if(isolate(time$started))
      time$inc<-isolate(time$inc)+1
  })
  #show the timer
  observeEvent(input$bt1 == TRUE, {
    toggle('timer1h')
    output$timer1 <- renderPrint({
      cat("you have used:", time$inc, "secs")})
  })
  
  observeEvent(input$bt2 == TRUE, {
    toggle('timer2h')
    output$timer2 <- renderPrint({
      cat("you have used:", time$inc, "secs")})
  })
  
  observeEvent(input$bt3 == TRUE, {
    toggle('timer3h')
    output$timer3 <- renderPrint({
      cat("you have used:", time$inc, "secs")})
  })
  
  observeEvent(input$bt4 == TRUE, {
    toggle('timer4h')
    output$timer4 <- renderPrint({
      cat("you have used:", time$inc, "secs")})
  })
  
  observeEvent(input$bt5 == TRUE, {
    toggle('timer5')
    output$timer5 <- renderPrint({
      cat("you have used:", time$inc, "secs")})
  })
  
  
  output$timer1 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  
  output$timer2 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  
  output$timer3 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  
  output$timer4 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  
  output$timer5 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  ################################### Bank A ###############################################################  
  numbers <- reactiveValues(dis = c(), cont = c(), nom = c(), ord = c())
  
  observeEvent(input$go,{
    numbers$dis = sample(1:10,4)
    numbers$cont = sample(11:36,4)
    numbers$nom = sample(37:56,4)
    numbers$ord = sample(57:71,4)
  })
  
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
    bank[numbers$dis[1],3]
  })
  output$disName2 <- renderText({
    bank[numbers$dis[2],3]
  })
  output$disName3 <- renderText({
    bank[numbers$dis[3],3]
  })
  output$disName4 <- renderText({
    bank[numbers$dis[4],3]
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
    bank[numbers$cont[1],3]
  })
  output$contName2 <- renderText({
    bank[numbers$cont[2],3]
  })
  output$contName3 <- renderText({
    bank[numbers$cont[3],3]
  })
  output$contName4 <- renderText({
    bank[numbers$cont[4],3]
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
    bank[numbers$nom[1],3]
  })
  output$nomName2 <- renderText({
    bank[numbers$nom[2],3]
  })
  output$nomName3 <- renderText({
    bank[numbers$nom[3],3]
  })
  output$nomName4 <- renderText({
    bank[numbers$nom[4],3]
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
    bank[numbers$ord[1],3]
  })
  output$ordName2 <- renderText({
    bank[numbers$ord[2],3]
  })
  output$ordName3 <- renderText({
    bank[numbers$ord[3],3]
  })
  output$ordName4 <- renderText({
    bank[numbers$ord[4],3]
  })
  ######################################  Bank B #############################################################
  numbersB <- reactiveValues(disB = c(), contB = c(), nomB = c(), ordB = c(), indexB = c(), questionB = data.frame())
  
  observeEvent(input$go,{
    numbersB$disB = sample(1:13,1)
    numbersB$contB = sample(14:39,1)
    numbersB$nomB = sample(40:58,1)
    numbersB$ordB = sample(59:74,1)
    
    numbersB$indexB = sample(c("A","B","C","D"),4)
    numbersB$questionB = cbind(bankB[c(numbersB$disB,numbersB$contB,numbersB$nomB,numbersB$ordB),],numbersB$indexB)
    
  })
  output$imgQ1 <- renderText({
    paste("A.",numbersB$questionB[numbersB$questionB[5] == "A",4])
  })
  
  output$image1 <- renderUI({
    img(src = numbersB$questionB[numbersB$questionB[5] == "A",3], width = "95%", height = "95%", style = "text-align: center")
  })
  output$imgQ2 <- renderText({
    paste("B.",numbersB$questionB[numbersB$questionB[5] == "B",4])
  })
  
  output$image2 <- renderUI({
    img(src = numbersB$questionB[numbersB$questionB[5] == "B",3], width = "95%", height = "95%")
  })
  output$imgQ3 <- renderText({
    paste("C.",numbersB$questionB[numbersB$questionB[5] == "C",4])
  })
  output$image3 <- renderUI({
    img(src = numbersB$questionB[numbersB$questionB[5] == "C",3], width = "95%", height = "95%")
  })
  output$imgQ4 <- renderText({
    paste("D.",numbersB$questionB[numbersB$questionB[5] == "D",4])
  })
  output$image4 <- renderUI({
    img(src = numbersB$questionB[numbersB$questionB[5] == "D",3], width = "95%", height = "95%")
  })
  
  
  
  ################################# Bank C #######################
  
  
  index <- reactiveValues(index = 18)
  
  index_list<-reactiveValues(listc=sample(1:17,17,replace=FALSE))
  
  observeEvent(input$previous4,{index_list$listc<-c(index_list$listc,sample(1:17,17,replace=FALSE))})
  
  observeEvent(input$next3,{
    index$index <- 18
    index$exp_index=2*index$index-1
    index$res_index=2*index$index
  })
  
  observeEvent(input$new,{
    index_list$listc=index_list$listc[-1]
    index$index <- index_list$listc[1]
    index$exp_index=2*index$index-1
    index$res_index=2*index$index
  })
  
  key1<-as.matrix(bankC[1:36,1])
  
  
  output$questionC <- renderUI({
    if (index$index == 1){
      h3(bankC[1,5])
    }else if (index$index == 2){
      h3(bankC[3,5])
    }else if (index$index == 3){
      h3(bankC[5,5])
    }else if (index$index == 4){
      h3(bankC[7,5])
    }
    else if (index$index == 5){
      h3(bankC[9,5])
    }else if (index$index == 6){
      h3(bankC[11,5])
    }else if (index$index == 7){
      h3(bankC[13,5])
    }else if (index$index == 8){
      h3(bankC[15,5])
    }
    else if (index$index == 9){
      h3(bankC[17,5])
    }else if (index$index == 10){
      h3(bankC[19,5])
    }else if (index$index == 11){
      h3(bankC[21,5])
    }else if (index$index == 12){
      h3(bankC[23,5])
    }else if (index$index == 13){
      h3(bankC[25,5])
    }else if (index$index == 14){
      h3(bankC[27,5])
    }else if (index$index == 15){
      h3(bankC[29,5])
    }else if (index$index == 16){
      h3(bankC[31,5])
    }else if (index$index == 17){
      h3(bankC[33,5])
    }else if (index$index == 18){
      h3(bankC[35,5])
    }
    
  })
  
  
  output$varEXP <- renderUI({
    if (index$index == 1){
      h3(bankC[1,4])
    }else if (index$index == 2){
      h3(bankC[3,4])
    }else if (index$index == 3){
      h3(bankC[5,4])
    }else if (index$index == 4){
      h3(bankC[7,4])
    }
    else if (index$index == 5){
      h3(bankC[9,4])
    }else if (index$index == 6){
      h3(bankC[11,4])
    }else if (index$index == 7){
      h3(bankC[13,4])
    }else if (index$index == 8){
      h3(bankC[15,4])
    }
    else if (index$index == 9){
      h3(bankC[17,4])
    }else if (index$index == 10){
      h3(bankC[19,4])
    }else if (index$index == 11){
      h3(bankC[21,4])
    }else if (index$index == 12){
      h3(bankC[23,4])
    }
    else if (index$index == 13){
      h3(bankC[25,4])
    }else if (index$index == 14){
      h3(bankC[27,4])
    }else if (index$index == 15){
      h3(bankC[29,4])
    }else if (index$index == 16){
      h3(bankC[31,4])
    }
    else if (index$index == 17){
      h3(bankC[33,4])
    }else if (index$index == 18){
      h3(bankC[35,4])
    }
  })
  
  output$varRES <- renderUI({
    if (index$index == 1){
      h3(bankC[2,4])
    }else if (index$index == 2){
      h3(bankC[4,4])
    }else if (index$index == 3){
      h3(bankC[6,4])
    }else if (index$index == 4){
      h3(bankC[8,4])
    }
    else if (index$index == 5){
      h3(bankC[10,4])
    }else if (index$index == 6){
      h3(bankC[12,4])
    }else if (index$index == 7){
      h3(bankC[14,4])
    }else if (index$index == 8){
      h3(bankC[16,4])
    }
    else if (index$index == 9){
      h3(bankC[18,4])
    }else if (index$index == 10){
      h3(bankC[20,4])
    }else if (index$index == 11){
      h3(bankC[22,4])
    }else if (index$index == 12){
      h3(bankC[24,4])
    }
    else if (index$index == 13){
      h3(bankC[26,4])
    }else if (index$index == 14){
      h3(bankC[28,4])
    }else if (index$index == 15){
      h3(bankC[30,4])
    }else if (index$index == 16){
      h3(bankC[32,4])
    }
    else if (index$index == 17){
      h3(bankC[34,4])
    }else if (index$index == 18){
      h3(bankC[36,4])
    }
  })
  
  #####################################Bank D#####################################################
  
  index2 <- reactiveValues(index2 = 9)
  
  
  index_listD<-reactiveValues(listD=sample(1:8,8,replace=FALSE))
  
  
  observeEvent(input$previous5,{index_listD$listD<-c(index_listD$listD,sample(1:8,8,replace=FALSE))})
  
  observeEvent(input$next4,{
    index2$index2 <- 9
    index2$explan= 3*index2$index2-2
    index2$respon=3*index2$index2-1
    index2$confou=3*index2$index2
    
  })
  
  observeEvent(input$new2,{
    index_listD$listD=index_listD$listD[-1]
    index2$index2 <- index_listD$listD[1]
    index2$explan= 3*index2$index2-2
    index2$respon=3*index2$index2-1
    index2$confou=3*index2$index2
    
  })
  
  key2<- as.matrix(bankD[1:27,1])
  
  output$questionD <- renderUI({
    if (index2$index2 == 1){
      h3(bankD[1,4])
    }else if (index2$index2 == 2){
      h3(bankD[4,4])
    }else if (index2$index2 == 3){
      h3(bankD[7,4])
    }else if (index2$index2 == 4){
      h3(bankD[10,4])
    }
    else if (index2$index2 == 5){
      h3(bankD[13,4])
    }
    else if (index2$index2 == 6){
      h3(bankD[16,4])
    }
    else if (index2$index2 == 7){
      h3(bankD[19,4])
    }
    else if (index2$index2 == 8){
      h3(bankD[22,4])
    }
    else if (index2$index2 == 9){
      h3(bankD[25,4])
    }
    
  })
  output$varEXPD <- renderUI({
    if (index2$index2 == 1){
      h3(bankD[1,3])
    }else if (index2$index2 == 2){
      h3(bankD[4,3])
    }else if (index2$index2 == 3){
      h3(bankD[7,3])
    }else if (index2$index2 == 4){
      h3(bankD[10,3])
    }
    else if (index2$index2 == 5){
      h3(bankD[13,3])
    }
    else if (index2$index2 == 6){
      h3(bankD[16,3])
    }
    else if (index2$index2 == 7){
      h3(bankD[19,3])
    }
    else if (index2$index2 == 8){
      h3(bankD[22,3])
    }
    else if (index2$index2 == 9){
      h3(bankD[25,3])
    }
    
  })
  
  output$varRESD <- renderUI({
    if (index2$index2 == 1){
      h3(bankD[2,3])
    }else if (index2$index2 == 2){
      h3(bankD[5,3])
    }else if (index2$index2 == 3){
      h3(bankD[8,3])
    }else if (index2$index2 == 4){
      h3(bankD[11,3])
    }
    else if (index2$index2 == 5){
      h3(bankD[14,3])
    }
    else if (index2$index2 == 6){
      h3(bankD[17,3])
    }
    else if (index2$index2 == 7){
      h3(bankD[20,3])
    }
    else if (index2$index2 == 8){
      h3(bankD[23,3])
    }
    else if (index2$index2 == 9){
      h3(bankD[26,3])
    }
    
  })
  
  output$varCOND <- renderUI({
    if (index2$index2 == 1){
      h3(bankD[3,3])
    }else if (index2$index2 == 2){
      h3(bankD[6,3])
    }else if (index2$index2 == 3){
      h3(bankD[9,3])
    }else if (index2$index2 == 4){
      h3(bankD[12,3])
    }
    else if (index2$index2 == 5){
      h3(bankD[15,3])
    }
    else if (index2$index2 == 6){
      h3(bankD[18,3])
    }
    else if (index2$index2 == 7){
      h3(bankD[21,3])
    }
    else if (index2$index2 == 8){
      h3(bankD[24,3])
    }
    else if (index2$index2 == 9){
      h3(bankD[27,3])
    }
    
  })
  ################################################################################################  
  
  observeEvent(input$submitA,{
    updateButton(session,"submitA",disabled = TRUE)
  })
  observeEvent(input$clear,{
    updateButton(session,"submitA",disabled = FALSE)
  })
  observeEvent(input$submitB,{
    updateButton(session,"submitB",disabled = TRUE)
  })
  observeEvent(input$clearB,{
    updateButton(session,"submitB",disabled = FALSE)
  })
  
  observeEvent(input$submitC,{
    updateButton(session,"submitC",disabled = TRUE)
  })
  
  
  
  observe({
    if (length(index_list$listc)==1){
      updateButton(session,"new", disabled = TRUE)
      updateButton(session,"submitC",disabled = TRUE)
      shinyalert("Oops!","You have used up all the tries. Please click 'previous' then click 'next' to re-enter this level to try again",type='error')
    }
  })
  
  observe({
    if (length(index_listD$listD)==1){
      updateButton(session,"new2", disabled = TRUE)
      updateButton(session,"submitD",disabled = TRUE)
      shinyalert("Oops!","You have used up all the tries. Please click 'previous' then click 'next' to re-enter this level to try again",type='error')
    }
  })
  
  
  observeEvent(input$submitC,{
    updateButton(session,"new",disabled = FALSE)
  })
  
  observeEvent(input$previous4,{
    updateButton(session,"submitC",disabled = FALSE)
  })
  
  observeEvent(input$new,{
    updateButton(session,"submitC",disabled = FALSE)
  })
  
  observeEvent(input$new,{
    updateButton(session,"new",disabled = TRUE)
  })
  
  observeEvent(input$submitD,{
    updateButton(session,"submitD",disabled = TRUE)
  })
  
  observeEvent(input$submitD,{
    updateButton(session,"new2",disabled = FALSE)
  })
  
  observeEvent(input$previous5,{
    updateButton(session,"submitD",disabled = FALSE)
  })
  
  observeEvent(input$new2,{
    updateButton(session,"submitD",disabled = FALSE)
  })
  
  observeEvent(input$new2,{
    updateButton(session,"new2",disabled = TRUE)
  })
  ##try for level3 
  
  
  
  observeEvent(input$submitC,{  
    observeEvent(input$new,{
      output$markc1 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    
    observe({
      output$markc1 <- renderUI({
        if (!is.null(input$explC)){
          if (any(input$explC == key1[index$exp_index,1])){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
      
    })
  })
  
  observeEvent(input$submitC,{ 
    observeEvent(input$new,{
      output$markc2 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$markc2 <- renderUI({
        if (!is.null(input$respC)){
          if (any(input$respC == key1[index$res_index,1])){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  
  observeEvent(input$new, {
    reset('explC')
    reset('respC')
    reset('submit')
  })  
  
  
  #########################check marks for level 4####################################
  observeEvent(input$submitD,{  
    observeEvent(input$new2,{
      output$markd1 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    
    observe({
      output$markd1 <- renderUI({
        if (!is.null(input$expla)){
          if (any(input$expla == key2[index2$explan,1])){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitD,{ 
    observeEvent(input$new2,{
      output$markd2 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$markd2 <- renderUI({
        if (!is.null(input$resp)){
          if (any(input$resp == key2[index2$respon,1])){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitD,{ 
    observeEvent(input$new2,{
      output$markd3 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$markd3 <- renderUI({
        if (!is.null(input$conf)){
          if (any(input$conf == key2[index2$confou,1])){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitD,{ 
    observeEvent(input$new2,{
      output$markd3 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$markd3 <- renderUI({
        if (!is.null(input$conf)){
          if (any(input$conf == key2[index2$confou,1])){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$new2, {
    reset('expla')
    reset('resp')
    reset('conf')
  })  
  
  
  ########################account for correct answers level 3##########################
  
  
  summationC<-reactiveValues(correct1 = c(0), started=FALSE)
  
  
  
  observeEvent(input$next3, {summationC$started <- TRUE})
  observeEvent(input$new, {summationC$started <- TRUE})
  observeEvent(input$submitC, {summationC$started <- TRUE})
  
  observeEvent(input$submitC,{ 
    for (i in c(input$explC)){
      if (any(input$explC == key1[index$exp_index,1])& any(input$respC == key1[index$res_index,1])){
        summationC$correct1 = c(summationC$correct1,1)
      }else{
        summationC$correct1 = c(summationC$correct1,0)
      }
    }
    # for (i in c(input$respC)){
    #   if (any(i == 'Response')){
    #     summationC$correct2 = c(summationC$correct2,1)
    #   }else{
    #     summationC$correct2 = c(summationC$correct2,0)
    #   }
    # }
    
    
    summation$summationC[input$submitC] <- sum(c(summationC$correct1))
  })
  
  output$correctC <- renderPrint({
    if (sum(c(summationC$correct1))==0) {cat("You have earned 0 points")}
    else{
      cat("You have earned",summation$summationC[input$submitC],'points')}
  })
  
  
  observeEvent(input$submitC,{
    if(summation$summationC[input$submitC] >= 5){
      updateButton(session, "next4",disabled = FALSE)
      updateButton(session, "new",disabled = TRUE)
    }})
  
  ################################## Counting Right and Wrong answers in level 4##################3
  summationD<-reactiveValues(correct1D = c(0), started=FALSE)
  test<-reactiveValues(A=FALSE,B=FALSE,C=TRUE)
  
  
  
  observeEvent(input$next4, {time$started <- TRUE})
  observeEvent(input$new2, {time$started <- TRUE})
  observeEvent(input$submitD, {time$started <- TRUE})
  
  observeEvent(input$submitD,{ 
    for (x in c(input$expla)){
      if (any(input$expla == key2[index2$explan,1])& any(input$resp== key2[index2$respon,1])&any(input$conf== key2[index2$confou,1])){
        summationD$correct1D = c(summationD$correct1D,1)
      }else{
        summationD$correct1D = c(summationD$correct1D,0)
      }
    }
    # for (i in c(input$resp)){
    #   if (any(i == 'response')){
    #     summationD$correct2D = c(summationD$correct2D,1)
    #   }else{
    #     summationD$correct2D = c(summationD$correct2D,0)
    #   }
    # }
    # for (i in c(input$conf)){
    #   if (any(i == 'confounding')){
    #     summationD$correct3D = c(summationD$correct3D,1)
    #   }else{
    #     summationD$correct3D = c(summationD$correct3D,0)
    #   }
    # }
    
    
  })
  
  
  
  output$correctD <- renderPrint({
    if (sum(c(summationD$correct1D))==0) {cat("You have earned 0 points")}
    else{
      cat("You have earned",sum(c(summationD$correct1D)),'points')}
  })
  
  
  observeEvent(input$submitD,{
    if(sum(c(summationD$correct1D)) >= 5){
      updateButton(session, "finish",disabled = FALSE)
      updateButton(session, "new2",disabled = FALSE)
    }})
  
  
  ##################################CHECK MARK IN LEVEL 1 AND 2##########################
  
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer1 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer1 <- renderUI({
        if (!is.null(input$drp1)){
          if (any(input$drp1 == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer2 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer2 <- renderUI({
        if (!is.null(input$drp2)){
          if (any(input$drp2 == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer3 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer3 <- renderUI({
        if (!is.null(input$drp3)){
          if (any(input$drp3 == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer4 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer4 <- renderUI({
        if (!is.null(input$drp4)){
          if (any(input$drp4 == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer5 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer5 <- renderUI({
        if (!is.null(input$drp5)){
          if (any(input$drp5 == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer6 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer6 <- renderUI({
        if (!is.null(input$drp6)){
          if (any(input$drp6 == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer7 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer7 <- renderUI({
        if (!is.null(input$drp7)){
          if (any(input$drp7 == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer8 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer8 <- renderUI({
        if (!is.null(input$drp8)){
          if (any(input$drp8 == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer9 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer9 <- renderUI({
        if (!is.null(input$drp9)){
          if (any(input$drp9 == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer10 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer10 <- renderUI({
        if (!is.null(input$drp10)){
          if (any(input$drp10 == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer11 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer11 <- renderUI({
        if (!is.null(input$drp11)){
          if (any(input$drp11 == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer12 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer12 <- renderUI({
        if (!is.null(input$drp12)){
          if (any(input$drp12 == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer13 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer13 <- renderUI({
        if (!is.null(input$drp13)){
          if (any(input$drp13 == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer14 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer14 <- renderUI({
        if (!is.null(input$drp14)){
          if (any(input$drp14 == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer15 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer15 <- renderUI({
        if (!is.null(input$drp15)){
          if (any(input$drp15 == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer16 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer16 <- renderUI({
        if (!is.null(input$drp16)){
          if (any(input$drp16 == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer17 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer17 <- renderUI({
        if (!is.null(input$drop1)){
          if (input$drop1 == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete",5]){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer18 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer18 <- renderUI({
        if (!is.null(input$drop2)){
          if (input$drop2 == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous",5]){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer19 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer19 <- renderUI({
        if (!is.null(input$drop3)){
          if (input$drop3 == numbersB$questionB[numbersB$questionB[1] == "QualNominal",5]){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer20 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer20 <- renderUI({
        if (!is.null(input$drop4)){
          if (input$drop4 == numbersB$questionB[numbersB$questionB[1] == "QualOrdinal",5]){
            img(src = "check.PNG",width = 30)
          }else{
            img(src = "cross.PNG",width = 30)
          }
        }
      })
    })
  })
  
  
  ####################################################################
  summation <- reactiveValues(summationA = c(rep(0,20)), summationB = c(rep(0,20)), summationC = c(rep(0,20)),summationD = c(rep(0,20)),summationScore = c(rep(0,20)))
  
  observeEvent(input$submitA,{
    score1 = c()
    score2 = c()
    score3 = c()
    score4 = c()
    for (i in c(input$drp1,input$drp2,input$drp3,input$drp4)){
      if (any(i == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
        score1 = c(score1,2.5)
      }else{
        score1 = c(score1,-1.5)
      }
    }
    for (i in c(input$drp5,input$drp6,input$drp7,input$drp8)){
      if (any(i == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
        score2 = c(score2, 2.5)}else{
          score2 = c(score2, -1.5)}
    }
    for (i in c(input$drp9,input$drp10,input$drp11,input$drp12)){
      if (any(i == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
        score3 = c(score3, 2.5)}else{
          score3 = c(score3, -1.5)}
    }
    for (i in c(input$drp13,input$drp14,input$drp15,input$drp16)){
      if (any(i == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
        score4 = c(score4, 2.5)}else{
          score4 = c(score4, -1.5)}
    }
    
    # summation$summationA <- c(summation$summationA, sum(c(score1,score2,score3,score4))) 
    summation$summationA[input$submitA] <- sum(c(score1,score2,score3,score4))
  })
  
  observeEvent(input$submitB,{
    score5 = c()
    
    for (i in input$drop1){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop2){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop3){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualNominal",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop4){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualOrdinal",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    
    #summation$summationB <- c(summation$summationB, sum(score5)) 
    summation$summationB[input$submitB] <- sum(score5)
  })
  values = reactiveValues(
    count = 0
  )
  observeEvent(input$submitA,{
    if(summation$summationA[input$submitA] == 40){
      updateButton(session, "next2",disabled = FALSE)
      #values$count = values$count + 40
    }
    
  })
  observeEvent(input$submitB,{
    if(summation$summationB[input$submitB] == 20){
      updateButton(session, "next3",disabled = FALSE)
      #values$count = values$count + 20
    }
    else{
      updateButton(session, "next3", disabled = TRUE)
    }
    
    
  })
  
  
  output$scoreA <- renderPrint({
    cat("Current score of this level is",summation$summationA[input$submitA])
  })
  output$scoreB <- renderPrint({
    cat("Current score of this level is",max(summation$summationB))
  })
  
  
  
  observeEvent(input$finish,{
    summation$summationA[which(summation$summationA == 0)] = summation$summationA[input$submitA]
    summation$summationB[which(summation$summationB == 0)] = summation$summationB[input$submitB]
    summation$summationScore = summation$summationA + summation$summationB+40
  })
  
  output$init <- renderPrint({
    
    # summation$summationA[which(summation$summationA == 0)] = summation$summationA[input$submitA]
    # summation$summationB[which(summation$summationB == 0)] = summation$summationB[input$submitB]
    # summation$summationScore = summation$summationA + summation$summationB
    if (any(summation$summationA != 0) & any(summation$summationB != 0) ){
      initialScore = summation$summationScore[which(summation$summationScore != 0)][1]
    }else{
      initialScore = 0
    }
    
    cat("Initial","\n",initialScore)
  })
  
  final <- reactiveValues(final = 0)
  observeEvent(input$finish,{
    score1 = c()
    score2 = c()
    score3 = c()
    score4 = c()
    score5 = c()
    for (i in c(input$drp1,input$drp2,input$drp3,input$drp4)){
      if (any(i == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
        score1 = c(score1,2.5)
      }else{
        score1 = c(score1,-1.5)
      }
    }
    for (i in c(input$drp5,input$drp6,input$drp7,input$drp8)){
      if (any(i == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
        score2 = c(score2, 2.5)}else{
          score2 = c(score2, -1.5)}
    }
    for (i in c(input$drp9,input$drp10,input$drp11,input$drp12)){
      if (any(i == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
        score3 = c(score3, 2.5)}else{
          score3 = c(score3, -1.5)}
    }
    for (i in c(input$drp13,input$drp14,input$drp15,input$drp16)){
      if (any(i == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
        score4 = c(score4, 2.5)}else{
          score4 = c(score4, -1.5)}
    }
    for (i in input$drop1){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop2){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop3){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualNominal",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop4){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualOrdinal",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    final$final = sum(c(score1,score2,score3,score4,score5))+40
  })
  
  output$end <- renderPrint({
    cat("Improved","\n", final$final)
  })
  
  output$totalScore <- renderPrint({
    cat("Total","\n",round(as.numeric(summation$summationScore[1]) * (2/3) + as.numeric(final$final) * (1/3), digits = 1))
  })
  ################################################LeaderBoard################
  
  output$checkName <- renderText({
    df = data.frame(data())
    df = df[,1]
    if (input$name %in% df){
      updateButton(session,"check",disabled = TRUE)
      print("The nickname has already been used. Please try another one.")
    }else{
      updateButton(session,"check",disabled = FALSE)
      print("The nickname is usable.")
    }
  })
  
  
  outputDir = "scores"
  
  # options(warn = -1)
  values = reactiveValues()
  
  update = reactive({
    value = data.frame("Name" = as.character(input$name),
                       "Score1" = as.numeric(summation$summationScore[1]),
                       "Score2" = as.numeric(final$final),
                       "TotalScore" = as.numeric(summation$summationScore[1]) * (2/3) + as.numeric(final$final) * (1/3),
                       "TimeTaken" = as.numeric(time$inc))
    
    
  })
  values$df = data.frame()
  
  
  saveQuestions <- function(data) {
    # data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.Date()), digest::digest(data))
    # Write the file to the local system
    write.csv(
      x = data,
      file = file.path(outputDir, fileName), 
      row.names = FALSE, quote = TRUE
    )
  }
  
  loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
  }
  # First pattern try
  # paste0("^", Sys.Date(), sep = '')
  x = as.character(as.numeric(Sys.Date()))
  y = as.character(as.numeric(Sys.Date() - 7))
  if(substring(x, 5, 5) >= 7){
    pattern = paste0(substring(x, 1, 4), "[", substring(y, 5, 5), "-", substring(x, 5, 5), "]", sep = '')
  }
  else{
    pattern = paste0(substring(y, 1, 4), "[",substring(y, 5, 5), "-9]|", substring(x,1,4), "[0-", substring(x, 5, 5), "]")
  }
  
  
  loadDataWeek <- function() {
    files = list.files(outputDir, pattern = pattern, full.names = TRUE)
    data = lapply(files, read.csv, stringsAsFactors = FALSE)
    data = do.call(rbind, data)
    data
  }
  
  data = reactive({
    data = loadData()
    data = data[order(-data[,"TotalScore"], data[,"TimeTaken"]),]
    # data
  })
  
  data2 = reactive({
    data = loadDataWeek()
    data = data[order(-data[,"TotalScore"], data[,"TimeTaken"]),]
  })
  
  output$total = renderText({
    as.numeric(summation$summationScore[1]) * (2/3) + as.numeric(final$final) * (1/3)
  })
  
  observeEvent(input$check, {
    scores = update()
    values$df = rbind(values$df, scores)
    saveQuestions(values$df)
  })
  observeEvent(input$weekhigh, {
    output$highscore = renderDataTable({
      # head(data(), 5)
      if(is.null(loadDataWeek()) == TRUE){
        "No Highscores This week"
      }
      else{
        data2()  
      } 
      
    })
  })
  
  observeEvent(input$totalhigh, {
    output$highscore = renderDataTable({
      if(is.null(loadData()) == TRUE){
        "No Highscores"
      }
      else{
        data()
      }
    })
  })
  
  ########train###########
  observe(
    if (sum(c(summationC$correct1))==1){
      output$train1 <- renderUI({
        img(src = 'train1.PNG', width = "20%", height = "20%")
      })}
    else if (sum(c(summationC$correct1))==2){
      output$train1 <- renderUI({
        img(src = 'train2.gif', width = "40%", height = "40%")
      })}
    else if (sum(c(summationC$correct1))==3){
      output$train1 <- renderUI({
        img(src = 'train3.gif', width = "60%", height = "60%")
      })}
    else if (sum(c(summationC$correct1))==4){
      output$train1 <- renderUI({
        img(src = 'train4.gif', width = "80%", height = "80%")
      })}
    else if (sum(c(summationC$correct1))==5){
      output$train1 <- renderUI({
        img(src ='train_f_02.PNG', width = "110%", height = "110%")
      })}
  )  
  
  ######################################Train 2##################################################
  observe(
    if (sum(c(summationD$correct1D))==1){
      output$trainB <- renderUI({
        img(src = 'train1B.PNG', width = "20%", height = "20%")
      })}
    else if (sum(c(summationD$correct1D))==2){
      output$trainB <- renderUI({
        img(src = 'train2B.gif', width = "40%", height = "40%")
      })}
    else if (sum(c(summationD$correct1D))==3){
      output$trainB <- renderUI({
        img(src = 'train3B.gif', width = "60%", height = "60%")
      })}
    else if (sum(c(summationD$correct1D))==4){
      output$trainB <- renderUI({
        img(src = 'train4B.gif', width = "80%", height = "80%")
      })}
    else if (sum(c(summationD$correct1D))==5){
      output$trainB <- renderUI({
        img(src ='train_f_02.PNG', width = "110%", height = "110%")
      })}
  )  
  ######################################################################################
  
  observeEvent(input$check,{
    updateButton(session,"check",disabled = TRUE)
  })
  # 
  # output$badge = renderUI({
  #   score = round(summation$summationScore[1] * (2/3) + final$final * (1/3))
  #   if(is.null(input$name) == T){
  #     place = 0
  #   }
  #   else{
  #     # for(i in 1:3)
  #     #   {
  #     #   if(data2()[i, 1] == input$initials){
  #     #     place = i
  #     #   }
  #     # }
  #     if(score > data2()[3,4]){
  #       
  #       if(score > data2()[2,4]){
  #         if(score > data2()[1,4]){
  #           place = 1
  #         }
  #         else if (score == data2()[1,4]){
  #           if (time$inc < data2()[1,5]){
  #             place = 1
  #           }else if (time$inc >= data2()[1,5]){
  #             place = 2
  #           }
  #         }else{
  #           place = 2
  #         }
  #       }
  #       else if(score == data2()[2,4]){
  #         if(time$inc > data2()[2,5]){
  #           place = 3
  #         }
  #         else{
  #           place = 2
  #         }
  #       }
  #       else{
  #         place = 3
  #       }
  #     }
  #     else if(score == data2()[3,4]){
  #       if(time$inc > data2()[3,5]){
  #         place = 0
  #       }
  #       else{
  #         if(time$inc == data2()[3,5]){
  #           place = 3
  #         }
  #         else{
  #           if(time$inc < data2()[2,5]){
  #             if(time$inc < data2()[1.5]){
  #               place = 1
  #             }
  #             else{
  #               place = 2
  #             }
  #           }
  #           else if(time$inc == data2()[2,5]){
  #             place = 2
  #           }
  #           else{
  #             place = 3
  #           }
  #         }
  #         
  #       }
  #     }
  #     else{
  #       place = 0
  #     }
  #   }
  #   
  #   
  #   
  #   
  #   if(place == 1){
  #     # 1st place image
  #     img(src = "trophy1st.PNG",width = 600)
  #   }
  #   
  #   else if(place == 2){
  #     # 2nd place
  #     img(src = "trophy2nd.PNG",width = 600)
  #   }
  #   
  #   else if(place == 3){
  #     # 3rd place
  #     img(src = "trophy3rd.PNG",width = 600)
  #   }
  #   else{
  #     ""
  #   }
  #   #else if(values$count == data2()[1,2] && time$inc <= )
  # })
  
  
})