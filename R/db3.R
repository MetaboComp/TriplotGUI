db3UI<-function(id){
  ns<-NS(id)
  ### a list of things that is put in the tab
  tagList(
    fluidRow(
      width=12,
      tags$h2("Plot Risk plots")
    ),

    fluidRow(
      width=12,
      box(
        width=6,
        title="The key input files ",
        fluidRow(
          column(12,
                 tags$b("Dataframe used for estimating risk (Upload your data in .rds, .rda, .csv, .xlsx format.)"),
                 tags$br(),
                 tags$b("The dataframe that its variables will be used to calculate correlations with scores (optional)")
          ),
        column(8,



               fileInput(inputId=ns("file_5"),
                         label=NULL,
                         buttonLabel = "Upload...",
                         multiple = F,
                         accept=c(".rds",".rda",".csv",".xlsx"))
        ),
        column(4,
               uiOutput(ns("button3s_act_showplot1"))
        )
        ),
        fluidRow(
          column(12,
               div(style = "margin-top: -30px"),
               uiOutput(outputId=ns("judgefile5")),
               br(),
               uiOutput(outputId = ns("atext5")),
               #div(style = "margin-top: -10px") ,

               uiOutput(ns("button5s_act_1")),
               # br(),
               htmlOutput(ns("datainfo_act51")),

               uiOutput(ns("button5s_act_2")),
               #  br(),
               htmlOutput(ns("datainfo_act52")),

               div(style = "margin-bottom: -10px"),
               hr(),
               div(style = "margin-top: -30px"),
               uiOutput(ns('button5s_act_3')),
               # br(),
               htmlOutput(ns("datainfo_act53")),
               htmlOutput(ns("datainfo_act54")),
               tableOutput(outputId = ns("hed")),
               tableOutput(outputId = ns("hed2")),
              tableOutput(outputId = ns("hed3")),
               tableOutput(outputId = ns("hed4")),
              textOutput(outputId = ns("hed5")),
              #uiOutput(ns('plotplotrisk'))
              plotOutput(ns("showriskplot")),
              ),
        tags$br()
        )),
      box(width=6,
          title="Other settings",
          fluidRow(
            column(12,
                   fileInput(inputId=ns("file_6"),
                             label="The dataframe that its variables will be used as confounders (optional)",
                             buttonLabel = "Upload...",
                             multiple = F,
                             accept=c(".rds",".rda",".csv",".xlsx")),
                   div(style = "margin-top: -30px"),

                   uiOutput(outputId=ns("judgefile6")),
                   br(),
                   uiOutput(outputId = ns("atext6")),

                   uiOutput(ns('button6s_act_1')),
                   # br(),
                   htmlOutput(ns("datainfo_act61")),
                   #tags$br(),   ## this workd
                   uiOutput(ns('button6s_act_2')),
                   # br(),
                   htmlOutput(ns("datainfo_act62")),  ### when this is printed there is hr
                   div(style = "margin-bottom: -10px"),
                   hr(),
                   div(style = "margin-top: -30px"),
                   uiOutput(ns('button6s_act_3')),
                   # br(),
                   htmlOutput(ns("datainfo_act63")),
                   htmlOutput(ns("datainfo_act64")),

                   div(style = "margin-bottom: -10px"),
                   hr(),
                   div(style = "margin-top: -30px"),
               #style="background-color:#ffa153",
               #helpText("Other settings"),
               uiOutput(ns("getconf_adj")),
               numericInput(inputId = ns("component_limits"),
                            label="How many components do you want to use",
                            value=2,
                            min=2,
                            max=2),
               ## always there
               selectInput(inputId =ns("multinomial"),
                           label="Multinomial",
                           #selected ="Spearman",
                           choices=c("No","Yes")),

               ### connect to page 1 input needs to be factorize
               selectInput(inputId =ns("pair"),
                           label="Pair (the variable should be included in page 1's Other settings)",
                           choices=list('NULL'),
                           multiple=F),   #### or a new variable?

               ## always there
               numericInput(inputId = ns("CI"),
                            label="Confidence level",
                            value=0.95,
                            min=0.01,
                            max=1,
                            step=0.01),
               numericInput(inputId = ns("riskWhisker_percentage"),
                            label="Whisker length is how many percentage of confidence interval?",
                            value=0.1,
                            min=0,
                            max=1,
                            step=0.001),
               selectInput(inputId =ns("riskOR"),
                           label="Do you want to risks to be plotted as odds ratio or not?",
                           #selected ="Spearman",
                           choices=c("No","Yes"))
        ))


      ))
  )
}



db3Server<-function(id,r){
  moduleServer(
    id,
    function(input,output,session){
      ns<-session$ns

      ##########################################################################
      ################# The action buttons, only show up when the file is there

      output$button5s_act_1<-renderUI({
        req(input$file_5)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button5_act_1'),
                       label="Check classes of the variables",
                       size="md")
        )
      })
      output$button5s_act_2<-renderUI({
        req(input$file_5)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button5_act_2'),
                       label="Remove variables",
                       size="xs")
        )
      })
      output$button5s_act_3<-renderUI({
        req(input$file_5)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button5_act_3'),
                       label="Which variables do you want to change to factor/numeric",
                       size="xs")
        )
      })

      output$button6s_act_1<-renderUI({
        req(input$file_6)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button6_act_1'),
                       label="Check classes of the variables",
                       size="md")
        )
      })
      output$button6s_act_2<-renderUI({
        req(input$file_6)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button6_act_2'),
                       label="Remove variables",
                       size="md")
        )
      })
      output$button6s_act_3<-renderUI({
        req(input$file_6)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button6_act_3'),
                       label="Which variables do you want to change to factor/numeric",
                       size="md")
        )
      })

      output$button3s_act_showplot1<-renderUI({
        # req(input$file_1)

        column(
          width=4,
          #tags$br(),

          actionButton(ns('button3_act_showplot1'),
                       label="Show plot",
                       size="md"),
          div(style = "margin-top: -30px")
        )
      })
      #####################################################################
      #######In the judgefile I check for error give value to the r$data_frame_1
      output$judgefile5<-renderUI({
        req(input$file_5)
        ext<-tools::file_ext(input$file_5$name)

        if(ext=="rda"){
          LoadToEnvironment <- function(RData, env=new.env()) {
            load(RData, env)
            return(env)
          }
          env <- reactiveFileReader(1000,
                                    session,
                                    input$file_5$name,
                                    LoadToEnvironment)
          r$data_frame_5<-  env()[[names(env())[1]]]
          if(is.null(dim(r$data_frame_5))){
            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_5)[2]==1){
            #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="rds"){
          a<-reactive({readRDS(input$file_5$datapath)})
          r$data_frame_5<-a()
          if(is.null(dim(r$data_frame_5))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_5)[2]==1){

          #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="csv"){
          a<-reactive({read.csv2(paste0(input$file_5$datapath))})

          r$data_frame_5<-a()
          if(is.null(dim(r$data_frame_5))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_5)[2]==1){

          #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }
          #  print(dim(r$data_frame_1))
        }else if (ext=="xlsx"){

          a<-reactive({openxlsx::read.xlsx(input$file_5$datapath)})
          r$data_frame_5<-a()
          if(is.null(dim(r$data_frame_5))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_5)[2]==1){

            #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        }else{ #r$data_frame_5<-NULL
          stop("The data type should be xlsx, csv, rda or rds")
        }
      })





      output$judgefile6<-renderUI({
        req(input$file_6)
        ext<-tools::file_ext(input$file_6$name)

        if(ext=="rda"){
          LoadToEnvironment <- function(RData, env=new.env()) {
            load(RData, env)
            return(env)
          }
          env <- reactiveFileReader(1000,
                                    session,
                                    input$file_6$name,
                                    LoadToEnvironment)
          r$data_frame_6<-  env()[[names(env())[1]]]
          if(is.null(dim(r$data_frame_6))){
            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_6)[2]==1){
            #   stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="rds"){
          a<-reactive({readRDS(input$file_6$datapath)})
          r$data_frame_6<-a()
          if(is.null(dim(r$data_frame_6))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_6)[2]==1){

            #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="csv"){
          a<-reactive({read.csv2(paste0(input$file_6$datapath))})

          r$data_frame_6<-a()
          if(is.null(dim(r$data_frame_6))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_6)[2]==1){

            #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        }else if (ext=="xlsx"){
          library(readxl)
          a<-reactive({openxlsx::read.xlsx(input$file_6$datapath)})
          r$data_frame_6<-a()
          if(is.null(dim(r$data_frame_6))){

            stop("It is not a dataframe. Dimension is 0")
          }
          if(dim(r$data_frame_6)[2]==1){

            #   stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        }else{ #r$data_frame_6<-NULL
          stop("The data type should be xlsx, csv,rda or rds")
        }
      })


      ###################################################################### all the buttons
      ######################################################################################
      dataModal_51<-function(failed=F){modalDialog(

        selectInput(ns("data_frame_5_class"),
                    "Please select the variables that you want to check their class",
                    choices=colnames(r$data_frame_5),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_51"),"Cancel"),
          actionButton(ns("ok_51"), "OK")
        )
      )
      }
      dataModal_52<-function(failed=F){modalDialog(
        selectInput(ns("data_frame_5_remove"),
                    "Please select the variables that you want to remove",
                    choices=colnames(r$data_frame_5),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_52"),"Cancel"),
          actionButton(ns("ok_52"), "OK and remove these variables")
        )
      )
      }

      observeEvent(r$data_frame_5,
                   {r$data_frame_5_rem<-r$data_frame_5},
                   ignoreNULL = F
      )

      ## step 1 what happens when button is clicked, the datamodal will show
      observeEvent(input$button5_act_1, {
        req(input$file_5)
        req(r$data_frame_5)
        # r$data_frame_1_rem<-r$data_frame_1
        showModal(dataModal_51())
      })
      observeEvent(input$button5_act_2, {
        req(input$file_5)
        req(r$data_frame_5)
        #      r$data_frame_5_rem<-r$data_frame_5
        showModal(dataModal_52())
      })

      ### what happens when button is clicked okay
      ### whatelse happens when button is clicked okay, but the input is empty
      observeEvent(input$ok_51,{
        req(input$data_frame_5_class)
        if(!is.null(input$data_frame_5_class)){

          removeModal()

        }else{
          showModal(dataModal_51(failed=T))
        }
      })
      observeEvent(input$ok_52,{
        req(input$data_frame_5_remove)
        if(!is.null(input$data_frame_5_remove)){
          removeModal()

        }else{
          showModal(dataModal_52(failed=T))
        }
      })
      ### what happens when click cancel

      observeEvent(input$cancel_51,{
        removeModal()
        r$page3$class5=NULL

      })
      observeEvent(input$cancel_52,{
        removeModal()
        r$page3$remove5=NULL
        r$data_frame_5_rem<-r$data_frame_5
      })
      ## step 2
      #### When a new dataframe is put in the saved values are removed
      observeEvent(

        eventExpr={r$data_frame_5},
        handlerExpr={
          req(input$data_frame_5_class)
          r$page3$class5=NULL
        },
        ignoreNULL = F
      )
      observeEvent(
        eventExpr={r$data_frame_5},
        handlerExpr={
          req(input$data_frame_5_remove)
          r$page3$remove5=NULL        ### it becomes null
          #r$data_frame_5_rem=r$data_frame_5
        },
        ignoreNULL = F
      )


      ## step 5
      #### When the button click again the original saved value will change
      observeEvent(input$button5_act_1,{
        # req(input$data_frame_1_class)
        r$page3$class5<- NULL
      }
      )
      observeEvent(input$button5_act_2,{
        # req(input$data_frame_1_remove)
        r$data_frame_5_rem<-r$data_frame_5
        r$page3$remove5<- NULL
      }
      )

      ## step 4
      # When there is an input, what is generated and saved
      observeEvent(input$data_frame_5_class,{
        s<-c()
        for(i in 1:length(input$data_frame_5_class))
        {s<-c(s,class(r$data_frame_5[,input$data_frame_5_class[i]])[1])
        }
        r$page3$class5<-s
      },
      ignoreNULL = F
      )
      observeEvent(input$data_frame_5_remove,{
        b<-r$data_frame_5
        r$data_frame_5_rem<-b[,!colnames(b)%in%input$data_frame_5_remove,drop=F]
        r$page3$remove5<- input$data_frame_5_remove
      },
      ignoreNULL = F
      )


      ## step 5
      ####  What is printed out and what is generated after the first input in Modal
      output$datainfo_act51<-renderText({
        req(r$page3$class5)
        req(input$data_frame_5_class)
        #for(i in ncol(r$data_frame_1)){
        if(is.null(r$page3$class5)){
          "No column selected"
        }else{
          paste("The class of the variables you checked are: ", paste(input$data_frame_5_class,
                                                                      rep("-",length(r$page3$class5)),
                                                                      r$page3$class5,
                                                                      collapse="; "))
        }

      })

      output$datainfo_act52<-renderText({
        req(r$page3$remove5)
        req(r$data_frame_5)
        req(input$data_frame_5_remove)
        # req(input$data_frame_1_remove)
        #for(i in ncol(r$data_frame_1)){
        if(is.null(r$page3$remove5)){
          "No column selected"
        }else{

          dimm<-reactive({dim(r$data_frame_5)[1]})
          len<-reactive({dim(r$data_frame_5)[2]-length(input$data_frame_5_remove)})
          paste("The following variables are removed:",paste(r$page3$remove5,collapse=","),"<br>",
                "<b>After remove the selected variables, the dataframe has","rows and",dimm(),
                len(),"columns, which will be used for the following steps","<br>")

        }

      })
      ##################################################################
      ############### The third button on the left column

      dataModal_53<-function(failed=F){modalDialog(
        selectInput(ns("data_frame_5_pro51"),
                    "Please select the variables that you want to change to factor",
                    choices=colnames(r$data_frame_5_rem),
                    multiple=T),
        selectInput(ns("data_frame_5_pro52"),
                    "Please select the variables that you want to change to numeric",
                    choices=colnames(r$data_frame_5_rem),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_53"),"Cancel"),
          actionButton(ns("ok_53"), "OK and change these variables")
        )
      )
      }
      observeEvent(
        input$data_frame_5_pro51,{

          updateSelectInput(inputId="data_frame_5_pro52",
                            selected=input$data_frame_5_pro52,
                            choices=colnames(r$data_frame_5_rem)[!colnames(r$data_frame_5_rem)%in%input$data_frame_5_pro51])

        },
        ignoreNULL = F
      )
      observeEvent(
        input$data_frame_5_pro52,{
          updateSelectInput(inputId="data_frame_5_pro51",
                            selected=input$data_frame_5_pro51,
                            choices=colnames(r$data_frame_5_rem)[!colnames(r$data_frame_5_rem)%in%input$data_frame_5_pro52])
        },
        ignoreNULL = F
      )

      observeEvent(r$data_frame_5_rem,
                   {r$data_frame_5_pros51<-r$data_frame_5_rem
                   },
                   ignoreNULL = F
      )


      observeEvent(input$ok_53,{
        #req(input$data_frame_2_pro51)
        if(!is.null(input$data_frame_5_pro51)|!is.null(input$data_frame_5_pro52)){
          removeModal()
        }else{
          showModal(dataModal_53(failed=T))
        }
      })

      ### build a variable r$page3$pro51 to pass the judgement
      observeEvent(input$cancel_53,{
        removeModal()
        r$page3$pro51<-NULL
        r$page3$pro52<-NULL
        r$data_frame_5_pros51<-r$data_frame_5_rem
        #r$data_frame_2_pros52<-r$data_frame_2_rem
      })

      ## step 1 what happens when button is clicked, the datamodal will show
      observeEvent(input$button5_act_3, {
        req(input$file_5)
        req(r$data_frame_5)
        # r$data_frame_2_pros51<-r$data_frame_2_rem
        showModal(dataModal_53())
      })

      ####  step 5 when ever the button is clicked the r$data_frame_2_pros51<-r$data_frame_2_rem
      observeEvent(input$button5_act_3,
                   {r$data_frame_5_pros51<-r$data_frame_5_rem
                   # r$data_frame_4_pros52<-r$data_frame_4_rem
                   r$page3$pro51<-NULL
                   r$page3$pro52<-NULL
                   }
      )
      observeEvent(input$button5_act_2,
                   {r$data_frame_5_pros51<-r$data_frame_5_rem
                   #r$data_frame_2_pros52<-r$data_frame_2_rem
                   r$page3$pro51<-NULL
                   r$page3$pro52<-NULL
                   }
      )
      ## step 4
      # When there is an input, what is generated and saved

      # tolist<-reactive({list(input$data_frame_2_pro51,input$data_frame_2_pro52)})
      observeEvent({input$data_frame_5_pro51

      },
      {
        req(r$data_frame_5_rem)
        #req(input$data_frame_2_pro51)
        if(!is.null(input$data_frame_5_pro51)&!is.null(input$data_frame_5_pro52)){
          r$page3$pro51<-input$data_frame_5_pro51
          r$page3$pro52<-input$data_frame_5_pro52
          g<-reactive({force_parttype(r$data_frame_5_rem,"factor",input$data_frame_5_pro51)})
          r$data_frame_5_pros5_tran<-g()
          f<-reactive({force_parttype(r$data_frame_5_pros5_tran,"numeric",input$data_frame_5_pro52)})
          r$data_frame_5_pros51<-f()

        }else if(!is.null(input$data_frame_5_pro51)&is.null(input$data_frame_5_pro52)){
          r$page3$pro51<-input$data_frame_5_pro51
          g<-reactive({force_parttype(r$data_frame_5_rem,"factor",input$data_frame_5_pro51)})
          r$data_frame_5_pros51<-g()

        }else if(is.null(input$data_frame_5_pro51)&!is.null(input$data_frame_5_pro52)){
          r$page3$pro52<-input$data_frame_5_pro52
          g<-reactive({force_parttype(r$data_frame_5_rem,"numeric",input$data_frame_5_pro52)})
          r$data_frame_5_pros51<-g()

        }
      },
      ignoreNULL = F
      )
      observeEvent({input$data_frame_5_pro52

      },
      {
        req(r$data_frame_5_rem)
        #req(input$data_frame_2_pro51)
        if(!is.null(input$data_frame_5_pro51)&!is.null(input$data_frame_5_pro52)){
          r$page3$pro51<-input$data_frame_5_pro51
          r$page3$pro52<-input$data_frame_5_pro52
          g<-reactive({force_parttype(r$data_frame_5_rem,"factor",input$data_frame_5_pro51)})
          r$data_frame_5_pros5_tran<-g()
          f<-reactive({force_parttype(r$data_frame_5_pros5_tran,"numeric",input$data_frame_5_pro52)})
          r$data_frame_5_pros51<-f()

        }else if(!is.null(input$data_frame_5_pro51)&is.null(input$data_frame_5_pro52)){
          r$page3$pro51<-input$data_frame_5_pro51
          g<-reactive({force_parttype(r$data_frame_5_rem,"factor",input$data_frame_5_pro51)})
          r$data_frame_5_pros51<-g()

        }else if(is.null(input$data_frame_5_pro51)&!is.null(input$data_frame_5_pro52)){
          r$page3$pro52<-input$data_frame_5_pro52
          g<-reactive({force_parttype(r$data_frame_5_rem,"numeric",input$data_frame_5_pro52)})
          r$data_frame_5_pros51<-g()

        }
      },
      ignoreNULL = F
      )


      ## step 5
      ####  What is printed out and what is generated after the first input in Modal
      output$datainfo_act53<-renderText({
        req(input$data_frame_5_pro51)
        #for(i in ncol(r$data_frame_1)){
        if(!is.null(r$page3$pro51)){

          paste("You have made",length(input$data_frame_5_pro51),"variables into factor. They are ",
                paste(r$page3$pro51,collapse=", "))
        }

      })
      output$datainfo_act54<-renderText({
        req(input$data_frame_5_pro52)
        if(!is.null(r$page3$pro52)){

          paste("You have made",length(input$data_frame_5_pro52),"variables into numeric. They are ",
                paste(r$page3$pro52,collapse=", "))
        }

      })
      ##########################################################
      ####The right column
      ###############################################################################
      ## the modal page
      dataModal_61<-function(failed=F){modalDialog(

        selectInput(ns("data_frame_6_class"),
                    "Please select the variables that you want to check their class",
                    choices=colnames(r$data_frame_6),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_61"),"Cancel"),
          actionButton(ns("ok_61"), "OK")
        )
      )
      }
      dataModal_62<-function(failed=F){modalDialog(
        selectInput(ns("data_frame_6_remove"),
                    "Please select the variables that you want to remove",
                    choices=colnames(r$data_frame_6),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_62"),"Cancel"),
          actionButton(ns("ok_62"), "OK and remove these variables")
        )
      )
      }

      observeEvent(r$data_frame_6,
                   {r$data_frame_6_rem<-r$data_frame_6},
                   ignoreNULL = F
      )

      ## step 1 what happens when button is clicked, the datamodal will show
      observeEvent(input$button6_act_1, {
        req(input$file_6)
        req(r$data_frame_6)
        # r$data_frame_1_rem<-r$data_frame_1
        showModal(dataModal_61())
      })
      observeEvent(input$button6_act_2, {
        req(input$file_6)
        req(r$data_frame_6)
        #      r$data_frame_6_rem<-r$data_frame_6
        showModal(dataModal_62())
      })

      ### what happens when button is clicked okay
      ### whatelse happens when button is clicked okay, but the input is empty
      observeEvent(input$ok_61,{
        req(input$data_frame_6_class)
        if(!is.null(input$data_frame_6_class)){

          removeModal()

        }else{
          showModal(dataModal_61(failed=T))
        }
      })
      observeEvent(input$ok_62,{
        req(input$data_frame_6_remove)
        if(!is.null(input$data_frame_6_remove)){
          removeModal()

        }else{
          showModal(dataModal_62(failed=T))
        }
      })
      ### what happens when click cancel

      observeEvent(input$cancel_61,{
        removeModal()
        r$page3$class6=NULL

      })
      observeEvent(input$cancel_62,{
        removeModal()
        r$page3$remove6=NULL
        r$data_frame_6_rem<-r$data_frame_6
      })
      ## step 2
      #### When a new dataframe is put in the saved values are removed
      observeEvent(

        eventExpr={r$data_frame_6},
        handlerExpr={
          req(input$data_frame_6_class)
          r$page3$class6=NULL
        }
      )
      observeEvent(
        eventExpr={r$data_frame_6},
        handlerExpr={
          req(input$data_frame_6_remove)
          r$page3$remove6=NULL        ### it becomes null
          #r$data_frame_6_rem=r$data_frame_6
        },
        ignoreNULL = F
      )


      ## step 3
      #### When the button click again the original saved value will change
      observeEvent(input$button6_act_1,{
        #req(input$data_frame_2_class)
        r$page3$class6<- NULL
      }
      )
      observeEvent(input$button6_act_2,{
        #req(input$data_frame_6_remove)
        r$data_frame_6_rem<-r$data_frame_6
        r$page3$remove6<- NULL
      }
      )

      ## step 6
      # When there is an input, what is generated and saved
      observeEvent(input$data_frame_6_class,{
        s<-c()
        for(i in 1:length(input$data_frame_6_class))
        {s<-c(s,class(r$data_frame_6[,input$data_frame_6_class[i]])[1])
        }
        r$page3$class6<-s
      },
      ignoreNULL = F
      )
      observeEvent(input$data_frame_6_remove,{
        #if(!is.null(input$data_frame_2_remove)) {
        b<-r$data_frame_6
        r$data_frame_6_rem<-b[,!colnames(b)%in%input$data_frame_6_remove,drop=F]
        r$page3$remove6<- input$data_frame_6_remove
        #}
      },
      ignoreNULL = F
      )


      ## step 5
      ####  What is printed out and what is generated after the first input in Modal
      output$datainfo_act61<-renderText({
        req(r$page3$class6)
        req(input$data_frame_6_class)
        #for(i in ncol(r$data_frame_1)){
        if(is.null(r$page3$class6)){
          "No column selected"
        }else{
          paste("The class of the variables you checked are: ", paste(input$data_frame_6_class,
                                                                      rep("-",length(r$page3$class6)),
                                                                      r$page3$class6,
                                                                      collapse="; "))
        }

      })

      output$datainfo_act62<-renderText({
        req(r$page3$remove6)
        req(r$data_frame_6)
        req(input$data_frame_6_remove)
        # req(input$data_frame_6_remove)
        #for(i in ncol(r$data_frame_6)){
        if(is.null(r$page3$remove6)){
          "No column selected"
        }else{

          dimm<-reactive({dim(r$data_frame_6)[1]})
          len<-reactive({dim(r$data_frame_6)[2]-length(input$data_frame_6_remove)})
          paste("The following variables are removed:",paste(r$page3$remove6,collapse=","),"<br>",
                "<b>After remove the selected variables, the dataframe has","rows and",dimm(),
                len(),"columns, which will be used for the following steps","<br>")

        }

      })

      ##################################################################
      ##################################################################
      ############### The third button on the right column

      dataModal_63<-function(failed=F){modalDialog(
        selectInput(ns("data_frame_6_pros61"),
                    "Please select the variables that you want to change to factor",
                    choices=colnames(r$data_frame_6_rem),
                    multiple=T),
        selectInput(ns("data_frame_6_pros62"),
                    "Please select the variables that you want to change to numeric",
                    choices=colnames(r$data_frame_6_rem),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_63"),"Cancel"),
          actionButton(ns("ok_63"), "OK and change these variables")
        )
      )
      }
      observeEvent(
        input$data_frame_6_pros61,{

          updateSelectInput(inputId="data_frame_6_pros62",
                            selected=input$data_frame_6_pros62,
                            choices=colnames(r$data_frame_6_rem)[!colnames(r$data_frame_6_rem)%in%input$data_frame_6_pros61])

        },
        ignoreNULL = F
      )
      observeEvent(
        input$data_frame_6_pros62,{
          updateSelectInput(inputId="data_frame_6_pros61",
                            selected=input$data_frame_6_pros61,
                            choices=colnames(r$data_frame_6_rem)[!colnames(r$data_frame_6_rem)%in%input$data_frame_6_pros62])
        },
        ignoreNULL = F
      )

      observeEvent(r$data_frame_6_rem,
                   {r$data_frame_6_pros61<-r$data_frame_6_rem
                   },
                   ignoreNULL = F
      )


      observeEvent(input$ok_63,{
        #req(input$data_frame_2_pros61)
        if(!is.null(input$data_frame_6_pros61)|!is.null(input$data_frame_6_pros62)){
          removeModal()
        }else{
          showModal(dataModal_63(failed=T))
        }
      })

      ### build a variable r$page3$pros61 to pass the judgement
      observeEvent(input$cancel_63,{
        removeModal()
        r$page3$pros61<-NULL
        r$page3$pros62<-NULL
        r$data_frame_6_pros61<-r$data_frame_6_rem
        #r$data_frame_2_pros62<-r$data_frame_2_rem
      })

      ## step 1 what happens when button is clicked, the datamodal will show
      observeEvent(input$button6_act_3, {
        req(input$file_6)
        req(r$data_frame_6)
        # r$data_frame_2_pros61<-r$data_frame_2_rem
        showModal(dataModal_63())
      })

      ####  step 3 when ever the button is clicked the r$data_frame_2_pros61<-r$data_frame_2_rem
      observeEvent(input$button6_act_3,
                   {r$data_frame_6_pros61<-r$data_frame_6_rem
                   # r$data_frame_6_pros62<-r$data_frame_6_rem
                   r$page3$pros61<-NULL
                   r$page3$pros62<-NULL
                   }
      )
      observeEvent(input$button6_act_2,
                   {r$data_frame_6_pros61<-r$data_frame_6_rem
                   #r$data_frame_2_pros62<-r$data_frame_2_rem
                   r$page3$pros61<-NULL
                   r$page3$pros62<-NULL
                   }
      )
      ## step 6
      # When there is an input, what is generated and saved

      # tolist<-reactive({list(input$data_frame_2_pros61,input$data_frame_2_pros62)})
      observeEvent({input$data_frame_6_pros61

      },
      {
        req(r$data_frame_6_rem)
        #req(input$data_frame_2_pros61)
        if(!is.null(input$data_frame_6_pros61)&!is.null(input$data_frame_6_pros62)){
          r$page3$pros61<-input$data_frame_6_pros61
          r$page3$pros62<-input$data_frame_6_pros62
          g<-reactive({force_parttype(r$data_frame_6_rem,"factor",input$data_frame_6_pros61)})
          r$data_frame_6_pros6_tran<-g()
          f<-reactive({force_parttype(r$data_frame_6_pros6_tran,"numeric",input$data_frame_6_pros62)})
          r$data_frame_6_pros61<-f()

        }else if(!is.null(input$data_frame_6_pros61)&is.null(input$data_frame_6_pros62)){
          r$page3$pros61<-input$data_frame_6_pros61
          g<-reactive({force_parttype(r$data_frame_6_rem,"factor",input$data_frame_6_pros61)})
          r$data_frame_6_pros61<-g()

        }else if(is.null(input$data_frame_6_pros61)&!is.null(input$data_frame_6_pros62)){
          r$page3$pros62<-input$data_frame_6_pros62
          g<-reactive({force_parttype(r$data_frame_6_rem,"numeric",input$data_frame_6_pros62)})
          r$data_frame_6_pros61<-g()

        }
      },
      ignoreNULL = F
      )
      observeEvent({input$data_frame_6_pros62

      },
      {
        req(r$data_frame_6_rem)
        #req(input$data_frame_2_pros61)
        if(!is.null(input$data_frame_6_pros61)&!is.null(input$data_frame_6_pros62)){
          r$page3$pros61<-input$data_frame_6_pros61
          r$page3$pros62<-input$data_frame_6_pros62
          g<-reactive({force_parttype(r$data_frame_6_rem,"factor",input$data_frame_6_pros61)})
          r$data_frame_6_pros6_tran<-g()
          f<-reactive({force_parttype(r$data_frame_6_pros6_tran,"numeric",input$data_frame_6_pros62)})
          r$data_frame_6_pros61<-f()

        }else if(!is.null(input$data_frame_6_pros61)&is.null(input$data_frame_6_pros62)){
          r$page3$pros61<-input$data_frame_6_pros61
          g<-reactive({force_parttype(r$data_frame_6_rem,"factor",input$data_frame_6_pros61)})
          r$data_frame_6_pros61<-g()

        }else if(is.null(input$data_frame_6_pros61)&!is.null(input$data_frame_6_pros62)){
          r$page3$pros62<-input$data_frame_6_pros62
          g<-reactive({force_parttype(r$data_frame_6_rem,"numeric",input$data_frame_6_pros62)})
          r$data_frame_6_pros61<-g()

        }
      },
      ignoreNULL = F
      )


      ## step 5
      ####  What is printed out and what is generated after the first input in Modal
      output$datainfo_act63<-renderText({
        req(input$data_frame_6_pros61)
        #for(i in ncol(r$data_frame_1)){
        if(!is.null(r$page3$pros61)){

          paste("You have made",length(input$data_frame_6_pros61),"variables into factor. They are ",
                paste(r$page3$pros61,collapse=", "))
        }

      })
      output$datainfo_act64<-renderText({
        req(input$data_frame_6_pros62)
        if(!is.null(r$page3$pros62)){

          paste("You have made",length(input$data_frame_6_pros62),"variables into numeric. They are ",
                paste(r$page3$pros62,collapse=", "))
        }

      })
      ##################################################################################
      #####################################################################
      #######print out the text for dimensionality check

      output$atext5<-renderUI({
        req(r$data_frame_5)

        #    if(!is.null(r$data_frame_3)){
        tags$b(paste("The original dataframe for the PCA plot(s) has",dim(r$data_frame_5)[1],"rows and",
                     dim(r$data_frame_5)[2],"columns."))

      })

      output$atext6<-renderUI({
        req(r$data_frame_6)

        #    if(!is.null(r$data_frame_2)){
        tags$b(paste("The dataframe for the PCA plot(s) has",dim(r$data_frame_6)[1],"rows and",
                     dim(r$data_frame_6)[2],"columns."))

      })

############################################################################################################
###################################################################################
 ### condition between input


      ### logic for component limit
      observeEvent(r$result1$pca_object$scores,{

        req(r$result1$pca_object$scores)
        #if(!is.null(r$data_frame_3)){
       # dim2<-dim(r$result1$pca_object$scores)[2]
        updateNumericInput(inputId = "component_limits",
                           value=min(2,r$page1_pc_num),
                           min=2,

                           max=r$page1_pc_num)
        #}

      },
      ignoreNULL = F,
      ignoreInit=T
      )

      observeEvent(r$data_frame_2_pros1,{
        req(r$data_frame_2_pros1)
        if(!is.null(r$data_frame_2_pros1)){
          updateSelectInput(inputId ="pair",

                            choices=c('',
                                         paste(colnames(r$data_frame_2_pros1)))
          )
        }
      },ignoreNULL = F
      )

      observeEvent(input$pair,
                   {req(r$data_frame_2_pros1)

                     if(input$pair!=''){
                       r$page3$pair=as.factor(r$data_frame_2_pros1[,input$pair])
                     }else {r$page3$pair=NULL}

                   },
                   ignoreNULL = F)


      #r$page3_riskORs=NULL
      observeEvent(input$riskOR,
                   {
                     if(input$riskOR=="Yes"){
                       r$page3_riskORs=T
                     }else if(input$riskOR=="No"){
                       r$page3_riskORs=F}

                   },
                   ignoreNULL = F)
      #r$page3_multinomials=F

      observeEvent(input$multinomial,
                   {
                     if(input$multinomial=="Yes"){
                       r$page3_multinomial=T
                       print(r$page3_multinomials)
                     }else if (input$multinomial=="No"){
                       r$page3_multinomials=F
                     }
                     print(r$page3_multinomials)
                   },
                   ignoreNULL = F)


#####################################################################################################
######################################################################################################################
## Error zone


      ##############################################
      ### Option for do confounder adjustment or not
      output$getconf_adj<-renderUI({
        req(r$data_frame_6_pros61)
        selectInput(inputId =ns("conf_adj"),
                    label="Do confounder adjustment or not",
                    choices=c("No","Yes"))

      })
      observeEvent(input$conf_adj,
                   {req(input$conf_adj)
                     if(input$conf_adj=="Yes"){
                       r$page3$conf_adj=T
                     }else{
                       r$page3$conf_adj=F
                     }

                   },ignoreNULL = F,
                   ignoreInit = F
      )


########################################################################################################################################
      ############################################################################################
      ########################################################################################################################################
      ##### core code
      scores_comp_list<-reactive({list(r$result1$pca_object$scores,
                                       input$component_limits,
                                       r$result1$pca_object$loadings)})
      observeEvent( scores_comp_list(),
                    {#freezeReactiveValue(input,"component_limits")
                      if(!is.null(r$result1$pca_object$scores)){
                        r$page3$makeTPOs<-makeTPO(r$result1$pca_object$scores,
                                                  r$result1$pca_object$loadings,
                                                  compLimit=input$component_limits,  #input$component_limit,   #### here is actually the input from page 2
                                                  hide_question=T)
                      }
                    })
      ## pass to global
      observeEvent(input$component_limits,{
        req(input$component_limits)
        r$page3_component_limits<-input$component_limits

      })
      TPO_list2<-reactive({list(r$page3$makeTPOs,
                                r$data_frame_5_pros51,
                                r$page3_multinomials,
                                #r$page3$multinomial,
                                r$page3$conf_adj,
                                input$CI,
                                #  input$riskWhisker_percentage#,
                                input$pair,
                                r$data_frame_6_pros61
      )})

      observeEvent(TPO_list2(),
                   {req(r$page3$makeTPOs)
                     req(r$data_frame_5_pros51)
                     req(r$page3_multinomials)

                     #req( r$page3_riskORs)
                     #  req(input$CI)
                     # req(r$page3_multinomials)
                     #freezeReactiveValue(input,"CI")

                     if(!is.null(r$data_frame_5_pros51)
                        &!is.null(r$page3_multinomials)

                     ){


                       if(!is.null(r$page3$conf_adj)&!is.null(r$data_frame_6_pros61)){
                         req(r$data_frame_6_pros61)
                         req(r$page3$conf_adj)
                         r$page3$makeRisk<-coefficient_get(TPObject=r$page3$makeTPOs,
                                                           outcomee=r$data_frame_5_pros51, ### needs to be a dataframe with numeric and factor values
                                                           CI=input$CI,
                                                             partial=r$page3$conf_adj,
                                                           multinomial=r$page3_multinomials,
                                                           pair=r$page3$pair,   ### orginally it is null that is why it is okay
                                                           confounder= r$data_frame_6_pros61  ### orginally it is null that is why it is okay
                         )
                 }else{r$page3$makeRisk<-coefficient_get(TPObject=r$page3$makeTPOs,
                                                        outcomee=r$data_frame_5_pros51, ### needs to be a dataframe with numeric and factor values
                                                         CI=input$CI,
                                                         # partial=r$page3$conf_adj,
                                                         multinomial=r$page3_multinomials,  ### orginally it is null that is why it is okay
                                                          pair=r$page3$pair,   ### orginally it is null that is why it is okay
                                                          #confounder= r$data_frame_6_pros61
                                       )


                       }

                     }

                   },
                   ignoreNULL = F,
                   ignoreInit = F
      )




      addRisk_list<-reactive({
        list(r$page3$makeTPOs,
             r$page3$makeRisk)
      })


      observeEvent(addRisk_list(),{
        req(r$page3$makeTPOs)
        req(r$page3$makeRisk)
        if(!is.null(r$page3$makeTPOs)&!is.null(r$page3$makeRisk)){

          r$page3$addRisk<-addRisk(r$page3$makeTPOs,
                                   r$page3$makeRisk)

        }
      },

      ignoreNULL=F)

      #####################
      ## pass to global
      observeEvent(r$page3$addRisk,{
        req(r$page3$addRisk)
        r$page3_addrisk<-r$page3$addRisk
      },
      ignoreNULL=F)

      plotrisk_list<-reactive({
        list(r$page3$addRisk,
             input$riskWhisker_percentage,
             r$page3_riskORs)
      })


      ##############################When riskOR is clicked there is no reaction
      observeEvent(plotrisk_list(),{
        req(r$page3$addRisk)
        req(r$page3_riskORs)
        req(input$riskWhisker_percentage)
        if(!is.null(r$page3$addRisk)){
          r$page3$plotss<-TriplotGUI(r$page3$addRisk,
                                     first_PC=1, ## The first PC to map
                                     second_PC=2, ## The first PC to map
                                     plotLoads=TRUE, ##Whether to plot loadings (TRUE; default) or suppress them (FALSE)
                                     plotScores=FALSE, ##Whether to plot scores (TRUE) or suppress them (FALSE; default)
                                     plotCorr=F,##Whether to plot correlations (TRUE; default) or suppress them (FALSE)
                                     plotRisk=T,
                                     ##For loadings
                                     loadLabels=TRUE, ###Whether to plot variable loading labels (TRUE; default) or not (FALSE)
                                     loadArrowLength=0.02,###Length of arrow tip , set it as 0 if you want to remove it
                                     loadCut=0, ###lower limit Loadings below the cut are plotted in light grey and without label
                                     #loadLim, ##higher limit,Plot range for loadings
                                     ##For correlations
                                     #colCorr,##Color vector for correlations
                                     pchCorr=16, ##Plotting character for correlations
                                     whichCorr=NULL, ##Which correlations to plot (vector of numbers)
                                     # corLim,##Plot range for correlation
                                     ##For risks
                                     # colRisk, ##Color vector for risk estimates
                                     pchRisk=15, ##Plotting character for risk estimates
                                     whichRisk=NULL, ##Which risk estimates to plot (vector of numbers)
                                     # riskLim, ##Plot range for risks
                                     riskWhisker_percentage=input$riskWhisker_percentage,## whisker length is how many percentage of confidence interval (This is only for the visualization purpose)
                                     riskOR=r$page3_riskORs, ##Specify whether to antilog risk layer scale (useful for log:ed risk estimates) ## Scores
                                     size=3
                                     # scoreLabels=FALSE ##Whether to plot observation score labels (TRUE) or not (FALSE; default)
          )
        }
      },ignoreNULL=F)
      output$showriskplot<-renderPlot({
        req(input$button3_act_showplot1)
        r$page3$plotss$Risk
        # r$page2$reset_check <- 1
        #  r$page2$plots$Correlation<-NULL
      })

     ##########################################################################################################################
    ##################################################################################################################################



    ################################################################
      ## test stuff
      ###test stuff
      output$hed<-renderTable({
        req(r$data_frame_5)
        req(r$page3$addRisk$riskMatrix)
        if(!is.null(r$page3$addRisk$riskMatrix)){
          head(r$page3$addRisk$riskMatrix,6)
        }
      })
      output$hed2<-renderTable({
        req(r$data_frame_5)
        req(r$page3$makeRisk)
        if(!is.null(r$page3$makeRisk)){
          length(r$page3$makeRisk)
        }
      })

      output$hed3<-renderTable({
        req(r$data_frame_5)
        req(r$page3$makeRisk$factorrisk_list)
        if(!is.null(r$page3$makeRisk$factorrisk_list)){
          length(r$page3$makeRisk$factorrisk_list)
        }
      })
      output$hed4<-renderTable({
        req(r$data_frame_5)
        req(r$data_frame_5_pros51)
        if(!is.null(r$data_frame_5_pros51)){
          dim(r$data_frame_5_pros51)
        }
      })
      output$hed5<-renderPrint({

        #req(r$page3_multinomials)
        r$page3_multinomials
      })

    }
  )



}
