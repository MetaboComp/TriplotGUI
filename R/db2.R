#' @export
db2UI<-function(id){
  ns<-NS(id)

  ### a list of things that is put in the tab
  tagList(
    fluidRow(
      width=12,
      tags$h2("Plot correlation plots")


    ),
    fluidRow(
      width=12,

      box(
        width=6,
        title="The key input files ",
     fluidRow(

       column(12,
              tags$b("Dataframe used for correlation analysis (Upload your data in .rds, .rda, .csv, .xlsx format.)"),
              tags$br(),
              tags$b("The dataframe that its variables will be used to calculate correlations with scores (optional)")
              ),
      column(8,
             #style="background-color:#b1f6c6",
             #helpText("Key Input"),


             fileInput(inputId=ns("file_3"),
                       label=NULL,
                       buttonLabel = "Upload...",
                       multiple = F,
                       accept=c(".rds",".rda",".csv",".xlsx"))
             ),
      column(4,
             uiOutput(ns("button2s_act_showplot1"))
             )
      ),
      fluidRow(
        column(12,

             div(style = "margin-top: -30px"),
             uiOutput(outputId=ns("judgefile3")),
             br(),
             uiOutput(outputId = ns("atext3")),
             #div(style = "margin-top: -10px") ,

             uiOutput(ns("button3s_act_1")),
             # br(),
             htmlOutput(ns("datainfo_act31")),

             uiOutput(ns("button3s_act_2")),
             #  br(),
             htmlOutput(ns("datainfo_act32")),

             div(style = "margin-bottom: -10px"),
             hr(),
             div(style = "margin-top: -30px"),
             uiOutput(ns('button3s_act_3')),
             # br(),
             htmlOutput(ns("datainfo_act33")),
             htmlOutput(ns("datainfo_act34")),
             #uiOutput(ns("plotplotcorr")),

             div(style = "margin-top: 30px"),
             uiOutput(ns("showcorrplot")),
             tags$br(),
             uiOutput(ns("downloadbutton2"))
           # plotOutput(outputId = ns("plot_result")),
            #uiOutput(outputId=ns("files")),
            #tableOutput(outputId = ns("hed")),
            # tableOutput(outputId = ns("hed2")),
            #tableOutput(outputId = ns("hed3")),
            #tableOutput(outputId = ns("hed4"))
      ))
      ),
     tags$br(),  #,
     #fluidRow(
    #   column(12,
    #          uiOutput(ns('createButtonUI')))
    # )

     box(width=6,
         title="Other settings",
         fluidRow(
           column(12,
            # style="background-color:#ffa153",
            # helpText("Other settings"),
            fileInput(inputId=ns("file_4"),
                      label="The dataframe that its variables will be used as confounders (optional)",
                      buttonLabel = "Upload...",
                      multiple = F,
                      accept=c(".rds",".rda",".csv",".xlsx")),
            div(style = "margin-top: -30px"),

            uiOutput(outputId=ns("judgefile4")),
            br(),
            uiOutput(outputId = ns("atext4")),

            uiOutput(ns('button4s_act_1')),
            # br(),
            htmlOutput(ns("datainfo_act41")),
            #tags$br(),   ## this workd
            uiOutput(ns('button4s_act_2')),
            # br(),
            htmlOutput(ns("datainfo_act42")),  ### when this is printed there is hr
            div(style = "margin-bottom: -10px"),
            hr(),
            div(style = "margin-top: -30px"),
            uiOutput(ns('button4s_act_3')),
            # br(),
            htmlOutput(ns("datainfo_act43")),
            htmlOutput(ns("datainfo_act44")),

            div(style = "margin-bottom: -10px"),
            hr(),
            div(style = "margin-top: -30px"),

            ### this is constrained by data input
            numericInput(inputId = ns("component_limit"),
                         label="How many components do you want to use",
                         value=2,
                         min=2,
                         max=2),
            ## always original
             selectInput(inputId =ns("method"),
                         label="What correlation method do you use?",
                         selected ="Spearman",
                         choices=c("pearson","spearman","kendall")),
            ##always original
             selectInput(inputId =ns("use"),
                         label="What method do you use for computing covariances in the presence of missing values?",
                         choices=c("everything", "all.obs", "complete.obs", "pairwise.complete.obs",
                                   "na.or.complete","pairwise")),
            ##always _r
             selectInput(inputId =ns("allowcategorical"),
                         label=" Categorical variables are forced to be numeric or be one-hot encoded",
                         choices=c("Forced to be numeric","One_hot_encoded")),

            ### _r onlyshow up whenconfounder input is there
            uiOutput(ns("getpartial"))

      )
      ))

      #corrData, ### needs to be a dataframe with numeric and factor values
      #use='pairwise',
      #method='spearman',
      #allowcategorical=F,  ## when categorical variables are not allowed, one hot encoding
      #partial=F,
      #confounder=NULL

    ))
}


#' @export
db2Server<-function(id,r){
  moduleServer(
    id,
    function(input,output,session){
      ns<-session$ns
      #output$createButtonUI<-renderUI(
      #  {req(r$'1'$dbPath)   ### The name of the tabs
      #    req(input$data_frame)
##################################################################################
      ############################################################
      ## download button

      output$downloadbutton2<-renderUI({
        req(r$page2$plots$Correlation)
        #req(r$data_frame_1)
        downloadButton(outputId=ns("download2"),
                       label="Download figure")

      })
      ##########################################################################
      ################# The action buttons, only show up when the file is there

      output$button3s_act_1<-renderUI({
        req(input$file_3)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button3_act_1'),
                       label="Check classes of the variables",
                       size="md")
        )
      })
      output$button3s_act_2<-renderUI({
        req(input$file_3)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button3_act_2'),
                       label="Remove variables",
                       size="xs")
        )
      })
      output$button3s_act_3<-renderUI({
        req(input$file_3)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button3_act_3'),
                       label="Which variables do you want to change to factor/numeric",
                       size="xs")
        )
      })

      output$button4s_act_1<-renderUI({
        req(input$file_4)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button4_act_1'),
                       label="Check classes of the variables",
                       size="md")
        )
      })
      output$button4s_act_2<-renderUI({
        req(input$file_4)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button4_act_2'),
                       label="Remove variables",
                       size="md")
        )
      })
      output$button4s_act_3<-renderUI({
        req(input$file_4)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button4_act_3'),
                       label="Which variables do you want to change to factor/numeric",
                       size="md")
        )
      })

      output$button2s_act_showplot1<-renderUI({
        # req(input$file_1)

        column(
          width=4,
          #tags$br(),

          actionButton(ns('button2_act_showplot1'),
                       label="Show&refresh plot",
                       size="md"),
          div(style = "margin-top: -30px")
        )
      })






      #####################################################################
      #######In the judgefile I check for error give value to the r$data_frame_3
      output$judgefile3<-renderUI({
        req(input$file_3)
        ext<-tools::file_ext(input$file_3$name)

        if(ext=="rda"){
          LoadToEnvironment <- function(RData, env=new.env()) {
            load(RData, env)
            return(env)
          }
          env <- reactiveFileReader(1000,
                                    session,
                                    input$file_3$name,
                                    LoadToEnvironment)
          r$data_frame_3<-  env()[[names(env())[1]]]
          if(is.null(dim(r$data_frame_3))){
            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_3)[2]==1){
            #    stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="rds"){
          a<-reactive({readRDS(input$file_3$datapath)})
          r$data_frame_3<-a()
          if(is.null(dim(r$data_frame_3))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_3)[2]==1){

            #   stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="csv"){
          a<-reactive({read.csv2(paste0(input$file_3$datapath))})

          r$data_frame_3<-a()
          if(is.null(dim(r$data_frame_3))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_3)[2]==1){

            #    stop("Column number is 1. The dataframe should have at least 2 variables")
          }
          #  #  print(dim(r$data_frame_3))
        }else if (ext=="xlsx"){
          library(readxl)
          a<-reactive({openxlsx::read.xlsx(input$file_3$datapath)})
          r$data_frame_3<-a()
          #r$data_frame_3<force_type(r$data_frame_3,"numeric")
          #r$data_frame_3<-as.data.frame( r$data_frame_3)
          if(is.null(dim(r$data_frame_3))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_3)[2]==1){

            #   stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        }else{ #r$data_frame_3<-NULL
          stop("The data type should be xlsx, csv, rda or rds")
        }
      })





      output$judgefile4<-renderUI({
        req(input$file_4)
        ext<-tools::file_ext(input$file_4$name)

        if(ext=="rda"){
          LoadToEnvironment <- function(RData, env=new.env()) {
            load(RData, env)
            return(env)
          }
          env <- reactiveFileReader(1000,
                                    session,
                                    input$file_4$name,
                                    LoadToEnvironment)
          r$data_frame_4<-  env()[[names(env())[1]]]
          if(is.null(dim(r$data_frame_4))){
            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_4)[2]==1){
            #   stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="rds"){
          a<-reactive({readRDS(input$file_4$datapath)})
          r$data_frame_4<-a()
          if(is.null(dim(r$data_frame_4))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_4)[2]==1){

            #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="csv"){
          a<-reactive({read.csv2(paste0(input$file_4$datapath))})

          r$data_frame_4<-a()
          if(is.null(dim(r$data_frame_4))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_4)[2]==1){

            #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        }else if (ext=="xlsx"){
          library(readxl)
          a<-reactive({openxlsx::read.xlsx(input$file_4$datapath)})
          r$data_frame_4<-a()

          if(is.null(dim(r$data_frame_4))){

            stop("It is not a dataframe. Dimension is 0")
          }
          if(dim(r$data_frame_4)[2]==1){

            #   stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        }else{ #r$data_frame_4<-NULL
          stop("The data type should be xlsx, csv,rda or rds")
        }
      })


###################################################################### all the buttons
######################################################################################
      dataModal_31<-function(failed=F){modalDialog(

        selectInput(ns("data_frame_3_class"),
                    "Please select the variables that you want to check their class",
                    choices=colnames(r$data_frame_3),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_31"),"Cancel"),
          actionButton(ns("ok_31"), "OK")
        )
      )
      }
      dataModal_32<-function(failed=F){modalDialog(
        selectInput(ns("data_frame_3_remove"),
                    "Please select the variables that you want to remove",
                    choices=colnames(r$data_frame_3),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_32"),"Cancel"),
          actionButton(ns("ok_32"), "OK and remove these variables")
        )
      )
      }

      observeEvent(r$data_frame_3,
                   {r$data_frame_3_rem<-r$data_frame_3},
                   ignoreNULL = F
      )

      ## step 1 what happens when button is clicked, the datamodal will show
      observeEvent(input$button3_act_1, {
        req(input$file_3)
        req(r$data_frame_3)
        # r$data_frame_3_rem<-r$data_frame_3
        showModal(dataModal_31())
      })
      observeEvent(input$button3_act_2, {
        req(input$file_3)
        req(r$data_frame_3)
        #      r$data_frame_3_rem<-r$data_frame_3
        showModal(dataModal_32())
      })

      ### what happens when button is clicked okay
      ### whatelse happens when button is clicked okay, but the input is empty
      observeEvent(input$ok_31,{
        req(input$data_frame_3_class)
        if(!is.null(input$data_frame_3_class)){

          removeModal()

        }else{
          showModal(dataModal_31(failed=T))
        }
      })
      observeEvent(input$ok_32,{
        req(input$data_frame_3_remove)
        if(!is.null(input$data_frame_3_remove)){
          removeModal()

        }else{
          showModal(dataModal_32(failed=T))
        }
      })
      ### what happens when click cancel

      observeEvent(input$cancel_31,{
        removeModal()
        r$page2$class3=NULL

      })
      observeEvent(input$cancel_32,{
        removeModal()
        r$page2$remove3=NULL
        r$data_frame_3_rem<-r$data_frame_3
      })
      ## step 2
      #### When a new dataframe is put in the saved values are removed
      observeEvent(

        eventExpr={r$data_frame_3},
        handlerExpr={
          req(input$data_frame_3_class)
          r$page2$class3=NULL
        },
        ignoreNULL = F
      )
      observeEvent(
        eventExpr={r$data_frame_3},
        handlerExpr={
          req(input$data_frame_3_remove)
          r$page2$remove3=NULL        ### it becomes null
          #r$data_frame_3_rem=r$data_frame_3
        },
        ignoreNULL = F
      )


      ## step 3
      #### When the button click again the original saved value will change
      observeEvent(input$button3_act_1,{
        # req(input$data_frame_3_class)
        r$page2$class3<- NULL
      }
      )
      observeEvent(input$button3_act_2,{
        # req(input$data_frame_3_remove)
        r$data_frame_3_rem<-r$data_frame_3
        r$page2$remove3<- NULL
      }
      )

      ## step 4
      # When there is an input, what is generated and saved
      observeEvent(input$data_frame_3_class,{
        s<-c()
        for(i in 1:length(input$data_frame_3_class))
        {s<-c(s,class(r$data_frame_3[,input$data_frame_3_class[i]])[1])
        }
        r$page2$class3<-s
      },
      ignoreNULL = F
      )
      observeEvent(input$data_frame_3_remove,{
        b<-r$data_frame_3
        r$data_frame_3_rem<-b[,!colnames(b)%in%input$data_frame_3_remove,drop=F]
        r$page2$remove3<- input$data_frame_3_remove
      },
      ignoreNULL = F
      )


      ## step 5
      ####  What is printed out and what is generated after the first input in Modal
      output$datainfo_act31<-renderText({
        req(r$page2$class3)
        req(input$data_frame_3_class)
        #for(i in ncol(r$data_frame_3)){
        if(is.null(r$page2$class3)){
          "No column selected"
        }else{
          paste("The class of the variables you checked are: ", paste(input$data_frame_3_class,
                                                                      rep("-",length(r$page2$class3)),
                                                                      r$page2$class3,
                                                                      collapse="; "))
        }

      })

      output$datainfo_act32<-renderText({
        req(r$page2$remove3)
        req(r$data_frame_3)
        req(input$data_frame_3_remove)
        # req(input$data_frame_3_remove)
        #for(i in ncol(r$data_frame_3)){
        if(is.null(r$page2$remove3)){
          "No column selected"
        }else{

          dimm<-reactive({dim(r$data_frame_3)[1]})
          len<-reactive({dim(r$data_frame_3)[2]-length(input$data_frame_3_remove)})
          paste("The following variables are removed:",paste(r$page2$remove3,collapse=","),"<br>",
                "<b>After remove the selected variables, the dataframe has","rows and",dimm(),
                len(),"columns, which will be used for the following steps","<br>")

        }

      })
      ##################################################################
      ############### The third button on the left column

      dataModal_33<-function(failed=F){modalDialog(
        selectInput(ns("data_frame_3_pro31"),
                    "Please select the variables that you want to change to factor",
                    choices=colnames(r$data_frame_3_rem),
                    multiple=T),
        selectInput(ns("data_frame_3_pro32"),
                    "Please select the variables that you want to change to numeric",
                    choices=colnames(r$data_frame_3_rem),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_33"),"Cancel"),
          actionButton(ns("ok_33"), "OK and change these variables")
        )
      )
      }
      observeEvent(
        input$data_frame_3_pro31,{

          updateSelectInput(inputId="data_frame_3_pro32",
                            selected=input$data_frame_3_pro32,
                            choices=colnames(r$data_frame_3_rem)[!colnames(r$data_frame_3_rem)%in%input$data_frame_3_pro31])

        },
        ignoreNULL = F
      )
      observeEvent(
        input$data_frame_3_pro32,{
          updateSelectInput(inputId="data_frame_3_pro31",
                            selected=input$data_frame_3_pro31,
                            choices=colnames(r$data_frame_3_rem)[!colnames(r$data_frame_3_rem)%in%input$data_frame_3_pro32])
        },
        ignoreNULL = F
      )

      observeEvent(r$data_frame_3_rem,
                   {r$data_frame_3_pros31<-r$data_frame_3_rem
                   },
                   ignoreNULL = F
      )


      observeEvent(input$ok_33,{
        #req(input$data_frame_2_pro31)
        if(!is.null(input$data_frame_3_pro31)|!is.null(input$data_frame_3_pro32)){
          removeModal()
        }else{
          showModal(dataModal_33(failed=T))
        }
      })

      ### build a variable r$page2$pro31 to pass the judgement
      observeEvent(input$cancel_33,{
        removeModal()
        r$page2$pro31<-NULL
        r$page2$pro32<-NULL
        r$data_frame_3_pros31<-r$data_frame_3_rem
        #r$data_frame_2_pros32<-r$data_frame_2_rem
      })

      ## step 1 what happens when button is clicked, the datamodal will show
      observeEvent(input$button3_act_3, {
        req(input$file_3)
        req(r$data_frame_3)
        # r$data_frame_2_pros31<-r$data_frame_2_rem
        showModal(dataModal_33())
      })

      ####  step 3 when ever the button is clicked the r$data_frame_2_pros31<-r$data_frame_2_rem
      observeEvent(input$button3_act_3,
                   {r$data_frame_3_pros31<-r$data_frame_3_rem
                   # r$data_frame_4_pros32<-r$data_frame_4_rem
                   r$page2$pro31<-NULL
                   r$page2$pro32<-NULL
                   }
      )
      observeEvent(input$button3_act_2,
                   {r$data_frame_3_pros31<-r$data_frame_3_rem
                   #r$data_frame_2_pros32<-r$data_frame_2_rem
                   r$page2$pro31<-NULL
                   r$page2$pro32<-NULL
                   }
      )
      ## step 4
      # When there is an input, what is generated and saved

      # tolist<-reactive({list(input$data_frame_2_pro31,input$data_frame_2_pro32)})
      observeEvent({input$data_frame_3_pro31

      },
      {
        req(r$data_frame_3_rem)
        #req(input$data_frame_2_pro31)
        if(!is.null(input$data_frame_3_pro31)&!is.null(input$data_frame_3_pro32)){
          r$page2$pro31<-input$data_frame_3_pro31
          r$page2$pro32<-input$data_frame_3_pro32
          g<-reactive({force_parttype(r$data_frame_3_rem,"factor",input$data_frame_3_pro31)})
          r$data_frame_3_pros3_tran<-g()
          f<-reactive({force_parttype(r$data_frame_3_pros3_tran,"numeric",input$data_frame_3_pro32)})
          r$data_frame_3_pros31<-f()

        }else if(!is.null(input$data_frame_3_pro31)&is.null(input$data_frame_3_pro32)){
          r$page2$pro31<-input$data_frame_3_pro31
          g<-reactive({force_parttype(r$data_frame_3_rem,"factor",input$data_frame_3_pro31)})
          r$data_frame_3_pros31<-g()

        }else if(is.null(input$data_frame_3_pro31)&!is.null(input$data_frame_3_pro32)){
          r$page2$pro32<-input$data_frame_3_pro32
          g<-reactive({force_parttype(r$data_frame_3_rem,"numeric",input$data_frame_3_pro32)})
          r$data_frame_3_pros31<-g()

        }
      },
      ignoreNULL = F
      )
      observeEvent({input$data_frame_3_pro32

      },
      {
        req(r$data_frame_3_rem)
        #req(input$data_frame_2_pro31)
        if(!is.null(input$data_frame_3_pro31)&!is.null(input$data_frame_3_pro32)){
          r$page2$pro31<-input$data_frame_3_pro31
          r$page2$pro32<-input$data_frame_3_pro32
          g<-reactive({force_parttype(r$data_frame_3_rem,"factor",input$data_frame_3_pro31)})
          r$data_frame_3_pros3_tran<-g()
          f<-reactive({force_parttype(r$data_frame_3_pros3_tran,"numeric",input$data_frame_3_pro32)})
          r$data_frame_3_pros31<-f()

        }else if(!is.null(input$data_frame_3_pro31)&is.null(input$data_frame_3_pro32)){
          r$page2$pro31<-input$data_frame_3_pro31
          g<-reactive({force_parttype(r$data_frame_3_rem,"factor",input$data_frame_3_pro31)})
          r$data_frame_3_pros31<-g()

        }else if(is.null(input$data_frame_3_pro31)&!is.null(input$data_frame_3_pro32)){
          r$page2$pro32<-input$data_frame_3_pro32
          g<-reactive({force_parttype(r$data_frame_3_rem,"numeric",input$data_frame_3_pro32)})
          r$data_frame_3_pros31<-g()

        }
      },
      ignoreNULL = F
      )


      ## step 5
      ####  What is printed out and what is generated after the first input in Modal
      output$datainfo_act33<-renderText({
        req(input$data_frame_3_pro31)
        #for(i in ncol(r$data_frame_3)){
        if(!is.null(r$page2$pro31)){

          paste("You have made",length(input$data_frame_3_pro31),"variables into factor. They are ",
                paste(r$page2$pro31,collapse=", "))
        }

      })
      output$datainfo_act34<-renderText({
        req(input$data_frame_3_pro32)
        if(!is.null(r$page2$pro32)){

          paste("You have made",length(input$data_frame_3_pro32),"variables into numeric. They are ",
                paste(r$page2$pro32,collapse=", "))
        }

      })
##########################################################
      ####The right column
   ###############################################################################
      ## the modal page
      dataModal_41<-function(failed=F){modalDialog(

        selectInput(ns("data_frame_4_class"),
                    "Please select the variables that you want to check their class",
                    choices=colnames(r$data_frame_4),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_41"),"Cancel"),
          actionButton(ns("ok_41"), "OK")
        )
      )
      }
      dataModal_42<-function(failed=F){modalDialog(
        selectInput(ns("data_frame_4_remove"),
                    "Please select the variables that you want to remove",
                    choices=colnames(r$data_frame_4),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_42"),"Cancel"),
          actionButton(ns("ok_42"), "OK and remove these variables")
        )
      )
      }

      observeEvent(r$data_frame_4,
                   {r$data_frame_4_rem<-r$data_frame_4},
                   ignoreNULL = F
      )

      ## step 1 what happens when button is clicked, the datamodal will show
      observeEvent(input$button4_act_1, {
        req(input$file_4)
        req(r$data_frame_4)
        # r$data_frame_3_rem<-r$data_frame_3
        showModal(dataModal_41())
      })
      observeEvent(input$button4_act_2, {
        req(input$file_4)
        req(r$data_frame_4)
        #      r$data_frame_4_rem<-r$data_frame_4
        showModal(dataModal_42())
      })

      ### what happens when button is clicked okay
      ### whatelse happens when button is clicked okay, but the input is empty
      observeEvent(input$ok_41,{
        req(input$data_frame_4_class)
        if(!is.null(input$data_frame_4_class)){

          removeModal()

        }else{
          showModal(dataModal_41(failed=T))
        }
      })
      observeEvent(input$ok_42,{
        req(input$data_frame_4_remove)
        if(!is.null(input$data_frame_4_remove)){
          removeModal()

        }else{
          showModal(dataModal_42(failed=T))
        }
      })
      ### what happens when click cancel

      observeEvent(input$cancel_41,{
        removeModal()
        r$page2$class4=NULL

      })
      observeEvent(input$cancel_42,{
        removeModal()
        r$page2$remove4=NULL
        r$data_frame_4_rem<-r$data_frame_4
      })
      ## step 2
      #### When a new dataframe is put in the saved values are removed
      observeEvent(

        eventExpr={r$data_frame_4},
        handlerExpr={
          req(input$data_frame_4_class)
          r$page2$class4=NULL
        }
      )
      observeEvent(
        eventExpr={r$data_frame_4},
        handlerExpr={
          req(input$data_frame_4_remove)
          r$page2$remove4=NULL        ### it becomes null
          #r$data_frame_4_rem=r$data_frame_4
        },
        ignoreNULL = F
      )


      ## step 3
      #### When the button click again the original saved value will change
      observeEvent(input$button4_act_1,{
        #req(input$data_frame_2_class)
        r$page2$class4<- NULL
      }
      )
      observeEvent(input$button4_act_2,{
        #req(input$data_frame_4_remove)
        r$data_frame_4_rem<-r$data_frame_4
        r$page2$remove4<- NULL
      }
      )

      ## step 4
      # When there is an input, what is generated and saved
      observeEvent(input$data_frame_4_class,{
        s<-c()
        for(i in 1:length(input$data_frame_4_class))
        {s<-c(s,class(r$data_frame_4[,input$data_frame_4_class[i]])[1])
        }
        r$page2$class4<-s
      },
      ignoreNULL = F
      )
      observeEvent(input$data_frame_4_remove,{
        #if(!is.null(input$data_frame_2_remove)) {
        b<-r$data_frame_4
        r$data_frame_4_rem<-b[,!colnames(b)%in%input$data_frame_4_remove,drop=F]
        r$page2$remove4<- input$data_frame_4_remove
        #}
      },
      ignoreNULL = F
      )


      ## step 5
      ####  What is printed out and what is generated after the first input in Modal
      output$datainfo_act41<-renderText({
        req(r$page2$class4)
        req(input$data_frame_4_class)
        #for(i in ncol(r$data_frame_3)){
        if(is.null(r$page2$class4)){
          "No column selected"
        }else{
          paste("The class of the variables you checked are: ", paste(input$data_frame_4_class,
                                                                      rep("-",length(r$page2$class4)),
                                                                      r$page2$class4,
                                                                      collapse="; "))
        }

      })

      output$datainfo_act42<-renderText({
        req(r$page2$remove4)
        req(r$data_frame_4)
        req(input$data_frame_4_remove)
        # req(input$data_frame_4_remove)
        #for(i in ncol(r$data_frame_4)){
        if(is.null(r$page2$remove4)){
          "No column selected"
        }else{

          dimm<-reactive({dim(r$data_frame_4)[1]})
          len<-reactive({dim(r$data_frame_4)[2]-length(input$data_frame_4_remove)})
          paste("The following variables are removed:",paste(r$page2$remove4,collapse=","),"<br>",
                "<b>After remove the selected variables, the dataframe has","rows and",dimm(),
                len(),"columns, which will be used for the following steps","<br>")

        }

      })

      ##################################################################
      ##################################################################
      ############### The third button on the right column

      dataModal_43<-function(failed=F){modalDialog(
        selectInput(ns("data_frame_4_pros41"),
                    "Please select the variables that you want to change to factor",
                    choices=colnames(r$data_frame_4_rem),
                    multiple=T),
        selectInput(ns("data_frame_4_pros42"),
                    "Please select the variables that you want to change to numeric",
                    choices=colnames(r$data_frame_4_rem),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_43"),"Cancel"),
          actionButton(ns("ok_43"), "OK and change these variables")
        )
      )
      }
      observeEvent(
        input$data_frame_4_pros41,{

          updateSelectInput(inputId="data_frame_4_pros42",
                            selected=input$data_frame_4_pros42,
                            choices=colnames(r$data_frame_4_rem)[!colnames(r$data_frame_4_rem)%in%input$data_frame_4_pros41])

        },
        ignoreNULL = F
      )
      observeEvent(
        input$data_frame_4_pros42,{
          updateSelectInput(inputId="data_frame_4_pros41",
                            selected=input$data_frame_4_pros41,
                            choices=colnames(r$data_frame_4_rem)[!colnames(r$data_frame_4_rem)%in%input$data_frame_4_pros42])
        },
        ignoreNULL = F
      )

      observeEvent(r$data_frame_4_rem,
                   {r$data_frame_4_pros41<-r$data_frame_4_rem
                   },
                   ignoreNULL = F
      )


      observeEvent(input$ok_43,{
        #req(input$data_frame_2_pros41)
        if(!is.null(input$data_frame_4_pros41)|!is.null(input$data_frame_4_pros42)){
          removeModal()
        }else{
          showModal(dataModal_43(failed=T))
        }
      })

      ### build a variable r$page2$pros41 to pass the judgement
      observeEvent(input$cancel_43,{
        removeModal()
        r$page2$pros41<-NULL
        r$page2$pros42<-NULL
        r$data_frame_4_pros41<-r$data_frame_4_rem
        #r$data_frame_2_pros42<-r$data_frame_2_rem
      })

      ## step 1 what happens when button is clicked, the datamodal will show
      observeEvent(input$button4_act_3, {
        req(input$file_4)
        req(r$data_frame_4)
        # r$data_frame_2_pros41<-r$data_frame_2_rem
        showModal(dataModal_43())
      })

      ####  step 3 when ever the button is clicked the r$data_frame_2_pros41<-r$data_frame_2_rem
      observeEvent(input$button4_act_3,
                   {r$data_frame_4_pros41<-r$data_frame_4_rem
                   # r$data_frame_4_pros42<-r$data_frame_4_rem
                   r$page2$pros41<-NULL
                   r$page2$pros42<-NULL
                   }
      )
      observeEvent(input$button4_act_2,
                   {r$data_frame_4_pros41<-r$data_frame_4_rem
                   #r$data_frame_2_pros42<-r$data_frame_2_rem
                   r$page2$pros41<-NULL
                   r$page2$pros42<-NULL
                   }
      )
      ## step 4
      # When there is an input, what is generated and saved

      # tolist<-reactive({list(input$data_frame_2_pros41,input$data_frame_2_pros42)})
      observeEvent({input$data_frame_4_pros41

      },
      {
        req(r$data_frame_4_rem)
        #req(input$data_frame_2_pros41)
        if(!is.null(input$data_frame_4_pros41)&!is.null(input$data_frame_4_pros42)){
          r$page2$pros41<-input$data_frame_4_pros41
          r$page2$pros42<-input$data_frame_4_pros42
          g<-reactive({force_parttype(r$data_frame_4_rem,"factor",input$data_frame_4_pros41)})
          r$data_frame_4_pros4_tran<-g()
          f<-reactive({force_parttype(r$data_frame_4_pros4_tran,"numeric",input$data_frame_4_pros42)})
          r$data_frame_4_pros41<-f()

        }else if(!is.null(input$data_frame_4_pros41)&is.null(input$data_frame_4_pros42)){
          r$page2$pros41<-input$data_frame_4_pros41
          g<-reactive({force_parttype(r$data_frame_4_rem,"factor",input$data_frame_4_pros41)})
          r$data_frame_4_pros41<-g()

        }else if(is.null(input$data_frame_4_pros41)&!is.null(input$data_frame_4_pros42)){
          r$page2$pros42<-input$data_frame_4_pros42
          g<-reactive({force_parttype(r$data_frame_4_rem,"numeric",input$data_frame_4_pros42)})
          r$data_frame_4_pros41<-g()

        }
      },
      ignoreNULL = F
      )
      observeEvent({input$data_frame_4_pros42

      },
      {
        req(r$data_frame_4_rem)
        #req(input$data_frame_2_pros41)
        if(!is.null(input$data_frame_4_pros41)&!is.null(input$data_frame_4_pros42)){
          r$page2$pros41<-input$data_frame_4_pros41
          r$page2$pros42<-input$data_frame_4_pros42
          g<-reactive({force_parttype(r$data_frame_4_rem,"factor",input$data_frame_4_pros41)})
          r$data_frame_4_pros4_tran<-g()
          f<-reactive({force_parttype(r$data_frame_4_pros4_tran,"numeric",input$data_frame_4_pros42)})
          r$data_frame_4_pros41<-f()

        }else if(!is.null(input$data_frame_4_pros41)&is.null(input$data_frame_4_pros42)){
          r$page2$pros41<-input$data_frame_4_pros41
          g<-reactive({force_parttype(r$data_frame_4_rem,"factor",input$data_frame_4_pros41)})
          r$data_frame_4_pros41<-g()

        }else if(is.null(input$data_frame_4_pros41)&!is.null(input$data_frame_4_pros42)){
          r$page2$pros42<-input$data_frame_4_pros42
          g<-reactive({force_parttype(r$data_frame_4_rem,"numeric",input$data_frame_4_pros42)})
          r$data_frame_4_pros41<-g()

        }
      },
      ignoreNULL = F
      )


      ## step 5
      ####  What is printed out and what is generated after the first input in Modal
      output$datainfo_act43<-renderText({
        req(input$data_frame_4_pros41)
        #for(i in ncol(r$data_frame_3)){
        if(!is.null(r$page2$pros41)){

          paste("You have made",length(input$data_frame_4_pros41),"variables into factor. They are ",
                paste(r$page2$pros41,collapse=", "))
        }

      })
      output$datainfo_act44<-renderText({
        req(input$data_frame_4_pros42)
        if(!is.null(r$page2$pros42)){

          paste("You have made",length(input$data_frame_4_pros42),"variables into numeric. They are ",
                paste(r$page2$pros42,collapse=", "))
        }

      })
      ##################################################################################
      #####################################################################
      #######print out the text for dimensionality check

      output$atext3<-renderUI({
        req(r$data_frame_3)

        #    if(!is.null(r$data_frame_3)){
        tags$b(paste("The original dataframe for the PCA plot(s) has",dim(r$data_frame_3)[1],"rows and",
                     dim(r$data_frame_3)[2],"columns."))

      })

      output$atext4<-renderUI({
        req(r$data_frame_4)

        #    if(!is.null(r$data_frame_2)){
        tags$b(paste("The dataframe for the PCA plot(s) has",dim(r$data_frame_4)[1],"rows and",
                     dim(r$data_frame_4)[2],"columns."))

      })
      #################################################################################################################
      #########################################logic between input
      compList2<-reactive({
        list(r$result1$pca_object$scores,
             r$page1_pc_num)
      })

      ### logic for component limit
      observeEvent(compList2(),{

        req(r$result1$pca_object$scores)
        #if(!is.null(r$data_frame_3)){
       # dim2<-dim(r$result1$pca_object$scores)[2]
        updateNumericInput(inputId = "component_limit",
                           #value=min(2,r$page1_pc_num),
                           min=2,
                           value=r$page1_pc_num,
                           max=r$page1_pc_num)
        #}

      },
      ignoreNULL = F,
      ignoreInit=T
      )
      ## pass to global
      observeEvent(input$component_limit,{
        req(input$component_limit)
        r$page2_component_limit<-input$component_limit

      })
      output$getpartial<-renderUI({
        req(r$data_frame_4)
        selectInput(inputId =ns("partial"),
                  label="Do partial correlation or not",
                  choices=c("No","Yes"))

      })

      observeEvent(input$allowcategorical,
                   {if(input$allowcategorical=="Forced to be numeric"){
                     r$page2$allowcategorical=T
                   }else{
                     r$page2$allowcategorical=F
                   }

                   })
      observeEvent(input$partial,
                   {if(input$partial=="Yes"){
                     r$page2$partial=T
                   }else{
                     r$page2$partial=F
                   }

                   })


      ############################################################################################
      #############################################################################################################################
      #########################################################################################################
      ####### core stuff
      scores_comp_list<-reactive({list(r$result1$pca_object$scores,
                                       input$component_limit,
                                       r$result1$pca_object$loadings)})
      observeEvent( scores_comp_list(),
                   {
                     if(!is.null(r$result1$pca_object$scores)){
                       r$page2$makeTPO<-makeTPO(r$result1$pca_object$scores,
                                          r$result1$pca_object$loadings,
                                          compLimit=input$component_limit,
                                          hide_question=T)
                     }
                   })
      TPO_list<-reactive({list(r$page2$makeTPO,
                               r$data_frame_3_pros31,
                               input$use,
                               input$method,
                               r$page2$allowcategorical ,
                               r$page2$partial,
                               r$data_frame_4_pros41
                               )})
      observeEvent(TPO_list(),
                   {req(r$page2$makeTPO)
                     req(r$data_frame_3_pros31)
                      if(!is.null(r$data_frame_3_pros31)){

                       if(!is.null(r$data_frame_4_pros41)&!is.null(r$page2$partial)){
                        r$page2$makeCorr<-makeCorr(r$page2$makeTPO,
                                           r$data_frame_3_pros31, ### needs to be a dataframe with numeric and factor values
                                           use=input$use,
                                           method=input$method,
                                           allowcategorical=r$page2$allowcategorical,  ## when categorical variables are not allowed, one hot encoding
                                         partial=r$page2$partial,
                                           confounder= r$data_frame_4_pros41,
                                           hide_question=T)


                       }else{
                         r$page2$makeCorr<-makeCorr(r$page2$makeTPO,
                                              r$data_frame_3_pros31, ### needs to be a dataframe with numeric and factor values
                                              use=input$use,
                                              method=input$method,
                                              allowcategorical=r$page2$allowcategorical,  ## when categorical variables are not allowed, one hot encoding
                                              #partial=r$page2$partial,
                                              #confounder= r$data_frame_4_pros41,
                                              hide_question=T)

                       }

                      }

                   },
                   ignoreNULL = F
                   )
      addCorr_list<-reactive({
        list(r$page2$makeTPO,
             r$page2$makeCorr)
      })

      observeEvent(addCorr_list(),{
        req(r$page2$makeTPO)
        req(r$page2$makeCorr$cor_estimate)
        r$page2$addCorr<-addCorr(r$page2$makeTPO,
                           r$page2$makeCorr$cor_estimate)
      },ignoreNULL=F)

      ### pass to global
      observeEvent(r$page2$addCorr,{
        req(r$page2$addCorr)
        r$page2_addCorr<-r$page2$addCorr
      })

      observeEvent(r$page2$addCorr,{
        req(r$page2$addCorr)
       req(r$data_frame_3_pros31)
      r$page2$plots<-TriplotGUI(r$page2$addCorr,
                                first_PC=1,   ## The first PC to map
                                second_PC=2,   ## The first PC to map
                                plotLoads=TRUE,   ##Whether to plot loadings (TRUE; default) or suppress them (FALSE)
                                plotScores=FALSE,   ##Whether to plot scores (TRUE) or suppress them (FALSE; default)
                                plotCorr=TRUE,   ##Whether to plot correlations (TRUE; default) or suppress them (FALSE)
                                plotRisk=F,


                                ##For loadings
                                loadLabels=TRUE,   ###Whether to plot variable loading labels (TRUE; default) or not (FALSE)
                                loadArrowLength=0.02,   ###Length of arrow tip , set it as 0 if you want to remove it
                                loadCut=0,    ###lower limit Loadings below the cut are plotted in light grey and without label
                                #loadLim,   ##higher limit,Plot range for loadings

                                ##For correlations
                                #colCorr,   ##Color vector for correlations
                                pchCorr=16,   ##Plotting character for correlations
                                whichCorr=NULL,   ##Which correlations to plot (vector of numbers)
                              #  corLim,     ##Plot range for correlations

                                ##For risks
                               # colRisk,    ##Color vector for risk estimates
                                pchRisk=15,    ##Plotting character for risk estimates
                                whichRisk=NULL,  ##Which risk estimates to plot (vector of numbers)
                               # riskLim,            ##Plot range for risks
                                riskWhisker_percentage=0.1,  ## whisker length is how many percentage of confidence interval (This is only for the visualization purpose)


                                riskOR=T  ##Specify whether to antilog risk layer scale (useful for log:ed risk estimates)

                                ## Scores
                                # scoreLabels=FALSE  ##Whether to plot observation score labels (TRUE) or not (FALSE; default)


      )
      },ignoreNULL=F)



#########################################################################
###### To show the stupid plot
#     Alternative
#     ppp <- eventReactive(input$button2_act_showplot1,
#                           {r$page2$plots$Correlation
#                           }
#
#      )
#      output$showplot<-renderPlot({
#        ppp()
#      })
  #    observeEvent(r$data_frame_1,{
  #      updateActionButton(inputId ="button2_act_showplot1" )
  #    })
  #    observeEvent(r$data_frame_3,{
  #      if()
  #      input$button2_act_showplot1<-reactive({NULL})
  #      updateActionButton(inputId ="button2_act_showplot1" )
  #    })
      output$showcorrplot<-renderUI({
        req(input$button2_act_showplot1)

          renderPlot({r$page2$plots$Correlation})


      })
      #######################################################
      ##########download stuff
      #observeEvent(r$page2$plots$Correlation,{

       # if(!is.null(r$page2$plots$Correlation)){

          output$download2<-downloadHandler(
            filename = function(){
              #  paste( 'iris.csv')
              "shiny_corrplot.pdf"
            },

            content = function(file){
              # pdf(file,onefile=T)
              #  r$page4$plotss$triplot

              #  dev.off()
              ggsave(file, r$page2$plots$Correlation,
                     width = 11, height = 11, dpi = 300, units = "in")
            }

          )


    #    }
   #   })


      ############################ How do I render to fix this problem
      ###################################################################

    #  output$plotplotcorr<-renderUI({
    #    req(r$data_frame_3)
    #    renderPlot({r$page2$plots$Correlation})
    #  })





      ###test stuff
      output$hed<-renderTable({
        req(r$data_frame_3)
        req(r$page2$makeTPO)
        if(!is.null(r$page2$makeTPO$scores)){
          dim(r$page2$makeTPO$scores)
        }
      })
      output$hed2<-renderTable({
        req(r$data_frame_3)
        req(r$page2$makeCorr$cor_estimate)
        if(!is.null(r$page2$makeCorr$cor_estimate)){
          dim(r$page2$makeCorr$cor_estimate)
        }
      })

      output$hed3<-renderTable({
        req(r$data_frame_3)
        req(r$page2$makeCorr$cor_estimate)
        if(!is.null(r$page2$makeCorr$cor_estimate)){
          head(r$page2$makeCorr$cor_estimate)
        }
      })
      output$hed4<-renderTable({
        req(r$data_frame_3)
        req(r$data_frame_3_pros31)
        if(!is.null(r$data_frame_3_pros31)){
          dim(r$data_frame_3_pros31)
        }
      })


#######################################################################33
    }
  )



}


