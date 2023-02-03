db1UI<-function(id){
  ns<-NS(id)

  ### a list of things that is put in the tab
  tagList(

    fluidRow(
      width=12,
      tags$h2("Plot PCA plots")
    ),

    fluidRow(
      width=12,
      box(
        width=6,
        title="The key input files ",
        fluidRow(
          column(12,
                tags$b("Dataframe used for Principal component analysis (Upload your data in .rds, .rda, .csv, .xlsx format.)"),
                tags$br(),
                tags$b("Note that all variables should be continuous/numeric in this dataframe")
                # shinyDirButton(ns('dir'), "Select a folder", "Select a folder", FALSE),


                 )),
        fluidRow(

                 #style="background-color:#b1f6c6",
                 #helpText("Key Input"),




                 uiOutput(ns("putinfile1")),
                 uiOutput(ns("button1s_act_reset1"))


          ),
          fluidRow(
            column(12,
               uiOutput(outputId=ns("judgefile1")),
                br(),
                uiOutput(outputId = ns("atext1")),
                 #div(style = "margin-top: -10px") ,

            uiOutput(ns("button1s_act_1")),
             # br(),
             htmlOutput(ns("datainfo_act11")),

              uiOutput(ns("button1s_act_2")),
            #  br(),
               htmlOutput(ns("datainfo_act12")),

            div(style = "margin-bottom: -10px"),   ## this line shrink 10px at the bottom
            hr(),
            div(style = "margin-top: -30px"),

              uiOutput(ns('button1s_act_3')),
            htmlOutput(ns("datainfo_act13")),
             tableOutput(ns("head")),
             tableOutput(ns("head2"))
          )
        ),

        #tags$br(),
        fluidRow(
          column(12,

                 #actionButton(inputId ="action-button",
                 #              label="Action"),
                 #hr(),      ## just a line
                 #submitButton(text="submit") ,
                 tags$br(),

                 uiOutput(outputId=ns("plot"))
                 #plotOutput(outputId=ns("aplot")),

                 #tableOutput(outputId =ns("atable")),

                 #uiOutput(outputId="data_frame_1"),
          )
        ),
        tags$br()


      ),
      box(width=6,
          title="Other settings",
          fluidRow(
                    column(8,
                     #div(style = "margin-top: -30px"),
                     fileInput(inputId=ns("file_2"),
                               label="Another dataframe that you has individual variables that you may use in the 4 pages (optional)",
                               buttonLabel = "Upload...",
                               multiple = F,
                               accept=c(".rds",".rda",".csv",".xlsx")),
                     div(style = "margin-top: -30px"),
                    ),
                    column(4,
                       #    uiOutput(ns("button2s_act_reset2"))
                    ),
                    ),
          fluidRow(
                column(12,
                       uiOutput(outputId=ns("judgefile2")),
                     br(),
                     uiOutput(outputId = ns("atext2")),

                     uiOutput(ns('button2s_act_1')),
                    # br(),
                     htmlOutput(ns("datainfo_act21")),
                     #tags$br(),   ## this workd
                     uiOutput(ns('button2s_act_2')),
                    # br(),
                     htmlOutput(ns("datainfo_act22")),  ### when this is printed there is hr
                    div(style = "margin-bottom: -10px"),
                     hr(),
                    div(style = "margin-top: -30px"),
                     uiOutput(ns('button2s_act_3')),
                    # br(),
                     htmlOutput(ns("datainfo_act23")),
                    htmlOutput(ns("datainfo_act24")),

                    div(style = "margin-bottom: -10px"),
                    hr(),
                    div(style = "margin-top: -30px")


            ),
            hr(),
            column(12,
                   #style="background-color:#ffa153",
                   #helpText("Other settings"),
                   #checkboxGroupInput(ns("what_to_plot"),
                   #                    "What plot do you want to have?",
                   #                    choiceNames=c("scores","loadings", "scoreloadings","screeplot"),
                   #                    choiceValues=c("score","loading", "scoreloading","scree")),

                   ## always there
                   checkboxGroupButtons(ns("what_to_plot"),
                                        "What plot do you want to have?",
                                        selected=character(0),
                                        choiceNames=c("scores","loadings", "scoreloadings","screeplot"),
                                        choiceValues=c("score","loading", "scoreloading","scree")),
                   ## always there
                   selectInput(inputId =ns("pc_type"),
                               label="To do prcomp() or principal()",
                               # selected=character(0),
                               choices=c("prcomp","principal")),

                   ### conditional to pc_type
                  uiOutput(ns("rotates")),
                   ## always there, conditional to pc_num
                   numericInput(inputId = ns("pc_num"),
                                label="Numeber of PCs",
                                value=2,
                                min=2,
                                max=2),
                   ## always there, conditional to pc_num
                   numericInput(inputId = ns("first_PC"),
                                label="First PC",
                                value=1,
                                min=1,
                                max=2,
                                step=1),
                   ## always there
                   numericInput(inputId = ns("second_PC"),
                                label="Second PC",
                                value=2,
                                min=1,
                                max=2,
                                step=1),
                   ## always there
                   selectInput(inputId =ns("center"),
                               label="Center",
                               choices=c("Yes","No")),
                   ## always there
                   selectInput(inputId =ns("scale"),
                               label="Scale",
                               choices=c("Yes","No")),


                   ## alwaysthere
                   selectInput(inputId =ns("eigenloading"),
                               label="Eigenloading",
                               choices=c("Loading","Eigen")),
                  ### depend on if scores and scores loadings are printed out or not
            ),
              column(4,
                     uiOutput(ns("colors"))
                     ),
            column(4,
                   uiOutput(ns("shapes"))
            ),
            column(4,
                   uiOutput(ns("sizes"))
            ),

            column(12,
                    ###depend on loading
                   uiOutput(ns("loadings_name")),
                   uiOutput(ns("loadings_cutvalues")),
                   uiOutput(ns("loadings_cutpercents")),
                   uiOutput(ns("loadings_scale_scoreloading"))











            )
          )
      )
    )
  )



}




db1Server<-function(id,r){
  moduleServer(
    id,
    function(input,output,session){
      ns<-session$ns
      #output$createButtonUI<-renderUI(
      #  {req(r$'1'$dbPath)   ### The name of the tabs
      #    req(input$data_frame)
#########################################################################
### 1 reset button

      observeEvent(input$button1_act_reset1,{

        runjs("history.go(0)")

        })


      output$putinfile1<-renderUI({
      column(8,
             fileInput(inputId=ns("file_1"),
                       label=NULL,
                       buttonLabel = "Upload...",
                       multiple = F,
                       accept=c(".rds",".rda",".csv",".xlsx")),

             div(style = "margin-top: -30px")
      )

      })
      output$button1s_act_reset1<-renderUI({
       # req(input$file_1)

        column(
          width=4,
          #tags$br(),

          actionButton(ns('button1_act_reset1'),
                       label="Reset Everything",
                       size="md"),
          div(style = "margin-top: -30px")
        )
      })




#####################################################################################
##############################
      ## pass to global
      observeEvent(input$pc_num,{

        r$page1_pc_num=input$pc_num
      },
      ignoreNULL = F)

##########################################################################
################# The action buttons, only show up when the file is there

      output$button1s_act_1<-renderUI({
        req(input$file_1)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button1_act_1'),
                       label="Check classes of the variables",
                       size="md")
        )
      })
      output$button1s_act_2<-renderUI({
        req(input$file_1)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button1_act_2'),
                       label="Remove variables",
                       size="xs")
        )
      })
      output$button1s_act_3<-renderUI({
        req(input$file_1)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button1_act_3'),
                       label="Force all variables to numeric",
                       size="xs")
        )
      })

      output$button2s_act_1<-renderUI({
        req(input$file_2)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button2_act_1'),
                       label="Check classes of the variables",
                       size="md")
        )
      })
      output$button2s_act_2<-renderUI({
        req(input$file_2)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button2_act_2'),
                       label="Remove variables",
                       size="md")
        )
      })
      output$button2s_act_3<-renderUI({
        req(input$file_2)
        column(
          width=12,
          tags$br(),
          actionButton(ns('button2_act_3'),
                       label="Which variables do you want to change to factor/numeric",
                       size="md")
        )
      })

#####################################################################
      #######In the judgefile I check for error give value to the r$data_frame_1
      output$judgefile1<-renderUI({
        req(input$file_1)
        ext<-tools::file_ext(input$file_1$name)

        if(ext=="rda"){
          LoadToEnvironment <- function(RData, env=new.env()) {
            load(RData, env)
            return(env)
          }
          env <- reactiveFileReader(1000,
                                    session,
                                    input$file_1$name,
                                    LoadToEnvironment)
          r$data_frame_1<-  env()[[names(env())[1]]]
          if(is.null(dim(r$data_frame_1))){
            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_1)[2]==1){
            stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="rds"){
          a<-reactive({readRDS(input$file_1$datapath)})
          r$data_frame_1<-a()
          if(is.null(dim(r$data_frame_1))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_1)[2]==1){

            stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="csv"){
          a<-reactive({read.csv2(paste0(input$file_1$datapath))})

          r$data_frame_1<-a()
          if(is.null(dim(r$data_frame_1))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_1)[2]==1){

            stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        #  print(dim(r$data_frame_1))
        }else if (ext=="xlsx"){
          library(readxl)
          a<-reactive({openxlsx::read.xlsx(input$file_1$datapath)})
          r$data_frame_1<-a()
          if(is.null(dim(r$data_frame_1))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_1)[2]==1){

            stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        }else{ #r$data_frame_1<-NULL
          stop("The data type should be xlsx, csv, rda or rds")
          }
      })





      output$judgefile2<-renderUI({
        req(input$file_2)
        ext<-tools::file_ext(input$file_2$name)

        if(ext=="rda"){
          LoadToEnvironment <- function(RData, env=new.env()) {
            load(RData, env)
            return(env)
          }
          env <- reactiveFileReader(1000,
                                    session,
                                    input$file_2$name,
                                    LoadToEnvironment)
          r$data_frame_2<-  env()[[names(env())[1]]]
          if(is.null(dim(r$data_frame_2))){
            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_2)[2]==1){
         #   stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="rds"){
          a<-reactive({readRDS(input$file_2$datapath)})
          r$data_frame_2<-a()
          if(is.null(dim(r$data_frame_2))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_2)[2]==1){

          #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }

        }else if(ext=="csv"){
          a<-reactive({read.csv2(paste0(input$file_2$datapath))})

          r$data_frame_2<-a()
          if(is.null(dim(r$data_frame_2))){

            stop("It is not a dataframe. Dimension is 0")}
          if(dim(r$data_frame_2)[2]==1){

          #  stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        }else if (ext=="xlsx"){
          library(readxl)
          a<-reactive({openxlsx::read.xlsx(input$file_2$datapath)})
          r$data_frame_2<-a()
          if(is.null(dim(r$data_frame_2))){

            stop("It is not a dataframe. Dimension is 0")
            }
          if(dim(r$data_frame_2)[2]==1){

         #   stop("Column number is 1. The dataframe should have at least 2 variables")
          }
        }else{ #r$data_frame_1<-NULL
          stop("The data type should be xlsx, csv,rda or rds")
          }
      })
########################################################################################
##################################################################################
##### All the buttons




############################################################
#### The first button on the left column
     ## the modal page
dataModal_11<-function(failed=F){modalDialog(

  selectInput(ns("data_frame_1_class"),
              "Please select the variables that you want to check their class",
              choices=colnames(r$data_frame_1),
              multiple=T),
  span("You could directly type in the name if you know the name"),

  footer = tagList(
    #modalButton("Cancel"),
    actionButton(ns("cancel_11"),"Cancel"),
    actionButton(ns("ok_11"), "OK")
  )
)
}
dataModal_12<-function(failed=F){modalDialog(
        selectInput(ns("data_frame_1_remove"),
                    "Please select the variables that you want to remove",
                    choices=colnames(r$data_frame_1),
                    multiple=T),
        span("You could directly type in the name if you know the name"),

        footer = tagList(
          #modalButton("Cancel"),
          actionButton(ns("cancel_12"),"Cancel"),
          actionButton(ns("ok_12"), "OK and remove these variables")
        )
      )
}

observeEvent(r$data_frame_1,
             {r$data_frame_1_rem<-r$data_frame_1},
             ignoreNULL = F
)

## step 1 what happens when button is clicked, the datamodal will show
observeEvent(input$button1_act_1, {
  req(input$file_1)
  req(r$data_frame_1)
 # r$data_frame_1_rem<-r$data_frame_1
  showModal(dataModal_11())
})
observeEvent(input$button1_act_2, {
        req(input$file_1)
        req(r$data_frame_1)
  #      r$data_frame_1_rem<-r$data_frame_1
        showModal(dataModal_12())
      })

### what happens when button is clicked okay
 ### whatelse happens when button is clicked okay, but the input is empty
      observeEvent(input$ok_11,{
        req(input$data_frame_1_class)
        if(!is.null(input$data_frame_1_class)){

          removeModal()

        }else{
          showModal(dataModal_11(failed=T))
        }
      })
      observeEvent(input$ok_12,{
        req(input$data_frame_1_remove)
        if(!is.null(input$data_frame_1_remove)){
       removeModal()

        }else{
          showModal(dataModal_12(failed=T))
        }
      })
 ### what happens when click cancel

      observeEvent(input$cancel_11,{
          removeModal()
        r$page1$class1=NULL

      })
      observeEvent(input$cancel_12,{
        removeModal()
        r$page1$remove1=NULL
        r$data_frame_1_rem<-r$data_frame_1
      })
## step 2
#### When a new dataframe is put in the saved values are removed
      observeEvent(

        eventExpr={r$data_frame_1},
        handlerExpr={
          req(input$data_frame_1_class)
          r$page1$class1=NULL
        },
        ignoreNULL = F
      )
      observeEvent(
        eventExpr={r$data_frame_1},
        handlerExpr={
          req(input$data_frame_1_remove)
          r$page1$remove1=NULL        ### it becomes null
          #r$data_frame_1_rem=r$data_frame_1
        },
        ignoreNULL = F
      )


## step 3
#### When the button click again the original saved value will change
      observeEvent(input$button1_act_1,{
       # req(input$data_frame_1_class)
        r$page1$class1<- NULL
      }
      )
      observeEvent(input$button1_act_2,{
       # req(input$data_frame_1_remove)
        r$data_frame_1_rem<-r$data_frame_1
        r$page1$remove1<- NULL
      }
      )

## step 4
# When there is an input, what is generated and saved
      observeEvent(input$data_frame_1_class,{
        s<-c()
        for(i in 1:length(input$data_frame_1_class))
        {s<-c(s,class(r$data_frame_1[,input$data_frame_1_class[i]])[1])
        }
        r$page1$class1<-s
      },
      ignoreNULL = F
      )
      observeEvent(input$data_frame_1_remove,{
        b<-r$data_frame_1
        r$data_frame_1_rem<-b[,!colnames(b)%in%input$data_frame_1_remove,drop=F]
        r$page1$remove1<- input$data_frame_1_remove
      },
      ignoreNULL = F
      )


## step 5
####  What is printed out and what is generated after the first input in Modal
      output$datainfo_act11<-renderText({
        req(r$page1$class1)
        req(input$data_frame_1_class)
        #for(i in ncol(r$data_frame_1)){
        if(is.null(r$page1$class1)){
          "No column selected"
        }else{
          paste("The class of the variables you checked are: ", paste(input$data_frame_1_class,
                                                                      rep("-",length(r$page1$class1)),
                                                                      r$page1$class1,
                                                                      collapse="; "))
        }

      })

      output$datainfo_act12<-renderText({
        req(r$page1$remove1)
        req(r$data_frame_1)
        req(input$data_frame_1_remove)
       # req(input$data_frame_1_remove)
        #for(i in ncol(r$data_frame_1)){
         if(is.null(r$page1$remove1)){
           "No column selected"
         }else{

          dimm<-reactive({dim(r$data_frame_1)[1]})
           len<-reactive({dim(r$data_frame_1)[2]-length(input$data_frame_1_remove)})
           paste("The following variables are removed:",paste(r$page1$remove1,collapse=","),"<br>",
           "<b>After remove the selected variables, the dataframe has","rows and",dimm(),
               len(),"columns, which will be used for the following steps","<br>")

         }

      })


####################################################################################
###### The third button on the left column
  ### if smth is logical or character, transform it to factor
  ### if smth is factor, transform to numeric
    ### when the button is clicked,   r$data_frame_1_processed is generated


      observeEvent(r$data_frame_1_rem,
                   {r$data_frame_1_processed<-r$data_frame_1_rem
                   },
                   ignoreNULL = F
      )
  ## step 1 what happens when click this button
    observeEvent(input$button1_act_3,{
      req(r$data_frame_1)

      frame_initial<-r$data_frame_1_processed
      if(!is.null(frame_initial)){
        r$page1$act13<-ncol(frame_initial)
      r$data_frame_1_processed<-force_type(frame_initial,"numeric")
      }
    }
    )
   ## step 2 saved values will be removed when you hit another button
    observeEvent(input$button1_act_2,{   ### when this is changed the whole thing does
        req(r$data_frame_1)
      r$page1$act13=NULL
      r$data_frame_1_processed=r$data_frame_1_rem

      }
  )
    ## step 3 saved value will be removed when you change the data
    observeEvent(r$data_frame_1,{   ### when this is changed the whole thing does
      req(r$data_frame_1)
      r$page1$act13=NULL
      r$data_frame_1_processed=r$data_frame_1_rem

    }
    ,ignoreNULL = F
    )

  #  observeEvent(r$data_frame_1_rem,{   ### when this is changed the whole thing does
  #    req(r$data_frame_1)
  #    r$data_frame_1_processed=r$data_frame_1_rem
  #
  #  }
  #  )


    output$datainfo_act13<-renderText({
      req(r$page1$act13)
      req(r$data_frame_1)
      req(r$data_frame_1_processed)
      # req(input$data_frame_1_remove)
      #for(i in ncol(r$data_frame_1)){
      if(is.null(r$page1$act13)){
        "No column selected"
      }else{
        dimm<-reactive({dim(r$data_frame_1_processed)[1]})
        len<-reactive({dim(r$data_frame_1_processed)[2]})
        paste("All variables in the data frame with",dimm(),"rows and ",len(),
              "columns are transformed to numeric")


      }

    })



######################################################################################
################The first and second button on the right column


    ## the modal page
    dataModal_21<-function(failed=F){modalDialog(

      selectInput(ns("data_frame_2_class"),
                  "Please select the variables that you want to check their class",
                  choices=colnames(r$data_frame_2),
                  multiple=T),
      span("You could directly type in the name if you know the name"),

      footer = tagList(
        #modalButton("Cancel"),
        actionButton(ns("cancel_21"),"Cancel"),
        actionButton(ns("ok_21"), "OK")
      )
    )
    }
    dataModal_22<-function(failed=F){modalDialog(
      selectInput(ns("data_frame_2_remove"),
                  "Please select the variables that you want to remove",
                  choices=colnames(r$data_frame_2),
                  multiple=T),
      span("You could directly type in the name if you know the name"),

      footer = tagList(
        #modalButton("Cancel"),
        actionButton(ns("cancel_22"),"Cancel"),
        actionButton(ns("ok_22"), "OK and remove these variables")
      )
    )
    }

    observeEvent(r$data_frame_2,
                 {r$data_frame_2_rem<-r$data_frame_2},
                 ignoreNULL = F
    )

    ## step 1 what happens when button is clicked, the datamodal will show
    observeEvent(input$button2_act_1, {
      req(input$file_2)
      req(r$data_frame_2)
      # r$data_frame_1_rem<-r$data_frame_1
      showModal(dataModal_21())
    })
    observeEvent(input$button2_act_2, {
      req(input$file_2)
      req(r$data_frame_2)
      #      r$data_frame_1_rem<-r$data_frame_1
      showModal(dataModal_22())
    })

    ### what happens when button is clicked okay
    ### whatelse happens when button is clicked okay, but the input is empty
    observeEvent(input$ok_21,{
      req(input$data_frame_2_class)
      if(!is.null(input$data_frame_2_class)){

        removeModal()

      }else{
        showModal(dataModal_21(failed=T))
      }
    })
    observeEvent(input$ok_22,{
      req(input$data_frame_2_remove)
      if(!is.null(input$data_frame_2_remove)){
        removeModal()

      }else{
        showModal(dataModal_22(failed=T))
      }
    })
    ### what happens when click cancel

    observeEvent(input$cancel_21,{
      removeModal()
      r$page1$class2=NULL

    })
    observeEvent(input$cancel_22,{
      removeModal()
      r$page1$remove2=NULL
      r$data_frame_2_rem<-r$data_frame_2
    })
    ## step 2
    #### When a new dataframe is put in the saved values are removed
    observeEvent(

      eventExpr={r$data_frame_2},
      handlerExpr={
        req(input$data_frame_2_class)
        r$page1$class2=NULL
      }
    )
    observeEvent(
      eventExpr={r$data_frame_2},
      handlerExpr={
        req(input$data_frame_2_remove)
        r$page1$remove2=NULL        ### it becomes null
        #r$data_frame_1_rem=r$data_frame_1
      },
      ignoreNULL = F
    )


    ## step 3
    #### When the button click again the original saved value will change
    observeEvent(input$button2_act_1,{
      #req(input$data_frame_2_class)
      r$page1$class2<- NULL
    }
    )
    observeEvent(input$button2_act_2,{
      #req(input$data_frame_2_remove)
      r$data_frame_2_rem<-r$data_frame_2
      r$page1$remove2<- NULL
    }
    )

    ## step 4
    # When there is an input, what is generated and saved
    observeEvent(input$data_frame_2_class,{
      s<-c()
      for(i in 1:length(input$data_frame_2_class))
      {s<-c(s,class(r$data_frame_2[,input$data_frame_2_class[i]])[1])
      }
      r$page1$class2<-s
    },
    ignoreNULL = F
    )
    observeEvent(input$data_frame_2_remove,{
      #if(!is.null(input$data_frame_2_remove)) {
      b<-r$data_frame_2
      r$data_frame_2_rem<-b[,!colnames(b)%in%input$data_frame_2_remove,drop=F]
      r$page1$remove2<- input$data_frame_2_remove
      #}
    },
    ignoreNULL = F
    )


    ## step 5
    ####  What is printed out and what is generated after the first input in Modal
    output$datainfo_act21<-renderText({
      req(r$page1$class2)
      req(input$data_frame_2_class)
      #for(i in ncol(r$data_frame_1)){
      if(is.null(r$page1$class2)){
        "No column selected"
      }else{
        paste("The class of the variables you checked are: ", paste(input$data_frame_2_class,
                                                                    rep("-",length(r$page1$class2)),
                                                                    r$page1$class2,
                                                                    collapse="; "))
      }

    })

    output$datainfo_act22<-renderText({
      req(r$page1$remove2)
      req(r$data_frame_2)
      req(input$data_frame_2_remove)
      # req(input$data_frame_1_remove)
      #for(i in ncol(r$data_frame_1)){
      if(is.null(r$page1$remove2)){
        "No column selected"
      }else{

        dimm<-reactive({dim(r$data_frame_2)[1]})
        len<-reactive({dim(r$data_frame_2)[2]-length(input$data_frame_2_remove)})
        paste("The following variables are removed:",paste(r$page1$remove2,collapse=","),"<br>",
              "<b>After remove the selected variables, the dataframe has","rows and",dimm(),
              len(),"columns, which will be used for the following steps","<br>")

      }

    })

##################################################################
##################################################################
    ############### The third button on the right column

    dataModal_23<-function(failed=F){modalDialog(
      selectInput(ns("data_frame_2_pro1"),
                  "Please select the variables that you want to change to factor",
                  choices=colnames(r$data_frame_2_rem),
                  multiple=T),
      selectInput(ns("data_frame_2_pro2"),
                  "Please select the variables that you want to change to numeric",
                  choices=colnames(r$data_frame_2_rem),
                  multiple=T),
      span("You could directly type in the name if you know the name"),

      footer = tagList(
        #modalButton("Cancel"),
        actionButton(ns("cancel_23"),"Cancel"),
        actionButton(ns("ok_23"), "OK and change these variables")
      )
    )
    }
    observeEvent(
      input$data_frame_2_pro1,{

        updateSelectInput(inputId="data_frame_2_pro2",
                          selected=input$data_frame_2_pro2,
                          choices=colnames(r$data_frame_2_rem)[!colnames(r$data_frame_2_rem)%in%input$data_frame_2_pro1])

      },
      ignoreNULL = F
    )
    observeEvent(
      input$data_frame_2_pro2,{
        updateSelectInput(inputId="data_frame_2_pro1",
                          selected=input$data_frame_2_pro1,
                          choices=colnames(r$data_frame_2_rem)[!colnames(r$data_frame_2_rem)%in%input$data_frame_2_pro2])
      },
      ignoreNULL = F
    )

    observeEvent(r$data_frame_2_rem,
                 {r$data_frame_2_pros1<-r$data_frame_2_rem
                 },
                 ignoreNULL = F
    )


    observeEvent(input$ok_23,{
      #req(input$data_frame_2_pro1)
      if(!is.null(input$data_frame_2_pro1)|!is.null(input$data_frame_2_pro2)){
        removeModal()
      }else{
        showModal(dataModal_23(failed=T))
      }
    })

### build a variable r$page1$pro1 to pass the judgement
    observeEvent(input$cancel_23,{
      removeModal()
      r$page1$pro1<-NULL
      r$page1$pro2<-NULL
      r$data_frame_2_pros1<-r$data_frame_2_rem
      #r$data_frame_2_pros2<-r$data_frame_2_rem
    })

    ## step 1 what happens when button is clicked, the datamodal will show
    observeEvent(input$button2_act_3, {
      req(input$file_2)
      req(r$data_frame_2)
      # r$data_frame_2_pros1<-r$data_frame_2_rem
      showModal(dataModal_23())
    })

    ####  step 3 when ever the button is clicked the r$data_frame_2_pros1<-r$data_frame_2_rem
    observeEvent(input$button2_act_3,
                 {r$data_frame_2_pros1<-r$data_frame_2_rem
                # r$data_frame_2_pros2<-r$data_frame_2_rem
                 r$page1$pro1<-NULL
                 r$page1$pro2<-NULL
                 }
    )
    observeEvent(input$button2_act_2,
                 {r$data_frame_2_pros1<-r$data_frame_2_rem
                 #r$data_frame_2_pros2<-r$data_frame_2_rem
                 r$page1$pro1<-NULL
                 r$page1$pro2<-NULL
                   }
    )
    ## step 4
    # When there is an input, what is generated and saved

   # tolist<-reactive({list(input$data_frame_2_pro1,input$data_frame_2_pro2)})
    observeEvent({input$data_frame_2_pro1

      },
      {
      req(r$data_frame_2_rem)
      #req(input$data_frame_2_pro1)
        if(!is.null(input$data_frame_2_pro1)&!is.null(input$data_frame_2_pro2)){
          r$page1$pro1<-input$data_frame_2_pro1
          r$page1$pro2<-input$data_frame_2_pro2
          g<-reactive({force_parttype(r$data_frame_2_rem,"factor",input$data_frame_2_pro1)})
          r$data_frame_2_pros_tran<-g()
          f<-reactive({force_parttype(r$data_frame_2_pros_tran,"numeric",input$data_frame_2_pro2)})
          r$data_frame_2_pros1<-f()

        }else if(!is.null(input$data_frame_2_pro1)&is.null(input$data_frame_2_pro2)){
          r$page1$pro1<-input$data_frame_2_pro1
          g<-reactive({force_parttype(r$data_frame_2_rem,"factor",input$data_frame_2_pro1)})
          r$data_frame_2_pros1<-g()

        }else if(is.null(input$data_frame_2_pro1)&!is.null(input$data_frame_2_pro2)){
          r$page1$pro2<-input$data_frame_2_pro2
          g<-reactive({force_parttype(r$data_frame_2_rem,"numeric",input$data_frame_2_pro2)})
          r$data_frame_2_pros1<-g()

        }
      },
      ignoreNULL = F
    )
    observeEvent({input$data_frame_2_pro2

    },
    {
      req(r$data_frame_2_rem)
      #req(input$data_frame_2_pro1)
      if(!is.null(input$data_frame_2_pro1)&!is.null(input$data_frame_2_pro2)){
        r$page1$pro1<-input$data_frame_2_pro1
        r$page1$pro2<-input$data_frame_2_pro2
        g<-reactive({force_parttype(r$data_frame_2_rem,"factor",input$data_frame_2_pro1)})
        r$data_frame_2_pros_tran<-g()
        f<-reactive({force_parttype(r$data_frame_2_pros_tran,"numeric",input$data_frame_2_pro2)})
        r$data_frame_2_pros1<-f()

      }else if(!is.null(input$data_frame_2_pro1)&is.null(input$data_frame_2_pro2)){
        r$page1$pro1<-input$data_frame_2_pro1
        g<-reactive({force_parttype(r$data_frame_2_rem,"factor",input$data_frame_2_pro1)})
        r$data_frame_2_pros1<-g()

      }else if(is.null(input$data_frame_2_pro1)&!is.null(input$data_frame_2_pro2)){
        r$page1$pro2<-input$data_frame_2_pro2
        g<-reactive({force_parttype(r$data_frame_2_rem,"numeric",input$data_frame_2_pro2)})
        r$data_frame_2_pros1<-g()

      }
    },
    ignoreNULL = F
    )


    ## step 5
    ####  What is printed out and what is generated after the first input in Modal
    output$datainfo_act23<-renderText({
      req(input$data_frame_2_pro1)
      #for(i in ncol(r$data_frame_1)){
      if(!is.null(r$page1$pro1)){

      paste("You have made",length(input$data_frame_2_pro1),"variables into factor. They are ",
              paste(r$page1$pro1,collapse=", "))
      }

    })
    output$datainfo_act24<-renderText({
      req(input$data_frame_2_pro2)
    if(!is.null(r$page1$pro2)){

      paste("You have made",length(input$data_frame_2_pro2),"variables into numeric. They are ",
            paste(r$page1$pro2,collapse=", "))
    }

    })


    ############################################################

    output$head<-renderTable({
      req(r$data_frame_1_rem)
      if(!is.null(r$data_frame_1_rem)){
        head(r$data_frame_1_rem,6)
      }
    })
    output$head2<-renderTable({
      req(r$data_frame_1_processed)
      if(!is.null(r$data_frame_1_processed)){
        head(r$data_frame_1_processed,6)
      }
    })
    #############################################################

##################################################################################
#####################################################################
      #######print out the text for dimensionality check

      output$atext1<-renderUI({
        req(r$data_frame_1)

        #    if(!is.null(r$data_frame_1)){
        tags$b(paste("The original dataframe for the PCA plot(s) has",dim(r$data_frame_1)[1],"rows and",
                     dim(r$data_frame_1)[2],"columns."))
        #if(dim(r$data_frame_1)[2]!=1){
        #
        #         }else{
        #          stop("The dataframe should be at least 2 columns")}
        #      }
      })

      output$atext2<-renderUI({
        req(r$data_frame_2)

        #    if(!is.null(r$data_frame_2)){
        tags$b(paste("The dataframe for the PCA plot(s) has",dim(r$data_frame_1)[1],"rows and",
                     dim(r$data_frame_2)[2],"columns."))
        #if(dim(r$data_frame_1)[2]!=1){
        #
        #         }else{
        #          stop("The dataframe should be at least 2 columns")}
        #      }
      })
#########################################################################
      ##############Conditional inputs
#### simply save values
     # r$page1$pc_num=reactiveValues()
#      observeEvent(input$pc_num,{
#        req(input$pc_num)
#        r$page1$pc_num<-input$pc_num


 #     })

#############################################################
##### number of pc depend on the dimention of the data frame
    #  combined<-reactive({
    #    req(r$data_frame_1)
    #    req(input$pc_num)
    #    list(r$data_frame_1,input$pc_num)
    #  })

      observeEvent(eventExpr={#dimension_r()
        r$data_frame_1
      },
      handlerExpr={
        #req(r$data_frame_1)
        #req(input$pc_num)
        dimension<-ncol(r$data_frame_1)
        #dimension<-dimension_r()
        if(dimension>=2){
          dimension_min<-2
          dimension_max<-dimension
          pc_min<-1
          pc_max<-dimension
        }else{
          dimension_min<-0
          dimension_max<-0
          pc_min<-0
          pc_max<-0
          #stop("The dataframe should at least have 2 variables")
        }
        if(round(dimension/2,0)>=2){
          dimension_value<-round(dimension/2,0)
        }else{

          dimension_value<-0
        }
        freezeReactiveValue(input,"pc_num")
        freezeReactiveValue(input,"first_PC")
        freezeReactiveValue(input,"second_PC")
        updateNumericInput(inputId = "pc_num",                  ## update do not put ns()
                           # label="Numeber of PCs",
                           value=dimension_value,
                           min=dimension_min,
                           max=dimension_max)
        updateNumericInput(inputId = "first_PC",                  ## update do not put ns()
                           value=1,
                           min=pc_min,
                           max=pc_max)
        updateNumericInput(inputId = "second_PC",                  ## update do not put ns()
                           value=2,
                           min=pc_min,
                           max=pc_max)

      }

      )

      observeEvent(eventExpr={#dimension_r()
        input$pc_num
      },
      handlerExpr={
        req(input$pc_num)
        freezeReactiveValue(input,"first_PC")
        freezeReactiveValue(input,"second_PC")
        updateNumericInput(inputId = "first_PC",                  ## update do not put ns()
                           value=1,

                           max=input$pc_num)
        updateNumericInput(inputId = "second_PC",                  ## update do not put ns()

                           value=2,
                           max=input$pc_num)
      },
      ignoreNULL = F

      )

#####################################################################
      ### add size color or shape variable when r_data_frame_2 is there

      output$colors<-renderUI({
        req(r$data_frame_2_pros1)
        req(input$what_to_plot)
        if(!any(input$what_to_plot%in%c("score","scoreloading"))){

        }else{
          selectInput(inputId =ns("color"),
                      selected=NULL,
                      label="Differeniate scores by color using a factor variable",
                      choices=colnames(r$data_frame_2_pros1),
                      multiple=T)
        }
      })
      output$shapes<-renderUI({
        req(r$data_frame_2_pros1)
        req(input$what_to_plot)
        if(!any(input$what_to_plot%in%c("score","scoreloading"))){

        }else{
        selectInput(inputId =ns("shape"),
                    selected=NULL,
                    label="Differeniate scores by shape using a factor variable",
                    choices=colnames(r$data_frame_2_pros1),
                    multiple=T
                    )
        }
      })

      output$sizes<-renderUI({
        req(r$data_frame_2_pros1)
        req(input$what_to_plot)
        if(!any(input$what_to_plot%in%c("score","scoreloading"))){

        }else{
        selectInput(inputId =ns("size"),
                    selected=NULL,
                    label="Differeniate scores by size using a numeric variable",
                    choices=colnames(r$data_frame_2_pros1),
                    multiple=T)
        }
      })


      observeEvent(r$data_frame_2_pros1,{
        req(r$data_frame_2_pros1)
        req(input$what_to_plot)
        if(!any(input$what_to_plot%in%c("score","scoreloading"))){

        }else{
          freezeReactiveValue(input,"color")
        updateSelectInput(inputId = "color",
                          selected=NULL,
                          choices=colnames(r$data_frame_2_pros1)
        )
        }

      },
      ignoreNULL = F)
      observeEvent(r$data_frame_2_pros1,{
        req(r$data_frame_2_pros1)
        req(input$what_to_plot)
        if(!any(input$what_to_plot%in%c("score","scoreloading"))){

        }else{
          freezeReactiveValue(input,"shape")
        updateSelectInput(inputId ="shape",
                          selected=NULL,
                          choices=colnames(r$data_frame_2_pros1)
        )
       }
      },
      ignoreNULL = F)
      observeEvent(r$data_frame_2_pros1,{
        req(r$data_frame_2_pros1)
        req(input$what_to_plot)
        if(!any(input$what_to_plot%in%c("score","scoreloading"))){

        }else{
          freezeReactiveValue(input,"size")
        updateSelectInput(inputId ="size",
                          selected=NULL,
                         choices=colnames(r$data_frame_2_pros1)
                         )
        }

      },
      ignoreNULL = F)

###############################################################
      ######### rotate

      output$rotates<-renderUI({
        req(input$pc_type)
        if(input$pc_type=="principal"){
          selectInput(inputId =ns("rotate"),
                      label="rotate (used when doing principal)",
                      choices=c("none", "varimax", "quartimax", "promax", "oblimin", "simplimax",  "cluster"))
        }else{
     }

      })

######################################################
      #### scale_score_loading
      output$loadings_scale_scoreloading<-renderUI({
        req(input$what_to_plot)
        if(!any(input$what_to_plot%in%c("scoreloading"))){

        }else{
          selectInput(inputId =ns("scale_scoreloading"),
                      label="Scale scoreloading",
                      choices=c("Yes","No"))


                             ###################this needs to be limited by the data frame
                    ###################this needs to be limited by the data frame
        }

      })
######################################################
      ## loadings name
      output$loadings_name<-renderUI({
        req(input$what_to_plot)
        if(!any(input$what_to_plot%in%c("loading","scoreloading"))){

        }else{
          selectInput(inputId =ns("loadings_name"),
                      label="Loading names",
                      choices=c("Yes","No"))                     ###################this needs to be limited by the data frame
          ###################this needs to be limited by the data frame
        }

      })

#### condition between loadings cut values and cut percent
##################################################
      #### cur percent
      output$loadings_cutpercents<-renderUI({
        req(input$what_to_plot)
        if(!any(input$what_to_plot%in%c("loading","scoreloading"))){

        }else{
          sliderInput(inputId =ns("loadings_cutpercent"),
                      label="Loadings cutpercent",
                      value=0,    ## The initial value
                      min=0,
                      max=1,
                      step=0.01)                        ###################this needs to be limited by the data frame


        }

      })

############################################################
      ####loadings cut value
      output$loadings_cutvalues<-renderUI({
        req(input$what_to_plot)
        req(input$loadings_cutpercent)
        if(!any(input$what_to_plot%in%c("loading","scoreloading"))){

        }else{
          if(input$loadings_cutpercent==0){
          sliderInput(inputId =ns("loadings_cutvalue"),
                      label="Loadings cutvalue (deactivated when loadings cutpercent is >0)",
                      value=0,    ## The initial value
                      min=0,
                      max=1,
                      step=0.01)                        ###################this needs to be limited by the data frame

          }
        }
      })




#### save smth

      ### scale

      observeEvent(input$scale,
                   {
                     if(input$scale=="Yes"){
                       r$page1$scale=T
                     }else{
                       r$page1$scale=F}

                   })

      ## center
      observeEvent(input$center,
                   {
                     if(input$center=="Yes"){
                       r$page1$center=T
                     }else{
                       r$page1$center=F}

                   })
      ###eigenloading_r
      observeEvent(input$eigenloading,
                   {
                     if(input$eigenloading=="Eigen"){
                       r$page1$eigenloading="eigen"
                     }else{
                       r$page1$eigenloading="loading"}

                   })

      #### rotate
      observeEvent(input$pc_type,{
        if(input$pc_type!="principal"){
          r$page1$rotate="none"

        }else{
          req(input$rotate)
          r$page1$rotate=input$rotate
        }


      })



      #### for size shape and color variable
      colorlist<-reactive({list(input$color,r$data_frame_2_pros1)})
      observeEvent(colorlist(),{
        req(input$what_to_plot)
        #req(r$data_frame_2_pros1)
        if(!any(input$what_to_plot%in%c("score","scoreloading"))){
          r$page1$color_r<-NULL
          r$page1$colorname_r<-NULL
          #  r$page1$color_r<-reactive({ NULL})
        }else{
          if(!is.null(input$color)){
            req(r$data_frame_2_pros1)
            if(length(input$color)>1){
              r$page1$color_r<-NULL
              r$page1$colorname_r<-NULL
            }else{
              r$page1$color_r<-r$data_frame_2_pros1[,
                                                    colnames(r$data_frame_2_pros1)==input$color,
                                                    drop=T]


              r$page1$colorname_r<-colnames(r$data_frame_2_pros1) [colnames(r$data_frame_2_pros1)==input$color]
            }
          }  else{
            r$page1$color_r<-NULL
            r$page1$colorname_r<-NULL
            #  r$page1$color_r<-reactive({NULL})
          }

        }

      },
      ignoreNULL = F)


      shapelist<-reactive({list(input$shape,r$data_frame_2_pros1)})
      observeEvent(shapelist(),{
        req(input$what_to_plot)
        #req(r$data_frame_2_pros1)
        if(!any(input$what_to_plot%in%c("score","scoreloading"))){
          r$page1$shape_r<-NULL
          r$page1$shapename_r<-NULL
          #  r$page1$shape_r<-reactive({ NULL})
        }else{
          if(!is.null(input$shape)){
            req(r$data_frame_2_pros1)
            if(length(input$shape)>1){
              r$page1$shape_r<-NULL
              r$page1$shapename_r<-NULL
            }else{
              r$page1$shape_r<-r$data_frame_2_pros1[,
                                                    colnames(r$data_frame_2_pros1)==input$shape,
                                                    drop=T]


              r$page1$shapename_r<-colnames(r$data_frame_2_pros1) [colnames(r$data_frame_2_pros1)==input$shape]
            }
          }  else{
            r$page1$shape_r<-NULL
            r$page1$shapename_r<-NULL
            #  r$page1$shape_r<-reactive({NULL})
          }

        }

      },
      ignoreNULL = F)

      sizelist<-reactive({list(input$size,r$data_frame_2_pros1)})
      observeEvent(sizelist(),{
        req(input$what_to_plot)
        #req(r$data_frame_2_pros1)
        if(!any(input$what_to_plot%in%c("score","scoreloading"))){
          r$page1$size_r<-NULL
          r$page1$sizename_r<-NULL
          #  r$page1$size_r<-reactive({ NULL})
        }else{
          if(!is.null(input$size)){
            req(r$data_frame_2_pros1)
            if(length(input$size)>1){
              r$page1$size_r<-NULL
              r$page1$sizename_r<-NULL
            }else{
              r$page1$size_r<-r$data_frame_2_pros1[,
                                                   colnames(r$data_frame_2_pros1)==input$size,
                                                   drop=T]


              r$page1$sizename_r<-colnames(r$data_frame_2_pros1) [colnames(r$data_frame_2_pros1)==input$size]
            }
          }  else{
            r$page1$size_r<-NULL
            r$page1$sizename_r<-NULL
            #  r$page1$size_r<-reactive({NULL})
          }

        }

      },
      ignoreNULL = F)

      ###############################

      ##### loading names
        loading_names_list<-reactive({list(input$what_to_plot,
                                           input$loadings_name)})
      observeEvent(loading_names_list(),{
      if(!any(input$what_to_plot%in%c("loading","scoreloading"))){
        r$page1$loadings_name<-
          T

      }else{
        req(input$loadings_name)

          if(input$loadings_name=="Yes"){
            r$page1$loadings_name<-T
          }else{
            r$page1$loadings_name<-F
          }

      }
      },
      ignoreNULL=F)

      ###  scale_scoreloading
      scale_scoreloading_list<-reactive({list(input$what_to_plot,
                                              input$scale_scoreloading)})
      observeEvent(scale_scoreloading_list(),{
      if(!any(input$what_to_plot%in%c("scoreloading"))){
        r$page1$scale_scoreloading<-T
      }else{
        req(input$scale_scoreloading)

          if(input$scale_scoreloading=="Yes"){
            r$page1$scale_scoreloading<-T
          }else{
            r$page1$scale_scoreloading<-F
          }


      }
      },
      ignoreNULL=F)
      ## loadings_cutpercent
      loadings_cutpercent_list<-reactive({list(input$what_to_plot,
                                              input$loadings_cutpercent)})
      observeEvent(loadings_cutpercent_list(),{
      if(!any(input$what_to_plot%in%c("loading","scoreloading"))){

        r$page1$loadings_cutpercent<-NULL
      }else{

        req(input$loadings_cutpercent)

        r$page1$loadings_cutpercent<-input$loadings_cutpercent


      }
      },
      ignoreNULL=F)
      ## loadings_cutvalue
      loadings_cutvalue_list<-reactive({list(input$what_to_plot,
                                               input$loadings_cutpercent,
                                             input$loadings_cutvalue)})
      observeEvent(loadings_cutvalue_list(),{
      if(!any(input$what_to_plot%in%c("loading","scoreloading"))){
        r$page1$loadings_cutvalue<-NULL


      }else{
        req(input$loadings_cutpercent)
        req(input$loadings_cutvalue)
        if(input$loadings_cutpercent==0){
          r$page1$loadings_cutvalue<-input$loadings_cutvalue
          r$page1$loadings_cutpercent<-NULL
        }else{
          r$page1$loadings_cutvalue<- NULL

        }

      }
      },
      ignoreNULL=F
      )
##################################################################
#####################This is to save the object for the sake of next page
            ### The object that will be saved
      biglist<-reactive({list(r$data_frame_1_processed,
                              input$pc_type,
                              input$pc_num,
                              input$scale,
                              input$center,
                              input$eigenloading,
                              input$rotate

      )})
      observeEvent(biglist(),{
        req(r$data_frame_1_processed)

        r$result1<-PCA_plots(r$data_frame_1_processed,
                             plottype = "score",
                             pc_type=input$pc_type,
                             pc_num=input$pc_num,
                             scale=r$page1$scale,
                             center=r$page1$center,
                             eigen_loading=r$page1$eigenloading,
                             rotate=r$page1$rotate,
                             plot=F
        )

      },ignoreNULL = F
      )



###################################plot stuff
      ## named a few variables in this namespace
      output$plot<-renderUI({
        req(r$data_frame_1)
        req(input$what_to_plot)
        req(input$pc_type)
        #
        #freezeReactiveValue(input,"what_to_plot")
        #freezeReactiveValue(input,"pc_type")
        plot_output_list <- lapply(1:length(input$what_to_plot),
                                   function(i) {
                                     plotname <- paste("plot", i, sep="")
                                     plotOutput(ns(plotname))
                                   })
        do.call(tagList, plot_output_list)
      })

      for (i in 1:4) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          my_i <- i
          plotname <- paste("plot", my_i, sep="")
          #freezeReactiveValue(input,"what_to_plot")
          output[[plotname]] <- renderPlot({
            req(r$data_frame_1)
            req(input$pc_type)
            req(input$what_to_plot[my_i])
            req(input$pc_num)
            req(input$first_PC)
            req(input$second_PC)
            req(input$scale)
            req(input$center)

            ### loadings_name



#observeEvent(r$data_frame_1_processed,{

#})

            PCA_plots(r$data_frame_1_processed,
                      plottype = input$what_to_plot[my_i],
                      pc_type=input$pc_type,
                      pc_num=input$pc_num,
                      scale=r$page1$scale,
                      center=r$page1$center,
                      eigen_loading=r$page1$eigenloading,
                      rotate=r$page1$rotate,
                       size_variable=r$page1$size_r,
                       size_variable_name=r$page1$sizename_r,
                      color_variable=r$page1$color_r,
                      color_variable_name=r$page1$colorname_r,
                      shape_variable=r$page1$shape_r,
                      shape_variable_name=r$page1$shapename_r,
                      scale_scoreloading=r$page1$scale_scoreloading,
                      first_PC=input$first_PC,
                      second_PC=input$second_PC,
                      loadings_name=r$page1$loadings_name,
                      #loadings_name=T,
                      loadings_cutpercent=r$page1$loadings_cutpercent,
                      loadings_cutvalue=r$page1$loadings_cutvalue,  ### currently,  the loadings not in the range will be ignored instead of showing grey lines
                      plot=T
                      )

          })
        })

       }





    }
  )
  #######


  #####
}



