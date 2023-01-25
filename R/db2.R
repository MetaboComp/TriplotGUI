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
      width=12,
      box(
        width=8,
        title="The key input files ",
     fluidRow(
      column(12,
             #style="background-color:#b1f6c6",
             #helpText("Key Input"),
             fileInput(inputId=ns("data_frame_1"),
                       label="The dataframe that its variables will be used to calculate correlations with scores"),
             fileInput(inputId=ns("confounder_dataframe"),
                       label="The dataframe that its variables will be used for adjustment for the correlations"),
             plotOutput(outputId = ns("plot_result")),
             uiOutput(outputId=ns("files"))

      )),
     tags$br(),
     fluidRow(
       column(12,
              uiOutput(ns('createButtonUI')))
     )
      ),
     box(width=4,
         title="Other settings",
         fluidRow(
           column(12,
            # style="background-color:#ffa153",
            # helpText("Other settings"),
             selectInput(inputId =ns("partial"),
                         label="Partial correlation",
                         selected ="Spearman",
                         choices=c("Pearson","Spearman","Kendell")),
             selectInput(inputId =ns("use"),
                         label="Use",
                         choices=c("pairwise")),
             selectInput(inputId =ns("allowcategorical"),
                         label="Allow categorical variables",
                         choices=c("Yes","No")),
             selectInput(inputId =ns("partial"),
                         label="Partial correlation",
                         choices=c("Yes","No")),
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



db2Server<-function(id,r){
  moduleServer(
    id,
    function(input,output,session){
      ns<-session$ns
      #output$createButtonUI<-renderUI(
      #  {req(r$'1'$dbPath)   ### The name of the tabs
      #    req(input$data_frame)
      output$files<-renderUI({
        req(input$data_frame_1)
        renderTable(r$page1$data_frame_1)
      })
    }
  )



}


