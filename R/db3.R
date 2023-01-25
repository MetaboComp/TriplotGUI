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
        width=8,
        title="The key input files ",
        fluidRow(
        column(6,
               #style="background-color:#b1f6c6",
               #helpText("Key Input"),
               fileInput(inputId=ns("outcome_dataframe"),
                         label="The dataframe that its variables will be used to estimates risks with scores"),
               fileInput(inputId=ns("confounder_dataframe"),
                         label="The dataframe that its variables will be used for adjustment for the risks"),
               plotOutput(outputId =ns( "plot_result"))
        ),
        tags$br(),

        fluidRow(
          column(12,
                 uiOutput(ns('createButtonUI')))
        )
        )),
      box(width=4,
          title="Other settings",
          fluidRow(
            column(12,
               #style="background-color:#ffa153",
               #helpText("Other settings"),
               selectInput(inputId =ns("multinomial"),
                           label="Multinomial",
                           #selected ="Spearman",
                           choices=c("Yes","No")),
               selectInput(inputId =ns("pair"),
                           label="Pair",
                           choices=c("colnames of the input dataframe")),   #### or a new variable?
               numericInput(inputId = ns("CI"),
                            label="Confidence level",
                            value=0.95,
                            min=0.01,
                            max=1)
        ))


      ))
  )
}



db3Server<-function(id,r){
  moduleServer(
    id,
    function(input,output,session){
      ns<-session$ns
      #output$createButtonUI<-renderUI(
      #  {req(r$'1'$dbPath)   ### The name of the tabs
      #    req(input$data_frame)
      output$plot_result<-renderPlot({plot(iris)})
    }
  )



}
