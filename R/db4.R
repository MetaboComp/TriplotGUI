db4UI<-function(id){
  ns<-NS(id)
  ### a list of things that is put in the tab
  tagList(
    fluidRow(
      width=12,
      tags$h2("Plot Triplot")
    ),
      fluidRow(
        width=12,
        box(
          width=6,
          title="The key input files ",
          fluidRow(
            column(12,
               #style="background-color:#b1f6c6",
               #helpText("The Triplot 2.0"),
               numericInput(inputId =ns( "first_PC"),
                            label="Which PC you want to plot as x axis",
                            value=5,
                            min=1,
                            max=10),
               numericInput(inputId = ns("second_PC"),
                            label="Which PC you want to plot as y axis",
                            value=5,
                            min=1,
                            max=10),


               plotOutput(outputId = ns("plot_result"))
            )

        ),
        tags$br(),

        fluidRow(
          column(12,
                 uiOutput(ns('createButtonUI')))
        )
        ),
        box(width=6,
            title="Other settings",
            fluidRow(
              column(4,
               #style="background-color:#b0c6fb",
               #helpText("Settings for loadings and scores"),
               selectInput(inputId =ns("plotLoads"),
                           label="Do you want to plot loadings",
                           #selected ="Spearman",
                           choices=c("Yes","No")),
               selectInput(inputId =ns("plotScores"),
                           label="Do you want to plot Scores",
                           #selected ="Spearman",
                           choices=c("Yes","No")),
               selectInput(inputId =ns("loadLabels"),
                           label="Do you want to have labels for loadings",
                           #selected ="Spearman",
                           choices=c("Yes","No")),
               numericInput(inputId = ns("loadArrowLength"),
                            value=0.02,
                            label="The lenght of arrow tip"

               ),
               numericInput(inputId = ns("loadCut"),
                            value=0,
                            label="The loadings smaller than this value is removed"
               ),
               numericInput(inputId = ns("LoadLim"),
                            value=0,
                            label="The limits of loadings in the plot")    ###  depends on the input
        ) ,


        column(4,
               #style="background-color:#b1f6c6",
               #helpText("Settings for correlations"),
               selectInput(inputId =ns("plotCorr"),
                           label="Do you want to plot correlations",
                           #selected ="Spearman",
                           choices=c("Yes","No")),



               #colCorr,   ##Color vector for correlations
               #pchCorr=16,   ##Plotting character for correlations
               #whichCorr=NULL,   ##Which correlations to plot (vector of numbers)

               numericInput(inputId = ns("corrLim"),
                            value=0,
                            label="The limits of correlationss in the plot")    ###  depends on the input
        ) ,


        column(4,
               #style="background-color:#ffa153",
               #helpText("Settings for Risks"),
               selectInput(inputId =ns("plotRisk"),
                           label="Do you want to plot Risks",
                           #selected ="Spearman",
                           choices=c("Yes","No")),
               #### a vector input????
               #colRisk,    ##Color vector for risk estimates
               #pchRisk=15,    ##Plotting character for risk estimates
               #whichRisk=NULL,  ##Which risk estimates to plot (vector of numbers)
               numericInput(inputId = ns("riskWhisker_percentage"),
                            label="whisker length is how many percentage of confidence interval?",
                            value=0.1,
                            min=0,
                            max=1),
               selectInput(inputId =ns("riskOR"),
                           label="Do you want to risks to be plotted as odds ratio or not?",
                           #selected ="Spearman",
                           choices=c("Yes","No")),
               numericInput(inputId = ns("riskLim"),
                            value=0,
                            label="The limits of risks in the plot")    ###  depends on the input

               #riskWhisker_percentage=0.1,  ## whisker length is how many percentage of confidence interval (This is only for the visualization purpose)
               # riskOR=T,  ##Specify whether to antilog risk layer scale (useful for log:ed risk estimates)

        )))))
}



db4Server<-function(id,r){
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
