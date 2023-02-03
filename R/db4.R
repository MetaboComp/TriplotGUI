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
            column(8,
               #style="background-color:#b1f6c6",
               #helpText("The Triplot 2.0"),
               numericInput(inputId =ns( "first_PC"),
                            label="Which PC you want to plot as x axis",
                            value=1,
                            min=1,
                            max=2),
               numericInput(inputId = ns("second_PC"),
                            label="Which PC you want to plot as y axis",
                            value=2,
                            min=1,
                            max=2),
               tableOutput(outputId = ns("hed")),
               tableOutput(outputId = ns("hed2")),
               textOutput(outputId = ns("hed3")),
           #    textOutput(outputId = ns("hed4")),

               plotOutput(outputId = ns("plot_result"))
            ),
            column(4,
                   uiOutput(ns("button4s_act_showplot1")))

        ),
        tags$br()


        ),
        box(width=6,
            title="Other settings",
            fluidRow(
              column(4,
               #style="background-color:#b0c6fb",
               #helpText("Settings for loadings and scores"),

               ## always
               selectInput(inputId =ns("plotLoads"),
                           label="Plot loadings?",
                           #selected ="Spearman",
                           choices=c("No","Yes")),
               ## always
               selectInput(inputId =ns("plotScores"),
                           label="Do you want to plot Scores",
                           #selected ="Spearman",
                           choices=c("No","Yes")),
                uiOutput(ns("load_related_things"))
  ###  depends on the input
        ) ,


        column(4,
               #style="background-color:#b1f6c6",
               #helpText("Settings for correlations"),

               ##always
               selectInput(inputId =ns("plotCorr"),
                           label="Plot correlations?",
                           #selected ="Spearman",
                           choices=c("No","Yes")),


               uiOutput(ns("corr_related_things"))
               #colCorr,   ##Color vector for correlations
               #pchCorr=16,   ##Plotting character for correlations
               #whichCorr=NULL,   ##Which correlations to plot (vector of numbers)
               ## only when there is plot correlations

        ) ,


        column(4,
               #style="background-color:#ffa153",
               #helpText("Settings for Risks"),

               ## always
               selectInput(inputId =ns("plotRisk"),
                           label="Plot Risks",
                           #selected ="Spearman",
                           choices=c("No","Yes")),
               #### a vector input????
               #colRisk,    ##Color vector for risk estimates
               #pchRisk=15,    ##Plotting character for risk estimates
               #whichRisk=NULL,  ##Which risk estimates to plot (vector of numbers)


               uiOutput(ns("risk_related_things"))

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

      ######################################################################################################################
      ## button that shows plot
      output$button4s_act_showplot1<-renderUI({
        # req(input$file_1)

        column(
          width=4,
          tags$br(),
          tags$br(),
          tags$br(),
         ##  div(style = "margin-bottom: -30px"),
          actionButton(ns('button4_act_showplot1'),
                       label="Show plot",
                       size="md"),
          div(style = "margin-top: -30px")
        )
      })


      ###########################################################################################################################
      ## conditions between input
      ########## first and sencond PC
      #########################################
      comlim_list<-reactive({
        req(r$page1_pc_num)
        req(r$page3_component_limits)
        req(r$page2_component_limit)
        list(r$page1_pc_num,
             r$page3_component_limits,
             r$page2_component_limit)
      })
      observeEvent(comlim_list(),
                   {req(r$page1_pc_num)
                     req(r$page3_component_limits)
                     req(r$page2_component_limit)
                     updateNumericInput(inputId = "first_PC",
                                        value=1,
                                        min=1,
                                        max=min(r$page1_pc_num,r$page3_component_limits,r$page2_component_limit),
                                        step=1)

                   },ignoreNULL = F
      )
      observeEvent(comlim_list(),
                   {req(r$page1_pc_num)
                     req(r$page3_component_limits)
                     req(r$page2_component_limit)
                     updateNumericInput(inputId = "second_PC",
                                        value=2,
                                        min=1,
                                        max=min(r$page1_pc_num,r$page3_component_limits,r$page2_component_limit),
                                        step=1)

                   },ignoreNULL = F
      )

      #########################################
      #plotLoad,Scores, Corr and Risk
      observeEvent(input$plotLoads,
                   {
                     if(input$plotLoads=="Yes"){
                       r$page4$plotLoads=T
                     }else{
                       r$page4$plotLoads=F}

                   })
      observeEvent(input$plotScores,
                   {
                     if(input$plotScores=="Yes"){
                       r$page4$plotScores=T
                     }else{
                       r$page4$plotScores=F}

                   })
      observeEvent(input$plotCorr,
                   {
                     if(input$plotCorr=="Yes"){
                       r$page4$plotCorr=T
                     }else{
                       r$page4$plotCorr=F}

                   })
      observeEvent(input$plotRisk,
                   {
                     if(input$plotRisk=="Yes"){
                       r$page4$plotRisk=T
                     }else{
                       r$page4$plotRisk=F}

                   })

##############################################################
      ## load related things
      output$load_related_things<-renderUI({
      req(r$page4$plotLoads)
      if(r$page4$plotLoads==T)  {
        tagList(
      selectInput(inputId =ns("loadLabels"),
                  label="Do you want to have labels for loadings",
                  #selected ="Spearman",
                  choices=c("No","Yes")),
      ## only when there is plotLoads
      numericInput(inputId = ns("loadArrowLength"),
                   value=0.02,
                   step=0.01,
                   label="The lenght of arrow tip"

      ),

      ## only when there is plotLoads
      numericInput(inputId = ns("LoadCut"),
                   value=0,
                   min=0,
                   max=0,
                   step=0,
                   label="The loadings smaller than this value is removed"
      ),
      ## only when there is plotLoads
      numericInput(inputId = ns("LoadLim"),
                   value=0,
                   min=0,
                   max=0,
                   step=0,
                   label="The limits of loadings in the plot")

        )
      }

      })



##################  Load Cut
      loadCut_List<-reactive({
        req(input$first_PC)
        req(input$second_PC)
        req(r$page4$plotLoads)
        list(r$result1$pca_object$loadings,
             input$first_PC,
             input$second_PC,
             r$page4$plotLoads
             #input$LoadLim
        )
      })
      observeEvent( loadCut_List(),
                    {req(r$result1$pca_object$loadings)
                      updateNumericInput(inputId ="LoadCut",
                                         max=max(sqrt(r$result1$pca_object$loadings[,input$first_PC]^2+r$result1$pca_object$loadings[,input$second_PC]^2)),
                                         value=max(sqrt(r$result1$pca_object$loadings[,input$first_PC]^2+r$result1$pca_object$loadings[,input$second_PC]^2)),
                                         step=0.01*max(sqrt(r$result1$pca_object$loadings[,input$first_PC]^2+r$result1$pca_object$loadings[,input$second_PC]^2))

                      )},ignoreNULL=F
      )
      ##################  Load limits
      loadLim_List<-reactive({
        req(input$first_PC)
        req(input$second_PC)
        req(r$page4$plotLoads)
        list(r$result1$pca_object$loadings,
             input$first_PC,
             input$second_PC,
             r$page4$plotLoads
             #input$LoadLim
        )
      })
      observeEvent( loadCut_List(),
                    {req(r$result1$pca_object$loadings)
                      updateNumericInput(inputId ="LoadLim",
                                         max=max(sqrt(r$result1$pca_object$loadings[,input$first_PC]^2+r$result1$pca_object$loadings[,input$second_PC]^2)),
                                         value=max(sqrt(r$result1$pca_object$loadings[,input$first_PC]^2+r$result1$pca_object$loadings[,input$second_PC]^2)),
                                         step=0.01*max(sqrt(r$result1$pca_object$loadings[,input$first_PC]^2+r$result1$pca_object$loadings[,input$second_PC]^2))

                      )},ignoreNULL=F
      )
#####################################################################################################
      ### corr related things
      output$corr_related_things<-renderUI({

        req(r$page4$plotCorr)
        if(r$page4$plotCorr==T)  {
              numericInput(inputId = ns("corrLim"),
                   value=0,
                   min=0,
                   max=0,
                   step=0,
                   label="The limits of correlations in the plot")    ###  depends on the input
        }
      })

     corrLim_List<-reactive({
        req(input$first_PC)
        req(input$second_PC)
        req(r$page4$plotCorr)
        list(r$page2$makeCorr$cor_estimate,
             input$first_PC,
             input$second_PC,
             r$page4$plotCorr
             #input$LoadLim
        )
      })
      observeEvent( corrLim_List(),
                    {     req(r$page2$makeCorr$cor_estimate)
                      updateNumericInput(inputId ="corrLim",
                                         min=0,
                                         value=1.1*max(abs(r$page2$makeCorr$cor_estimate)),
                                         max=1.1*max(abs(r$page2$makeCorr$cor_estimate)),
                                         step=0.01*1.1*max(abs(r$page2$makeCorr$cor_estimate))

                      )},ignoreNULL=F
      )
##########################################################################################
      ### Risk related things
      ## only when plor risk is true

      output$risk_related_things<-renderUI({

        req(r$page4$plotRisk)
        if(r$page4$plotRisk==T)  {

          tagList(
          numericInput(inputId = ns("riskLim"),
                       value=0,
                       min=0,
                       max=0,
                       step=0,
                       label="The limits of risks in the plot"),    ###  depends on the input

          ## always
          numericInput(inputId = ns("riskWhisker_percentage"),
                       label="Whisker length is how many percentage of confidence interval?",
                       value=0.1,
                       min=0,
                       step=0.01,
                       max=1),
          ## always
          selectInput(inputId =ns("riskOR"),
                      label="Risks to be plotted as odds ratio?",

                      choices=c("No", "Yes"))
          )
        }
      })

      observeEvent(input$riskOR,{
       # r$page4$riskOR=NULL
        req(input$riskOR)
        if(input$riskOR=="Yes"){
          r$page4$riskOR=T
        }else{
          r$page4$riskOR=F
        }

      },ignoreNULL=F)

      riskLim_List<-reactive({
        req(input$first_PC)
        req(input$second_PC)
        req(r$page4$plotRisk)
        #req(input$riskOR)
        req(r$page4$riskOR)
        #req(r$data_frame_1)
        list(r$page3_addrisk,
             input$first_PC,
             input$second_PC,
             r$page4$plotRisk,
             r$data_frame_1,
             #input$riskOR
             r$page4$riskOR

        )
      })
      observeEvent( riskLim_List(),
                    {     req(r$page3_addrisk)
                      #r$page4$riskOR==T
                      if(r$page4$plotRisk==T&r$page4$riskOR==T){
                        r$page4$lim_T<-#reactive({
                          1.1*max(max(exp(r$page3_addrisk$riskMatrix[,c(input$first_PC,input$second_PC),drop=F]+
                                                     r$page3_addrisk$riskSE[,c(input$first_PC,input$second_PC),drop=F]))-1,
                                               1-min(exp(r$page3_addrisk$riskMatrix[,c(input$first_PC,input$second_PC),drop=F]-
                                                           r$page3_addrisk$riskSE[,c(input$first_PC,input$second_PC),drop=F])))
                     #    })
                        updateNumericInput(inputId ="riskLim",
                                           min=0,
                                           value=r$page4$lim_T,
                                           max=r$page4$lim_T,
                                           step=0.01*r$page4$lim_T
                        )
                      }else if(r$page4$plotRisk==T&r$page4$riskOR==F){
                        r$page4$lim_F<- #reactive({
                          1.1*max(abs(r$page3_addrisk$riskMatrix[,c(input$first_PC,input$second_PC),drop=F]+
                                                 r$page3_addrisk$riskSE[,c(input$first_PC,input$second_PC),drop=F]),
                                           abs(r$page3_addrisk$riskMatrix[,c(input$first_PC,input$second_PC),drop=F]-
                                                 r$page3_addrisk$riskSE[,c(input$first_PC,input$second_PC),drop=F])
                                           )
                          #})
                        updateNumericInput(inputId ="riskLim",
                                           min=0,
                                           value=r$page4$lim_F,
                                           max=r$page4$lim_F,
                                           step=0.01*r$page4$lim_F
                        )

                      }



                      },ignoreNULL=F
      )



      ################################################################
      ## test stuff
      ###test stuff
      output$hed<-renderTable({
        req(r$page3_addrisk$riskMatrix)
        req(r$data_frame_5)
        if(!is.null(r$page3_addrisk$riskMatrix)){
          head(r$page3_addrisk$riskMatrix,6)
        }
      })
      output$hed2<-renderTable({
        req(r$data_frame_5)
        req(r$page3_addrisk$riskSE)
        if(!is.null(r$page3_addrisk$riskSE)){
        head(r$page3_addrisk$riskSE,6)
        }
      })

      output$hed3<-renderPrint({
        req(r$page4$lim_T)

        if(!is.null(r$page4$lim_T)){
          r$page4$lim_T
        }
      })
      output$hed4<-renderPrint({
        req(r$page4$lim_F)

        if(!is.null(r$page4$lim_F)){
          r$page4$lim_F
        }
      })
  ###############################################################################

      ###########################################################################################################
      ###core
      output$showtriplot<-renderPlot({
        req(input$button4_act_showplot1)
          ###the plot
      })



    }
  )



}
