#' @export
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
            column(9,
               #style="background-color:#b1f6c6",
               #helpText("The Triplot 2.0"),
               numericInput(inputId = ns("component_limits"),
                            label="Number of components (You could increase the range by changeing this in page 2 and 3)",
                            value=2,
                            min=2,
                            max=2),
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


               div(style = "margin-top: 30px"),
               uiOutput(ns("showtriplot")),
               tags$br(),

               uiOutput(ns("downloadbutton4"))
               #tableOutput(outputId = ns("hed")),
               #tableOutput(outputId = ns("hed2")),
               #tableOutput(outputId = ns("hed3")),
               #tableOutput(outputId = ns("hed4")),
               #tableOutput(outputId = ns("hed5"))

            ),

            #column(3,
                   uiOutput(ns("button4s_act_showplot1"))
             #      )

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

#' @export

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
          width=3,
          tags$br(),
          tags$br(),
          tags$br(),
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
############################################################
      ## download button

  output$downloadbutton4<-renderUI({
    req(r$page4$plotss$triplot)
    #req(r$data_frame_1)
    downloadButton(outputId=ns("download4"),
                   label="Download figure")

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
      ### logic for component limit
      observeEvent(comlim_list(),{
        req(r$page1_pc_num)
        req(r$page3_component_limits)
        req(r$page2_component_limit)
        updateNumericInput(inputId = "component_limits",

                           value=min(r$page1_pc_num,r$page3_component_limits,r$page2_component_limit),
                           min=2,

                           max=min(r$page1_pc_num,r$page3_component_limits,r$page2_component_limit),
                           step=1

        )
      },
      ignoreNULL = F

      )

      observeEvent(input$component_limits,
                   {
                     updateNumericInput(inputId = "first_PC",
                                        value=1,
                                        min=1,
                                        max=input$component_limits,
                                        step=1)

                   },ignoreNULL = F
      )
      observeEvent(input$component_limits,
                   {
                     updateNumericInput(inputId = "second_PC",
                                        value=2,
                                        min=1,
                                        max=input$component_limits,
                                        step=1)

                   },ignoreNULL = F
      )



      #########################################
      #plotLoad,Scores, Corr and Risk
      observeEvent(input$plotLoads,
                   {req(input$plotLoads)
                     if(input$plotLoads=="Yes"){
                       r$page4$plotLoads=T
                     }else{
                       r$page4$plotLoads=F
                       r$page4$loadLabels=F
                       r$page4$loadArrowLength=0.02
                       r$page4$LoadCut=0
                       r$page4$LoadLim=NULL}

                   })
      observeEvent(input$plotScores,
                   {req(input$plotScores)
                     if(input$plotScores=="Yes"){
                       r$page4$plotScores=T
                     }else{
                       r$page4$plotScores=F
                      }

                   })
      observeEvent(input$plotCorr,
                   {req(input$plotCorr)
                     if(input$plotCorr=="Yes"){
                       r$page4$plotCorr=T
                     }else{
                       r$page4$plotCorr=F
                       r$page4$corrLim=NULL}

                   })
      observeEvent(input$plotRisk,
                   {req(input$plotRisk)
                     if(input$plotRisk=="Yes"){
                       r$page4$plotRisk=T
                     }else{
                       r$page4$plotRisk=F
                       r$page4$riskLim=NULL
                       r$page4$riskWhisker_percentage=0.1
                       r$page4$riskOR=T
                       }

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

### Load Labels
      #r$page4<- reactiveValues()
      loadLabels_list<-reactive({list(
        r$page4$plotLoads,
        input$loadLabels
      )})
      observeEvent(loadLabels_list(),{
        # r$page4$riskOR=NULL
        req(input$loadLabels)
        req( r$page4$plotLoads)
        if(r$page4$plotLoads==T){
        if(input$loadLabels=="Yes"){
          r$page4$loadLabels=T
        }else{
          r$page4$loadLabels=F
        }
        }
      },ignoreNULL=F)
### load Arrow length
      loadArrowLength_list<-reactive({list(
        r$page4$plotLoads,
        input$loadArrowLength
      )})
      observeEvent( loadArrowLength_list(),{
        req( r$page4$plotLoads)
        req( input$loadArrowLength)
        if(r$page4$plotLoads==T){
          r$page4$loadArrowLength<-input$loadArrowLength
        }
      })

    #  observeEvent(r$page4$loadLabels)

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
                                         value=0,#max(sqrt(r$result1$pca_object$loadings[,input$first_PC]^2+r$result1$pca_object$loadings[,input$second_PC]^2)),
                                         step=0.01*max(sqrt(r$result1$pca_object$loadings[,input$first_PC]^2+r$result1$pca_object$loadings[,input$second_PC]^2))

                      )},ignoreNULL=F
      )

      loadCut_list2<-reactive({list(
        r$page4$plotLoads,
        input$LoadCut
      )})
      observeEvent( loadCut_list2(),{
        req( r$page4$plotLoads)
        req( input$LoadCut)
        if(r$page4$plotLoads==T){
          r$page4$LoadCut<-input$LoadCut
        }
      })


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


      loadLim_list2<-reactive({list(
        r$page4$plotLoads,
        input$LoadLim
      )})
      observeEvent( loadLim_list2(),{
        req( r$page4$plotLoads)
        req( input$LoadLim)
        if(r$page4$plotLoads==T){
          r$page4$LoadLim<-input$LoadLim
        }
      })
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
                                         value=1.1*max(abs(r$page2$makeCorr$cor_estimate[,c(input$first_PC,input$second_PC)])),
                                         max=1.1*max(abs(r$page2$makeCorr$cor_estimate[,c(input$first_PC,input$second_PC)])),
                                         step=0.01*1.1*max(abs(r$page2$makeCorr$cor_estimate[,c(input$first_PC,input$second_PC)]))

                      )},ignoreNULL=F
      )
      corrLim_List2<-reactive({
        list(input$corrLim,
             r$page4$plotCorr)

      })
      observeEvent(corrLim_List2(),{
        req(r$page4$plotCorr)
        req(input$corrLim)
        if(r$page4$plotCorr==T){
        r$page4$corrLim<-input$corrLim
        }
      })

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
## risk OR          ## always
          selectInput(inputId =ns("riskOR"),
                      label="Risks to be plotted as odds ratio?",

                      choices=c("No", "Yes"))
          )
        }
      })


      riskOR_list<-reactive({
        list(input$riskOR,
             r$page4$plotRisk)
      })

      observeEvent(input$riskOR,{
       # r$page4$riskOR=NULL
       req(input$riskOR)
        req(r$page4$plotRisk)
        if(r$page4$plotRisk==T){
        if(input$riskOR=="Yes"){
          r$page4$riskOR=T
        }else{
          r$page4$riskOR=F
        }
        }
      },ignoreNULL=F)



### riskLim
      riskLim_List<-reactive({

        list(r$page3_addRisk,
             input$first_PC,
             input$second_PC,
             r$page4$plotRisk,
             r$data_frame_1,

             r$page4$riskOR

        )
      })
      observeEvent( riskLim_List(),
                    {     req(r$page3_addRisk)
                      if(!is.null(r$page4$riskOR)&!is.null(r$page4$plotRisk))
                      if(r$page4$plotRisk==T&r$page4$riskOR==T){
                        r$page4$lim_T<-#reactive({
                          1.1*max(max(exp(r$page3_addRisk$riskMatrix[,c(input$first_PC,input$second_PC),drop=F]+
                                                     r$page3_addRisk$riskSE[,c(input$first_PC,input$second_PC),drop=F]))-1,
                                               1-min(exp(r$page3_addRisk$riskMatrix[,c(input$first_PC,input$second_PC),drop=F]-
                                                           r$page3_addRisk$riskSE[,c(input$first_PC,input$second_PC),drop=F])))
                     #    })
                        updateNumericInput(inputId ="riskLim",
                                           min=0,
                                           value=r$page4$lim_T,
                                           max=r$page4$lim_T,
                                           step=0.01*r$page4$lim_T
                        )
                      }else if(r$page4$plotRisk==T&r$page4$riskOR==F){
                        r$page4$lim_F<- #reactive({
                          1.1*max(abs(r$page3_addRisk$riskMatrix[,c(input$first_PC,input$second_PC),drop=F]+
                                                 r$page3_addRisk$riskSE[,c(input$first_PC,input$second_PC),drop=F]),
                                           abs(r$page3_addRisk$riskMatrix[,c(input$first_PC,input$second_PC),drop=F]-
                                                 r$page3_addRisk$riskSE[,c(input$first_PC,input$second_PC),drop=F])
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

      riskLim_List2<-reactive({

        list(
            r$page4$plotRisk,
             input$riskLim
        )
      })
      observeEvent(riskLim_List2(),{
        req(input$riskLim)
        req(r$page4$plotRisk)
        if(r$page4$plotRisk==T){
          r$page4$riskLim<-input$riskLim
        }


      })


      riskWhisker_percentage_list<-reactive({
        list(r$page4$plotRisk,
             input$riskWhisker_percentage )
      })
     observeEvent(riskWhisker_percentage_list(),
                  {req(r$page4$plotRisk)
                    req(input$riskWhisker_percentage)
                    if(r$page4$plotRisk==T)
                      r$page4$riskWhisker_percentage<-input$riskWhisker_percentage

                    })

  ###############################################################################

      ###########################################################################################################
      ### Finally co4re
      scores_comp_list4<-reactive({
        list(r$result1$pca_object$scores,
             input$component_limits,
             r$result1$pca_object$loadings,
             input$first_PC,
             input$second_PC,
             r$page2_addCorr,
             r$page3_addRisk,
             r$page4$plotCorr,
             r$page4$plotRisk
             )
        })
      observeEvent( scores_comp_list4(),
                    {
                      if(!is.null(r$result1$pca_object$scores)){
                        r$page4$makeTPO<-makeTPO(r$result1$pca_object$scores,
                                                 r$result1$pca_object$loadings,
                                                 compLimit=input$component_limits,
                                                 hide_question=T)
                        r$page4$makeTPO$scores<-r$page4$makeTPO$scores[,c(input$first_PC, input$second_PC),drop=F]
                        r$page4$makeTPO$loadings<-r$page4$makeTPO$loadings[,c(input$first_PC,  input$second_PC),drop=F]
                        if(!is.null(r$page2_addCorr$corrMatrix)&r$page4$plotCorr==T){
                        r$page4$makeTPO$corrMatrix<-r$page2_addCorr$corrMatrix[,c(input$first_PC,  input$second_PC),drop=F]
                        r$page4$makeTPO$nCorr<-r$page4$makeTPO$nCorr+nrow(r$page2_addCorr$corrMatrix)
                        }
                        if(!is.null(r$page3_addRisk$riskMatrix)&r$page4$plotRisk==T){
                          r$page4$makeTPO$riskMatrix<-r$page3_addRisk$riskMatrix[,c(input$first_PC,  input$second_PC),drop=F]
                          r$page4$makeTPO$nRisk<-r$page4$makeTPO$nRisk+nrow(r$page3_addRisk$riskMatrix)

                          }
                        if(!is.null(r$page3_addRisk$riskSE)&r$page4$plotRisk==T){
                          r$page4$makeTPO$riskSE<-r$page3_addRisk$riskSE[,c(input$first_PC,  input$second_PC),drop=F]
                        }
                        if(!is.null(r$page3_addRisk$riskP)&r$page4$plotRisk==T){
                          r$page4$makeTPO$riskP<-r$page3_addRisk$riskP[,c(input$first_PC,  input$second_PC),drop=F]
                        }
                      }

                    })



      triplot_list<-reactive({list(r$page4$makeTPO,
                               r$data_frame_3_pros31,
                               r$data_frame_5_pros51,
                               r$page4$plotLoads,
                               r$page4$plotScores,
                               r$page4$plotCorr,
                               r$page4$plotRisk,
                               r$page4$LoadCut,
                               r$page4$LoadLim,
                               r$page4$loadLabels,
                               r$page4$loadArrowLength,

                               r$page4$corrLim,
                               r$page4$riskLim,
                               r$page4$riskWhisker_percentage,
                               r$page4$riskOR,
                               r$data_frame_4_pros41,
                               r$data_frame_6_pros61

      )})


      observeEvent(#r$page4$makeTPO,
                   triplot_list(),
                   {
        req(r$page4$makeTPO)
      # req(r$page4$plotLoads)
      #  req(r$page4$plotScores)
      #  req(r$page4$plotCorr)
      #  req(r$page4$plotRisk)
        # req(r$page3$riskORs)
        #req(input$riskWhisker_percentage)
        if(!is.null(r$page4$makeTPO)){
         # r$page4$loadLabelss<-reactive({
        #      ifelse(!is.null(r$page4$loadLabels),
        #         r$page4$loadLabels,
        #         T)
        #  })
          r$page4$plotss<-TriplotGUI(r$page4$makeTPO,
                                     first_PC=input$first_PC, ## The first PC to map
                                     second_PC=input$second_PC, ## The first PC to map
                                     plotLoads= r$page4$plotLoads, ##Whether to plot loadings (TRUE; default) or suppress them (FALSE)
                                     plotScores=r$page4$plotScores, ##Whether to plot scores (TRUE) or suppress them (FALSE; default)F
                                     plotCorr=r$page4$plotCorr,##Whether to plot correlations (TRUE; default) or suppress them (FALSE)
                                     plotRisk=r$page4$plotRisk,
                                     ##For loadings
                                    loadLabels=r$page4$loadLabels,
                                    #ifelse(!is.null(r$page4$loadLabels),
                                    #              r$page4$loadLabels,
                                    #                 F),


                                   ###Whether to plot variable loading labels (TRUE; default) or not (FALSE)
                                   loadArrowLength=r$page4$loadArrowLength,###Length of arrow tip , set it as 0 if you want to remove it
                                     loadCut=r$page4$LoadCut, ###lower limit Loadings below the cut are plotted in light grey and without label
                                     loadLim=r$page4$LoadLim, ##higher limit,Plot range for loadings
                                     ##For correlations
                                     #colCorr,##Color vector for correlations
                                     pchCorr=16, ##Plotting character for correlations
                                     whichCorr=NULL, ##Which correlations to plot (vector of numbers)
                                     corLim=r$page4$corrLim,##Plot range for correlation
                                     ##For risks
                                    riskLim=r$page4$riskLim, #input$riskLim, ##Color vector for risk estimates
                                     pchRisk=15, ##Plotting character for risk estimates
                                     whichRisk=NULL, ##Which risk estimates to plot (vector of numbers)
                                     # colRisk, ##Plot range for risks
                                    riskWhisker_percentage=r$page4$riskWhisker_percentage,## whisker length is how many percentage of confidence interval (This is only for the visualization purpose)
                                  riskOR=r$page4$riskOR,
                                        #  ifelse(!is.null(r$page4$riskOR),
                                        #                r$page4$riskOR,
                                        #                 T),##Specify whether to antilog risk layer scale (useful for log:ed risk estimates) ## Scores
                                     size=3
                                     # scoreLabels=FALSE ##Whether to plot observation score labels (TRUE) or not (FALSE; default)
          )
        }
      },ignoreNULL=F)

      output$showtriplot<-renderUI({
        req(input$button4_act_showplot1)
        renderPlot({r$page4$plotss$triplot})
      })
#######################################################
      ##########download stuff
      observeEvent(r$page4$plotss$triplot,{

        if(!is.null(r$page4$plotss$triplot)){

        output$download4<-downloadHandler(
        filename = function(){
        #  paste( 'iris.csv')
         "shiny_triplot.pdf"
          },

        content = function(file){
        # pdf(file,onefile=T)
        #  r$page4$plotss$triplot

        #  dev.off()
         ggsave(file, r$page4$plotss$triplot,
                width = 11, height = 11, dpi = 300, units = "in")
           }

      )


        }
      })




      ################################################################
      ## test stuff
      ###test stuff
      output$hed<-renderTable({

        req(r$page4$makeTPO$scores)
        #if(!is.null(r$page3_addRisk$riskMatrix)){
          head(r$page4$makeTPO$scores,6)
        #}
      })
      output$hed2<-renderTable({

        req(r$page4$makeTPO$corrMatrix)
        #if(!is.null(r$page3_addRisk$riskMatrix)){
        head(r$page4$makeTPO$corrMatrix,6)
        #}
      })

      output$hed3<-renderTable({

        req(r$page4$makeTPO$riskMatrix)
        #if(!is.null(r$page3_addRisk$riskMatrix)){
        head(r$page4$makeTPO$riskMatrix,6)
        #}
      })
      output$hed4<-renderTable({

        req(r$page4$makeTPO$riskSE)
        #if(!is.null(r$page3_addRisk$riskMatrix)){
        head(r$page4$makeTPO$riskSE,6)
        #}
      })
      output$hed5<-renderTable({

        req(r$page4$makeTPO$riskP)
        #if(!is.null(r$page3_addRisk$riskMatrix)){
        head(r$page4$makeTPO$riskP,6)
        #}
      })




    }
  )



}


