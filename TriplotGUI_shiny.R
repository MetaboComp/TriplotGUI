#####  shiny app

library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

server <- function(input,
                   output,
                   session
                   ){
  r<-reactiveValues( ) ##  similar to a list, but with special capabilities for reactive programming.
  r$page1=reactiveValues()
  r$page2=reactiveValues()
  r$page3=reactiveValues()
  r$page4=reactiveValues()

  db1Server("page1",r)
  db2Server("page2",r)
  db3Server("page3",r)
  db4Server("page4",r)
    }

ui <- dashboardPage(

  skin="blue-light",
  dashboardHeader(title="TriplotGUI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PCA plots",
               tabName="page1"),
      menuItem("Correlations with scores",
               tabName="page2"),
      menuItem("Risk Estimates with scores",
               tabName="page3"),
      menuItem("The Triplot",
               tabName="page4")
    )
  ),
  dashboardBody(
    useShinyjs(rmd=TRUE),
    tabItems(       ### plural
      tabItem(      ### singular
        tabName="page1", db1UI("page1")),
      tabItem(      ### singular
        tabName="page2", db2UI("page2")),
      tabItem(      ### singular
        tabName="page3", db3UI("page3")),
      tabItem(      ### singular
        tabName="page4", db4UI("page4"))
    )
  )
)




shinyApp(ui=ui,
         server=server)



