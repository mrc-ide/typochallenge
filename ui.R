shiny::shinyUI(
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::titlePanel("The Typo Challenge"),
    shiny::uiOutput("typoapp")))
