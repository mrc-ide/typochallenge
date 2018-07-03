shiny::shinyUI(
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::tags$style(shiny::HTML(".footer {color: #666666; line-height: 80%; font-size: 80%;}")),
    shiny::titlePanel("The Typo Challenge"),
    shiny::uiOutput("typoapp"),
    shiny::includeHTML("footer.html")))
