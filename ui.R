shiny::shinyUI(
  shiny::fluidPage(
    shiny::tags$style(shiny::HTML(".trophy-1 {color: #80ced6;}")),  
    shiny::tags$style(shiny::HTML(".trophy-2 {color: #82b74b;}")),  
    shinyjs::useShinyjs(),
    shiny::tags$style(shiny::HTML(".footer {color: #666666; line-height: 80%; font-size: 80%;}")),
    shiny::titlePanel("The Typo Challenge"),
    shiny::uiOutput("typoapp"),
    shiny::includeHTML("footer.html")))
