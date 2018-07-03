shiny::shinyUI(
  shiny::fluidPage(
    shiny::tags$style(shiny::HTML(".trophy-1 {color: #80ced6;}")),  
    shiny::tags$style(shiny::HTML(".trophy-2 {color: #82b74b;}")),  
    shiny::tags$style(shiny::HTML(".trophy-3 {color: #eea29a;}")),  
    shiny::tags$style(shiny::HTML(".trophy-legend {color: #A9A9A9;}")),  
    shinyjs::useShinyjs(),
    shiny::tags$style(shiny::HTML(".footer {color: #666666; line-height: 80%; font-size: 80%;}")),
    shiny::titlePanel("The Typo Challenge"),
    shiny::uiOutput("typoapp"),
    shiny::includeHTML("footer.html")))
