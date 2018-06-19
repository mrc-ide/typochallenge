start_panel <- function() {
  shiny::tagList(
    shiny::includeMarkdown("doc_start.md"),
    shiny::actionButton("survey", "Start the challenge", class = "btn-primary"))
}


survey_panel <- function() {
  shiny::tagList(
    shiny::p("We are going to ask some questions about your computer"),

    shiny::img(src = "layouts.png"),

    shiny::radioButtons(
      "survey_keyboard_layout",
      "Please select your keyboard layout",
      c("AZERTY (bottom image)",
        "QUERTY (top image)",
        "Other"),
      selected = NA),

    shiny::radioButtons(
      "survey_keyboard_input",
      "Do you use the numeric keypad or the row of numbers to enter numbers",
      c("Numeric keypad (purple buttons in image, on right)",
        "Top row (blue bottons in image, on top)",
        "A bit of both, I am uncontrollable"),
      selected = NA),

    shiny::actionButton("challenge", "To the typos!", class = "btn-primary"))
}


challenge_panel <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::textInput("challenge_date", "Type the date", ""),
      ## shiny::fluidRow("Press enter to submit", style = "margin-left: 0px;"),
      shiny::actionButton("submit", "Submit this answer",
                          class = "btn-primary"),
      shiny::hr(),
      shiny::actionButton("end", "End the challenge", class = "btn-danger")),
    shiny::mainPanel(
      shiny::plotOutput("date_image")))
}


end_panel <- function() {
  shiny::tagList(
    shiny::p("this is the end"))
}


next_date <- function() {
  runif(20)
}


render_date <- function(date) {
  shiny::renderPlot(plot(date))
}


shiny::shinyServer(
  function(input, output, session) {
    values <- shiny::reactiveValues(
      id = uuid::UUIDgenerate(),
      start_time = Sys.time(),
      survey = NULL,
      data = NULL,
      date = NULL)

    ## Here's the logic moving through the sections
    shiny::observeEvent(
      input$survey, {
        message(sprintf("Starting survey for %s / %s",
                        values$id, values$start_time))
        output$typoapp <- shiny::renderUI(survey_panel())
      })

    shiny::observeEvent(
      input$challenge, {
        values$survey <- list(input$survey_keyboard_layout,
                              input$survey_keyboard_input)
        output$typoapp <- shiny::renderUI(challenge_panel())
        values$date <- next_date()
      })

    shiny::observeEvent(
      input$submit, {
        message("submitting!")
        values$date <- next_date()
      })

    shiny::observe({
      if (!is.null(values$date)) {
        output$date_image <- render_date(values$date)
      }
    })

    shiny::observeEvent(
      input$end,
      output$typoapp <- shiny::renderUI(end_panel()))

    output$typoapp <- shiny::renderUI(
      ## start_panel())
      ## challenge_panel())
      survey_panel())
  }
)
