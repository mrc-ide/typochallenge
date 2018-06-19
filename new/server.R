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
  shiny::tagList(
    shiny::p("this is the challenge"),
    shiny::actionButton("end", "Finish", class = "btn-primary"))
}


end_panel <- function() {
  shiny::tagList(
    shiny::p("this is the end"))
}


shiny::shinyServer(
  function(input, output, session) {
    values <- shiny::reactiveValues(
      id = uuid::UUIDgenerate(),
      start_time = Sys.time(),
      survey = NULL,
      data = NULL)

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
      })

    shiny::observeEvent(
      input$end,
      output$typoapp <- shiny::renderUI(end_panel()))

    output$typoapp <- shiny::renderUI(
      start_panel())
  }
)
