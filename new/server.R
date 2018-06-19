source("plot.R")

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
      shiny::actionButton("challenge_submit", "Submit this answer",
                          class = "btn-primary"),
      shiny::hr(),
      shiny::actionButton("end", "End the challenge", class = "btn-danger")),
    shiny::mainPanel(
      shiny::plotOutput("date_image"),
      shiny::uiOutput("date_prev")))
}


end_panel <- function() {
  shiny::tagList(
    shiny::includeMarkdown("doc_end.md"))
}


validate_date <- function(user_date, real_date) {
  real_date$user <- user_date
  real_date$correct <- check_date(user_date, real_date$date)
  real_date
}


check_date <- function(typed_date, date) {
  isTRUE(as.Date(typed_date, format = "%d/%m/%Y") == date)
}


render_prev <- function(prev, stats) {
  if (!is.null(prev)) {
    ## common <- sprintf("You have entered %s / %s correctly",
    ##                   stats$correct, stats$total)
    date_real <- format(prev$date, "%d/%m/%Y")
    if (prev$correct) {
      title <- "Last entry was correct"
      body <- sprintf("You entered '%s' correctly", date_real)
      type <- "success"
    } else {
      title <- "Typo in previous entry"
      body <- sprintf("You entered '%s' but the real date was '%s'",
                      prev$user, date_real)
      type <- "danger"
    }
    shiny::div(
      class = sprintf("panel panel-%s", type),
      shiny::div(class = "panel-heading",title),
      shiny::div(class = "panel-body", body))
  }
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
        values$date <- new_date()
      })

    shiny::observeEvent(
      input$challenge_submit, {
        message("submitting!")
        isolate({
          values$prev <- validate_date(input$challenge_date, values$date)
          values$date <- new_date()
        })
      })

    shiny::observe({
      if (!is.null(values$date)) {
        date <- values$date
        output$date_image <- shiny::renderPlot(
          plot_date(date), width = date$width, height = date$height)
      }
    })

    shiny::observe({
      if (!is.null(values$prev)) {
        output$date_prev <- shiny::renderUI(render_prev(values$prev))
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
