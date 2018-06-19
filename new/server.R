source("plot.R")

APP_VERSION <- "1.0.0"
PATH_OUTPUT <- "contributions"

start_panel <- function() {
  shiny::tagList(
    shiny::includeMarkdown("doc_start.md"),
    shiny::actionButton("survey", "Start the challenge", class = "btn-primary"))
}


survey_panel <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::radioButtons(
        "survey_keyboard_layout",
        "Please select your keyboard layout",
        c("AZERTY (top image)",
          "QUERTY (bottom image)",
          "Other"),
        selected = NA),

      shiny::radioButtons(
        "survey_keyboard_input",
        "Do you use the numeric keypad or the row of numbers to enter numbers",
        c("Top row (blue keys in image, on top)",
          "Numeric keypad (purple keys in image, on right)",
          "A bit of both, I am uncontrollable"),
        selected = NA),

      shiny::hr(),
      shiny::actionButton("challenge", "To the typos!", class = "btn-primary")),
    shiny::mainPanel(
      shiny::img(src = "layouts.png")))
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
      shiny::tags$script(
        '$("#challenge_date").keydown(function(event) {
      if (event.keyCode === 13) { $("#challenge_submit").click(); }});'),
      shiny::plotOutput("date_image"),
      shiny::uiOutput("date_prev")))
}


end_panel <- function() {
  shiny::tagList(
    shiny::includeMarkdown("doc_end.md"))
}


validate_date <- function(user_date, real_date, start_time) {
  real_date$elapsed <- as.numeric(Sys.time() - start_time, "secs")
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


update_data <- function(prev, data) {
  keep <- names(data_cols)
  if (is.null(data$time_best)) {
    time_best <- prev$elapsed
    time_total <- time_best
  } else {
    time_best <- min(prev$elapsed, data$best)
    time_total <- prev$elapsed + data$time_total
  }
  prev$date <- format(prev$date, "%d/%m/%Y")

  list(rows = c(data$rows, list(prev[keep])),
       time_best = time_best,
       time_total = time_total)
}

shiny::shinyServer(
  function(input, output, session) {
    values <- shiny::reactiveValues(
      ## Session identifier
      id = uuid::UUIDgenerate(),
      start_time = Sys.time(),
      ## Data on the survey
      survey = NULL,
      ## Used for tracking how long things take
      timestamp = NULL,
      ## Current challenge date
      date = NULL,
      ## Last challenge date and validation
      prev = NULL,
      ## All survey data so far
      data = list())

    ## Here's the logic moving through the sections
    shiny::observeEvent(
      input$survey, {
        message(sprintf("Starting survey for %s / %s",
                        values$id, values$start_time))
        output$typoapp <- shiny::renderUI(survey_panel())
      })

    shiny::observeEvent(
      input$challenge, {
        values$survey <- list(
          keyboard_layout = input$survey_keyboard_layout,
          keyboard_input = input$survey_keyboard_input)
        output$typoapp <- shiny::renderUI(challenge_panel())
        values$date <- new_date()
      })

    shiny::observeEvent(
      input$challenge_submit, {
        message("submitting!")
        isolate({
          values$prev <- validate_date(input$challenge_date, values$date,
                                       values$timestamp)
          values$data <- update_data(values$prev, values$data)
          values$date <- new_date()
        })
      })

    shiny::observe({
      if (!is.null(values$date)) {
        date <- values$date
        shiny::updateTextInput(session, "challenge_date", value = "")
        output$date_image <- shiny::renderPlot(
          plot_date(date), width = date$width, height = date$height)
        values$timestamp <- Sys.time()
      }
    })

    shiny::observe({
      if (!is.null(values$prev)) {
        output$date_prev <- shiny::renderUI(render_prev(values$prev))
      }
    })

    shiny::observeEvent(
      input$end, {
        save_data(values, TRUE, PATH_OUTPUT)
        output$typoapp <- shiny::renderUI(end_panel())
      })

    output$typoapp <- shiny::renderUI(
      ##start_panel())
      ## challenge_panel())
      survey_panel())

    session$onSessionEnded(function() {
      isolate({
        message(sprintf("Detected session closed for '%s'", values$id))
        save_data(values, FALSE, PATH_OUTPUT)
        output$typoapp <- shiny::renderUI(end_panel())
      })
    })
  }
)

data_cols <- list(date = character(1),
                  user = character(1),
                  date_format = character(1),
                  elapsed = numeric(1),
                  correct = logical(1))


data_to_table <- function(data) {
  rows <- Map(
    function(name, type) vapply(data$rows, "[[", type, name),
    names(data_cols), unname(data_cols))
  as.data.frame(rows, stringsAsFactors = FALSE)
}


save_data <- function(values, clean_exit, path) {
  if (!is.null(values$data)) {
    message(sprintf("Saving data for '%s'", values$id))
    dir.create(path, FALSE, TRUE)
    ret <- list(id = values$id,
                start_time = values$start_time,
                app_version = APP_VERSION,
                clean_exit = clean_exit,
                survey = values$survey,
                data = data_to_table(values$data))
    dest <- file.path(path, sprintf("%s.rds", ret$id))
    saveRDS(ret, dest)
    values$data <- NULL
  }
}
