source("plot.R")

APP_VERSION <- "1.0.0"
PATH_OUTPUT <- "contributions"

list_countries <- sort(rworldmap::countryExData[ , 2])

cache <- new.env(parent = emptyenv())

read_contributions <- function() {
  d <- lapply(dir(PATH_OUTPUT, pattern = "\\.rds$"), read_contribution)
  i <- vapply(d, is.null, logical(1))
  if (any(i)) {
    d <- d[!i]
    s <- do.call("rbind", d)
    list(total_sum = as.integer(sum(s[, "total"])),
         total_mean = mean(s[, "total"]),
         correct_sum = as.integer(sum(s[, "correct"])),
         correct_mean = mean(s[, "correct"]),
         best_total = as.integer(max(s[, "total"])),
         best_correct = as.integer(max(s[, "correct"])),
         best_best = min(s[, "best"]),
         best_mean = mean(s[, "best"]),
         best_mean = min(s[, "mean"]),
         mean_mean = mean(s[, "mean"]))
  } else {
    NULL
  }
}


read_contribution <- function(p) {
  if (p %in% names(cache)) {
    return(cache[[p]])
  }
  d <- readRDS(file.path(PATH_OUTPUT, p))
  i <- d$data$correct
  if (any(i)) {
    t <- d$data$elapsed[i]
    ret <- c(total = length(i),
             correct = sum(i),
             best = min(t),
             mean = mean(t))
  } else {
    ret <- NULL
  }
  cache[[p]] <- ret
  ret
}


start_panel <- function() {
  shiny::tagList(
    shiny::includeHTML("overview.html"),
    shiny::p("To start, click on 'Start the challenge'"),
    shiny::p(shiny::tags$b("Good luck!")),
    shiny::actionButton("survey", "Start the challenge", class = "btn-primary"),
    shiny::includeHTML("doc_sharing.html"))
}


survey_panel <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      shiny::h4("A few questions before starting..."),
      
      shiny::h6("We ask these questions so that we can better understand why typos are made."), 
      
      shiny::textInput("today", "What is today's date?", value = "", 
                       width = NULL, placeholder = NULL),
      
      shiny::radioButtons(
        "gender",
        "Please select your gender",
        c("Prefer not to say",
          "Male",
          "Female",
          "Other"),
        selected = NA),
      
      shiny::selectInput("year_birth", "Please select your year of birth:", 
                         choices = c("", 2018:1900),
                         selectize = TRUE),
      
      shiny::selectInput("country_from", "Where are you from?", 
                            choices = c("", 
                                        list_countries),
                         selectize = TRUE),
      
      shiny::selectInput("country_residence", "Where do you currently live?", 
                         choices = c("", 
                                     list_countries),
                         selectize = TRUE),
      
      shiny::selectInput("device", "Which device are you using to do the challenge?", 
                         choices = c("", 
                                     list("Computer", 
                                          "Tablet", 
                                          "Mobile phone")),
                         selectize = TRUE),
      
      shiny::radioButtons(
        "survey_keyboard_layout",
        "Please select your keyboard layout (see image on right panel)",
        c("AZERTY (top)",
          "QWERTY (bottom)",
          "Other"),
        selected = NA),
      
      shiny::radioButtons(
        "survey_keyboard_input",
        "Do you use the numeric keypad or the row of numbers to enter numbers (see image on right panel)",
        c("Top row (blue keys, top of keyboard)",
          "Numeric keypad (purple keys, right of keyboard)",
          "A bit of both"),
        selected = NA),
      
      shiny::hr(),
      shiny::actionButton("instructions", "To the typos!", class = "btn-primary")),
    shiny::mainPanel(
      shiny::img(src = "layouts.png")))
}

instructions_panel <- function() {
  shiny::tagList(
    shiny::p("You have not yet started the challenge"),
    shiny::includeHTML("instructions.html"),
    shiny::p("To start the challenge, click on 'Start the challenge'"),
    shiny::p(shiny::tags$b("Good luck!")),
    shiny::actionButton("challenge", "Start the challenge", class = "btn-primary"),
    shiny::includeHTML("doc_sharing.html"))
}


challenge_panel <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::textInput("challenge_date", "Type the date", ""),
      ## shiny::fluidRow("Press enter to submit", style = "margin-left: 0px;"),
      shiny::actionButton("challenge_submit", "Submit this answer",
                          class = "btn-primary"),
      shiny::hr(),
      shiny::includeHTML("instructions_short.html"),
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
    shiny::includeHTML("doc_end.html"),
    shiny::actionButton("continue", "Continue challenge?",
                        shiny::icon("play"),
                        class = "btn-success",
                        title = "Continue, keeping your statistics"),
    shiny::actionButton("retry", "Retry challenge?",
                        shiny::icon("refresh"),
                        class = "btn-primary",
                        title = "Retry challenge (but keep survey data)"),
    shiny::actionButton("new_user", "New user",
                        shiny::icon("user-plus"),
                        class = "btn-danger",
                        title = "Start as new user"),
    shiny::includeHTML("doc_sharing.html"))
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


render_prev <- function(prev, data, global) {
  if (!is.null(prev)) {
    ## common <- sprintf("You have entered %s / %s correctly",
    ##                   stats$correct, stats$total)
    date_real <- format(prev$date, "%d/%m/%Y")
    if (prev$correct) {
      title <- "Last entry was correct"
      body_feedback <- sprintf("You entered '%s' correctly", date_real)
      type <- "success"
      icon <- "check"
    } else {
      title <- "Typo in previous entry"
      body_feedback <- sprintf("You entered '%s' but the real date was '%s'",
                               prev$user, date_real)
      type <- "danger"
      icon <- "times"
    }
    
    n_entered <- length(data$rows)
    if (is.null(data$time_best)) {
      body_stats <- sprintf(
        "You have not recorded any correct dates (out of %d %s)",
        n_entered, ngettext(n_entered, "try", "tries"))
    } else {
      s1 <- sprintf(
        "You have recorded %d correct %s (out of %d %s).\n",
        data$n_correct, ngettext(data$n_correct, "date", "dates"),
        n_entered, ngettext(n_entered, "try", "tries"))
      s2 <- sprintf(
        "This answer: %ss, best time, %ss, average %ss.\n",
        round(prev$elapsed, 2),
        round(data$time_best, 2),
        round(data$time_total / n_entered, 2))
      body_stats <- lapply(list(s1, s2), shiny::p)
    }

    if (is.null(global)) {
      all_time_stats <- "You are the first to take the challenge!"
    } else {
      s3 <- sprintf(
        "Average number of entries: %s dates, %s correct.",
        round(global$total_mean, 2), round(global$correct_mean, 2))
      s4 <- sprintf(
        "Speed:  %ss fastest, %ss average.",
        round(global$best_mean, 2), round(global$mean_mean, 2))
      all_time_stats <- lapply(list(s3, s4), shiny::p)
    }
    
    shiny::div(
      class = "panel-group",
      shiny::div(
        class = sprintf("panel panel-%s", type),
        shiny::div(class = "panel-heading",
                   shiny::icon(paste(icon, "fa-lg")),
                   title),
        shiny::div(class = "panel-body", body_feedback)),
      shiny::div(
        class = "panel panel-info",
        shiny::div(class = "panel-heading",
                   shiny::icon(paste("cog", "fa-lg")),
                   "Your statistics"),
        shiny::div(class = "panel-body", body_stats)),
      shiny::div(
        class = "panel panel-info",
        shiny::div(class = "panel-heading",
                   shiny::icon(paste("cogs", "fa-lg")),
                   "All time statistics"),
        shiny::div(class = "panel-body", all_time_stats)))
  }
}


update_data <- function(prev, data) {
  if (prev$correct) {
    if (is.null(data$time_best)) {
      data$time_best <- prev$elapsed
      data$time_total <- prev$elapsed
      data$n_correct <- 1L
    } else {
      data$time_best <- min(prev$elapsed, data$time_best)
      data$time_total <- prev$elapsed + data$time_total
      data$n_correct <- data$n_correct + 1L
    }
  }
  
  prev$date <- format(prev$date, "%d/%m/%Y")
  data$rows <- c(data$rows, list(prev[names(data_cols)]))
  data
}

init_data <- function(values) {
  values$id <- uuid::UUIDgenerate()
  values$start_time <- Sys.time()
  values$survey <- NULL
  values$timestamp <- NULL
  values$date <- NULL
  values$prev <- NULL
  values$data <- list()
  values$global <- read_contributions()
  message(sprintf("Starting session: '%s'", values$id))
}

shiny::shinyServer(
  function(input, output, session) {
    values <- shiny::reactiveValues(
      id = NULL, start_time = NULL, survey = NULL, timestamp = NULL,
      date = NULL, prev = NULL, data = NULL, global = NULL)
    
    ## Here's the logic moving through the sections
    shiny::observeEvent(
      input$survey, {
        init_data(values)
        output$typoapp <- shiny::renderUI(survey_panel())
      })
    
    shiny::observeEvent(
      input$instructions, {
        output$typoapp <- shiny::renderUI(instructions_panel())
      })
    
    shiny::observeEvent(
      input$challenge, {
        values$survey <- list(
          gender = input$gender, 
          year_birth = input$year_birth,
          country_from = input$country_from,
          country_residence = input$country_residence,
          device = input$device,
          keyboard_layout = input$survey_keyboard_layout,
          keyboard_input = input$survey_keyboard_input)
        output$typoapp <- shiny::renderUI(challenge_panel())
        values$date <- new_date()
      })
    
    shiny::observeEvent(
      input$challenge_submit, {
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
        output$date_prev <- shiny::renderUI(
          render_prev(values$prev, values$data, values$global))
      }
    })
    
    shiny::observeEvent(
      input$end, {
        save_data(values, TRUE, PATH_OUTPUT)
        output$typoapp <- shiny::renderUI(end_panel())
      })
    
    shiny::observeEvent(
      input$new_user, {
        output$typoapp <- shiny::renderUI(start_panel())
      })

    shiny::observeEvent(
      input$retry, {
        last <- values$last
        init_data(values)
        values$survey <- last$survey
        values$id_parent <- last$id_parent %||% last$id
        output$typoapp <- shiny::renderUI(challenge_panel())
        values$date <- new_date()
      })

    shiny::observeEvent(
      input$continue, {
        restore_data(values)
        output$typoapp <- shiny::renderUI(challenge_panel())
        values$date <- new_date()
      })

    output$typoapp <- shiny::renderUI(start_panel())
    
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


nms_save <- c("id", "start_time", "survey", "continued", "data", "id_parent")


save_data <- function(values, clean_exit, path) {
  if (!is.null(values$data)) {
    message(sprintf("Saving data for '%s'", values$id))
    dir.create(path, FALSE, TRUE)
    values$last <- setNames(lapply(nms_save, function(v) values[[v]]),
                            nms_save)
    ret <- list(id = values$id,
                id_parent = values$id_parent,
                start_time = values$start_time,
                app_version = APP_VERSION,
                clean_exit = clean_exit,
                survey = values$survey,
                continued = !isTRUE(values$continued),
                data = data_to_table(values$data))
    dest <- file.path(path, sprintf("%s.rds", ret$id))
    saveRDS(ret, dest)
    values$data <- NULL
  }
}


restore_data <- function(values) {
  last <- values$last
  message(sprintf("Restoring data for '%s'", last$id))
  for (v in nms_save) {
    values[[v]] <- last[[v]]
  }
  values$continued <- TRUE
}


`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
