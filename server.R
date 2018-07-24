source("plot.R")
source("functions.R")

APP_VERSION <- "1.0.0"
PATH_OUTPUT <- "contributions"
DEVEL_VERSION <- FALSE
COUNTRIES <- readLines("countries.txt")

cache <- new.env(parent = emptyenv())
click_js <- readChar("click.js", file.size("click.js"))


start_panel <- function() {
  shiny::tagList(
    shiny::includeHTML("include/overview.html"),
    shiny::actionButton("consent", "Begin!",
                        shiny::icon("play"), class = "btn-primary"),
    shiny::includeHTML("include/doc_sharing.html"))
}


consent_panel <- function() {
  consent_msg <- shiny::strong("By ticking this box, you are confirming that you are 18 years old or older, and you are agreeing that we can store and analyse your data, and we can make this data publically available.")
  shiny::tagList(
    shiny::includeHTML("include/consent.html"),
    shiny::checkboxInput("consent_tick", consent_msg, 
                         value = FALSE, 
                         width = '100%'),
    shiny::actionButton("survey", "Next step",
                        shiny::icon("play"), class = "btn-primary"),
    shiny::includeHTML("include/doc_sharing.html"))
}


survey_panel <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      shiny::h4("A few questions before starting..."),
      
      shiny::h6("We ask these questions so that we can better understand why typos are made."), 
      
      shiny::textInput("today", "What is today's date?", value = "", 
                       width = NULL, placeholder = NULL),
      
      shiny::selectInput("gender", "Please select your gender", 
                         choices = c("", list("Male",
                                              "Female",
                                              "Other")),
                         selectize = TRUE),
      
      shiny::selectInput("year_birth", "Please select your year of birth:", 
                         choices = c("", 2018:1900),
                         selectize = TRUE),
      
      shiny::selectInput("country_from", "Where are you from?", 
                         choices = c("", 
                                     COUNTRIES),
                         selectize = TRUE),
      
      shiny::selectInput("country_residence", "Where do you currently live?", 
                         choices = c("", 
                                     COUNTRIES),
                         selectize = TRUE),
      
      shiny::selectInput("device",
                         "Which device are you using to do the challenge?",
                         choices = c("", 
                                     list("Computer", 
                                          "Tablet", 
                                          "Other")),
                         selectize = TRUE),
      
      shiny::selectInput("survey_keyboard_layout",
                         "Please select your keyboard layout (see image on right panel)",
                         choices = c("", 
                                     list("AZERTY (top)",
                                          "QWERTY (bottom)",
                                          "Other")),
                         selectize = TRUE),
      
      shiny::selectInput("survey_keyboard_input",
                         "Do you use the numeric keypad or the row of numbers to enter numbers (see image on right panel)",
                         choices = c("", 
                                     list("Numeric keypad (purple keys, right of keyboard)",
                                          "Top row (blue keys, top of keyboard)",
                                          "A bit of both")),
                         selectize = TRUE),
      
      shiny::hr(),
      shiny::actionButton("instructions", "To the typos!", class = "btn-primary")),
    shiny::mainPanel(
      shiny::img(src = "layouts_cropped.png")))
}


instructions_panel <- function() {
  shiny::tagList(
    shiny::p("You have not yet started the challenge"),
    shiny::includeHTML("include/instructions.html"),
    shiny::p(
      "To start the challenge, click on the 'Start the challenge' button"),
    shiny::p(shiny::tags$b("Good luck!")),
    shiny::actionButton("challenge", "Start the challenge",
                        class = "btn-primary"),
    shiny::includeHTML("include/doc_sharing.html"))
}


challenge_panel <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::textInput("challenge_date", "Type the date", ""),
      shiny::actionButton("challenge_submit", "Submit this answer",
                          class = "btn-primary"),
      if (DEVEL_VERSION) {
        shiny::actionButton("devel_correct_date", "CHEAT!",
                            class = "btn-warning")
      },
      shiny::hr(),
      shiny::includeHTML("include/instructions_short.html"),
      shiny::hr(),
      shiny::actionButton("end", "End the challenge", class = "btn-danger")),
    shiny::mainPanel(
      shiny::tags$script(click_js),
      shiny::plotOutput("date_image"),
      shiny::uiOutput("date_prev")))
}


end_panel <- function(id, data, global) {
  statistics <- panel_statistics(data, global)
  
  withdrawal_title <- "Important information about the data you shared today"
  withdrawal_msg <- paste("If you have changed your mind and do not want us",
                          "to store and use your data today, please tick this",
                          "box and click on 'withdraw my contribution' below",
                          "and your data will be discarded automatically.")
  subscribe_txt <- paste("Keep informed about the progress of this study",
                         "by giving us your email address.  We will store",
                         "this in a way that is totally separate, and not",
                         "relateable to your survey data. If you provide",
                         "an address we will email when we produce research",
                         "outputs based on the data that you have provided",
                         "today.")
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::div(
        class = "panel-group",
        shiny::actionButton("continue", "Continue?",
                            shiny::icon("play"),
                            class = "btn-success",
                            title = "Continue, keeping your statistics"),
        shiny::actionButton("retry", "Retry?",
                            shiny::icon("refresh"),
                            class = "btn-primary",
                            title = "Retry challenge (but keep survey data)"),
        shiny::actionButton("new_user", "New user",
                            shiny::icon("user-plus"),
                            class = "btn-danger",
                            title = "Start as new user")),
      statistics$trophies,
      statistics$user,
      statistics$global),
    shiny::mainPanel(
      shiny::includeHTML("include/doc_end.html"),
      shiny::h4(withdrawal_title),
      shiny::tagList(
        shiny::checkboxInput("withdrawal_tick",
                             shiny::strong(withdrawal_msg),
                             value = FALSE,
                             width = "100%")),
      shiny::actionButton("withdraw", "Withdraw my contribution",
                          shiny::icon("trash"),
                          class = "btn-danger"),
      shiny::br(""),
      shiny::p(subscribe_txt),
      shiny::textInput("email", "Enter your email address", placeholder = "you@server.com"),
      shiny::textInput("email_confirm", "Confirm your email address", placeholder = "you@server.com"),
      shiny::actionButton("subscribe", "Subscribe",
                          shiny::icon("envelope"),
                          class = "btn-primary"),
      shiny::br(""),
      shiny::includeHTML("include/doc_sharing.html")))
}


end_panel_withdrawn <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::actionButton("new_user", "New user",
                          shiny::icon("user-plus"),
                          class = "btn-danger",
                          title = "Start as new user")),
    shiny::mainPanel(
      shiny::includeHTML("include/doc_end_2.html"),
      shiny::br(""),
      shiny::includeHTML("include/doc_sharing.html")))
}


validate_date <- function(user_date, real_date, start_time) {
  real_date$elapsed <- as.numeric(Sys.time() - start_time, "secs")
  real_date$user <- user_date
  real_date$correct <- check_date(user_date, real_date$date)
  real_date
}

as_date <- function(x) {
  re <- "^\\s*[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}\\s*$"
  if (grepl(re, x)) {
    as.Date(x, format = "%d/%m/%Y")
  } else {
    as.Date(NA)
  }
}


check_date <- function(typed_date, date) {
  isTRUE(as_date(typed_date) == date)
}


render_prev <- function(prev, data, global) {
  if (!is.null(prev)) {
    ## common <- sprintf("You have entered %s / %s correctly",
    ##                   stats$correct, stats$total)
    date_real <- format(prev$date, "%d/%m/%Y")
    if (prev$correct) {
      title <- "Last entry was correct"
      body_feedback <- sprintf("You entered '%s' correctly in %ss",
                               date_real, round(prev$elapsed, 2))
      type <- "success"
      icon <- "check"
    } else {
      title <- "Typo in previous entry"
      body_feedback <- sprintf("You entered '%s' but the real date was '%s'",
                               prev$user, date_real)
      type <- "danger"
      icon <- "times"
    }
    
    statistics <- panel_statistics(data, global)
    feedback <- shiny::div(
      class = sprintf("panel panel-%s", type),
      shiny::div(class = "panel-heading",
                 shiny::icon(paste(icon, "fa-lg")),
                 title),
      shiny::div(class = "panel-body", body_feedback))
    
    shiny::fluidRow(
      shiny::column(6,
                    shiny::div(
                      class = "panel-group",
                      shiny::div(
                        feedback,
                        statistics$trophies))),
      shiny::column(6,
                    shiny::div(
                      class = "panel-group",
                      shiny::div(
                        statistics$user,
                        statistics$global))))
  }
}


panel_statistics <- function(data, global) {
  n_entered <- length(data$rows)
  n_correct <- data$n_correct
  n_correct_fast <- data$n_correct_fast
  if (is.null(data$time_best)) {
    body_stats <- sprintf(
      "You have not recorded any correct dates (out of %d %s)",
      n_entered, ngettext(n_entered, "try", "tries"))
  } else {
    s1_bold <- "Number of entries: "
    
    s2_bold <- "Speed (per correct entry): "
    s2 <- sprintf(
      "fastest %ss, average %ss.\n",
      round(data$time_best, 2),
      round(data$time_total / n_correct, 2))
    body_bold <- lapply(list(s1_bold, s2_bold), shiny::strong)
    args <- list(
      shiny::span(
        class = "trophy-1",
        shiny::icon(paste("cog", "fa-lg"))), 
      shiny::strong("All: "), 
      sprintf(
        "%s",
        n_entered))
    body_1 <- do.call(shiny::p, args)
    args <- list(
      shiny::span(
        class = "trophy-2",
        shiny::icon(paste("cog", "fa-lg"))), 
      shiny::strong("Correct: "), 
      sprintf(
        "%s",
        n_correct))
    body_2 <- do.call(shiny::p, args)
    args <- list(
      shiny::span(
        class = "trophy-3",
        shiny::icon(paste("cog", "fa-lg"))), 
      shiny::strong("Correct in <5s: "), 
      sprintf(
        "%s",
        n_correct_fast))
    body_3 <- do.call(shiny::p, args)
    
    body_stats <- list(body_bold[[1]], body_1, body_2, body_3, 
                       body_bold[[2]], shiny::p(s2))
  }
  
  trophy_levels <- c(0,
                     if (DEVEL_VERSION) 1, 2,
                     5, 10, 50, 100, 500, 1000, 5000)
  trophy_entered <- max(which(n_entered >= trophy_levels)) - 1L
  if (any(n_correct >= trophy_levels)) {
    trophy_correct <- max(which(n_correct >= trophy_levels)) - 1L
  } else {
    trophy_correct <- 0L
  }
  if (any(n_correct_fast >= trophy_levels)) {
    trophy_correct_fast <- max(which(n_correct_fast >= trophy_levels)) - 1L
  } else {
    trophy_correct_fast <- 0L
  }
  if (trophy_correct == 0) {
    trophies <- "Keep going to get your first trophy!"
  } else {
    s6 <- shiny::strong("All: ")
    args <- list()
    args[[1]] <- s6
    args2 <- lapply(seq_len(trophy_entered), function(e)
      shiny::span(
        class = "trophy-1",
        shiny::icon(paste("trophy", "fa-lg"))))
    args[seq(2, length(args2) + 1)] <- args2
    s6_troph <- do.call(shiny::p, args)
    
    s7 <- shiny::strong("Correct: ")
    args <- list()
    args[[1]] <- s7
    args2 <- lapply(seq_len(trophy_correct), function(e)
      shiny::span(
        class = "trophy-2",
        shiny::icon(paste("trophy", "fa-lg"))))
    args[seq(2, length(args2) + 1)] <- args2
    s7_troph <- do.call(shiny::p, args)
    
    if (trophy_correct_fast > 0) {
      s8 <- shiny::strong("Correct in <5s: ")
      args <- list()
      args[[1]] <- s8
      args2 <- lapply(seq_len(trophy_correct_fast), function(e)
        shiny::span(
          class = "trophy-3",
          shiny::icon(paste("trophy", "fa-lg"))))
      args[seq(2, length(args2) + 1)] <- args2
      s8_troph <- do.call(shiny::p, args)
      trophies <- lapply(list(s6_troph, s7_troph, s8_troph), shiny::p)
    } else {
      trophies <- lapply(list(s6_troph, s7_troph), shiny::p)
    }
    
    ### Adding legend ###
    trophies[[length(trophies) + 1]] <- shiny::strong("Levels:")
    for(i in seq_len(length(trophy_levels)-1)) {
      leg1 <- sprintf("%s x ", i)
      leg2 <- sprintf(" = %s+ entries", trophy_levels[i+1])
      args <- list()
      args[[1]] <- leg1
      args[[2]]  <- shiny::span(
        class = "trophy-legend",
        shiny::icon(paste("trophy", "fa-lg"))) 
      args[[3]] <- leg2
      legend <- do.call(shiny::p, args)
      
      trophies[[length(trophies) + 1]] <- legend
    }
    
  }
  
  if (is.null(global)) {
    all_time_stats <- "You are the first to take the challenge!"
  } else {
    s3_bold <- "Number of entries: "
    s3 <- "total (average / best per contributor)"
    s4_bold <- "Speed (per correct entry): "
    s4 <- sprintf(
      "fastest %ss, average %ss.\n",
      round(global$best_mean, 2), 
      round(global$mean_mean, 2))
    all_time_bold <- lapply(list(s3_bold, s4_bold), shiny::strong)
    
    args <- list(
      shiny::span(
        class = "trophy-1",
        shiny::icon(paste("cogs", "fa-lg"))), 
      shiny::strong("All: "), 
      sprintf(
        "%s (%s / %s)",
        round(global$total_sum, 2), 
        round(global$total_mean, 2),
        round(global$total_max, 2)))
    all_time_1 <- do.call(shiny::p, args)
    args <- list(
      shiny::span(
        class = "trophy-2",
        shiny::icon(paste("cogs", "fa-lg"))), 
      shiny::strong("Correct: "), 
      sprintf(
        "%s (%s / %s)",
        round(global$correct_sum, 2),
        round(global$correct_mean, 2),
        round(global$correct_max, 2)))
    all_time_2 <- do.call(shiny::p, args)
    args <- list(
      shiny::span(
        class = "trophy-3",
        shiny::icon(paste("cogs", "fa-lg"))), 
      shiny::strong("Correct in <5s: "), 
      sprintf(
        "%s (%s / %s)",
        round(global$correct_less_5s_sum, 2),
        round(global$correct_less_5s_mean, 2),
        round(global$correct_less_5s_max, 2))
      
    )
    all_time_3 <- do.call(shiny::p, args)
    
    all_time_stats <- list(all_time_bold[[1]], shiny::p(s3),
                           all_time_1, all_time_2, all_time_3, 
                           all_time_bold[[2]], shiny::p(s4))
  }
  
  list(user = shiny::div(
    class = "panel panel-info",
    shiny::div(class = "panel-heading",
               shiny::icon(paste("cog", "fa-lg")),
               "Your statistics"),
    shiny::div(class = "panel-body", body_stats)),
    
    global = shiny::div(
      class = "panel panel-info",
      shiny::div(class = "panel-heading",
                 shiny::icon(paste("cogs", "fa-lg")),
                 "All time statistics (excluding this session)"),
      shiny::div(class = "panel-body", all_time_stats)),
    
    trophies = shiny::div(
      class = "panel panel-info",
      shiny::div(class = "panel-heading",
                 shiny::icon(paste("trophy", "fa-lg")),
                 "Your achievements"),
      shiny::div(class = "panel-body", trophies)))
}


update_data <- function(prev, data) {
  if (prev$correct) {
    fast_entry <- prev$elapsed < 5
    if (is.null(data$time_best)) {
      data$time_best <- prev$elapsed
      data$time_total <- prev$elapsed
      data$n_correct <- 1L
      data$n_correct_fast <- if (fast_entry) 1L else 0L
    } else {
      data$time_best <- min(prev$elapsed, data$time_best)
      data$time_total <- prev$elapsed + data$time_total
      data$n_correct <- data$n_correct + 1L
      if (fast_entry) {
        data$n_correct_fast <- data$n_correct_fast + 1L
      }
    }
  }
  prev$date <- format(prev$date, "%d/%m/%Y")
  data$rows <- c(data$rows, list(prev[names(data_cols)]))
  data
}


init_data <- function(values) {
  values$id <- uuid::UUIDgenerate()
  values$parent_id <- NULL
  values$start_time <- Sys.time()
  values$survey <- NULL
  values$timestamp <- NULL
  values$date <- NULL
  values$prev <- NULL
  values$data <- list()
  values$global <- read_contributions(cache = cache)
  message(sprintf("Starting session: '%s'", values$id))
}


shiny::shinyServer(
  function(input, output, session) {
    values <- shiny::reactiveValues(
      id = NULL, start_time = NULL, survey = NULL, timestamp = NULL,
      date = NULL, prev = NULL, data = NULL, global = NULL)
    
    ## Here's the logic moving through the sections
    
    shiny::observeEvent(
      input$consent, {
        shinyjs::disable("consent")
        output$typoapp <- shiny::renderUI(consent_panel())
      })
    
    shiny::observeEvent(
      input$survey, {
        if (input$consent_tick) {
          shinyjs::disable("survey")
          init_data(values)
          output$typoapp <- shiny::renderUI(survey_panel())
        } else {
          message("User did not consent")
          showModal(modalDialog(
            "You need to tick the consent box to continue",
            title = "Warning",
            easyClose = TRUE))
        }
      })
    
    shiny::observeEvent(
      input$instructions, {
        shinyjs::disable("instructions")
        output$typoapp <- shiny::renderUI(instructions_panel())
      })
    
    shiny::observeEvent(
      input$challenge, {
        shinyjs::disable("challenge")
        values$survey <- list(
          gender = input$gender, 
          year_birth = input$year_birth,
          country_from = input$country_from,
          country_residence = input$country_residence,
          device = input$device,
          keyboard_layout = input$survey_keyboard_layout,
          keyboard_input = input$survey_keyboard_input, 
          today = input$today)
        output$typoapp <- shiny::renderUI(challenge_panel())
        values$date <- new_date()
      })
    
    shiny::observeEvent(
      input$challenge_submit, {
        shinyjs::disable("challenge_submit")
        isolate({
          values$prev <- validate_date(input$challenge_date, values$date,
                                       values$timestamp)
          values$data <- update_data(values$prev, values$data)
          values$date <- new_date()
        })
        session$sendCustomMessage("focus_challenge_date", "")
      })
    
    if (DEVEL_VERSION) {
      shiny::observeEvent(
        input$devel_correct_date, {
          correct <- format(values$date$date, format = "%d/%m/%Y")
          shiny::updateTextInput(session, "challenge_date",
                                 value = correct)
        })
    }
    
    shiny::observe({
      if (!is.null(values$date)) {
        date <- values$date
        shiny::updateTextInput(session, "challenge_date", value = "")
        output$date_image <- shiny::renderPlot(
          plot_date(date), width = date$width, height = date$height)
        shinyjs::enable("challenge_submit")
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
        shinyjs::disable("end")
        shiny::isolate({
          data <- values$data
          id <- values$id
          output$typoapp <- shiny::renderUI(end_panel(id, data, values$global))
          save_data(values, TRUE, PATH_OUTPUT)
        })
      })
    
    shiny::observeEvent(
      input$subscribe, {
       # browser()
        
        if(are_emails_equal(input$email, input$email_confirm) & is_valid_email(input$email))
        {
          showModal(modalDialog(
            shiny::includeHTML("include/gdpr.html"),
            title = "Please confirm you want to subscribe",
            easyClose = TRUE,
            footer = shiny::tagList(
              shiny::actionButton("confirm_subscribe",
                                  "subscribe",
                                  shiny::icon("envelope"),
                                  class = "btn-primary",
                                  title = "Confirm subscription"),
              modalButton("Dismiss"))))
        } else
        {
          showModal(modalDialog(
            "Incorrect email address or email addresses not matching. Please correct.",
            title = "Warning",
            easyClose = TRUE))
        }
      })
    
    shiny::observeEvent(
      input$withdraw, {
        shinyjs::disable("withdraw")
        shiny::isolate({
          data <- values$data
          id <- values$id
          
          txt_confirm <- paste(
            "To confirm you would like to withdraw your contribution to",
            "the typo challenge, please click 'Withdraw my contribution',",
            "otherwise, click 'Dismiss'")
          txt_invalid <- paste(
            "You clicked on 'withdraw my contribution', but did not tick",
            "the box confirming this is what you want to do. Please also",
            "tick the box to proceed to withdrawing your data.")
          
          if (input$withdrawal_tick) {
            showModal(modalDialog(
              txt_confirm,
              title = "Are you sure you want to withdraw?",
              easyClose = TRUE,
              footer = shiny::tagList(
                shiny::actionButton("confirm_withdraw",
                                    "Withdraw my contribution",
                                    class = "btn-danger",
                                    title = "Confirm withdrawal"),
                modalButton("Dismiss"))))
            ## TODO: I don't think that this is neeed?
            output$typoapp <- shiny::renderUI(
              end_panel(id, data, values$global))
          } else {
            # did not withdraw
            showModal(modalDialog(
              txt_invalid,
              title = "Warning",
              easyClose = TRUE))
            ## Also this not needed?
            output$typoapp <- shiny::renderUI(
              end_panel(id, data, values$global))
          }
          
        })
      })
    
    shiny::observeEvent(
      input$confirm_withdraw, {
        shiny::removeModal() # remove the popup asking to confirm withdrawal
        shinyjs::disable("confirm_withdraw")
        shiny::isolate({
          data <- values$data
          id <- values$id
          discard_data(id, PATH_OUTPUT)
          output$typoapp <- shiny::renderUI(end_panel_withdrawn())
        })
      })
    
    shiny::observeEvent(
      input$confirm_subscribe, {
        #browser()
        shiny::removeModal() # remove the popup asking to confirm withdrawal
          shinyjs::disable("subscribe")
          save_email(input$email)
          shiny::updateTextInput(session, "email", 
                                 label = "Enter your email address",
                                 value = NA,
                                 placeholder = "Thank you for subscribing!")
          shiny::updateTextInput(session, "email_confirm", 
                                 label = "Confirm your email address",
                                 value = NA,
                                 placeholder = "Thank you for subscribing!")
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
        if (!is.null(values$data)) {
          message(sprintf("Detected session closed for '%s'", values$id))
          shiny::isolate({
            data <- values$data
            id <- values$id
            output$typoapp <- shiny::renderUI(end_panel(id, data, data$global))
            save_data(values, TRUE, PATH_OUTPUT)
          })
        }
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


discard_data <- function(id, path) {
  file_to_remove <- file.path(path, sprintf("%s.rds", id))
  message(sprintf("Discarding output for '%s'", id))
  file.remove(file_to_remove)
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

## The issue here is that we need to save the email address in a way
## that:
##
## 1. is robust in the face of multiple processes writing at once
## 2. does not preserve order or time information so it is not
##    relatable to the actual data
##
## The 'thor' package will not save any ordering or time metadata
## along with the name, is fast, and does between process file
## locking.
save_email <- function(address) {
  dest <- "emails.db"
  if (!requireNamespace("thor", quietly = TRUE)) {
    message("'thor' is not installed so not saving addresses")
    return()
  }
  thor::mdb_env(dest)$put(address, "")
}


is_valid_email <- function(x) { # very basic validation
  length(strsplit(x, "@", fixed = TRUE)[[1]]) == 2
}

are_emails_equal <- function(x, y) # needs some work to deal with capital letters and spaces maybe?
{
  isTRUE(x == y)
}

