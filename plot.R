#######################################
### functions to create a vector with weekdays ###
### currently starting on Sunday ###
#######################################

#' create a vector with weekdays 
#' 
#' @return a vector of weekdays
#' @export
#' @examples
#' weekday_names()
weekday_names <- function() {
  # Names of the week days
  day_names <- paste0(c("Sun", "Mon", "Tues",
                        "Wednes", "Thurs", "Fri",
                        "Satur"), 
                      "day")
  return(day_names)
}


#######################################
### functions to plot a calendar page ###
#######################################

#' Plots a calendar page
#' 
#' @param date A date
#' @param width In pixel the width of the calendar page to be plotted.
#' @param height In pixel the height of the calendar page to be plotted.
#' @return NULL
#' @export
#' @examples
#' plot_calendar_page(Sys.Date()) # show a page with today's date circled in red
plot_calendar_page <- function(date = as.Date("01/01/2017", 
                                              format = "%d/%m/%Y"), 
                               width = 500, 
                               height = 400) {
  
  if (inherits(date, "Date")) {
    stop("date should be in date format.") # date has to be a date
  }
  
  # Names of the week days
  day_names <- weekday_names()
  
  # convert the date in the POSIXlt classes
  date_lt <- as.POSIXlt(date)
  
  # Get the month of the date (numeric 0:11) and convert into character
  month <- month.name[date_lt$mon + 1]
  
  # Get the weekday of the date (numeric 0:6) 
  wday <- date_lt$wday + 1
  
  # Get the monthday of the date (numeric 1:31)
  mday <- date_lt$mday
  
  # Get the year of the date (numeric year-1900) and convert into character
  year <- as.character(date_lt$year + 1900)
  
  # Calculate the weekday of the first day of the month
  first_dmonth <- as.POSIXlt(date-date_lt$mday + 1)$wday
  
  # opens an empty plotting area
  par(mar = c(0, 0, 1, 0))
  plot(NULL, 
       xlim = c(0, width), ylim = c(0, height * 1.1), 
       axes = FALSE, 
       xlab = "", ylab="", 
       main = paste(month, year))
  rect(0, 0, width, height)
  
  # Write the days of the week on top of the page
  text(labels = day_names, (1:7-0.5) * width / 7, height, pos = 3, cex = .8)
  
  # test if the calendar needs five or six rows
  last_day_of_previous_month <- date - as.POSIXlt(date)$mday
  remaining_from_previous_month <- first_dmonth
  if (as.POSIXlt(last_day_of_previous_month - 
                 remaining_from_previous_month + 5 * 7 + 1)$mon == 
      date_lt$mon) {
    nrow <- 6 # if still in same month after 5 rows ( = weeks) need a 6th row
  } else {
    nrow <- 5
  } 
  
  # may be nice to do this not in a loop but dates and matrices not friends
  # Plot the days in the calendar
  for (i in 1:7) {
    for (j in 1:nrow) {
      #Calculate the day corresponding to the position in the calendar
      current_day <- as.POSIXlt(date - as.POSIXlt(date)$mday - first_dmonth +
                                  i + (j - 1) * 7)
      
      if (current_day$mon == date_lt$mon) {
        dcol <- "black" else dcol <- "grey"
        text(labels = current_day$mday,
             x = (i - 0.5) * width / 7,
             y = height - (j - 0.5) * height / nrow, 
             col = dcol)
      }
      
      if (current_day == date_lt) {
        points(x = (i - 0.5) * width / 7,
               y = height - (j - 0.5) * height / nrow,
               col = "red", pch = 1, cex = 5.5, lwd = 4)
      }
    }
  }
  
  #plot the vertical lines
  for (i in 1:6) {
    lines(rep(i * width / 7, 2), c(0, height))
  }
  
  #plot the horizontal lines
  for (i in 1:(nrow - 1)) {
    lines(c(0, width),rep(i * height / nrow, 2))  
  }
  
}

#######################################
### functions to plot a written date in full text ###
#######################################

#' Write a date in full text
#' 
#' @param date A date
#' @return NULL
#' @export
#' @examples
#' full_text_date(as.Date("01/01/1900", "%d/%m/%Y") + 
#'   sample.int(55000, size = 1))
full_text_date <- function(date = as.Date("01/01/2017", 
                                          format = "%d/%m/%Y"), 
                           format = c("dmy", "mdy")) {
  format <- match.arg(format)
  
  # Names of the week days
  day_names <- weekday_names()
  
  # convert the date in the POSIXlt classes
  date_lt <- as.POSIXlt(date)
  
  # derive the suffix for the numeral
  if (date_lt$mday %in% c(1, 21, 31)) suf <- "st" else
    if (date_lt$mday %in% c(2, 22)) suf <- "nd" else
      if (date_lt$mday %in% c(3, 23)) suf <- "rd" else
        suf <- "th"
  
  if (format == "dmy"){
    written_date <- paste(day_names[date_lt$wday + 1], 
                          date_lt$mday, suf, 
                          month.name[date_lt$mon + 1],
                          date_lt$year + 1900)
  }else if (format == "mdy"){
    written_date <- paste(day_names[date_lt$wday + 1],
                          month.name[date_lt$mon + 1],
                          date_lt$mday,suf,
                          date_lt$year + 1900)
  }
  
  # do the plotting
  par(mar=c(0, 0, 0, 0))
  plot(NULL, xlim = c(0, 300), ylim = c(0, 50), 
       axes = FALSE, 
       xlab = "", ylab = "", main = "")
  # rect(xleft = 0, xright = 300, ybottom = 0, ytop = 50)
  legend("topleft", written_date, bty = "n", cex = 2)
}

#######################################
### functions to generate a plot of a handwritten date ###
#######################################

#' Plots a handwritten date
#' 
#' @param date A date
#' @param d an object of class mnist containing a database of handwritten digits (see \code{rmnist::load_mnist}).
#' @return NULL
#' @import rmnist
#' @export
#' @examples
#' plot_handwritten_date(Sys.Date()) # print today's date
plot_handwritten_date <- function(date = as.Date("01/01/2017", 
                                                 format = "%d/%m/%Y"), 
                                  d = load_mnist(download_if_missing = TRUE)) {
  if (inherits(date, "Date")) {
    stop("date should be in date format.") # date has to be a date
  }
  
  # convert date to string with "/" separator
  date <- as.character(date, format="%d/%m/%Y")
  
  # create a "dot" image to separate day, month and year
  width <- 3
  dot_mat <- matrix(0, 28, 28)
  for (i in 20 + (1:width)) {
    dot_mat[i,(28-width)/2+(1:width)] <- 1
  }
  
  separator <- t(dot_mat)
  class(separator) <- c("mnist_digit", "matrix")
  attr(separator, "label") <- 0 # this S3 class forces to have an integer label
  attr(separator, "label") <- "mnist_digit"
  attr(separator, "data") <- "matrix"
  
  # create a panel of 10 images for dd/mm/yyyy
  par(mfrow=c(1, 10), mar=c(0, 0, 0, 0))
  
  # generate a random sample of handwritten images for each character 
  # in the date string, forcing repeated numbers to have the same handwriting 
  # every time they appear 
  date_idx <- strsplit(date, NULL)[[1]]
  date_idx_unique <- unique(date_idx)
  chosen_image_unique <- lapply(date_idx_unique, function(k) {
    if (k == "/") {
      chosen_image <- separator 
    }else {
      chosen_image <- d[[sample(which(d$label %in% as.numeric(k)), 1)]]
    }
    return(chosen_image)
  })
  
  # do the plotting
  for (idx in date_idx) {
    plot(chosen_image_unique[[match(idx, date_idx_unique)]], box = FALSE)
  }
  
}