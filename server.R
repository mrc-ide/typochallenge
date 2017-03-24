library(shiny)
library(rmnist)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  ########################################
  # attempts at making the enter key work as well as submit button #
  # not working for now #
  ########################################
  
  # # There are state variables for the input text and GO button
  # curr.val <- "" # Corresponds to the current displayed input$myinput
  # curr.go  <- 0  # Corresponds to the last known GO value (integer)
  # 
  # lastEvent <- reactive({
  #   # Is reactive to the following events
  #   input$GO
  #   input$lastkeypresscode
  #   
  #   # Decide which action should be taken
  #   if(input$GO > curr.go) {
  #     # The user pushed the GO actionButton, so take action
  #     action <- 1
  #     curr.go <<- input$GO
  #   } else if(input$lastkeypresscode == 13) {
  #     # The user pressed the Enter key, so take action
  #     action <- 1
  #   } else {
  #     # The user did anything else, so do nothing
  #     action <- 0
  #   }
  #   
  #   return(action)
  # })
  
  ########################################
  # initial appearance #
  ########################################
  
  # Default output text, which will be changed once the user clicks on the submit button
  output$text <- renderText("You have not yet started the challenge")
  
  # Initially display a handwritten randomly drawn date
  output$imageDate <- renderPlot({
    values$dateToType<-as.Date("01/01/1900", "%d/%m/%Y")+sample.int(55000, size=1)
    
    plot_handwritten_date(values$dateToType)
  }, width=200, height = 40 ) 
  
  ########################################
  # once the user enters a date and presses submit #
  ########################################
  
  values <- reactiveValues(Ntyp = 0, dateToType = as.Date("28/12/2014", "%d/%m/%Y"), flag_page=2, tabEntries = NULL)
  
  tdate <- eventReactive(input$validateButton, {
    input$typedDate
    renderText(input$typedDate)
  })
  
  # increments the number of dates typed in until now
  observeEvent(input$submit,isolate({
    values$Ntyp <- values$Ntyp + 1
  }))
  
  observeEvent(input$submit, {
    #saveData(formData())
    isolate({
      values$tabEntries <- rbind(values$tabEntries,c(input$typedDate,as.character(format(values$dateToType, "%d/%m/%Y"))))
      values$dateToType <- as.Date("01/01/1900", "%d/%m/%Y")+sample.int(55000, size=1)
      output$table <- renderTable(values$tabEntries)
      # resetting the entry to be blank after the user clicks the submit button
      updateTextInput(session, "typedDate", "Type the date", "") 
    })
    
    # choose the way the date will be displayed
    if(values$Ntyp<3) output_choice<-1 else output_choice<-sample.int(3, size=1)
    
    if(output_choice==1) # handwritten date
    {
      output$imageDate <- renderPlot({
        plot_handwritten_date(values$dateToType)
      }, width=200, height = 40 )
    } else if(output_choice==2) # calendar date
    {
      output$imageDate <- renderPlot({
        plot_calendar_page(values$dateToType)
      }, width=500, height = 400 ) 
    }
    else # text date
    {
      output$imageDate <- renderPlot({
        full_text_date(values$dateToType)
      }, width=500, height = 200 ) 
    }
    
    # display as new output text the number of dates typed in until now
    txt2 <- paste0(make_title("YOUR STATS"),
                      "You have entered a total of ",values$Ntyp," dates")
    # and if an error was made, display it
    if((values$tabEntries[nrow(values$tabEntries),1] == values$tabEntries[nrow(values$tabEntries),2])) 
    {
      txt1 <- "" 
    } else 
    {
      txt1 <- paste0(make_title("TYPO AT LAST ENTRY!"),
                      "The date was '",
                      values$tabEntries[nrow(values$tabEntries),2],
                      "' and you typed in '",
                      values$tabEntries[nrow(values$tabEntries),1],"'.\n\n\n")
    }
    output$text <- renderText(paste0(txt1, txt2))
  })
  
  ########################################
  # upon clicking the "end" button #
  ########################################
  
  # when user presses end of challenge button, save the dates entered up to now in file
  observeEvent(input$end, {
    saveData(values$tabEntries)
  })
  
  ########################################
  # what happends if browser is closed #
  ########################################
  session$onSessionEnded(function() {
    isolate({
      saveData(values$tabEntries) # save data
      stopApp}) # make sure app is stopped
  })
  
  ########################################
  # questions #
  ########################################
  ### what happens if the server crashes? 
  ### maybe save temp files every 10 entries so we don't loose too much data 
  ### if people type in a large number of dates and server crashes or they loose connection 
  
})


########################################
# function to make a title look nice in the output #
########################################
make_title <- function(txt)
{
  ntot <- 36
  nspaces <- (ntot - nchar(txt))/2
  return(paste0( makeNstr("-",ntot),"\n",
                  makeNstr(" ", nspaces),
                  txt,
                  makeNstr(" ", nspaces),"\n",
                  makeNstr("-",ntot),"\n"))
}

########################################
# saving entries #
########################################

responsesDir <- file.path("contributions") # where to save the contributions

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")  # current time used to name the file in which contribution is saved

saveData <- function(data) { # function to save the contribution
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  data <- as.data.frame(data)
  names(data) <- c("TypedDate","TrueDate")
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = FALSE)
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
plot_calendar_page <- function(date=as.Date("01/01/2017", format="%d/%m/%Y"), width=500, height=400) # date has to be a date
{
  #Names of the calendar months
  month.names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  #Names of the week days
  day.names <- paste0(c("Sun","Mon","Tues","Wednes","Thurs","Fri","Satur"),"day")
  
  #convert the date in the POSIXlt classes
  date_lt<-as.POSIXlt(date)
  
  #Get the month of the date (numeric 0:11) and convert into character
  month=month.names[date_lt$mon+1]
  
  #Get the weekday of the date (numeric 0:6) 
  wday=date_lt$wday+1
  
  #Get the monthday of the date (numeric 1:31)
  mday=date_lt$mday
  
  #Get the year of the date (numeric year-1900) and convert into character
  year=as.character(date_lt$year+1900)
  
  #Calculate the weekday of the first day of the month
  first_dmonth<-as.POSIXlt(date-date_lt$mday+1)$wday
  
  #opens an empty plotting area
  plot(NULL,xlim=c(0,width),ylim=c(0,height*1.1), axes = F, xlab="",ylab="", main=paste(month,year))
  rect(0,0,width,height)
  
  #Write the days of the week on top of the page
  text(labels=day.names,(1:7-0.5)*width/7,height,pos=3,cex=.8)
  
  #test if the calendar needs five or six rows
  if(as.POSIXlt(date-as.POSIXlt(date)$mday-first_dmonth+36)$mon==date_lt$mon)
    nrow=6 else nrow=5
  
  #Plot the days in the calendar
  for(i in 1:7)
    for(j in 1:nrow)
    {
      #Calculate the day corresponding to the position in the calendar
      current_day<-as.POSIXlt(date-as.POSIXlt(date)$mday-first_dmonth+i+(j-1)*7)
      
      if(current_day$mon==date_lt$mon)
        dcol="black" else dcol="grey"
        text(labels=current_day$mday,x=(i-0.5)*width/7,y=height-(j-0.5)*height/nrow,col=dcol)
        
        if(current_day==date_lt)
          points(x=(i-0.5)*width/7,y=height-(j-0.5)*height/nrow,col="red",pch=1,cex=5.5,lwd=4)
    }
  
  #plot the vertical lines
  for(i in 1:6)
    lines(rep(i*width/7,2),c(0,height))
  
  #plot the horizontal lines
  for(i in 1:(nrow-1))
    lines(c(0,width),rep(i*height/nrow,2))  
  
}

#######################################
### functions to plot a written a date in full text ###
#######################################

#' Write a date in full text
#' 
#' @param date A date
#' @return NULL
#' @export
#' @examples
#' plot_calendar_page(Sys.Date()) # show a page with today's date circled in red
full_text_date<-function(date=as.Date("01/01/2017", format="%d/%m/%Y"))
{
  #Names of the calendar months
  month.names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  #Names of the week days
  day.names <- paste0(c("Sun","Mon","Tues","Wednes","Thurs","Fri","Satur"),"day")
  
  #convert the date in the POSIXlt classes
  date_lt<-as.POSIXlt(date)
  
  #derive the suffixe the numeral
  if(date_lt$mday %in% c(1,21,31)) suf="st" else
    if(date_lt$mday %in% c(2,22)) suf="nd" else
      if(date_lt$mday %in% c(3,23)) suf="rd" else
        suf="th"
  
  written_date <- paste0(day.names[date_lt$wday+1]," ",date_lt$mday,suf," ",month.names[date_lt$mon + 1]," ",date_lt$year+1900)
  
  plot(NULL,xlim=c(0,300),ylim=c(0,50), axes = F, xlab="",ylab="", main="")
  rect(xleft = 0, xright = 300, ybottom = 0, ytop = 50)
  text(labels=written_date,x=150,y=25, cex=2)
}

full_text_date(as.Date("01/01/1900", "%d/%m/%Y")+sample.int(55000, size=1))

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
plot_handwritten_date <- function(date=as.Date("01/01/2017", format="%d/%m/%Y"), d=load_mnist(download_if_missing = TRUE)) # date has to be a date
{
  # convert date to string with "/" separator
  date <- as.character(date, format="%d/%m/%Y")
  
  # create a "dot" image to separate day, month and year
  width <- 3
  dot_mat <- matrix(0, 28, 28)
  for(i in 20+(1:width))
  {
    dot_mat[i,(28-width)/2+(1:width)] <- 1
  }
  
  separator <- t(dot_mat)
  class(separator) <- c("mnist_digit", "matrix")
  attr(separator, "label") <- 0 ### this S3 class forces to have an integer label...
  attr(separator, "label") <- "mnist_digit"
  attr(separator, "data") <- "matrix"
  
  # create a panel of 10 images for dd/mm/yyyy
  par(mfrow=c(1, 10), mar=c(0, 0, 0, 0))
  
  # generate a random sample of handwritten images for each character in the date string, forcing repeated numbers to have the same handwriting every time they appear 
  date_idx <- strsplit(date, NULL)[[1]]
  date_idx_unique <- unique(date_idx)
  chosen_image_unique <- lapply(date_idx_unique, function(k) {
    if (k == "/") chosen_image <- separator else chosen_image <- d[[sample(which(d$label %in% as.numeric(k)), 1)]]; return(chosen_image)
  })
  
  # do the plotting
  for(idx in date_idx)
  {
    plot(chosen_image_unique[[match(idx, date_idx_unique)]], box=FALSE)
  }
  
}
