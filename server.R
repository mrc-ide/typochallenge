library(shiny)
library(shinyjs)
library(rmnist)

source("plot.R")
source("text.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  dateFormat <- c("Handwritten","Calendar","TextDayMonthYear","TextMonthDayYear")
  
  ########################################
  # initial appearance #
  ########################################
  
  # Default output text, which will be changed once the user enters the first date
  output$text <- renderText('You have not yet started the challenge')
  instructions <- paste0(make_title("INSTRUCTIONS"),
                 "You are going to be presented with dates, in various formats.\n\n",
                 "You need to enter these dates in the format 'dd/mm/yyyy'.\n", 
                 "For instance, if the date is '25th of April 2017, you need to type in '25/04/2017'.\n", 
                 "Note that '25/4/2017' will also be accepted as a correct entry.\n\n",
                 "To validate an entry, type the 'Enter' button. \n",
                 "You will then be prompted with a new date to enter. \n\n",
                 "You can stop at any point, by clicking on 'End the challenge'.\n\n",
                 "To start the challenge, click on 'Start the challenge'. \n",
                 "Good luck!")
  output$text_instructions <- renderText(instructions)
  
  observeEvent(input$start,isolate({
    # Default output text, which will be changed once the user enters the first date
    output$text <- renderText('')
    output$text_stats <- renderText('')
    
    # Initially display a handwritten randomly drawn date
    output$imageDate <- renderPlot({
      values$dateToType<-as.Date("01/01/1900", "%d/%m/%Y")+sample.int(55000, size=1)
      values$date_format <- 1
      values$final_data_saved <- FALSE
      plot_handwritten_date(values$dateToType)
    }, width=200, height = 40 ) 
    
  }))
  
  ########################################
  # once the user enters a date and presses enter #
  ########################################
  
  values <- reactiveValues(Ntyp = 0, lastTimePoint = Sys.time(), shortestEntry=10000, dateToType = as.Date("28/12/2014", "%d/%m/%Y"), flag_page=2, tabEntries = NULL)
  
  ### not sure what this is used for
  # tdate <- eventReactive(input$validateButton, {
  #   input$typedDate
  #   renderText(input$typedDate)
  # })
  
  onevent("keydown", "typedDate", function(event) {
    if (event$keyCode == 13) { # once the person presses enter
      
      #deals with personal statistics
      isolate({
        pastTimePoint <- values$lastTimePoint
        values$lastTimePoint <- Sys.time()
        timeForTyping <- round(values$lastTimePoint-pastTimePoint, digits = 2)
        correctFlag<-checkDateIsCorrect(input$typedDate,values$dateToType)
        if((timeForTyping<values$shortestEntry)&correctFlag)  #must also check that entry is correct!
          values$shortestEntry<-timeForTyping
        values$tabEntries <- rbind(values$tabEntries,c(input$typedDate,as.character(format(values$dateToType, "%d/%m/%Y")),dateFormat[values$date_format],timeForTyping,correctFlag))
      })
      
      # increments the number of dates typed in until now
      values$Ntyp <- values$Ntyp + 1
      
      # record the date typed in and compare with true date
      #saveData(formData())
      isolate({
        # new date to type randomly chosen
        values$dateToType <- as.Date("01/01/1900", "%d/%m/%Y")+sample.int(55000, size=1)
        # choose the way the date will be displayed
        values$date_format <- sample.int(4, size=1)
        # provide opportunity to save again
        values$final_data_saved <- FALSE
        
        output$table <- renderTable(values$tabEntries)
        # resetting the entry to be blank after the user presses enter
        updateTextInput(session, "typedDate", "Type the date", "") 
      })
      
      if(values$date_format==1) # handwritten date
      {
        output$imageDate <- renderPlot({
          plot_handwritten_date(values$dateToType)
        }, width=200, height = 40 )
      } else if(values$date_format==2) # calendar date
      {
        output$imageDate <- renderPlot({
          plot_calendar_page(values$dateToType)
        }, width=500, height = 400 ) 
      }
      else if(values$date_format==3) # text date in day month year format
      {
        output$imageDate <- renderPlot({
          full_text_date(values$dateToType, format="dmy")
        }, width=500, height = 200 ) 
      }else # text date in month day year format
      {
        output$imageDate <- renderPlot({
          full_text_date(values$dateToType, format="mdy")
        }, width=500, height = 200 ) 
      }
      
      ouptut_txt <- generate_personal_stats(values)
      output$text <- ouptut_txt$text
      output$text_stats <- ouptut_txt$text_stats
      
    }
    
  })
  
  ########################################
  # upon clicking the "end" button #
  ########################################
  
  # when user presses end of challenge button, save the dates entered up to now in file
  observeEvent(input$end, isolate({ # save data unless already done
    
    
    if(sum(values$tabEntries[,5]==TRUE))
    {
      txt2 <- paste0(make_title("YOUR STATS"),
                     "You have entered a total of ",values$Ntyp," dates (", sum(values$tabEntries[,5]==FALSE), " mistake(s))")
      txt3<-paste("\nYou have taken an average of",round(mean(as.double(values$tabEntries[values$tabEntries[,5]==TRUE,4])),digit=2),"s per correct entry.\nYour personal record for a (correct) entry is",values$shortestEntry,"seconds.")
    }
    else
    {  
      txt2 <- "You have not typed any correct date..."
      txt3 <- ""
    }
    
    output$text <- renderText('Challenge over, thank you for your participation!')
    output$text_stats <- renderText(paste0(txt2,txt3))
    
    if(!values$final_data_saved) 
    {
      saveData(values$tabEntries) # save data 
      values$tabEntries <- NULL # reset entries to nothing, so that if a second set of data is entered it is recorded without the first set which has just been recorded
      values$final_data_saved <- TRUE # record the fact that we have already saved the data
    }
    
  }))
  
  ########################################
  # what happends if browser is closed #
  ########################################
  session$onSessionEnded(function() {
    isolate({
      if(!values$final_data_saved) 
      {
        saveData(values$tabEntries) # save data 
        values$tabEntries <- NULL # reset entries to nothing, so that if a second set of data is entered it is recorded without the first set which has just been recorded
        values$final_data_saved <- TRUE # record the fact that we have already saved the data
      }
      stopApp}) # make sure app is stopped
  })
  
  ########################################
  # questions #
  ########################################
  ### what happens if the server crashes? 
  ### maybe save temp files every 10 entries so we don't loose too much data 
  ### if people type in a large number of dates and server crashes or they loose connection 
  
  outputOptions(output, "text", suspendWhenHidden = FALSE)
  
})




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
  names(data) <- c("TypedDate","TrueDate","TrueDateFormat","TypingTime","TypedCorrectly")
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = FALSE)
}





#############################################
### function to validate a typed date     ###
#############################################

#' Plots a handwritten date
#' 
#' @param typedDate A character vector
#' @param date A date
#' @return True or False
#' @export
#' @examples
checkDateIsCorrect <- function(typedDate="1/1/2017", date=as.Date("01/01/2017", format="%d/%m/%Y"))
{
  result<-(as.Date(typedDate, format="%d/%m/%Y")==date)
  if(is.na(result))
    return(FALSE) else
      return(result)
}
