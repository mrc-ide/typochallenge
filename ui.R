library(shiny)
library(rmnist)
library(Hmisc)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  titlePanel("The Typo Challenge"),
  sidebarLayout(
    sidebarPanel(
    
      #conditionalPanel(
      #  condition = "flag_page == 1",
      #  textInput("typedDate", "Type the date", "")),
      
      useShinyjs(),
      textInput("typedDate", "Type the date (press enter to submit)", ""),
      tags$script('
        $(document).on("keydown", function (e) {
                  Shiny.onInputChange("lastkeypresscode", e.keyCode);
                  });
                  '),
      actionButton("end", "End the challenge", class = "btn-primary")
      ),
    mainPanel(
      plotOutput("imageDate"),
      #tableOutput('table'),
      verbatimTextOutput("text")
    )
  )
))
