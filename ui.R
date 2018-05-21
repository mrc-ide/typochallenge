library(shiny)
library(shinyjs)
library(rmnist)
library(Hmisc)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel("The Typo Challenge"),
  sidebarLayout(
    sidebarPanel(
      
      useShinyjs(),
      
      ### Initial panel, before the challenge is started ###
      conditionalPanel(
        condition = "output.text == 'You have not yet started the challenge'",
        actionButton("start", "Start the challenge", class = "btn-primary")
      ),
      
      ### Susequent panel, once the challenge is started ###
      conditionalPanel(
        condition = "output.text != 'You have not yet started the challenge' && output.text != 'Challenge over, thank you for your participation!'",
        textInput("typedDate", "Type the date", ""),
        
        tags$script('
        $(document).on("keydown", function (e) {
                  Shiny.onInputChange("lastkeypresscode", e.keyCode);
                  });
                  '),
        fluidRow("Press enter to submit", style = "margin-left: 0px;"),
        actionButton("end", "End the challenge", class = "btn-primary", style = "margin-top: 100px;"))
      
      ### Final panel, once the challenge is over ###
      
    ),
    mainPanel(
      
      conditionalPanel(
        condition = "output.text != 'You have not yet started the challenge' && output.text != 'Challenge over, thank you for your participation!'",
        plotOutput("imageDate")
        ),
      
      #tableOutput('table'),
      verbatimTextOutput("text"),
      
      conditionalPanel(
        condition = "output.text != 'You have not yet started the challenge'",
        verbatimTextOutput("text_stats")
      ),
      
      conditionalPanel(
        condition = "output.text == 'You have not yet started the challenge'",
        verbatimTextOutput("text_instructions")
      )
    )
  )
))
