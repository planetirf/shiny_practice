#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#ui.R
# Define UI, Adding elements to app.
ui <- fluidPage("Welcome to Planet Irf",
                ## *Input() functions [buttons, checkpox, password input, sliders, text, date]
                ## *Output() functions
                
                ##
                sliderInput(inputId = "num",
                            label = "Choose a Number",
                            value = 50, min = 1, max = 100),
                
                actionButton(inputId = "clicks",
                             label="Update"),
                
                textInput(inputId = "title",
                          label = "Write a title",
                          value = "Histogram of Random Normal Values"),
                
                ##outputs()
                plotOutput("hist"),
                
                verbatimTextOutput("stats")
                )


#server.R
# Define server logic 
# Rule 1. Save objects to display to output$
# Rule 2. Build objects to display with render()
# Rule 3. Use input values with input$
server <- function(input, output) {
   ## use reactive() to create a passable data()
     data <- reactive({
        rnorm(input$num)
    })
    
     observeEvent(input$clicks, {
         print(as.numeric(input$clicks))
     })
    
    output$hist <- renderPlot({
        hist(data(), 
             main = isolate(input$title))
    })
    
    output$stats <- renderPrint({
        summary(data())
    })
    
    
    
}

# app.R
# Run the application 
shinyApp(ui = ui, server = server)
