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
ui <- fluidPage(
                "Welcome to Planet Irf",
                ## *Input() functions [buttons, checkpox, password input, sliders, text, date]
                ## *Output() functions
                tags$a( href = "www.rstudio.com", "RStduio"),
                ## yields HTML: <a href="www.rstudio.com">Rstudio</a>
                tags$h1("First Level"),
                tags$h2("Second Level"),
                tags$h3("Third Level"),
                tags$h4("Fourth Level"),
                tags$h5("Fifth Level"),
                tags$h6("Sixth Level"),
                ##
                tags$p("text in a <p>"),
                
                tags$em("italics"),
                tags$strong("bold"),
                tags$code("code"),
                
                ## Nesting tags together
                tags$p("This is a nested", 
                      tags$strong("bolded words"),
                      "app"),
                
                #Line Break
                tags$br(), 
                ## horizontal rule
                tags$hr(),
                
                sliderInput(inputId = "num",
                            label = "Choose a Number",
                            value = 50, min = 1, max = 100),
                
                actionButton(inputId = "go",
                             label="Update"),
                
                actionButton(inputId = "norm",
                             label="Normal"),
                
                actionButton(inputId = "unif",
                             label="Uniform"),
                
                textInput(inputId = "title",
                          label = "Write a title",
                          value = "Histogram of Random Normal Values"),
                
                ##outputs()
                plotOutput("hist"),
                plotOutput("hist2"),
                
                verbatimTextOutput("stats")
                )


#server.R
# Define server logic 
# Rule 1. Save objects to display to output$
# Rule 2. Build objects to display with render()
# Rule 3. Use input values with input$
server <- function(input, output) {
    
    
    
    rv <- reactiveValues(data = rnorm(100))
    
    observeEvent(input$norm, { rv$data <- rnorm(100)})
    observeEvent(input$unif, { rv$data <- runif(100)})
    
    output$hist2 <- renderPlot({
        hist(rv$data)
    })
    
   ## use reactive() to create a passable data()
     data <- eventReactive(input$go,{
        rnorm(input$num)
    })
    
     observeEvent(input$go, {
         print(as.numeric(input$go))
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
