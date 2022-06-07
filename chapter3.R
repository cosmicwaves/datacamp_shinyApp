# Reactive Programming ----
## Reactive expressions ----

server <- function(input, output, session){
  rval_babynames <- reactive({
    babynames |> 
      filter(name == input$name)
  })
  output$plot_trendy_names <- plotly::renderPlotly({
    rval_babynames() |> 
      ggplot(val_bnames, aes(x = year, y = n))+
      geom_col()
  })
  output$table_trendy_names <- DT::renderDT({
    rval_babynames()
  })
}


## Observers ----
ui <- fluidPage(
  textInput("name", "Enter Name"),
)

server <- function(input, output, session){
  observe(
    showModal(modalDialog(
      paste("Hello", input$name)
    ))
  )
}

####
ui <- fluidPage(
  textInput("name", "Enter your name")
)

server <- function(input, output, session){
  observe({
    showNotification(
      paste("You entered the name", input$name)
    )
  })
}

## Stop-delay-trigger ----
# Stopping(Isolating)
server <- function(input, output, session){
  output$greeting <- renderText({
    paste(isolate({input$greeting_type}), input$name, sep = ", ")
  })
}

# Delaying ----
server <- function(input, output, session){
  rv_greeting <- eventReactive(input$show_greeting, {
    paste("Hello,", input$name)
  })
  output$greeting <- renderText({rv_greeting})
}


server <- function(input, output, session){
  observeEvent(input$show_greeting, {
    showModal(modalDialog(paste("Hello", input$name)))
  })
}

## More Examples ----

server <- function(input, output, session) {
  # MODIFY CODE BELOW: Wrap in observeEvent() so the help text 
  # is displayed when a user clicks on the Help button.
  
  # Display a modal dialog with bmi_help_text
  # MODIFY CODE BELOW: Uncomment code
  observeEvent(input$show_help, {showModal(modalDialog(bmi_help_text))
  })
  
  rv_bmi <- eventReactive(input$show_bmi, {
    input$weight/(input$height^2)
  })
  output$bmi <- renderText({
    bmi <- rv_bmi()
    paste("Hi", input$name, ". Your BMI is", round(bmi, 1))
  })
}

ui <- fluidPage(
  titlePanel('BMI Calculator'),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter your name'),
      numericInput('height', 'Enter your height in meters', 1.5, 1, 2),
      numericInput('weight', 'Enter your weight in Kilograms', 60, 45, 120),
      actionButton("show_bmi", "Show BMI"),
      # CODE BELOW: Add an action button named "show_help"
      actionButton("show_help", "Help")
    ),
    mainPanel(
      textOutput("bmi")
    )
  )
)

shinyApp(ui = ui, server = server)


