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

####
x <- 20
cut(x, breaks = c(1,21,41,61,81), labels = c("a","b","c","d"))
