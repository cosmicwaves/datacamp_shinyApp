# Build and Alien Sightings Dasboard ----

ui <- fluidPage(
  titlePanel("Alien Sightings Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Choose a U.S. state:", choices = ),
      dateRangeInput("date_range", "Choose a date range:")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("nb_sighted", "Number Sighted"),
        tabPanel("duration", "Duration Table")
      )
    )
  )
)

### Example

server <- function(input, output) {
  output$shapes <- renderPlot({
    usa_ufo_sightings %>%
      filter(state == input$state) %>%
      filter(date_sighted > input$dates[1] & date_sighted <input$dates[2]) %>%
      group_by(shape) %>%
      ggplot()+
      geom_bar(aes(x = shape, y = ..count..))
  }) 

  output$duration_table <- renderTable({
    usa_ufo_sightings %>%
      filter(state == input$state) %>%
      filter(date_sighted > input$dates$start & date_sighted <input$dates$end) %>%
      group_by(shape )%>%
      summarise(nb_of_sighted = count(shape),
                mean = mean(duration_sec),
                median = median(duration_sec),
                max = max(duration_sec),
                min = min(duration_sec)) %>%
      table()
  })
  
  
}

ui <- fluidPage(
  titlePanel("UFO Sightings"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Choose a U.S. state:", choices = unique(usa_ufo_sightings$state)),
      dateRangeInput("dates", "Choose a date range:",
                     start = "1920-01-01",
                     end = "1950-01-01")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Plot", plotOutput("shapes")),
        tabPanel(title = "Table", tableOutput("duration_table"))
        )
    )
  )
)

shinyApp(ui, server)