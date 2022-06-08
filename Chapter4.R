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


## 2014 Mental Health Tech Survey ----
### Custom error messages, validation ----
server <- function(input, output, session){
  output$age <- renderTable({
    validate(
      need(input$age != "", "Be sure to select an age.")
    )
    
    osmi |> 
      summarize(avg_age = mean(Age))
  })
}

### Shiny Widgets ----
install.packages("shinyWidgets")
library(shinyWidgets)
shinyWidgetsGallery()


### Explore cuisines ----

ui <- fluidPage(
  titlePanel("Explore Cuisines"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cuisine", "Select Cuisine",
                  unique(recipes$cuisine)),
      sliderInput("nb_ingredients", "Select No. of Ingredients",
                  5, 100, 20)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", d3wordcloudOutput("wc_ingredients")),
        tabPanel("Plot", plotly::plotlyOutput("plot_top_ingredients")),
        tabPanel("Table", DT::DTOutput("dt_top_ingredients"))
      )
    )
  )
)

server <- function(input, output, session){
  output$dt_top_ingredients <- DT::renderDT({
    recipes |> 
      filter(cuisine == input$cuisine) |> 
      count(ingredient, name = "nb_recipes") |> 
      arrange(desc(nb_recipes)) |> 
      head(input$nb_ingredients)
  })


recipes_enriched <- recipes |>
  count(cuisine, ingredient, name = "nb_recipes") |>
  tidytext::bind_tf_idf(ingredient, cuisine, nb_recipes)

rval_top_ingredients <- reactive({
recipes_enriched |> 
    filter(cuisine == input$cuisine) |> 
    arrange(desc(tf_idf)) |> 
    head(input$nb_ingredients) |> 
    mutate(ingredient = forcats:: fct_reorder(ingredient, tf_idf))
  })

output$plot_top_ingredients <- plotly::renderPlotly({
  rval_top_ingredients() |> 
    ggplot(aes(x = ingredient, y = tf_idf))+
    geom_col()+
    coord_flip()
})

output$wc_ingredients <- d3wordcloud::renderD3wordcloud({
  d <- rval_top_ingredients()
  d3wordcloud(d$ingredient, d$nb_recipes, tooltip = TRUE)
})

}


## Mass Shootings ----

ui <- bootstrapPage(
  theme = shinythemes::shinytheme("simplex"),
  leaflet::leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, id = "controls",
                sliderInput("nb_fatalities", "Minimum Fatalities",
                            1, 40, 10),
                dateRangeInput("date_range", "Select Date",
                               "2010-01-01", "2019-12-01"),
                actionButton("show_about", "About")
                ),
  tags$style(type = "text/css",
             "html, body {width:100%; height:100%}
              #controls{background}-color:white;padding:20px;}
            ")
)

server <- function(input, output, session){
  observeEvent(input$show_about, {
    showModal(modalDialog(text_about, title = "About"))
  })
  rval_mass_shootings <- reactive({
    mass_shootings |> 
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        fatalities >= input$nb_fatalities
      )
    })

  output$map <-  leaflet::renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView( -98.58, 39.82, zoom = 5) |>  #lat,long,zoom
      addCircleMarkers(
        popup = ~summary,
        radius = ~fatalities,
        fillColor = "red", color = "red", weight = 1
      )
  })
}

shinyApp(ui, server)
