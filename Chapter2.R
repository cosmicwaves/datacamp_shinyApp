# Chapter 2 ----

## Input and Outputs ----
ui <- fluidPage(
  textInput("name", "enter a name:"),
  selectInput("animal", "Dogs or cats?", choices = c("dogs", "cats")),
  textOutput("question"),
  textOutput("answer")
)

server <- function(input, output, session){
  output$question <- renderText({
    paste("Do you prefer dogs or cats?,", input$name,"?")
  })
  output$answer <- renderText({
    paste("I prefer", input$animal, "!")
  })
}

shinyApp(ui, server)


## Non-shiny output and render ----
install.packages("babynames")
library(babynames)

ui <- fluidPage(
  DT::DTOutput("babynames_table")
)

server <- function(input,output){
  output$babynames_table <- DT::renderDT({
    babynames |> 
      dplyr::sample_frac(.1)
  })
}

shinyApp(ui, server)

## Example ----
library(tidyverse)
ui <- fluidPage(
  titlePanel("What's in a Name?"),
  # Add select input named "sex" to choose between "M" and "F"
  selectInput('sex', 'Select Sex', choices = c("F", "M")),
  # Add slider input named "year" to select year between 1900 and 2010
  sliderInput('year', 'Select Year', min = 1900, max = 2010, value = 1900),
  # CODE BELOW: Add table output named "table_top_10_names"
  tableOutput("table_top_10_names")
)
server <- function(input, output, session){
  # Function to create a data frame of top 10 names by sex and year 
  top_10_names <- function(){
    top_10_names <- babynames |>  
      filter(sex == input$sex) |>  
      filter(year == input$year) |>  
      top_n(10, prop)
  }
  # CODE BELOW: Render a table output named "table_top_10_names"
  output$table_top_10_names <- renderTable({
    top_10_names()
  })
  
}
shinyApp(ui = ui, server = server)

## Layouts and Themes ----

### Layout with multiple tabs

ui <- fluidPage(
  titlePanel("Histogram"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("nb_bins", "# Bins", 5,10,5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Waiting",
                 plotOutput("hist_waiting")),
        tabPanel("Eruptions",
                 plotOutput("hist_eruptions"))
      )
    )
  )
)


server <- function(input, output, session){
  output$hist_waiting <- renderPlot({
    hist(faithful$waiting,
         breaks = input$nb_bins,
         col = "steelblue")
  })
  output$hist_eruptions <- renderPlot({
    hist(faithful$eruptions,
         breaks = input$nb_bins,
         col = "steelblue")
  })
}

shinyApp(ui, server)

### Layout with theme selector

ui <- fluidPage(
  titlePanel("Histogram"),
  shinythemes::themeSelector(),
  sidebarLayout(
    sidebarPanel(sliderInput("nb_bins",
                             "# of Bins",
                             5,10,5)),
    mainPanel(plotOutput("hist"))
  )
)

server <- function(input, output, session){
  output$hist <- renderPlot(
    hist(faithful$waiting,
         breaks = input$nb_bins,
         colur = "steelblue")
  )
}

shinyApp(ui, server)


### Layout with manual theme selection
ui <- fluidPage(
  titlePanel("Histogram"),
  theme = shinythemes::shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(sliderInput("nb_bins",
                             "# of Bins",
                             5,10,5)),
    mainPanel(plotOutput("hist"))
  )
)

server <- function(input, output, session){
  output$hist <- renderPlot(
    hist(faithful$waiting,
         breaks = input$nb_bins,
         colur = "steelblue")
  )
}

shinyApp(ui, server)


### Build an app with gapminder
ui <- fluidPage(
  titlePanel("Life Expectation vs. GDP Per Capita"),
  shinythemes::shinytheme(),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select Continent",
                  choices = unique(gapminder$continent)),
      sliderInput("year", "Select Year",1952,2007,1992, step = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(plotOutput("plot"),
        ),
        tabPanel(DT::DTOutput("table"))
      )
    )
  )
)

server <- function(input, output, session){
  output$plot <- renderPlot({
    data <- gapminder |> 
      filter(year == input$year) |> 
      filter(continent == input$continent) 
    print(data)
    ggplot(data, aes(x = gdpPercap), y = lifeExp)+
      geom_point()
  })
  output$table <- DT::renderDT({
    gapminder |> 
      filter(year == input$year) |> 
      filter(continent == input$continent)
  })
}

serverApp(ui, server)