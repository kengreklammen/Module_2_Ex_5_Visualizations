# Install libraries (if not already installed)
install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("bslib")
install.packages("plotly")

# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)
library(plotly)

# Load data
data(iris)

# Define UI ---------------------------------------------------------------
ui <- fluidPage(
  # App title
  titlePanel("Iris Dataset"),
  
  # Theme
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    version = 5
  ),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Filter 1: Select species
      selectInput(
        inputId = "species",
        label = "Choose a Species:",
        choices = levels(iris$Species)
      ),
      
      # Filter 2: Sepal length range
      sliderInput(
        inputId = "sepal_length",
        label = "Sepal Length Range:",
        min = min(iris$Sepal.Length),
        max = max(iris$Sepal.Length),
        value = c(min(iris$Sepal.Length), max(iris$Sepal.Length))
      ),
      
      # Filter button
      actionButton(
        inputId = "filter_data",
        label = "Filter Data"
      )
    ),
    
    # Main panel
    mainPanel(
      # Table output 
      tableOutput(outputId = "filtered_table"),
      
      # Violin plot 
      plotlyOutput("violin_plot")
    )
  )
)

# Define server logic -----------------------------------------------------
server <- function(input, output) {
  # Filtered data based on user input 
  filtered_data <- reactive({
    iris %>%
      filter(Species == input$species,
             Sepal.Length >= input$sepal_length[1],
             Sepal.Length <= input$sepal_length[2])
  })
  
  # Render filtered table 
  output$filtered_table <- renderTable({
    filtered_data()
  })
  
  # Render violin plot 
  output$violin_plot <- renderPlotly({
    fig <- plot_ly(filtered_data(), type = 'violin')
    
    fig <- fig %>%
      add_trace(
        x = ~Species[filtered_data()$Species == 'setosa'],
        y = ~Sepal.Length[filtered_data()$Species == 'setosa'],
        legendgroup = 'Setosa',
        scalegroup = 'Setosa',
        name = 'Setosa',
        side = 'negative',
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        color = I("blue")
      )
    
    fig <- fig %>%
      add_trace(
        x = ~Species[filtered_data()$Species == 'versicolor'],
        y = ~Sepal.Length[filtered_data()$Species == 'versicolor'],
        legendgroup = 'Versicolor',
        scalegroup = 'Versicolor',
        name = 'Versicolor',
        side = 'positive',
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        color = I("green")
      )
    
    fig <- fig %>%
      add_trace(
        x = ~Species[filtered_data()$Species == 'virginica'],
        y = ~Sepal.Length[filtered_data()$Species == 'virginica'],
        legendgroup = 'Virginica',
        scalegroup = 'Virginica',
        name = 'Virginica',
        side = 'positive',
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        color = I("red")
      )
    
    fig <- fig %>%
      layout(
        xaxis = list(title = "Species"),
        yaxis = list(title = "Sepal Length", zeroline = FALSE),
        violingap = 0,
        violingroupgap = 0,
        violinmode = 'overlay'
      )
    
    fig
  })
}

# Run the application -----------------------------------------------------
shinyApp(ui, server)