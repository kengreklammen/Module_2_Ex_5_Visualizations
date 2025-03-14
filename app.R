# Install libraries (if not already installed)
if (!require("shiny")) install.packages("shiny")
if (!require("bslib")) install.packages("bslib")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("reactable")) install.packages("reactable")
if (!require("plotly")) install.packages("plotly")

# Load libraries
library(dplyr)
library(tibble)
library(shiny)
library(bslib)
library(shinythemes)
library(shinyWidgets)
library(shinyFeedback)
library(reactable)
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
      
      # Filter 2: Sepal length range (Arpad's part)
      sliderInput(
        inputId = "sepal_length",
        label = "Sepal Length Range:",
        min = min(iris$Sepal.Length),
        max = max(iris$Sepal.Length),
        value = c(min(iris$Sepal.Length), max(iris$Sepal.Length))
      ),
      
      # Filter button (Ruben's part)
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
      plotlyOutput("violin_plot"),
      
      # Additional outputs for team members
      layout_columns(
        col_widths = c(6, 6),
        
        # Card 1: Miguel's part (DT table)
        card(
          card_header("DT Table"),
          full_screen = TRUE,
          card_body(dataTableOutput("dt_table"))
        ),
        
        # Card 2: Ruben's part (Highcharter plot)
        card(
          card_header("Highcharter Plot"),
          full_screen = TRUE,
          card_body(highchartOutput("highcharter_plot"))
        ),
        
        # Card 3: Arpad's part (Reactable table)
        card(
          card_header("Reactable Table"),
          full_screen = TRUE,
          card_body(reactableOutput("reactable_table"))
        ),
        
        # Card 4: Additional output (optional)
        card(
          card_header("Additional Output"),
          full_screen = TRUE,
          card_body(plotOutput("additional_plot"))
        )
      )
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
  
  # Render DT table (Miguel's part)
  output$dt_table <- renderDataTable({
    filtered_data()
  })
  
  # Render Highcharter plot (Ruben's part)
  output$highcharter_plot <- renderHighchart({
    # Add Highcharter code here
    highchart() %>%
      hc_add_series(data = filtered_data()$Sepal.Length, type = "column")
  })
  
  # Render Reactable table (Arpad's part)
  output$reactable_table <- renderReactable({
    reactable(filtered_data())
  })
  
  # Render additional plot (optional)
  output$additional_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Sepal.Length, y = Petal.Length)) +
      geom_point() +
      theme_minimal()
  })
}

# Run the application -----------------------------------------------------
shinyApp(ui, server)