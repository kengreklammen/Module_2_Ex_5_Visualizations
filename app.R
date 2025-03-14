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
ui <- page_fluid(
  # App title
  titlePanel("Iris Dataset"),
  
  # Theme
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    version = "5"
  ),
  
  # Sidebar layout with input and output definitions
  navset_card_pill(
    sidebar = sidebar(
      # FARIBA: filter1: select > choose species
      selectInput(
        inputId = "species",
        label = "Choose a Species:",
        choices = levels(iris$Species)
      ),
      
      # ARPAD: filter2: slider > sepal.length
      sliderInput(
        inputId = "sepal_length",
        label = "Sepal Length Range:",
        min = min(iris$Sepal.Length),
        max = max(iris$Sepal.Length),
        value = c(min(iris$Sepal.Length), max(iris$Sepal.Length))
      ),
      
      # RUBEN: button: select random species
      actionButton(
        inputId = "random_select",
        label = "Choose a Random Species"
      )
    ),
    
    # Main panel
    nav_panel(
      title = "",
      layout_columns(
        col_widths = c(6, 6),
        
        # FARIBA: Violin plot with Plotly
        card(
          card_header("Violin Plot (Sepal Length by Species)"),
          full_screen = TRUE,
          card_body(plotlyOutput("violin_plot"))
        ),
        
        # MIGUEL: DT table
        card(
          card_header("DT Table"),
          full_screen = TRUE,
          card_body(dataTableOutput("dt_table"))
        ),
        
        # RUBEN: Highcharter plot
        card(
          card_header("Highcharter Plot"),
          full_screen = TRUE,
          card_body(highchartOutput("highcharter_plot"))
        ),
        
        # ARPAD: Reactable table
        card(
          card_header("Reactable Table"),
          full_screen = TRUE,
          card_body(reactableOutput("reactable_table"))
        )
      )
    )
  )
)

# Define server logic -----------------------------------------------------
server <- function(input, output) {
  # FARIBA: Filter data based on species and sepal length
  filtered_data <- reactive({
    iris %>%
      filter(Species == input$species,
             Sepal.Length >= input$sepal_length[1],
             Sepal.Length <= input$sepal_length[2])
  })
  
  # FARIBA: Render violin plot
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
  
  # MIGUEL: Render DT table
  output$dt_table <- renderDataTable({
    filtered_data()
  })
  
  # RUBEN: Render Highcharter plot
  output$highcharter_plot <- renderHighchart({
    highchart() %>%
      hc_add_series(data = filtered_data()$Sepal.Length, type = "column")
  })
  
  # ARPAD: Render Reactable table
  output$reactable_table <- renderReactable({
    reactable(filtered_data())
  })
  
  # RUBEN: Random species selection
  data_filtered_ruben <- eventReactive(input$random_select, {
    iris |>
      filter(Species == sample(Species, 1))
  })
  
  # RUBEN: Log button clicks
  observe({
    print(paste("User has clicked on the button", input$random_select, "time(s)"))
  })
}

# Run the application -----------------------------------------------------
shinyApp(ui, server, options = list())