# install libraries
if(!require("shiny")) install.packages("shiny")
if(!require("bslib")) install.packages("bslib")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("dplyr")) install.packages("dplyr")
if(!require("tidyr")) install.packages("tidyr")
if(!require("reactable")) install.packages("reactable")
if(!require("DT")) install.packages("DT")
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
library(DT)
library(plotly)

# load data
data(iris)

# Define UI ---------------------------------------------------------------
ui <- 
  page_fluid(
    # app title ----
    titlePanel("Iris dataset"),
    # theme
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      version = "5"),
    # sidebar layout with input and output definitions ----
    navset_card_pill(
      sidebar = sidebar(
        # FARIBA: filter1: select > choose species
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
        ),
        # ARPAD: filter2: slider > sepal.length
        sliderInput("sepal", "Sepal length:", min = 4.3, max = 7.9, value = c(4.3, 7.9)),       
        
        # RUBEN: button: select random species
        actionButton("random_select",
                     "Choose a random specie")
      ),
      #FARIBA > CHART W/ PLOTLY 
      nav_panel(title = "Plotly",
                layout_columns(
                  col_widths = c(12),
                  card(
                    card_header("Plot"),
                    full_screen = T,
                    card_body(plotlyOutput("violin_plot")),
                  ),
                  card(
                    card_header("Table"),
                    full_screen = T,
                    card_body(tableOutput(outputId = "filtered_table")),
                    ),
                  )
                ),
      #MIGUEL > DT
      nav_panel(title = "DT",
                layout_columns(
                  col_widths = c(12),
                  card(
                    card_header("DT example"),
                    full_screen = T,
                    card_body(dataTableOutput("table1")),
                    ),
                  )
                ),
      #ARPAD > REACTABLE
      nav_panel(title = "Reactable",
                layout_columns(
                  col_widths = c(12),
                  card(
                    card_header("Reactable example"),
                    full_screen = T,
                    card_body(reactableOutput("table3")),
                    ),
                  )
                ),
      #RUBEN > HIGHCHARTER
      nav_panel(title = "Highcharter",
                layout_columns(
                  col_widths = c(12),
                  card(
                    card_header("Highcharter example"),
                    full_screen = T,
                    card_body(dataTableOutput("")),
                    ),
                  )
                ),
      nav_spacer(),
      nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
      )
    )
# Define server logic -----------------------------------------------------
server <- function(input, output) {
  #EACH TEAM MEMBER TO WRITE THE SERVER LOGIC
  
  #ARPAD
  filtered_sepal <- eventReactive(input$sepal, {
    iris |> 
      dplyr::filter(Sepal.Length >= input$sepal[1] & Sepal.Length <= input$sepal[2])
  })
  output$table3 <- renderReactable({
    reactable(filtered_sepal())
  })
  
  # MIGUEL
  filtered_dataDT <- reactive({
    iris |>
    filter(Species == input$species & Sepal.Length >= input$sepal[1] & Sepal.Length <= input$sepal[2])
  })

  # Render the DT table based on the filtered data
  output$table1 <- renderDataTable({
    datatable(filtered_dataDT(), filter = "top", colnames = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width", "Species"))
  })
  
  #RUBEN
  # eventReactive() to choose a random species when pressing an input button
  data_filtered_ruben <- eventReactive(input$random_select,{
    iris |>
      dplyr::filter(Species == sample(Species,1))
  })
  # Use observe() to log changes in the console.
  observe({
    print(paste("User has clicked on the button",input$random_select,"time(s)"))
  })
  
  #FARIBA
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
shinyApp(ui, server, options = list())