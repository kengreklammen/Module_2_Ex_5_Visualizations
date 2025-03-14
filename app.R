# install libraries
install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")

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
          slidebarpanel(
            sliderInput(
              inputId = "species",
              label = "Choose a Species:",
              choices = levels(iris$Species)
            ),
            sliderInput(
              inputId = "sepal_length",
              label = "Sepal Length Range:",
              min = min(iris$Sepal.Length),
              max = max(iris$Sepal.Length),
              value = c(min(iris$Sepal.Length), max(iris$Sepal.Length))
            ),
            actionButton(
              inputId = "filter_data",
              label = "Filter Data"
            )
          ),
          
          mainPanel(
            tableOutput(outputId = "filtered_table")
          )
        )
    ),
          
        # FARIBA: filter1: select > choose species
        # ARPAD: filter2: slider > sepal.length
        # RUBEN: button: select random species
      
      nav_panel(title = "",
                layout_columns(
                  col_widths = c(6,6),
                  
                  
                  # Create the violin plot
                  fig <- iris %>%
                    plot_ly(type = 'violin'),
                  
                  # Add traces for each species
                  fig <- fig %>%
                    add_trace(
                      x = ~Species[IRIS$Species == 'setosa'],
                      y = ~Sepal.Length[IRIS$Species == 'setosa'],
                      legendgroup = 'Setosa',
                      scalegroup = 'Setosa',
                      name = 'Setosa',
                      side = 'negative',
                      box = list(
                        visible = TRUE
                      ),
                      meanline = list(
                        visible = TRUE
                      ),
                      color = I("blue")
                    ),
                  
                  fig <- fig %>%
                    add_trace(
                      x = ~Species[IRIS$Species == 'versicolor'],
                      y = ~Sepal.Length[IRIS$Species == 'versicolor'],
                      legendgroup = 'Versicolor',
                      scalegroup = 'Versicolor',
                      name = 'Versicolor',
                      side = 'positive',
                      box = list(
                        visible = TRUE
                      ),
                      meanline = list(
                        visible = TRUE
                      ),
                      color = I("green")
                    ),
                  
                  fig <- fig %>%
                    add_trace(
                      x = ~Species[IRIS$Species == 'virginica'],
                      y = ~Sepal.Length[IRIS$Species == 'virginica'],
                      legendgroup = 'Virginica',
                      scalegroup = 'Virginica',
                      name = 'Virginica',
                      side = 'positive',
                      box = list(
                        visible = TRUE
                      ),
                      meanline = list(
                        visible = TRUE
                      ),
                      color = I("red")
                    ),
                  
                  # Customize the layout
                  fig <- fig %>%
                    layout(
                      xaxis = list(
                        title = "Species"  # X-axis label
                      ),
                      yaxis = list(
                        title = "Sepal Length",  # Y-axis label
                        zeroline = FALSE
                      ),
                      violingap = 0,
                      violingroupgap = 0,
                      violinmode = 'overlay'
                    ),
                  
                  # Display the plot
                  fig, 
                  
                  
                  card(
                    card_header(""),
                    full_screen = T,
                    card_body(dataTableOutput("") ),
                    ),
                  #MIGUEL > DT
                  card(
                    card_header(""),
                    full_screen = T,
                    card_body(dataTableOutput("") ),
                  ),
                  #RUBEN > HIGHCHARTER
                  card(
                    card_header(""),
                    full_screen = T,
                    card_body(dataTableOutput("") ),
                  ),
                  #ARPAD > REACTABLE
                  card(
                    card_header(""),
                    full_screen = T,
                    card_body(dataTableOutput("") ),
                  ),
                ),
      ),
      nav_spacer(),
      nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
    )
  
# Define server logic -----------------------------------------------------
server <- function(input, output) {
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
  #EACH TEAM MEMBER TO WRITE THE SERVER LOGIC
}
# Run the application -----------------------------------------------------
shinyApp(ui, server, options = list())