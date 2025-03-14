# install libraries
if(!require("shiny")) install.packages("shiny")
if(!require("bslib")) install.packages("bslib")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("dplyr")) install.packages("dplyr")
if(!require("tidyr")) install.packages("tidyr")
if(!require("reactable")) install.packages("reactable")
if(!require("DT")) install.packages("DT")

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
        # ARPAD: filter2: slider > sepal.length
          sliderInput("sepal", "Sepal length:", min = 4.3, max = 7.9, value = c(4.3, 7.9)),       
        # RUBEN: button: select random species
          actionButton("random_select",
                       "Choose a random specie")
      ),
        nav_panel(title = "",
                layout_columns(
                  col_widths = c(6,6),
                  #FARIBA > CHART W/ PLOTLY 
                  card(
                    card_header(""),
                    full_screen = T,
                    card_body(dataTableOutput("") ),
                    ),
                  #MIGUEL > DT
                  card(
                    card_header("DT example"),
                    full_screen = T,
                    card_body(dataTableOutput("table1")),
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
                    card_body(reactableOutput("table3")),
                    ),
                  )
                ),
        nav_spacer(),
        nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
      )
    )
# Define server logic -----------------------------------------------------
server <- function(input, output) {
  filtered_sepal <- eventReactive(input$sepal, {
    iris |> 
      dplyr::filter(Sepal.Length >= input$sepal[1] & Sepal.Length <= input$sepal[2])
    
  })
  
  #EACH TEAM MEMBER TO WRITE THE SERVER LOGIC

  #RUBEN
  # eventReactive() to choose a random specie when pressing an input button
  data_filtered_ruben <- eventReactive(input$random_select,{
    iris |>
      dplyr::filter(Species == sample(Species,1))
  })
  # Use observe() to log changes in the console.
  observe({
    print(paste("User has clicked on the button",input$random_select,"time(s)"))
  })
  
  #ARPAD
  output$table3 <- renderReactable({
    reactable(filtered_sepal())

    filtered_data <- iris |>
    filter(Species == "setosa" & Sepal.Length >= 4.0 & Sepal.Length <= 5.0)
  
  # MIGUEL
  # Render the DT table based on the filtered data
  output$table1 <- renderDataTable({
    datatable(filtered_data, filter = "top", colnames = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width", "Species"))
  })
}
# Run the application -----------------------------------------------------
shinyApp(ui, server, options = list())