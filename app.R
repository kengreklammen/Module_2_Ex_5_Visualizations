# install libraries

if(!require("shiny")) install.packages("shiny")
if(!require("bslib")) install.packages("bslib")
if(!require("ggplot")) install.packages("ggplot")
if(!require("dplyr")) install.packages("dplyr")
if(!require("tidyr")) install.packages("tidyr")
if(!require("reactable")) install.packages("reactable")

library(shiny)
library(reactable)


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
        # RUBEN: button: select random species
        
        sliderInput("sepal", "Sepal length:", min = 4.3, max = 7.9, value = c(4.3, 7.9)),
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
                    card_body(reactableOutput("table3")),
                  ),
                ),
      ),
      nav_spacer(),
      nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
    ),
  )
# Define server logic -----------------------------------------------------
server <- function(input, output) {
  filtered_sepal <- eventReactive(input$sepal, {
    iris |> 
      dplyr::filter(Sepal.Length >= input$sepal[1] & Sepal.Length <= input$sepal[2])
    
  })
  
  #EACH TEAM MEMBER TO WRITE THE SERVER LOGIC
  output$table3 <- renderReactable({
    reactable(filtered_sepal())
  })
}
# Run the application -----------------------------------------------------
shinyApp(ui, server, options = list())