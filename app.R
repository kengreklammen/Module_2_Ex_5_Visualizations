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
        # FARIBA: filter1: select > choose species
        # ARPAD: filter2: slider > sepal.length
        # RUBEN: button: select random species
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
                    card_body(dataTableOutput("") ),
                  ),
                ),
      ),
      nav_spacer(),
      nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
    ),
  )
# Define server logic -----------------------------------------------------
server <- function(input, output) {
  
  #EACH TEAM MEMBER TO WRITE THE SERVER LOGIC
}
# Run the application -----------------------------------------------------
shinyApp(ui, server, options = list())