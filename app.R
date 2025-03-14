# install libraries
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")

library(shiny)
library(bslib)
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
  
  # filtered_data <- data |>
  #   filter(data$Species == "setosa" & data$Sepal.Length >= 4.0 & data$Sepal.Length <= 5.0)
  
  # Render the DT table based on the filtered data
  output$table1 <- renderDataTable(iris)
  
  # , options = list(), class = "display",
  #           callback = JS("return table;"), rownames, colnames, container,
  #           caption = NULL, filter = c("none", "bottom", "top"), escape = TRUE,
  #           style = "auto", width = NULL, height = NULL, elementId = NULL,
  #           fillContainer = getOption("DT.fillContainer", NULL),
  #           autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
  #           selection = c("multiple", "single", "none"), extensions = list(),
  #           plugins = NULL, editable = FALSE)
}
# Run the application -----------------------------------------------------
shinyApp(ui, server, options = list())