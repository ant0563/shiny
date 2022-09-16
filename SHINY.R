library(shiny)
library(readxl)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(plotly)
library(DT)
##############

final <- read_excel("C:/Users/User/Desktop/R/project/產品入庫成本明細/產品入庫成本分析彙整.xlsx", 
                    sheet = "產品入庫成本明細表+8月")


##############
ui <- fluidPage(
  titlePanel("產品入庫成本明細"),
      selectizeInput(
        inputId = "產品品號", 
        label = "產品品號", 
        choices = unique(final$產品品號), 
        selected = "Abilene",
        multiple = TRUE),
      # Add plot output named 'shapes'
      plotlyOutput(outputId = "p"),
      # Add table output named 'duration_table'
      tableOutput("duration_table")
)

server <- function(input, output, ...) {
  # CODE BELOW: Create a plot output name 'shapes', of sightings by shape,
  # For the selected inputs
  output$p <- renderPlotly({
    plot_ly(final, x = ~製令編號, y = ~單位生產成本) %>%
      filter(產品品號 %in% input$產品品號) %>%
      group_by(產品品號) %>%
      add_lines()
  })
  # CODE BELOW: Create a table output named 'duration_table', by shape, 
  # of # sighted, plus mean, median, max, and min duration of sightings
  # for the selected inputs
  output$duration_table <- renderTable({
    final %>%
      filter(產品品號 %in% input$產品品號
      ) %>%
      group_by(產品品號)
  })
}

shinyApp(ui,server)
