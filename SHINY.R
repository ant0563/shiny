library(shiny)
library(readxl)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(plotly)
library(DT)
##############
入庫成本分析 <- read_excel("C:/Users/User/Desktop/R/project/產品入庫成本明細/產品入庫成本分析彙整.xlsx", 
                     sheet = "扣除令號合併")

final <- read_excel("C:/Users/User/Desktop/R/project/產品入庫成本明細/產品入庫成本分析彙整.xlsx", 
                    sheet = "產品入庫成本明細表+8月")

ui數據<-fluidPage(dataTableOutput("dynamic"))
server數據 <- function(input, output, session)  {
  output$dynamic <- renderDataTable(入庫成本分析, options = list(pageLength =100))
}

shinyApp(ui數據, server數據)

##############

ui1<-fluidPage(
  titlePanel("入庫成本分析明細"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("產品品號",
                       "產品品號:",
                       c("All",
                         unique(as.character(入庫成本分析$產品品號))))
    ),
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)# end of fluidPage



sever1<-function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- 入庫成本分析
    if (input$產品品號 != "All") {
      data <- data[入庫成本分析$產品品號 == input$產品品號,]
    }
    
    
    
    data
  }))
  output$plot <- DT::renderDataTable(DT::datatable({
    data <- 入庫成本分析
    if (input$產品品號 != "All") {
      data <- data[入庫成本分析$產品品號 == input$產品品號,]
    }
    data
  }))  
}


shinyApp(ui1, sever1)

################





ui <- fluidPage(
  titlePanel("Example panel"),
  tabsetPanel(
    tabPanel(
      "example text",
      sidebarPanel(
        width = 4,
        dateRangeInput("daterange", "Select dates",
                       format = "yyyy-mm-dd",
                       start = min("2015-01-01"),
                       end = max("2015-01-10")),
        numericInput("hourmin", "Select mininum hour", 10, 0, 23),
        numericInput("hourmax", "Select maximum hour", 22, 0, 23),
        pickerInput(channel, "Select channel",
                    choices = unique(channel),
                    options = list("actions-box" = T, "live-search" = T),
                    multiple = T,
                    selected = unique(channel))), # end of sidebarPanel
      mainPanel(column(width = 10, plotOutput("barplot", width = "100%")),
                column(width = 8, DTOutput("table"))
      ) # end of mainPanel
    ) # end of tabPanel
  ) # end of tabsetPanel
) # end of fluidPage


uix<-fluidPage(
  titlePanel("生產入庫明細分析"),
  tabsetPanel(
    tabPanel(
      "test",
      sidebarPanel(
        width = 4,
        
        pickerInput(入庫成本分析, "產品品號:",
                    choices = unique(入庫成本分析),
                    options = list("actions-box" = T, "live-search" = T),
                    multiple = T,
                    selected = unique(入庫成本分析))), # end of sidebarPanel
      mainPanel(column(width = 10, plotOutput("barplot", width = "100%")),
                column(width = 8, DTOutput("table"))
      ) # end of mainPanel
    ) # end of tabPanel
  ) # end of tabsetPanel
)



###品號對應###!讚讚####
ui2 <- fluidPage(
  selectizeInput(
    inputId = "產品品號", 
    label = "產品品號", 
    choices = unique(入庫成本分析$產品品號), 
    selected = "Abilene",
    multiple = TRUE),
  plotlyOutput(outputId = "p")
)

###########
server2 <- function(input, output, ...) {

  output$p <- renderPlotly({
    plot_ly(入庫成本分析, x = ~製令編號, y = ~單位生產成本) %>%
      filter(產品品號 %in% input$產品品號) %>%
      group_by(產品品號) %>%
      add_lines()
  })
}

shinyApp(ui2, server2)





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
