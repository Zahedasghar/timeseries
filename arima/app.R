# Load necessary libraries
library(shiny)
library(ggplot2)
library(forecast)

# Define UI
ui <- fluidPage(
  
  titlePanel("AirPassengers Data Forecasting"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("forecast_method", "Select Forecasting Method:",
                  choices = c("ETS", "ARIMA", "Holt-Winters")),
      numericInput("forecast_periods", "Number of Forecast Periods:", 12)
    ),
    
    mainPanel(
      plotOutput("forecast_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$forecast_plot <- renderPlot({
    
    # Convert AirPassengers to time series
    ts_data <- ts(AirPassengers, frequency = 12)
    
    if (input$forecast_method == "ETS") {
      # Forecast using ETS
      forecast_result <- forecast(ets(ts_data), h = input$forecast_periods)
      
    } else if (input$forecast_method == "ARIMA") {
      # Forecast using ARIMA
      forecast_result <- forecast(auto.arima(ts_data), h = input$forecast_periods)
      
    } else if (input$forecast_method == "Holt-Winters") {
      # Forecast using Holt-Winters
      forecast_result <- forecast(HoltWinters(ts_data), h = input$forecast_periods)
    }
    
    # Plotting forecast
    autoplot(forecast_result) + 
      labs(title = "AirPassengers Forecast",
           x = "Date",
           y = "Number of Passengers")
    
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
