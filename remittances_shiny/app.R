# Load necessary libraries
library(shiny)
library(fpp3)
library(tsibble)
library(dplyr)
library(ggplot2)
library(fable)
library(forecast)
library(readr)
library(lubridate)

# Load data
rmt <- read.csv("D:/RepTemplates/timeseries/data/remittances_karandaaz.csv")

# Convert to time series
rmt$date <- as.Date(rmt$date, format = "%Y-%m-%d")

# UI
ui <- fluidPage(
  
  titlePanel("Remittances Forecasting"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:",
                  choices = c("Saudi.Arabia", "United.Arab.Emirates", "USA", "United.Kingdom", "EU.Countries")),
      selectInput("method", "Select Forecasting Method:",
                  choices = c("SNAIVE", "SES", "Holt", "Holt-Winters", "ARIMA")),
      actionButton("submit_button", "Generate Forecast")
    ),
    
    mainPanel(
      plotOutput("forecast_plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  observeEvent(input$submit_button, {
    
    # Filter data based on selected country
    selected_data <- rmt %>% dplyr::filter(country == input$country)
    
    if(nrow(selected_data) == 0) {
      return(NULL)  # Return NULL if no data matches the selected country
    }
    
    # Convert to tsibble
    ts_data <- as_tsibble(selected_data, key = country, index = date) %>%
      fill_gaps(remittances = 0)  # Fill gaps with 0s
    
    # Check if ts_data is empty after filling gaps
    if(nrow(ts_data) == 0) {
      return(NULL)
    }
    
    # Forecast based on selected method
    forecast_result <- switch(input$method,
                              "SNAIVE" = ts_data |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12),
                              "SES" = ts_data |> model(SES(remittances)) |> forecast(h = 12),
                              "Holt" = ts_data |> model(Holt(remittances)) |> forecast(h = 12),
                              "Holt-Winters" = ts_data |> model(HoltWinters(remittances)) |> forecast(h = 12),
                              "ARIMA" = ts_data |> model(ARIMA(remittances)) |> forecast(h = 12)
    )
    
    # Plot forecast
    output$forecast_plot <- renderPlot({
      forecast_result |> autoplot(ts_data) +
        labs(title = paste("Forecast for", input$country, "using", input$method),
             x = "Date",
             y = "Remittances") +
        theme_minimal()
    })
    
  })
  
}

shinyApp(ui = ui, server = server)
