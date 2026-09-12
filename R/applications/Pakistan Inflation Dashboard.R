# Pakistan Inflation Dashboard - R Shiny Application
# Install required packages first:
# install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "tidyr", "DT"))

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)

# Data Setup
# Current inflation measures (September 2025)
inflation_measures <- data.frame(
  name = c("Headline CPI", "Core CPI", "Food Inflation", 
           "Non-Food Inflation", "Urban CPI", "Rural CPI"),
  current = c(5.6, 7.0, 3.8, 8.2, 5.8, 5.3),
  previous = c(3.0, 7.2, 0.5, 8.5, 3.2, 2.7),
  target = c(6.0, 6.5, 6.0, 6.0, 6.0, 6.0),
  description = c("Overall consumer price inflation",
                  "Excludes food and energy",
                  "Food and beverages component",
                  "Excluding food items",
                  "Urban areas inflation",
                  "Rural areas inflation"),
  stringsAsFactors = FALSE
)

# Add status column
inflation_measures <- inflation_measures %>%
  mutate(
    status = case_when(
      abs(current - target) <= 0.5 ~ "target",
      current < target - 0.5 ~ "below",
      current > target + 0.5 & current < target + 2 ~ "above",
      TRUE ~ "high"
    ),
    change = current - previous
  )

# Historical data (last 24 months)
historical_data <- data.frame(
  month = c("Oct-23", "Nov-23", "Dec-23", "Jan-24", "Feb-24", "Mar-24",
            "Apr-24", "May-24", "Jun-24", "Jul-24", "Aug-24", "Sep-24",
            "Oct-24", "Nov-24", "Dec-24", "Jan-25", "Feb-25", "Mar-25",
            "Apr-25", "May-25", "Jun-25", "Jul-25", "Aug-25", "Sep-25"),
  headline = c(26.9, 29.2, 29.7, 28.3, 23.1, 20.7, 17.3, 11.8, 12.6, 
               11.1, 9.6, 6.9, 7.2, 4.9, 4.1, 2.7, 1.8, 0.7, 0.5, 
               3.5, 3.2, 4.5, 3.0, 5.6),
  core = c(18.5, 19.2, 20.1, 19.8, 17.5, 16.2, 15.1, 14.2, 13.8, 
           12.5, 11.3, 10.2, 9.8, 9.1, 8.5, 8.2, 7.9, 7.6, 7.4, 
           7.3, 7.1, 7.0, 7.2, 7.0),
  food = c(30.5, 34.1, 35.2, 32.8, 26.5, 23.8, 18.5, 10.2, 11.5, 
           9.8, 7.5, 3.8, 4.2, 1.5, 0.8, -1.2, -2.5, -4.1, -4.8, 
           1.2, 0.8, 2.5, -0.5, 3.8),
  policy = c(22.0, 22.0, 22.0, 22.0, 22.0, 22.0, 22.0, 22.0, 22.0,
             19.5, 19.5, 17.5, 17.5, 15.0, 15.0, 12.0, 12.0, 12.0,
             12.0, 11.0, 11.0, 11.0, 11.0, 11.0),
  stringsAsFactors = FALSE
)

# Component data
component_data <- data.frame(
  category = c("Food & Beverages", "Food & Beverages", "Food & Beverages",
               "Housing & Utilities", "Housing & Utilities", "Housing & Utilities",
               "Transportation", "Transportation",
               "Other", "Other", "Other"),
  item = c("Perishable Foods", "Non-Perishable Foods", "Beverages",
           "Electricity", "Gas", "Housing Rent",
           "Motor Fuel", "Transport Services",
           "Healthcare", "Education", "Communication"),
  value = c(-2.5, 8.2, 12.5, 25.8, 35.2, 4.5, -8.5, 15.3, 9.8, 14.2, 3.1),
  stringsAsFactors = FALSE
)

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Pakistan Inflation Dashboard",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("Components", tabName = "components", icon = icon("chart-pie")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    br(),
    div(style = "padding: 15px;",
        h5("Key Statistics", style = "color: white; font-weight: bold;"),
        p(style = "color: #ecf0f1; font-size: 12px;",
          "Policy Rate: 11.0%"),
        p(style = "color: #ecf0f1; font-size: 12px;",
          "Target: 5-7%"),
        p(style = "color: #ecf0f1; font-size: 12px;",
          "Last Updated: Sep 2025")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .info-box { min-height: 100px; }
        .info-box-icon { height: 100px; line-height: 100px; }
        .info-box-content { padding-top: 10px; padding-bottom: 10px; }
        .status-target { background-color: #d4edda; border: 2px solid #28a745; }
        .status-below { background-color: #d1ecf1; border: 2px solid #17a2b8; }
        .status-above { background-color: #fff3cd; border: 2px solid #ffc107; }
        .status-high { background-color: #f8d7da; border: 2px solid #dc3545; }
        .measure-box { padding: 15px; margin: 10px 0; border-radius: 8px; }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        
        fluidRow(
          box(
            title = "Pakistan Inflation Dashboard - Underlying Inflation Measures & Monetary Policy Tracker",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            p("This dashboard tracks multiple measures of inflation in Pakistan to provide a comprehensive view of price pressures."),
            tags$div(
              style = "display: flex; gap: 20px; font-size: 12px; margin-top: 10px;",
              tags$span(style = "display: flex; align-items: center;",
                        tags$span(style = "width: 12px; height: 12px; background-color: #28a745; border-radius: 50%; margin-right: 5px;"),
                        "Within Target"),
              tags$span(style = "display: flex; align-items: center;",
                        tags$span(style = "width: 12px; height: 12px; background-color: #ffc107; border-radius: 50%; margin-right: 5px;"),
                        "Moderately Above"),
              tags$span(style = "display: flex; align-items: center;",
                        tags$span(style = "width: 12px; height: 12px; background-color: #dc3545; border-radius: 50%; margin-right: 5px;"),
                        "Significantly Above"),
              tags$span(style = "display: flex; align-items: center;",
                        tags$span(style = "width: 12px; height: 12px; background-color: #17a2b8; border-radius: 50%; margin-right: 5px;"),
                        "Below Target")
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("policyRateBox", width = 4),
          valueBoxOutput("realRateBox", width = 4),
          valueBoxOutput("expectationsBox", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Current Inflation Measures (September 2025)",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            DTOutput("measuresTable")
          )
        ),
        
        fluidRow(
          box(
            title = "Inflation Trends Overview",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("overviewChart", height = "400px")
          )
        )
      ),
      
      # Time Series Tab
      tabItem(
        tabName = "timeseries",
        
        fluidRow(
          box(
            title = "Inflation Trends & Monetary Policy Rate",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("timeseriesChart", height = "500px"),
            br(),
            div(
              style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px;",
              tags$strong("Key Insight: "),
              "Pakistan's policy rate has been cut from 22% to 11% since June 2024, 
              as inflation dropped from a peak of nearly 30% to current levels. The State Bank 
              maintained rates at 11% since May 2025 due to energy price adjustments and flood-related risks."
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Select Measures to Display",
            width = 12,
            checkboxGroupInput("selectedMeasures",
                               NULL,
                               choices = c("Headline CPI" = "headline",
                                           "Core CPI" = "core",
                                           "Food Inflation" = "food",
                                           "Policy Rate" = "policy"),
                               selected = c("headline", "core", "policy"),
                               inline = TRUE)
          )
        )
      ),
      
      # Components Tab
      tabItem(
        tabName = "components",
        
        fluidRow(
          box(
            title = "Inflation by Component Category",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("componentsChart", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Component Details",
            width = 12,
            DTOutput("componentsTable")
          )
        ),
        
        fluidRow(
          box(
            title = "Note",
            width = 12,
            status = "warning",
            p("Energy prices (electricity and gas) remain the largest contributors to inflation 
              following recent tariff adjustments. Food inflation has moderated significantly from earlier highs.")
          )
        )
      ),
      
      # About Tab
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            title = "About This Dashboard",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            h4("Purpose"),
            p("This dashboard tracks multiple measures of inflation in Pakistan to provide a 
              comprehensive view of price pressures in the economy, similar to the Federal Reserve 
              Bank of Atlanta's Underlying Inflation Dashboard."),
            
            h4("Data Sources"),
            tags$ul(
              tags$li("State Bank of Pakistan (SBP)"),
              tags$li("Pakistan Bureau of Statistics (PBS)"),
              tags$li("International Monetary Fund (IMF)")
            ),
            
            h4("Update Frequency"),
            p("Monthly - Data is updated following the release of official CPI figures"),
            
            h4("Methodology"),
            p("Inflation measures are color-coded based on their distance from the SBP's target range of 5-7%:"),
            tags$ul(
              tags$li(tags$strong("Green:"), " Within target range (±0.5pp)"),
              tags$li(tags$strong("Yellow:"), " Moderately above target (0.5-2.0pp above)"),
              tags$li(tags$strong("Red:"), " Significantly above target (>2.0pp above)"),
              tags$li(tags$strong("Blue:"), " Below target range")
            ),
            
            h4("Key Indicators"),
            tags$ul(
              tags$li(tags$strong("Headline CPI:"), " Overall consumer price inflation"),
              tags$li(tags$strong("Core CPI:"), " Excludes volatile food and energy prices"),
              tags$li(tags$strong("Food Inflation:"), " Food and beverages component"),
              tags$li(tags$strong("Policy Rate:"), " State Bank of Pakistan's benchmark interest rate")
            ),
            
            h4("Contact"),
            p("For questions or feedback about this dashboard, please contact the analytics team.")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Value Boxes
  output$policyRateBox <- renderValueBox({
    valueBox(
      "11.0%",
      "Current Policy Rate",
      subtitle = "Unchanged since May 2025",
      icon = icon("percent"),
      color = "blue"
    )
  })
  
  output$realRateBox <- renderValueBox({
    valueBox(
      "5.4%",
      "Real Interest Rate",
      subtitle = "Policy Rate - Headline CPI",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$expectationsBox <- renderValueBox({
    valueBox(
      "66.6",
      "Inflation Expectations",
      subtitle = "Consumer Expectations Index ↓",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  # Measures Table
  output$measuresTable <- renderDT({
    df <- inflation_measures %>%
      mutate(
        `Current (%)` = sprintf("%.1f%%", current),
        `Change (pp)` = sprintf("%+.1f", change),
        `Target (%)` = sprintf("%.1f%%", target),
        `Status` = case_when(
          status == "target" ~ "Within Target",
          status == "below" ~ "Below Target",
          status == "above" ~ "Above Target",
          status == "high" ~ "Significantly Above"
        )
      ) %>%
      select(Measure = name, 
             Description = description,
             `Current (%)`,
             `Change (pp)`,
             `Target (%)`,
             Status)
    
    datatable(df, 
              options = list(
                dom = 't',
                pageLength = 10,
                ordering = FALSE
              ),
              rownames = FALSE) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c("Within Target", "Below Target", "Above Target", "Significantly Above"),
          c("#d4edda", "#d1ecf1", "#fff3cd", "#f8d7da")
        ),
        fontWeight = 'bold'
      )
  })
  
  # Overview Chart
  output$overviewChart <- renderPlotly({
    # Create ordered factor for months
    data_for_plot <- historical_data %>%
      mutate(month_ordered = factor(month, levels = month))
    
    plot_ly(data_for_plot) %>%
      add_trace(
        x = ~month_ordered, 
        y = ~headline, 
        type = 'scatter', 
        mode = 'lines+markers',
        name = 'Headline CPI', 
        line = list(color = '#3b82f6', width = 3),
        marker = list(size = 4)
      ) %>%
      add_trace(
        x = ~month_ordered, 
        y = ~core, 
        type = 'scatter', 
        mode = 'lines+markers',
        name = 'Core CPI', 
        line = list(color = '#f59e0b', width = 2),
        marker = list(size = 4)
      ) %>%
      add_trace(
        x = ~month_ordered, 
        y = ~policy, 
        type = 'scatter', 
        mode = 'lines',
        name = 'Policy Rate', 
        line = list(color = '#ef4444', width = 3, dash = 'dash')
      ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "", 
          tickangle = -45,
          categoryorder = "trace",
          tickfont = list(size = 10)
        ),
        yaxis = list(
          title = "Percentage (%)",
          titlefont = list(size = 12)
        ),
        hovermode = 'x unified',
        legend = list(
          x = 0.02, 
          y = 0.98,
          bgcolor = 'rgba(255, 255, 255, 0.8)',
          bordercolor = '#e2e8f0',
          borderwidth = 1
        ),
        margin = list(l = 60, r = 30, t = 20, b = 100)
      )
  })
  
  # Time Series Chart with selections - FIXED VERSION
  output$timeseriesChart <- renderPlotly({
    # Create ordered factor for proper chronological display
    data_for_plot <- historical_data %>%
      mutate(month_ordered = factor(month, levels = month))
    
    p <- plot_ly(data_for_plot)
    
    if ("headline" %in% input$selectedMeasures) {
      p <- p %>% add_trace(
        x = ~month_ordered, 
        y = ~headline, 
        type = 'scatter', 
        mode = 'lines+markers',
        name = 'Headline CPI', 
        line = list(color = '#3b82f6', width = 3),
        marker = list(size = 4)
      )
    }
    
    if ("core" %in% input$selectedMeasures) {
      p <- p %>% add_trace(
        x = ~month_ordered, 
        y = ~core, 
        type = 'scatter', 
        mode = 'lines+markers',
        name = 'Core CPI', 
        line = list(color = '#f59e0b', width = 2),
        marker = list(size = 4)
      )
    }
    
    if ("food" %in% input$selectedMeasures) {
      p <- p %>% add_trace(
        x = ~month_ordered, 
        y = ~food, 
        type = 'scatter', 
        mode = 'lines+markers',
        name = 'Food Inflation', 
        line = list(color = '#10b981', width = 2),
        marker = list(size = 4)
      )
    }
    
    if ("policy" %in% input$selectedMeasures) {
      p <- p %>% add_trace(
        x = ~month_ordered, 
        y = ~policy, 
        type = 'scatter', 
        mode = 'lines',
        name = 'Policy Rate', 
        line = list(color = '#ef4444', width = 3, dash = 'dash')
      )
    }
    
    p %>% layout(
      xaxis = list(
        title = "", 
        tickangle = -45,
        categoryorder = "trace",  # Important: maintains chronological order
        tickfont = list(size = 10)
      ),
      yaxis = list(
        title = "Percentage (%)",
        titlefont = list(size = 12)
      ),
      hovermode = 'x unified',
      legend = list(
        x = 0.02, 
        y = 0.98,
        bgcolor = 'rgba(255, 255, 255, 0.8)',
        bordercolor = '#e2e8f0',
        borderwidth = 1
      ),
      margin = list(l = 60, r = 30, t = 20, b = 100)
    )
  })
  
  # Components Chart
  output$componentsChart <- renderPlotly({
    # Sort and prepare data
    data_sorted <- component_data %>%
      arrange(value) %>%
      mutate(
        item_ordered = factor(item, levels = item),
        color_val = case_when(
          value > 10 ~ '#dc3545',
          value > 0 ~ '#ffc107',
          TRUE ~ '#28a745'
        )
      )
    
    plot_ly(data_sorted) %>%
      add_trace(
        y = ~item_ordered,
        x = ~value,
        type = 'bar',
        orientation = 'h',
        marker = list(color = ~color_val),
        text = ~paste0(sprintf("%.1f", value), "%"),
        textposition = 'outside',
        textfont = list(size = 11),
        hovertemplate = paste(
          '<b>%{y}</b><br>',
          'YoY Change: %{x:.1f}%',
          '<extra></extra>'
        )
      ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "Year-over-Year Change (%)",
          titlefont = list(size = 12)
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 10)
        ),
        showlegend = FALSE,
        margin = list(l = 150, r = 80, t = 40, b = 60)
      )
  })
  
  # Components Table
  output$componentsTable <- renderDT({
    df <- component_data %>%
      mutate(
        `Value (%)` = sprintf("%.1f%%", value)
      ) %>%
      select(Category = category, 
             Item = item,
             `YoY Change (%)` = `Value (%)`)
    
    datatable(df, 
              options = list(
                pageLength = 15,
                ordering = TRUE
              ),
              rownames = FALSE) %>%
      formatStyle(
        'YoY Change (%)',
        backgroundColor = styleInterval(c(0), c('#e8f5e9', '#ffebee')),
        fontWeight = 'bold'
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)