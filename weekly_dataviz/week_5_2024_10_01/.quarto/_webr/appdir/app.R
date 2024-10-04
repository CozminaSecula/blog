library(tidyverse)
library(janitor)
library(plotly)
library(shinyWidgets)
library(ggrepel)
library(shiny)


raw_df <- tibble(
    Year = c(2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009),
    `EURO STOXX 50` = c("27%", "-15%", "14%", "6%", "26%", "-16%", "24%", "0%", "-5%", "-8%", "27%", "20%", "-17%", "-10%", "30%"),
    `FTSE 100` = c("14%", "-7%", "17%", "-9%", "21%", "-14%", "23%", "-1%", "-6%", "-5%", "21%", "15%", "-2%", "8%", "41%"),
    `MSCI China` = c("-11%", "-22%", "-22%", "30%", "24%", "-19%", "54%", "1%", "-8%", "8%", "4%", "23%", "-18%", "5%", "63%"),
    `Nasdaq-100` = c("55%", "-33%", "27%", "48%", "39%", "-0%", "33%", "7%", "9%", "19%", "36%", "18%", "4%", "20%", "54%"),
    `NYSE Arca Gold Bugs` = c("8%", "-9%", "-12%", "25%", "52%", "-16%", "6%", "65%", "-31%", "-16%", "-55%", "-10%", "-12%", "34%", "43%"),
    `S&P 500` = c("26%", "-18%", "29%", "18%", "31%", "-4%", "22%", "12%", "1%", "14%", "32%", "16%", "2%", "15%", "26%")
)

df_returns_p <- raw_df |>
  pivot_longer(cols = c("EURO STOXX 50","FTSE 100", "MSCI China", "Nasdaq-100", "NYSE Arca Gold Bugs", "S&P 500"),
               names_to = "index",
               values_to = "performance") |>
  mutate(performance = as.numeric(str_remove(performance, "%")))|>
  clean_names()


# Customize colors for the indices
index_colors <- c(
  "EURO STOXX 50" = "#174A7E",
  "FTSE 100" = "#c97b1d",
  "MSCI China" = "#800000",
  "Nasdaq-100" = "#95B3D7",
  "NYSE Arca Gold Bugs" = "#fdbf11",
  "S&P 500" = "#0C8040"
)

source("2024_09_30/theme_rdv.R")

# Define indices
index <- unique(df_returns_p$index)


# Define UI
ui <- fluidPage(
  # Main title and subtitle together
  fluidRow(
    column(12,
           h1("Stock Index Annual Returns (2009-2023)", style = "font-family: Lato; font-size: 30px;"),  # Main title
           h4(HTML("EuroStoxx 50, FTSE 100, MSCI China, Nasdaq-100, NYSE Arca Gold BUGS,<br> and S&P 500: annual returns over the past 14 years."), style = "font-family: Lato; font-size: 20px;") # Subtitle with styling
    )
  ),

  # Set the sidebar to be on the right side
  sidebarLayout(
    mainPanel(
      plotlyOutput("plot"),  # Plot on the left side

      # Add the source information below the plot
      fluidRow(
        column(12,
               p(
                 "Data source: ",
                 tags$a(href = "https://data.world/makeovermonday/financial-markets-historical-performance", "Makeover Monday/Financial Markets Historical Performance"),  # Example source link
                 style = "font-size: 12px; color: grey; margin-top: 20px;"),
               br(),
               "SOURCE & ARTICLE:",
               tags$a(href = "https://curvo.eu/backtest/en/compare-indexes/euro-stoxx-50-vs-ftse-100-vs-msci-china-vs-nasdaq-100-vs-nyse-arca-gold-bugs-vs-sp-500?currency=usd#annual-returns", "EURO STOXX 50 vs FTSE 100 vs MSCI China vs Nasdaq-100 vs NYSE Arca <br> Gold BUGS vs S&P 500: historical performance")
        )
      )
    ),

    sidebarPanel(
      # MultiInput with indices names only
      multiInput(
        inputId = "indices",
        label = "Select Index:",
        choices = index,  # Only index names
        selected = c("Nasdaq-100", "S&P 500")
      ),
      # Slider for year range input
      sliderInput("yearRange", "Year Range:",
                  min = min(df_returns_p$year),
                  max = max(df_returns_p$year),
                  value = c(min(df_returns_p$year),
                            max(df_returns_p$year)),
                  step = 1)
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Ensure the year column is numeric (if it's a factor, this would be problematic)
  df_returns_p$year <- as.numeric(as.character(df_returns_p$year))

  filtered_data <- reactive({
    df_returns_p |>
      filter(year >= input$yearRange[1] & year <= input$yearRange[2]) |>
      filter(index %in% input$indices)
  })

  output$plot <- renderPlotly({
    # Create ggplot object
    p2 <- ggplot(filtered_data(),
                aes(y = performance,
                    x = year,
                    color = index)) +
      geom_line(aes(group = index, colour = index), size = 0.4) +
      geom_point(size = 1.9) +
      geom_hline(yintercept = 0,
                 colour = "#000000",
                 linewidth = 0.5) +
      scale_color_manual(values = index_colors) +
      scale_y_continuous(limits = c(-70, 70),
                         breaks = seq(-70, 70, by = 10),
                         labels = scales::percent_format(scale = 1)) +
      scale_x_continuous(limits = c(2009, 2023),
                         breaks = seq(2009, 2023, by = 2)) +
      labs(
        title = "",
        x = "",
        y = ""
      ) +
      theme_rdv() +
      theme(legend.position = "none")
    # Convert ggplot object to plotly object
    ggplotly(p2, tooltip = c("index", "year", "performance"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

