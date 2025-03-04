# ---------------------------------------------
# Import libraries
# ---------------------------------------------
library(shiny)
library(plotly)
library(data.table)
# ---------------------------------------------

# ---------------------------------------------
# Import source scripts
# ---------------------------------------------
source("interp_idw.R")  
source("interp_idw_gstat.R")  
source("interp_kriging.R") 
source("interp_akima.R") 
# ---------------------------------------------

# ---------------------------------------------
# Upload data for analysis
# ---------------------------------------------
# data_merged_setected_times = read.csv("output/data_merged_setected_times.csv")
df_all <- na.omit(data_merged_setected_times)

# ---------------------------------------------

# ---------------------------------------------
# Define UI
# ---------------------------------------------
ui <- fluidPage(
  titlePanel("Amalie - soil temperature and moisture interactive graph"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("day", "Choose day:",
                value = as.Date("2023-12-25"),
                min   = as.Date("2020-12-25"),
                max   = as.Date("2024-10-27")),
      selectInput("time", "Choose time (UTC):",
                  choices = c("00:00:00", "04:00:00", "08:00:00",
                              "12:00:00", "16:00:00", "20:00:00"),
                  selected = "08:00:00"),
      selectInput("tempChoice", "Choose variable:", 
                  choices = c("T1", "T2", "T3", "moisture"), selected = "T1"),
      selectInput("interpMethod", "Choose interpolation method:", 
                  choices = c("IDW", "IDWgstat", "Kriging", "AkimaLinear"), selected = "IDW"),
      checkboxInput("interp", "Show Interpolation", value = FALSE)
    ),
    mainPanel(
      plotlyOutput("myPlot")
    )
  )
)
# ---------------------------------------------

# ---------------------------------------------
# Define server
# ---------------------------------------------
server <- function(input, output, session) {
  
  # Reactive expression that filters data - date and time
  filtered_data <- reactive({
    user_dt <- as.POSIXct(
      paste(input$day, input$time),
      tz = "UTC"
    )
    df_sub <- df_all[df_all$DTM == user_dt, ]
    df_sub
  })
  
  # Reactive expression to subset chosen temperature
  selected_data <- reactive({
    df_filtered <- filtered_data()  
    df_filtered$VarSelected <- df_filtered[[ input$tempChoice ]]
    df_selected <- df_filtered[, c("ID", "x", "y", "VarSelected")]
    df_selected
  })
  
  # Reactive expression to do interpolation if the user checks the box
  interpolation_data <- reactive({
    method <- input$interpMethod
    
    req(input$interp)  # only proceed if input$interp == TRUE
    dat <- selected_data()
    
    # We can unify arguments
    x <- dat$x
    y <- dat$y
    z <- dat$VarSelected
    
    # Call the chosen function
    if (method == "IDW") {
      interp_df <- idw_interpol(x, y, z)
    } else if (method == "IDWgstat") {
      interp_df <- idw_gstat_interpol(x, y, z)
    } else if (method == "Kriging") {
      interp_df <- kriging_interpol(x, y, z)
    } else {
      interp_df <- akima_interpol(x, y, z)
    }
    
    interp_df
  })
  
  # Draw the plot
  output$myPlot <- renderPlotly({
    dat <- selected_data()     # the raw measuring points
    plt <- plot_ly()          # start an empty plot_ly object
    
    # Estimate unit for the selected variable
    if (input$tempChoice == "moisture") {
      unit_name = " [m3/m3]"
    }
    else {
      unit_name = " [˚C]"
    }
    
    # Validate data availability due to NA's in the dataset
    if(nrow(dat) == 0) {
      showNotification("No data for your selection!", type = "warning")
    }
    validate(
      need(nrow(dat) > 0, "No data available — can't create the plot.")
    )
    
    # If the user checked "Show Interpolation", add a trace for the interpolated surface
    if (input$interp) {
      interp_df <- interpolation_data()
      
      plt <- plt %>%
        add_trace(
          data = interp_df,
          x = ~x,
          y = ~y,
          z = ~z_pred,
          type = "contour",    # "contour", "heatmap", etc.
          colors = "YlOrRd",
          showscale = FALSE
        )
      
      # Add black line around the measuring points to be visible
      plt <- plt %>%
        add_trace(
          data = dat,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "markers",
          name = "",
          showlegend = FALSE,
          colorscale = "YlOrRd",
          colors = "YlOrRd",
          marker = list(
            size = 10,
            line = list(width = 2, color = "black")
          )
        )
      
    }
    
    # Measurements without interpolation
    plt <- plt %>%
      add_trace(
        data = dat,
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "markers",
        name = "",
        showlegend = FALSE,
        colorscale = "YlOrRd",
        colors = "YlOrRd",
        marker = list(
          size = 10,
          colorbar = list(title = paste(input$tempChoice, unit_name))
        ),
        color = ~VarSelected,  # if you want the points also in color
        text = ~paste(
          "ID:", ID,
          "<br>x:", x,
          "<br>y:", y,
          paste0("<br>", input$tempChoice, ": ", VarSelected)
        ),
        hoverinfo = "text"
      ) 
    
    # Final layout
    plt %>% layout(
      title = "Measurement points with interpolation",
      xaxis = list(title = "X coordinate"),
      yaxis = list(title = "Y coordinate")
    )
    
  })
}
# ---------------------------------------------

# ---------------------------------------------
# Run the shiny application
# ---------------------------------------------
shinyApp(ui, server)
# ---------------------------------------------
