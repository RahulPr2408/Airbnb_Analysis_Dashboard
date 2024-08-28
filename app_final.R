library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)
library(plotly)
library(shinyBS) # Adding shinyBS for modal functionality
library(jsonlite)
library(leaflet)
library(viridis)
library(shinycssloaders)


# Load the cleaned Airbnb data
data <- read.csv("Airbnb_data.csv")

# Ensure last_review is correctly formatted as a date and extract year
data <- data %>%
  mutate(last_review = as.Date(last_review, format = "%Y-%m-%d"),
         review_year = year(last_review),
         review_month = format(last_review, "%Y-%m"))
# Parse amenities column and count the number of amenities
data <- data %>%
  mutate(amenities = gsub("\\[|\\]|\"", "", amenities)) %>% # Remove brackets and quotes
  mutate(amenities = strsplit(amenities, ", ")) %>% # Split the amenities into a list
  mutate(amenities_count = sapply(amenities, length)) # Count the number of amenities

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        background-color: #F8F9FB;
        background-image: url('https://images.pexels.com/photos/14024788/pexels-photo-14024788.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=1'); /* Replace with the URL of your background image */
        background-size: cover;
        background-attachment: fixed;
      }
      .main-header {
        color: white;
      }
      .main-header .logo {
        font-weight: bold;
        font-size: 24px;
        color: white;
      }
      .navbar {
        background-color: rgba(0, 0, 0, 0.45);
        color: white;
      }
      .navbar-default .navbar-nav > li > a {
        color: white;
      }
      .navbar-default .navbar-nav > li > a:hover {
        background-color: rgba(255, 255, 255, 0.8);
        color: black;
      }
      .container {
        background: rgba(255, 255, 255, 0.8); /* White with some transparency */
        border-radius: 8px;
        padding: 20px;
        margin-top: 20px;
      }
      .box {
        border-radius: 8px;
        border: 1px solid #E3E6EC;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        background-color: rgba(255, 255, 255, 0.8);
        margin-bottom: 20px;
      }
      .box-header {
        background-color: rgba(255, 255, 255, 0.8);
        border-bottom: 1px solid #E3E6EC;
        margin-left: 10px;
        margin-right: 10px;
      }
      .box-body {
        background-color: rgba(255, 255, 255, 0.8);
        margin: 10px;
      }
      .small-box {
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .selectize-input, .selectize-dropdown {
        border-radius: 8px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        border-radius: 8px;
      }
      .landing-page {
        text-align: center;
        margin-top: 100px;
      }
      .landing-page h1 {
        font-size: 50px;
        color: white;
      }
      .landing-page p {
        font-size: 20px;
        color: white;
      }
      .landing-page .btn {
        font-size: 18px;
        padding: 10px 20px;
        margin-top: 20px;
      }
      .text-container {
        background: rgba(0, 0, 0, 0.5); /* Semi-transparent background */
        border-radius: 10px;
        padding: 20px;
        display: inline-block;
      }
      .btn-primary {
        font-size: 16px;
        padding: 8px 16px;
      }
      .table-container {
        height: 350px;
        overflow-y: auto;
      }
      .pie-container {
        margin-left: 20px;
      }
      .box-body-pie {
      padding-top: 80px
      }
    "))
  ),
  
  navbarPage(
    title = div(class = "main-header", "Airbnb Dashboard"),
    id = "navbar",
    tabPanel("Home", value = "home",
             div(class = "landing-page",
                 div(class = "text-container",
                     h1("Welcome to the Airbnb Analysis Dashboard"),
                     p("This dashboard provides insights into Airbnb listings across various cities. Use the filters to explore the data."),
                     p("By utilizing this dashboard, Airbnb owners can make data-driven decisions that enhance their property listings, attract more guests, and ultimately increase their revenue. Whether you are a seasoned host or new to the Airbnb platform, our dashboard offers the insights needed to succeed in the competitive short-term rental market."),
                     p("We invite you to explore the various visualizations and analyses presented in this dashboard, and discover how you can leverage these insights to optimize your Airbnb business strategy."),
                     actionButton("goDashboard", "Go to Dashboard", class = "btn btn-primary")
                 )
             )
    ),
    tabPanel("Dashboard", value = "dashboard",
             fluidRow(
               column(12, class = "container",
                      div(class = "box",
                          div(class = "box-header",
                              h3("Filters")
                          ),
                          div(class = "box-body",
                              fluidRow(
                                column(6, selectInput("cityInput", "Select City:", choices = c("All", "Austin", "Istanbul", "Melbourne", "Toronto", "Paris"))),
                                column(6, sliderInput("yearInput", "Select Year:", min = 2010, max = 2024, value = c(2010, 2024), step = 1, sep = "")),
                                column(6, sliderInput("priceInput", "Price Range:", min = min(data$price, na.rm = TRUE), max = max(data$price, na.rm = TRUE), value = c(min(data$price, na.rm = TRUE), max(data$price, na.rm = TRUE))))
                                
                              )
                          )
                      )
               )
             ),
             fluidRow(
               column(12, class = "container",
                      div(class = "box",
                          div(class = "box-header",
                              h3("Geographical Map of Listings with Prices")
                          ),
                          div(class = "box-body",
                              withSpinner( 
                                leafletOutput("geoMap", height = 400),
                                type = 4
                                )
                          )
                      )
               )
             ),
             fluidRow(
               column(12, class = "container",
                      div(class = "box",
                          div(class = "box-header",
                              h3("Average Price of Listings in Each City by Room Type")
                          ),
                          div(class = "box-body",
                              withSpinner( 
                                plotlyOutput("pricePlot", height = 350),
                                type = 4
                              )
                          )
                      )
               )
             ),
             fluidRow(
               column(12, class = "container",
                      div(class = "box",
                          div(class = "box-header",
                              h3("Booking Trends Over Time")
                          ),
                          div(class = "box-body",
                              withSpinner( 
                              plotlyOutput("bookingTrendsPlot", height = 350),
                              type = 4
                          )
                          )
                      )
               )
             ),
             fluidRow(
               column(12, class = "container",
                      fluidRow(
                        column(6,
                               div(class = "box",
                                   div(class = "box-header",
                                       h3("Listings with Highest Occupancy Rates")
                                   ),
                                   div(class = "box-body",
                                       div(class = "table-container", 
                                           DTOutput("occupancyTable")),
                                       downloadButton("downloadReport", "Download Report")
                                   )
                               )
                        ),
                        column(6,
                               div(class = "box",
                                   div(class = "box-header",
                                       h3("Proportion of Instant Bookable Listings"),
                                       actionButton("detailedAnalysis", "Detailed Analysis", class = "btn btn-primary", style = "float: right; margin-top: 10px;")
                                   ),
                                   div(class = "box-body box-body-pie",
                                       withSpinner( 
                                       plotlyOutput("instantBookablePlot", height = 300),
                                       type = 4
                                       )
                                   )
                               )
                        )
                      )
               )
             ),
             
             fluidRow(
               column(12, class = "container",
                      div(class = "box",
                          div(class = "box-header",
                              h3("Trends in Price vs. Count of Amenities")
                          ),
                          div(class = "box-body",
                              withSpinner( 
                              plotlyOutput("priceAmenitiesPlot", height = 350),
                              type = 4
                              )
                          )
                      )
               )
             ),
             fluidRow(
               column(12, class = "container",
                      div(class = "box",
                          div(class = "box-header",
                              h3("Average Review Scores by Host Response Rate")
                          ),
                          div(class = "box-body",
                              withSpinner( 
                              plotlyOutput("responseRatePlot", height = 350),
                              type = 4
                              )
                              
                          )
                      )
               )
             ),
             fluidRow(
               column(12, class = "container",
                      div(class = "box",
                          div(class = "box-header",
                              h3("Review Scores by Room Type")
                          ),
                          div(class = "box-body",
                              withSpinner( 
                              plotlyOutput("reviewScoresBoxPlot", height = 350),
                              type = 4
                          )
                          )
                      )
               )
             ),
             fluidRow(
               column(12, class = "container",
                      div(class = "box",
                          div(class = "box-header",
                              h3("Top 10 Hosts and Room Types by Year")
                          ),
                          div(class = "box-body",
                              withSpinner(
                              plotlyOutput("top10Hosts", height = 350),
                              type = 4),
                          htmlOutput("description")
                          )
                      )
               )
             ),
             fluidRow(
               column(12, class = "container",
                      div(class = "box",
                          div(class = "box-header",
                              h3("Average Minimum Number of Nights by Room Type")
                          ),
                          div(class = "box-body",
                              withSpinner( 
                              plotlyOutput("avgMinNightsPlot", height = 350),
                              type = 4
                              )
                          )
                      )
               )
             )
             
    )
  ),
  
  # Modal for detailed analysis
  bsModal("modal1", "Detailed Analysis of Instant Bookable Listings", "detailedAnalysis", size = "large",
          DTOutput("detailedTable"))
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$goDashboard, {
    updateTabsetPanel(session, "navbar", selected = "dashboard")
  })
  
  observe({
    if (input$navbar == "home") {
      updateTabsetPanel(session, "navbar", selected = "home")
    }
  })

  # Filter data based on selected city, year, and price range
  filtered_data <- reactive({
    filtered <- data
    if (!"All" %in% input$cityInput) {
      filtered <- filtered %>% filter(city %in% input$cityInput)
    }
    filtered <- filtered %>% filter(review_year >= input$yearInput[1], review_year <= input$yearInput[2])
    filtered <- filtered %>% filter(price >= input$priceInput[1], price <= input$priceInput[2])
    return(filtered)
  })
  
  # Calculate average price per city and room type
  avg_price <- reactive({
    filtered_data() %>%
      group_by(city, room_type) %>%
      summarise(avg_price = mean(price, na.rm = TRUE))
  })
  
  output$pricePlot <- renderPlotly({
    p <- ggplot(avg_price(), aes(x = city, y = avg_price, fill = room_type, 
                                 text = paste("Room Type:", room_type, "<br>Average Price: $", 
                                              round(avg_price, 2), "<br>City:", city, "<br>Year:", 
                                              input$yearInput[1], "-", input$yearInput[2]))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial", color = "#1F2E3C"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        x = "City",
        y = "Average Price ($)",
        fill = "Room Type"
      ) +
      scale_fill_manual(values = room_type_colors)
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(color = "black"))
      )
  })
  
  # Calculate booking trends over time
  booking_trends <- reactive({
    data %>%
      filter(if (input$cityInput == "All") TRUE else city == input$cityInput) %>%
      filter(review_year >= input$yearInput[1], review_year <= input$yearInput[2]) %>%
      group_by(review_year, city) %>%
      summarise(listings = n()) %>%
      arrange(review_year) %>%
      ungroup()
  })
  
  output$bookingTrendsPlot <- renderPlotly({
    bt <- ggplot(booking_trends(), aes(x = review_year, y = listings, color = city, group = city, text = paste("Year:", review_year, "<br>Listings:", listings, "<br>City:", city))) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial", color = "#1F2E3C"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        x = "Year",
        y = "Number of Listings"
      ) +
      scale_x_continuous(breaks = seq(input$yearInput[1], input$yearInput[2], by = 1))
    
    ggplotly(bt, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(color = "black"))
      )
  })
  
  # Calculate occupancy rates for different periods
  occupancy_rates <- reactive({
    filtered_data() %>%
      group_by(city, room_type) %>%
      summarise(
        occupancy_30 = round(mean(availability_30, na.rm = TRUE), 2),
        occupancy_60 = round(mean(availability_60, na.rm = TRUE), 2),
        occupancy_90 = round(mean(availability_90, na.rm = TRUE), 2),
        occupancy_365 = round(mean(availability_365, na.rm = TRUE), 2)
      )
  })
  
  output$occupancyTable <- renderDT({
    datatable(occupancy_rates(), options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() { paste("occupancy_report", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(occupancy_rates(), file)
    }
  )
  
  # Calculate instant bookable listings
  instant_bookable <- reactive({
    filtered_data() %>%
      mutate(instant_bookable = ifelse(instant_bookable == "t", "Instant Bookable", "Non Instant Bookable")) %>%
      group_by(instant_bookable) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
  })
  
  output$instantBookablePlot <- renderPlotly({
    ib <- plot_ly(instant_bookable(), labels = ~instant_bookable, values = ~count, type = 'pie',
                  textinfo = 'label+percent', insidetextorientation = 'radial',
                  hoverinfo = 'text', text = ~paste("Instant Bookable:", instant_bookable, "<br>Count:", count),
                  marker = list(colors = c("#006CFF", "#FF6C00")))
    
    ib %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        showlegend = TRUE
      )
  })
  
  # Detailed analysis for instant bookable listings
  output$detailedTable <- renderDT({
    filtered_data() %>%
      mutate(instant_bookable = ifelse(instant_bookable == "t", "Instant Bookable", "Non Instant Bookable")) %>%
      group_by(city, room_type, instant_bookable) %>%
      summarise(count = n()) %>%
      arrange(city, room_type, desc(count))
  })
  # Calculate price vs. amenities
  price_amenities <- reactive({
    filtered_data() %>%
      group_by(city, room_type, amenities_count) %>%
      summarise(avg_price = mean(price, na.rm = TRUE)) %>%
      arrange(city, room_type, amenities_count)
  })
  
  output$priceAmenitiesPlot <- renderPlotly({
    pa <- ggplot(price_amenities(), aes(x = amenities_count, y = avg_price, color = city, text = paste("Amenities Count:", amenities_count, "<br>Average Price: $", round(avg_price, 2), "<br>City:", city, "<br>Room Type:", room_type))) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial", color = "#1F2E3C"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        x = "Count of Amenities",
        y = "Average Price ($)"
      )
    
    ggplotly(pa, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(color = "black"))
      )
  })
  
  # Calculate average review scores by host response rate
  review_scores_response_rate <- reactive({
    filtered_data() %>%
      mutate(host_response_rate = as.numeric(gsub("%", "", host_response_rate))) %>% # Convert to numeric
      filter(!is.na(host_response_rate)) %>% # Filter out rows with NA host_response_rate
      group_by(city, host_response_rate) %>%
      summarise(avg_review_score = mean(review_scores_rating, na.rm = TRUE), .groups = 'drop') %>%
      arrange(city, host_response_rate)
  })
  
  output$responseRatePlot <- renderPlotly({
    rr <- ggplot(review_scores_response_rate(), aes(x = host_response_rate, y = avg_review_score, color = city, text = paste("Host Response Rate:", host_response_rate, "<br>Average Review Score:", round(avg_review_score, 2), "<br>City:", city))) +
      geom_point() +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial", color = "#1F2E3C"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        x = "Host Response Rate (%)",
        y = "Average Review Score"
      ) +
      scale_x_continuous(
        breaks = seq(0, 100, by = 10)  # Adjust this sequence as needed
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(rr, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        xaxis = list(
          tickvals = seq(0, 100, by = 10),  # Match this sequence to the scale_x_continuous
          ticktext = seq(0, 100, by = 10),  # Match this sequence to the scale_x_continuous
          title = "Host Response Rate (%)"
        )
      )
  })

  # Geographical Map of Listings with Prices
  output$geoMap <- renderLeaflet({
    # Get the filtered data within the reactive context
    data <- filtered_data()
    
    # Define color palette function with the updated domain
    pal <- colorNumeric(palette = "plasma", domain = data$price)
    
    leaflet(data = data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~pal(price),
        radius = ~log(price + 1) * 2,  # Adjusting marker size based on price
        fillOpacity = 0.8,
        popup = ~paste("Price: $", price, "<br>City:", city, "<br>Room Type:", room_type)
      ) %>%
      addLegend(
        pal = pal, values = ~price, title = "Price",
        position = "bottomright"
      )
  })
  
  
  # Box Plot of Review Scores by Room Type
  
  output$reviewScoresBoxPlot <- renderPlotly({
    box_plot <- ggplot(filtered_data(), aes(x = room_type, y = review_scores_rating, fill = city, text = paste("City:", city, "<br>Room Type:", room_type, "<br>Review Score:", review_scores_rating))) +
      geom_boxplot() +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial", color = "#1F2E3C"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        x = "Room Type",
        y = "Review Scores"
      )
    
    ggplotly(box_plot, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(color = "black"))
      )
  })
  
  # Define colors for room types
  room_type_colors <- c("Entire home/apt" = "#1f77b4", 
                        "Private room" = "#ff7f0e", 
                        "Shared room" = "#2ca02c", 
                        "Hotel room" = "#d62728")
  output$top10Hosts <- renderPlotly({
    req(filtered_data())
    
    top_hosts <- filtered_data() %>%
      group_by(host_name, room_type) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      group_by(host_name) %>%
      summarise(total_count = sum(count), .groups = 'drop') %>%
      top_n(10, total_count) %>%
      inner_join(filtered_data(), by = "host_name") %>%
      group_by(host_name, room_type) %>%
      summarise(count = n(), .groups = 'drop')
    
    p <- ggplot(top_hosts, aes(x = reorder(host_name, -count), y = count, fill = room_type)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = room_type_colors) +
      coord_flip() +
      labs(title = "Top 10 Hosts and Room Types by Year",
           x = "Host Name", y = "Count",
           fill = "Room Type") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$description <- renderText({
    req(filtered_data())
    
    data_desc <- filtered_data() %>%
      group_by(room_type) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count))
    
    desc <- "<p>In the selected year and city, the distribution of room types among the top hosts are as follows:</p><ul>"
    
    for (i in 1:nrow(data_desc)) {
      room_type <- data_desc$room_type[i]
      count <- data_desc$count[i]
      color <- room_type_colors[room_type]
      desc <- paste0(desc, "<li><span style='color:", color, "'>", room_type, ": ", count, "</span></li>")
    }
    
    desc <- paste0(desc, "</ul>")
    
    HTML(desc)
  })
  
  
  # Calculate average min night by room type
  avg_min_nights_filter <- reactive({
    filtered_data() %>%
      group_by(city, room_type) %>%
      summarise(avg_min_nights = mean(minimum_nights, na.rm = TRUE))
  })
  
  output$avgMinNightsPlot <- renderPlotly({
    
    avg_min_nights <- avg_min_nights_filter()
    
    minNight <- ggplot(avg_min_nights, aes(x = reorder(city, -avg_min_nights), 
                                           y = avg_min_nights, fill = room_type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
      scale_fill_manual(values = room_type_colors) +
     coord_flip() +
      theme_minimal()+
      theme(
        text = element_text(family = "Arial", color = "#1F2E3C"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        x = "City", 
        y = "Average Minimum Nights",
        fill = "Room Type"
      ) 
    
    ggplotly(minNight, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(color = "black"))
      )
  }) 

}
# Run the application 
shinyApp(ui = ui, server = server)
