library(shiny)
library(shinydashboard)
library(stats)
library(maps)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(dplyr)

airbnb<-read.csv("/home/onat/Documents/doritos_cubun/data/Aemf1.csv",header=T)

# ui ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "purple",
  
  # title ---------
  dashboardHeader(title = "Airbnb Guide"),
  
  # sidebar -------
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("Welcome!", icon = icon("heart"), tabName = "page1"),
                menuItem("Map", icon = icon("map"), tabName = "page2"),
                menuItem("Number of Houses", icon = icon("home"), tabName = "page3"),
                conditionalPanel('input.sidebarid=="page3"',
                                 selectInput(inputId = "fill_color", label = "Fill Color", choices = c("blue", "red", "green"))),
                menuItem("Satisfaction", icon = icon("smile"), tabName = "page4"),
                conditionalPanel(
                  'input.sidebarid == "page4"',
                  selectInput("city", "City:", choices = c("Amsterdam", "Athens", "Barcelona", "Berlin", "Budapest", "Lisbon", "Paris", "Rome", "Vienna")),
                  selectInput("capacity", "Person Capacity:", choices = c(2, 3, 4, 5, 6)),
                  selectInput("cleanliness", "Minimum Cleanliness Rating:", choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                ),
                menuItem("Cleanliness", icon = icon("broom"), tabName = "page5"),
                conditionalPanel(
                  'input.sidebarid == "page5"',
                  selectInput("city0", "City:", choices = c("Amsterdam", "Athens", "Barcelona", "Berlin", "Budapest", "Lisbon", "Paris", "Rome", "Vienna")),
                  selectInput("capacity0", "Person Capacity:", choices = c(2, 3, 4, 5, 6)),
                  numericInput(
                    inputId = "price0",
                    label = "Maximum Price: ",
                    value = 200,
                    min = 0
                  )
                ),
                menuItem("Price Distribution", icon = icon("wallet"), tabName = "page6"),
                conditionalPanel('input.sidebarid == "page6"',
                                 sliderInput("price2", "Set Your Maximum Price: ", 1, 2000, 50)),
                menuItem("Price Prediction", icon=icon("dollar"), tabName = "page7"),
                conditionalPanel('input.sidebarid=="page7"',
                                 selectInput("city1", "Choose a City:", choices = unique(airbnb$City)),
                                 selectInput("x_variable", "Select X-axis Variable:", choices = c("City Center Distance (km)", "Metro Distance (km)")))
                               
    )
  ),
  
  #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  
  # body ----
  dashboardBody(
    tabItems(
      # page 1 ----
      tabItem(tabName = "page1", box(
        title = shiny::h4("Doritos Cubun"),background = "light-blue", solidHeader = T ,collapsible = TRUE,
        shiny::h5("Duhan Onat Karadayı, 2502177"), 
      ),box(
        title = "A Guide for Airbnb in Europe",background = "maroon", solidHeader = T ,collapsible = TRUE,
        shiny::h5("Explore the enchanting cities of Europe through the Airbnb dataset, featuring nine captivating destinations including Amsterdam, 
        Athens, Barcelona, Berlin, Budapest, Lisbon, Paris, Rome, and Vienna. Gain valuable insights into accommodation options, such as price, cleanliness, and 
        guest satisfaction, allowing informed decisions for personalized travel experiences. Uncover patterns and correlations,
        discovering the allure of each city and immersing yourself in the vibrant cultures and unique offerings of these European gems.")
      ),box(
        title = shiny::h4("Our Variables"),background = "navy", solidHeader = T ,collapsible = TRUE,
        shiny::h5("City: Categorical data representing the city where the data is collected."),
        shiny::h5("Price: Numeric data representing the price associated with a particular observation",),
        shiny::h5("Day: Categorical data representing the day associated with a particular observation."),
        shiny::h5("Room Type: Categorical data representing the type of room (e.g., shared room, private room) associated with a particular observation."),
        shiny::h5("Shared Room: Categorical data indicating whether the room is shared or not."),
        shiny::h5("Private Room: Categorical data indicating whether the room is private or not."),
        shiny::h5("Person Capacity: Numeric data representing the capacity or number of people a room can accommodate."),
        shiny::h5("Superhost: Categorical data indicating whether the host is a superhost or not."),
        shiny::h5("Multiple Rooms: Categorical data indicating whether there are multiple rooms available."),
        shiny::h5("Business: Categorical data indicating whether the accommodation is suitable for business purposes."),
        shiny::h5("Cleanliness Rating: Numeric data representing the rating for cleanliness."),
        shiny::h5("Guest Satisfaction: Numeric data representing the satisfaction rating given by guests."),
        shiny::h5("Bedrooms: Numeric data representing the number of bedrooms in the accommodation.."),
        shiny::h5("City Center (km): Numeric data representing the distance of the accommodation from the city center in kilometers."),
        shiny::h5("Attraction Index: Numeric data representing an index or score for attractions in the city."),
        shiny::h5("Restaurant Index: Numeric data representing an index or score for restaurants in the city."),
      )),
      #page 2----
      tabItem(tabName = "page2",
              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
              leafletOutput("map")),
      # page 4 ----
      tabItem(tabName = "page4", 
              plotOutput("bar_plot")),
      # page 5 ----
      tabItem(tabName = "page5",
              plotOutput("scatter_plot")
      ),
      # page 6 ----
      tabItem(tabName = "page6",
              plotOutput("box_plot")
      ),
      # page 3 ----
      tabItem(tabName="page3",
              plotOutput("bar_plot1")),
      # page 5 ----
      tabItem(tabName = "page7",
              shiny::h3("Price Prediction with Multiple Linear Regression"),
              plotOutput("scatter_plot5"),
              numericInput("city_center_km", "City Center Distance (km):", value = 5, min = 0, max = 100),
              numericInput("metro_distance_km", "Metro Distance (km):", value = 2, min = 0, max = 100),
              actionButton("predict_btn", "Predict Price"),
              verbatimTextOutput("predicted_price"))
    )
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output, session) {
  #Map-------------------------------------------------------
  lon<-c(4.92737059634333,23.715776683292063,2.16440735384126,13.365996699163654,19.04105213651238,
         -9.140252718231098,2.3453543427609578,12.466583418719035,16.37310270798232)
  lat<- c(52.3641665675856,37.984127643247675,41.39004306665118,52.603453663342044,47.49824101765792,
          38.726644682802956,48.862470327500844,41.9093249291986,48.207709325683346)
  summary_stats <- reactive({
    airbnb_summary <- airbnb %>%
      group_by(City) %>%
      summarize(
        Mean_Cleanliness = mean(Cleanliness.Rating),
        Mean_Attraction = mean(Attraction.Index),
        Mean_Price = mean(Price),
        Mean_Satisfaction = mean(Guest.Satisfaction),
        Num_Houses = n()
      )
    
    airbnb_summary
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 14, lat = 46, zoom = 5) %>%
      addProviderTiles("CartoDB.Voyager")
  })
  
  observe({
    summary_data <- summary_stats() 
    if (input$sidebarid == "page2") {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data = summary_data,
          lng = ~lon,
          lat = ~lat,
          color = rainbow(9),
          radius = 10,
          popup = paste(
            "City:", summary_data$City, "<br>",
            "Average Cleanliness (Out of 10):", round(summary_data$Mean_Cleanliness,2), "<br>",
            "Mean Attraction:", round(summary_data$Mean_Attraction,2), "<br>",
            "Average Price(₺):", round(summary_data$Mean_Price,2), "<br>",
            "Average Satisfaction:", round(summary_data$Mean_Satisfaction,2), "<br>",
            "Number of Houses:", summary_data$Num_Houses
          ),
          labelOptions = labelOptions(noHide = TRUE, direction = "auto")
        )
    }
  })
  
  # Satisfaction--------------------------------------------------------------------
  filtered_data <- reactive({
    filter(airbnb, City == input$city, Person.Capacity == input$capacity, Cleanliness.Rating >= input$cleanliness)
  })
  
  
  output$bar_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Guest.Satisfaction, y = Price)) +
      geom_smooth(se=T) + geom_jitter() + 
      labs(
        title = "Price v Satisfaction",
        x = "Satisfaction",
        y = "Price (₺)"
      )
  })
  
  #Cleanliness-------------------------------------------------------------------------
  
  filtered_dat <- reactive({
    airbnb %>%
      filter(City == input$city0, 
             Person.Capacity == input$capacity0, 
             Price <= input$price0) 
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(data = filtered_dat(), aes(x = as.factor(Cleanliness.Rating), y = Price)) +
      geom_boxplot(fill = "skyblue", color = "black") + 
      labs(title = "Cleanliness v. Price",
           subtitle=paste0("Corr(X,Y) = ", cor(airbnb$Cleanliness.Rating,airbnb$Price), "\n",
                           "Number of Observations: ", nrow(filtered_dat())),
           x = "Cleanliness (X)",
           y = "Price (Y)") + theme_minimal() 
  })
  
  
  #Price Dist.------------------------------------------
  filtered_data2 <- reactive({
    airbnb %>% 
      filter(Price < input$price2)
  })
  output$box_plot <- renderPlot({
    ggplot(data=filtered_data2(), aes(City, Price))+
      geom_boxplot(fill="red",color="black") + ggtitle("Price Distrubiton for Each City") + theme_minimal()
  })
  #Number of houses---------------------------
  fill_color <- reactive({
    input$fill_color
  })
  
  
  output$bar_plot1 <- renderPlot({
    ggplot(airbnb, aes(x = City)) +
      geom_bar(fill = fill_color(), color = 'black') +
      theme_minimal() +
      xlab('City') +
      ylab('Number of Houses') +
      ggtitle('Number of Houses in Each City')+ geom_text(stat='count', aes(label=..count..), vjust=-1)
  })
  #Distance-------------------------------------
  filtered_data5 <- reactive({
    airbnb %>% filter(City == input$city1)
  })
  
  x_variable <- reactive({
    if (input$x_variable == "City Center Distance (km)") {
      "City.Center..km."
    } else {
      "Metro.Distance..km."
    }
  })
  
  output$scatter_plot5 <- renderPlot({
    x_var <- x_variable()
    corr <- cor(filtered_data5()[[x_var]], filtered_data5()$Price)
    ggplot(filtered_data5(), aes_string(x = x_variable(), y = "Price")) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "lm", se = T, color = "red") +
      labs(x = input$x_variable, y = "Price",
           title = paste0("Price vs. ", input$x_variable),
           subtitle = paste0("Number of Observations: ", nrow(filtered_data()), "\n",
                             "Corr(X,Y) =", round(corr, 2)))
    
  })
  
  observeEvent(input$predict_btn, {
    new_data <- data.frame(
      City.Center..km. = input$city_center_km,
      Metro.Distance..km. = input$metro_distance_km
    )
    
    prediction <- predict(lm(Price ~ City.Center..km. + Metro.Distance..km., data = filtered_data5()), new_data)
    
    output$predicted_price <- renderPrint({
      if (prediction < 0) {
        "Your inputs are not valid for this dataset, please try again."
      } else {
        paste0("₺", round(prediction, 2))
      }
    })
  })
  #t-test---------------------------------------------------
  
}

# shiny app --------------------------------------------------------------------

shinyApp(ui, server)

