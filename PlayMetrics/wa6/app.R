library(shiny)
library(viridis)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- function() {
  genres <-
    c(
      "Action",
      "Adventure",
      "Fighting",
      "Misc",
      "Platform",
      "Puzzle",
      "Racing",
      "Role-Playing",
      "Shooter",
      "Simulation",
      "Sports",
      "Strategy"
    )
  
  fluidPage(
    titlePanel("PlayMetrics"),
    
    h3("Global Historical Sales of Video Games"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "year_range",
          "Year Range:",
          min = 1980,
          max = 2020,
          value = c(1980, 2020),
          step = 10
        ),
        
        selectInput(
          "bar_selection",
          "Top 5 money maker by:",
          choices = c("Genre", "Platform")
        ),
        
        selectInput(
          "color_scheme",
          "Select color scheme:",
          choices = c("blue", "yellow", "red", "grey")
        ),
        
        selectInput(
          "genre_selection",
          "Select Genres:",
          choices = genres,
          selected = genres,
          multiple = TRUE
        ),
        
        checkboxInput(
          "hexbin_regression",
          "Regression line in hexbin plot",
          value = FALSE)
      ),
      
      mainPanel(
        plotOutput("barplot"),
        plotOutput("hexbinplot"))
    ))
}

server <- function(input, output) {
  # create a reactive expression that subsets
  # the data based on the year_range input
  master <- reactive({
    # read the data
    df <-
      read.csv(file = "Video_Games_Sales_as_at_22_Dec_2016.csv",
               header = TRUE)
    
    df <- na.omit(df)
    
    # subset the data based on the year_range input
    df <-
      df[df$Year_of_Release >= input$year_range[1] &
           df$Year_of_Release <= input$year_range[2], ]
    
    # subset the data based on the genre_selection input
    df <- df[df$Genre %in% input$genre_selection, ]
    
    # normalize the user score by multiplying it
    # by a factor of 10 to align it with the same scale in critic score
    df$User_Score <- df$User_Score * 10
    
    return(df)
  })
  
  agg_Genre <- reactive({
    agg <- aggregate(Global_Sales ~ Genre, data = master(), sum)
    agg <- agg[order(-agg$Global_Sales),][1:5, ]
    agg
  })
  
  agg_Platform <- reactive({
    agg <- aggregate(Global_Sales ~ Platform, data = master(), sum)
    agg <- agg[order(-agg$Global_Sales),][1:5, ]
    agg
  })
  
  
  output$barplot <- renderPlot({
    if (input$bar_selection == "Genre") {
      bp <-
        ggplot(agg_Genre(),
               aes(x = Global_Sales,
                   y = reorder(Genre, Global_Sales))) +
        geom_bar(stat = "identity",
                 fill = paste(input$color_scheme, "2")) +
        labs(
          x = "Global Sales (Millions)",
          y = "Genre",
          title = paste(
            "Top 5 Genres in Global Sales between",
            input$year_range[1],
            "and",
            input$year_range[2]
          )
        ) +
        theme_minimal()
      
      bp
    }
    else if (input$bar_selection == "Platform") {
      bp <-
        ggplot(agg_Platform(),
               aes(x = Global_Sales,
                   y = reorder(Platform, Global_Sales))) +
        geom_bar(stat = "identity",
                 fill = paste(input$color_scheme, "2")) +
        labs(
          x = "Global Sales (Millions)",
          y = "Platform",
          title = paste(
            "Top 5 Platforms in Global Sales between",
            input$year_range[1],
            "and",
            input$year_range[2]
          )
        ) +
        theme_minimal()
      
      bp
    }
  })
  
  output$hexbinplot <- renderPlot({
    p <- ggplot(master(), aes(x = User_Score, y = Critic_Score)) +
      geom_hex(bins = 20) +
      labs(x = "User Score",
           y = "Critic Score",
           title = "Hexbin plot of Critic Score vs User Score") +
      theme_minimal()
    
    # react to color theme for th plot
    p <- p +
      switch(
        input$color_scheme,
        "blue" = scale_fill_viridis(option = "G"),
        "yellow" = scale_fill_viridis(option = "E"),
        "red" = scale_fill_viridis(option = "F"),
        "grey" = scale_fill_gradient2(low = "grey", high = "black"),
        scale_fill_gradient2(low = "grey", high = "black")
      )
    
    # react to the regression line option
    if (input$hexbin_regression) {
      p <- p +
        geom_smooth(method = "lm",
                    formula = 'y ~ x')
    }
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)