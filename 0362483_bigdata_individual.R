# install necessary packages ####
# install.packages(c("shiny", "ggplot2", "dplyr", "readr", "lubridate", "tidyr", "shinythemes", "GGally", "cluster", "rpart", "plotly"))

# import libraries ####
library(shiny)       # for creating interactive web applications
library(ggplot2)     # for creating graphics and visualizations
library(dplyr)       # for data manipulation
library(readr)       # for reading csv files
library(lubridate)   # for working with dates and times
library(tidyr)       # for tidying data
library(shinythemes) # for applying themes to shiny apps
library(GGally)      # for creating scatter plot matrices
library(cluster)     # for clustering analysis
library(rpart)       # for recursive partitioning and regression trees
library(plotly)      # for interactive visualizations

# load the data set ####
data <- read_csv("C:/Users/Tuladhar/Desktop/BD/individual/superstore_dataset.csv")

# convert date format ####
data$order_date <- as.Date(data$order_date, format = "%d-%m-%y")
data$ship_date <- as.Date(data$ship_date, format = "%d-%m-%y")

# calculate profit margin ####
if(!"profit_margin" %in% colnames(data)) {
  data <- data %>% mutate(profit_margin = profit / sales)
}

# define ui for the shiny app ####
ui <- fluidPage(
  theme = shinytheme("superhero"), # apply superhero theme
  titlePanel("Superstore Sales and Profits Analysis"), # title panel for the app
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category:", 
                  choices = c("All Categories", unique(data$category))), # drop-down for category selection
      selectInput("region", "Select Region:", 
                  choices = c("All Regions", unique(data$region))),     # drop-down for region selection
      dateRangeInput("dateRange", "Select Date Range:", 
                     start = min(data$order_date), end = max(data$order_date)), # date range input for selecting date range
      
      # additional text below ####
      tags$hr(),  # horizontal line for separation
      tags$p("Module Code: ITS66904", style = "color: #ffffff; text-align: left;"), # module code
      tags$p("Module Name: Big Data Technologies", style = "color: #ffffff; text-align: left;"), # module name
      tags$p("Task 1: Individual Assignment", style = "color: #ffffff; text-align: left;"), # task information
      tags$hr(),  # horizontal line for separation
      tags$p("Full Name: Sujal Ratna Tuladhar", style = "color: #ffffff; text-align: left;"), # full name
      tags$p("Section, Group: B, 5", style = "color: #ffffff; text-align: left;"), # section and group
      tags$p("Student ID: 22013 230", style = "color: #ffffff; text-align: left;"), # student id
      tags$p("University ID: 036 2483", style = "color: #ffffff; text-align: left;") # university id
    ),
    
    mainPanel(
      
      plotOutput("profitVsSales"),     # plot output for profit vs sales
      plotOutput("quantityByState"),   # plot output for quantity sold by state
      plotOutput("salesByRegion"),     # plot output for total sales by region
      plotOutput("timeSeriesPlot"),    # plot output for time series plot
      plotOutput("scatterMatrix"),     # plot output for scatter plot matrix
      plotOutput("decisionTreePlot"),  # plot output for decision tree classification
      plotOutput("profitByCategory"),  # plot output for profit margin by category
      plotOutput("discountDistribution"), # plot output for discount distribution
      plotOutput("kmeansPlot"),        # plot output for k-means clustering
      plotlyOutput("profitMarginSunburst"), # changed to plotlyOutput for sunburst chart
      plotOutput("profitPlot"),
    )
  )
)

# define server logic ####
server <- function(input, output) {
  
  # reactive expression to filter data ####
  filtered_data <- reactive({
    data %>%
      filter(
        (input$category == "All Categories" | category == input$category), # filter by category
        (input$region == "All Regions" | region == input$region),         # filter by region
        order_date >= input$dateRange[1],                                 # filter by start date
        order_date <= input$dateRange[2]                                  # filter by end date
      )
  })
  
  # total sales by region ####
  output$salesByRegion <- renderPlot({
    filtered_data() %>%
      group_by(region) %>%
      summarise(sales = sum(sales, na.rm = TRUE)) %>%
      ggplot(aes(x = region, y = sales, fill = region)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Sales by Region", x = "Region", y = "Sales")
  })
  
  # profit margin by category ####
  output$profitByCategory <- renderPlot({
    filtered_data() %>%
      group_by(category) %>%
      summarise(profit_margin = mean(profit_margin, na.rm = TRUE)) %>%
      ggplot(aes(x = category, y = profit_margin, fill = category)) +
      geom_bar(stat = "identity") +
      labs(title = "Profit Margin by Category", x = "Category", y = "Profit Margin")
  })
  
  # discount distribution ####
  output$discountDistribution <- renderPlot({
    ggplot(filtered_data(), aes(x = discount)) +
      geom_histogram(bins = 20, fill = "skyblue", color = "black") +
      geom_density(alpha = .2, fill = "#FF6666") +
      labs(title = "Distribution of Discounts", x = "Discount", y = "Frequency")
  })
  
  # profit vs sales ####
  output$profitVsSales <- renderPlot({
    ggplot(filtered_data(), aes(x = sales, y = profit, color = category)) +
      geom_point() +
      labs(title = "Profit vs. Sales by Category", x = "Sales", y = "Profit")
  })
  
  # total quantity sold by state ####
  output$quantityByState <- renderPlot({
    filtered_data() %>%
      group_by(state) %>%
      summarise(quantity = sum(quantity, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(state, -quantity), y = quantity, fill = state)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Quantity Sold by State", x = "State", y = "Quantity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate state names for better readability
  })
  
  # render sunburst chart for profit margin distribution by category ####
  output$profitMarginSunburst <- renderPlotly({
    
    plot_ly(
      data = data,
      labels = ~subcategory,
      parents = ~category,
      values = ~profit_margin,
      type = 'sunburst',
      branchvalues = 'total'
    ) %>%
      layout(title = "Profit Margin Distribution by Category and Subcategory")
  })
  
  output$profitMarginPie <- renderPlot({
    filtered_data() %>%
      group_by(category) %>%
      summarise(total_profit = sum(profit_margin, na.rm = TRUE)) %>%
      ggplot(aes(x = "", y = total_profit, fill = category)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = "Profit Margin Distribution by Category", fill = "Category", x = NULL, y = NULL) +
      theme_void()
  })
  
  # monthly sales over time ####
  output$timeSeriesPlot <- renderPlot({
    filtered_data() %>%
      mutate(month = floor_date(order_date, "month")) %>%
      group_by(month) %>%
      summarise(sales = sum(sales, na.rm = TRUE)) %>%
      ggplot(aes(x = month, y = sales)) +
      geom_line() +
      labs(title = "Monthly Sales Over Time", x = "Date", y = "Sales")
  })
  
  # sales, profit, discount, and quantity ####
  output$scatterMatrix <- renderPlot({
    filtered_data() %>%
      select(sales, profit, discount, quantity) %>%
      ggpairs() +
      labs(title = "Scatterplot Matrix of Sales, Profit, Discount, and Quantity")
  })
  
  # sales profit ####
  output$kmeansPlot <- renderPlot({
    cluster_data <- filtered_data() %>%
      select(sales, profit) %>%
      na.omit() # remove rows with missing values
    kmeans_result <- kmeans(cluster_data, centers = 3) # apply k-means clustering with 3 clusters
    cluster_data <- cluster_data %>%
      mutate(cluster = as.factor(kmeans_result$cluster)) # add cluster assignment to data
    ggplot(cluster_data, aes(x = sales, y = profit, color = cluster)) +
      geom_point() +
      labs(title = "K-means Clustering of Sales and Profit", x = "Sales", y = "Profit")
  })
  
  # category classification ####
  output$decisionTreePlot <- renderPlot({
    tree_data <- filtered_data() %>%
      select(sales, profit, category) %>%
      na.omit() # remove rows with missing values
    tree_model <- rpart(category ~ sales + profit, data = tree_data, method = "class") # build decision tree model
    rpart.plot::rpart.plot(tree_model, main = "Decision Tree for Category Classification")
  })
  
}

# run shiny app ####
shinyApp(ui = ui, server = server)
