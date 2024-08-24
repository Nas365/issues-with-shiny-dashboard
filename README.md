UI. FILE
# Load libraries
library(shiny)
library(tidyverse)

# Application Layout
shinyUI(
  fluidPage(
    br(),
    # TASK 1: Application title
    titlePanel(title = "Trends in Demographic and Income"),
    p("Explore the difference between people who earn less than 50K and more than 50K. You can filter the data by country, then explore various demogrphic information."),
    
    # TASK 2: Add first fluidRow to select input for country
    fluidRow(
      column(12, 
             wellPanel(
               selectInput(inputId = "country", 
                           label = "Country:",
                           choices = c("United States", "Canada", "Mexico", "Germany", "Philippines"),
                           selected = "United States")  # add select input 
             )
      )
    ),
    # TASK 3: Add second fluidRow to control how to plot the continuous variables
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a continuous variable and graph type (histogram or boxplot) to view on the right."),
               radioButtons(
                 inputId = "continuous_variable",   # add radio buttons for continuous variables
                 label = "Select Continuous Variable:",
                 choices = c("Age" = "age", "Hours per Week" = "hours_per_week")    # add radio buttons for chart type
               )
             )
      ),  # Closing the first column correctly
      column(9,
             wellPanel(
               radioButtons(
                 inputId = "graph_type",
                 label = "Select Graph Type:",
                 choices = c("Histogram" = "histogram", "Boxplot" = "boxplot")
               )
             )
      )  
    ), 
  
    # Plot output for continuous variables
    fluidRow(
      column(12,
             plotOutput(outputId = "p1")
      )
    ),  
    # TASK 4: Add third fluidRow to control how to plot the categorical variables
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a categorical variable to view bar chart on the right. Use the check box to view a stacked bar chart to combine the income levels into one graph. "),
               radioButtons("categorical_variables",
                            "Categorical Variables:",
                            choices = c("education", "workclass", "sex"),
                            selected = "education"),    
               checkboxInput("is_stacked", "Stack Bars", value = FALSE)
               )
             ),
      column(9, plotOutput("p2"))  # add plot output
    )
  )
)



SERVER.R
# Load libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(stringr)

# Read in data
adult <- read_csv("adult.csv")

# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$country)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$p1 <- renderPlot({
    if (input$graph_type == "histogram") {
      # Histogram
      ggplot(df_country(), aes_string(x = input$continuous_variable)) +
        geom_histogram() +  # histogram geom
        labs(y = "Number of People",  # labels
             title = paste("Trend of", input$continuous_variable)) + 
        facet_wrap(~prediction) +
        theme_economist() +
        scale_color_economist()
    } else {
      # Boxplot
      ggplot(df_country(), aes_string(y = input$continuous_variable, x = "prediction")) +
        geom_boxplot() +  # Boxplot geom
        coord_flip() +  # Flip coordinates
        labs(title = paste("How", input$continuous_variable, "value is spread")) +  
        facet_wrap(~prediction) +
        theme_economist() +
        scale_color_economist()
    }
  })
  
  # TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$p2 <- renderPlot({
    # Bar chart
    p <- ggplot(df_country(), aes_string(x = input$categorical_variable)) +
      labs(y = "Number of People",
           title = paste("Trend of", input$continuous_variable)) +  # Labels
      theme(axis.text.x = element_text(angle = 45), legend.position = "bottom")
    
    if (input$is_stacked) {
      p + geom_bar(aes(fill = prediction), position = "stack") +
        theme_economist() +
        scale_color_economist()
    } else {
      p + 
        geom_bar(aes(fill = !!input$categorical_variable), position = "dodge") + 
        facet_wrap(~prediction) +
        
# TASK 7
theme_economist() +
        scale_color_economist()
    }
  })
})
