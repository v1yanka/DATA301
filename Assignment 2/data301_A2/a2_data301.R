library(shiny)
library(ggplot2)
library(dplyr)

#filtering data to only include summer olympics
#for labeling purposes, i've changed the values of the sex feature from "F" and "M" to "Female" and "Male"
summer_data <- read.csv("athlete_events.csv") %>%
  filter(Season == "Summer") %>%
  mutate(Sex = ifelse(Sex == "F", "Female", "Male"))


ui <- fluidPage(
  titlePanel("Summer Olympics Interactive Visualisation Tool"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Customise the Plots"),
      helpText("Click to select one or more options. Use the backspace key to remove selections."),
      selectInput("sports", "Select Sport(s):", 
                  choices = c("All Sports", sort(unique(summer_data$Sport))), # used sort() to arrange in alphabetical order
                  selected = "All Sports", multiple = TRUE),
      helpText("Tip: Remember to deselect 'All Sports' if you want to choose specific options."), # instructions
      selectInput("nocs", "Select NOC(s):", 
                  choices = c("All NOCs", sort(unique(summer_data$NOC))), # used sort() to arrange in alphabetical order
                  selected = "All NOCs", multiple = TRUE),
      helpText("Tip: Remember to deselect 'All NOCs' if you want to choose specific options."),
      sliderInput("year_range", "Select Year Range:", 
                  min = min(summer_data$Year), max = max(summer_data$Year), 
                  value = c(min(summer_data$Year), max(summer_data$Year))),
      radioButtons("bar_mode", "Bar Chart Mode:", 
                   choices = c("Stacked" = "stack", "Side-by-Side" = "dodge")),
      radioButtons("compareType", "Comparison Type:",
                   choices = c("Combined Gender View" = "combined", "Split Gender View" = "gender"))
    ),
    
    mainPanel(
      h3("Barplot Showing Medals won by each NOC"),
      plotOutput("medal_plot"),
      h3("Counts of Athletes vs Events in Summer Olympics"),
      plotOutput("athlete_event_plot")
    )
  )
)

server <- function(input, output) {
  
  # filtering data on user input
  filtered_data <- reactive({
    filtered_summer_data <- summer_data
    
    # filtering by selected sports options
    if (!"All Sports" %in% input$sports) {
      filtered_summer_data <- filtered_summer_data %>%
        filter(Sport %in% input$sports)
    }
    
    # filtering by selected nocs options
    if (!"All NOCs" %in% input$nocs) {
      filtered_summer_data <- filtered_summer_data %>%
        filter(NOC %in% input$nocs)
    }
    
    # filtering by year range
    filtered_summer_data <- filtered_summer_data %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2])
    
    filtered_summer_data
  })
  
  # using reactive data to count medals by events rather than athletes(so medals for team events are only counted as one medal) 
  medalData <- reactive({
    filtered_data() %>%
      filter(!is.na(Medal)) %>%
      distinct(NOC, Event, Medal, Sex, .keep_all = TRUE) %>%
      group_by(NOC, Medal, Sex) %>%
      summarise(count = n_distinct(Event), .groups = "drop")
  })
  
  # 1a stacked bar chart 
  output$medal_plot <- renderPlot({
    if (input$compareType == "gender") {
      ggplot(medalData(), aes(x = NOC, y = count, fill = Medal)) +
        geom_bar(stat = "identity", position = input$bar_mode) +
        facet_wrap(~ Sex) +
        scale_fill_manual(values = c("Gold" = "gold", "Silver" = "grey", "Bronze" = "brown")) +
        labs(x = "NOC(s)", y = "Medal Count", fill = "Medal",
             title = "A Split Gender View of the Total Number of Medals Won by Female and Male Athletes within each NOC") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      ggplot(medalData(), aes(x = NOC, y = count, fill = Medal)) + 
        geom_bar(stat = "identity", position = input$bar_mode) +
        scale_fill_manual(values = c("Gold" = "gold", "Silver" = "grey", "Bronze" = "brown")) +
        labs(x = "NOC(s)", y = "Medal Count", fill = "Medal",
             title = "A Combined Gender View of the Total Number of Medals Won within each NOC") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })

  # 1b scatter plot of athletes vs events
  output$athlete_event_plot <- renderPlot({
    plot_data <- filtered_data() %>%
      distinct(Games, Year, Event, ID, Sex) 
    
    if (input$compareType == "gender") {
      plot_data %>%
        group_by(Games, Year, Sex) %>%
        summarise(Events = n(),
                  Athletes = n_distinct(ID),
                  .groups = "drop") %>%
        ggplot(aes(x = Athletes, y = Events, shape = Sex)) +
        geom_point(aes(color = factor(Year))) +
        theme_minimal() +
        labs(x = "Number of Athletes", y = "Number of Events", shape = "Gender", color = "Year") +
        facet_wrap(~ Sex)
    } else {
      plot_data %>%
        group_by(Games, Year) %>%
        summarise(Events = n(),
                  Athletes = n_distinct(ID),
                  .groups = "drop") %>%
        ggplot(aes(x = Athletes, y = Events)) +
        geom_point(aes(color = factor(Year))) +
        theme_minimal() +
        labs(x = "Number of Athletes", y = "Number of Events", color = "Year")
    }
  })
}
shinyApp(ui = ui, server = server)

