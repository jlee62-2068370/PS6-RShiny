library(shiny)
library(tidyverse)
library(colourpicker)

## Loading Data
social_media <- read_delim("WhatsgoodlyData-6.csv")

university_names <-  social_media %>%
  mutate(name = `Segment Description`) %>%
  filter(`Segment Type` == "University") %>%
  distinct(name) %>%
  arrange(name)
  
ui <- fluidPage(
    titlePanel("Social Media Influences on Shopping"),
    tabsetPanel(type = "tabs",
                
        tabPanel("About This Data", 
            p("The dataset chosen for this problem set is entitled ", 
              em("Social Influence on Shopping"), "by Adam Halper on kaggle. The
              target audience is mainly college students (including both Gen-Z 
              and Millennials. The data set as a whole is primarily composed of 
              people within the given age range (with different backgrounds such
              as university, sexes, employment status, income bracket, and etc.)
              However, the data that is used in this project, specifically
              deals with", strong("University level students"), "at multiple well 
              known schools across out nation. Here, we analyze if social media
              platforms like Facebook, Instagram, Twitter, Snapchat, and 'None'
              (if there is not any social media influence on shopping behavior)
              have an effect on a University student's shopping habits"),
            
            tags$div(
              "In this data the following sections are analyzed: ",
              tags$ul(
                tags$li(strong("Segment Type"), "(University Level)"),
                tags$li(strong("Segment Description"), "(University Name)"),
                tags$li(strong("Count"), "(Number of Students agreeing to a 
                        specific social media platform influencing their sopping
                        habits"),
                tags$li(strong("Answer"), "(Social Media Platform)")
              ),
              
              p("Below is a small random sample of the dataset that will be used: "),
              
              dataTableOutput("sample")
            )
        ),
        
        tabPanel("Table", 
            sidebarLayout(
                mainPanel(
                  dataTableOutput("table")
                ),
                sidebarPanel(
                  selectInput("UniversityName", "Please select a University", 
                      c("None", "Show All", university_names$name)),
                  textOutput("tableText")
                ),
            )
        ),
        
        tabPanel("Plot",
            sidebarLayout(
                mainPanel(
                  plotOutput("plot")
                ),
                sidebarPanel(
                  fluidRow(
                           selectInput("UniversityName1", "University Choice 1", 
                                       c(university_names$name)),
                           selectInput("UniversityName2", "University Choice 2", 
                                       c("None", university_names$name))
                  ),
                  fluidRow(
                    colourInput("color1", "Color of University Choice 1", 
                                value = "#7FFFD4", palette = "limited"),
                    colourInput("color2", "Color of University Choice 2", 
                                value = "#E066FF", palette = "limited"),
                    textOutput("plotText")
                  )
                )
            )
        )
    )
)


server <- function(input, output) {
  output$sample <- renderDataTable({
    social_media %>%
      filter(`Segment Type` == "University") %>%
      select(`Segment Type`, `Segment Description`, Answer, Count) %>%
      sample_n(10)
  })
  
  output$plot <- renderPlot({
    plot_data <- social_media %>%
      select(`Segment Description`, Answer, Count ) %>%
      filter(`Segment Description` %in% c(input$UniversityName1, 
          input$UniversityName2)) %>%
      group_by(`Segment Description`)
    
    ggplot(plot_data, aes(Answer, Count, fill=factor(`Segment Description`))) +
      geom_col(position = "dodge") +
      labs(title = "Number of Students' Shopping Habits Influenced by Social Media Platforms",
           x = "Social Media Platform",
           y = "Count",
           fill = "University Name(s)") +
      scale_fill_manual(values = c(input$color1, input$color2))
  })
  
  output$table <- renderDataTable({
    if (input$UniversityName != "Show All") {
      social_media %>%
        filter(`Segment Type` == "University",
               `Segment Description` == input$UniversityName) %>%
        select(`Segment Description`, Answer, Count) %>%
        reframe(name = `Segment Description`, Answer, Count)
    } else {
      social_media %>%
        filter(`Segment Type` == "University") %>%
        select(`Segment Description`, Answer, Count) %>%
        reframe(name = `Segment Description`, Answer, Count) %>%
        arrange(name)
    }
    })
  
  output$tableText <- renderText({
      if (input$UniversityName != "Show All") {
        number <- social_media %>%
          filter(`Segment Type` == "University", 
                 `Segment Description` == input$UniversityName) %>%
          summarise(n = n())
        paste("Showing", number$n, "results for", input$UniversityName)
      } else {
        number <- social_media %>%
          filter(`Segment Type` == "University") %>%
          summarise(n = n())
        paste("Showing", number$n, "results for", input$UniversityName)
      }
    })
  
  output$plotText <- renderText({
    paste("Comparing Data between ", input$UniversityName1, "and", input$UniversityName2)
  })
}

shinyApp(ui = ui, server = server)
