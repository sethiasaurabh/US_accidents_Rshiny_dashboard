#=============================================================================================

### Loading required packages:

library("shiny")
library("DT")
library("plotly")
library("plyr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("shinydashboard")
library("shinythemes")

### Read data:

data <- read.csv("D:\\Analytics\\Rshiny_data.csv", header = T)

head(data)

str(data)

factor(data$Year)
#=============================================================================================

# Dataset Information
print("***** Dimension *****")
dim(data)
print("***** Column Names *****")
names(data)
print("***** Summary *****")
summary(data)
print("***** Structure *****")
dplyr::glimpse(data)
print("***** Sample *****")
dplyr::sample_n(data, 4)


#=============================================================================================

ui <- fluidPage (theme = shinytheme("united"),  #Dashboard theme
                 titlePanel(' Explore Accidents in USA ', windowTitle =  "Explore Accidents in USA"),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput('State', 'Select State', unique(data$State)),
                     radioButtons('Year', 'Select Year', choices = c("2016", "2017", "2018", "2019"), selected = "2016",
                                  inline = F, width = NULL),
                   ),
                   mainPanel(
                     tabsetPanel(                                                               #create tabs in dashboard
                       tabPanel('Top 10 Cities', plotly::plotlyOutput('plot_us_accidents')),
                       tabPanel('Data_Table for all Cities', DT::DTOutput('dt_us_accidents')),
                       tabPanel('Severity Level', plotly::plotlyOutput('plot_us_severity'), verbatimTextOutput("info")),
                       tabPanel('Weather Conditions', plotly::plotlyOutput('plot_us_weather'))
                     )
                   )
                 ))

server <- function(input, output, session) {
  
###Showing notification on the right bottom of dashboard to view the current selection of the State and Year:  
  
  observe({
    showNotification(
      paste('Selected State and Year to know the accidents details of the cities and major affecting factors:', input$State, input$Year)
    )
  })
  
  
### First plot shows the information of total no. of accidents of Top 10 cities of every state and for every year.
#With this plot a viewer would able to know the top 10 cities of every state where max no. of accidents occurred. 
  
  total_accidents <- reactive({
    data %>% 
      filter(State == input$State) %>%
      filter(Year==  input$Year) %>%
      group_by(State ,City, Year) %>%
      dplyr::summarise(TOTAL_ACCIDENTS = n()) %>%   #create new column with sum of Total accidents
      arrange(desc(TOTAL_ACCIDENTS)) %>%
      head(10)
  })
  
  
  output$plot_us_accidents <- plotly::renderPlotly({
    total_accidents() %>%
      ggplot(aes(x = reorder(City, -TOTAL_ACCIDENTS), y = TOTAL_ACCIDENTS, color= City)) +
      geom_col() + labs(title = "Showing accidents count of Top 10 Cities of every State with year:") + 
      theme(title = element_text(family = "Calibri", 
                                 size = 13, 
                                 face = "bold"))
  })
  
### This table shows the information of total no. of accidents by maximum to minimum order of all cities of every state
# and for every year, this can help viewer to know which city in state had how many accidents which can be useful if a 
# viewer wants to know about condition of a particular city.
  
  output$dt_us_accidents <- DT::renderDT({
    dt_data <- data %>%
      filter(State== input$State)%>%
      filter(Year==  input$Year) %>%
      group_by(State,City) %>%
      dplyr::summarise(TOTAL_ACCIDENTS = n())%>%
      arrange(desc(TOTAL_ACCIDENTS))%>%
      head(50)
    
  })
  
  
### This graph shows the severity level of accidents from 1 to 4, means the more the severity level is, the major accident
#is there for different states for different years. In some states severity of level 1 does not exist for example in year 
# 2016 there were no accidents of severity level 1 in North Carolina.
  
  us_severity <- reactive({
    data %>% 
      filter(State == input$State) %>%
      filter(Year==  input$Year) %>%
      group_by(State, Year, Severity) %>%
      dplyr::summarise(TOTAL_ACCIDENTS = n()) %>%
      arrange(desc(TOTAL_ACCIDENTS))
    
  })

  
  output$plot_us_severity <- plotly::renderPlotly({
    us_severity() %>%
      ggplot(aes(x = Severity, y = TOTAL_ACCIDENTS, color = Severity)) +
      geom_col() + labs(title = "Showing levels of accidents severity with their counts from Low to High severity (1 to 4):") + 
      theme(title = element_text(family = "Calibri", 
                                        size = 13, 
                                        face = "bold"))
  })
  
### This graph shows the top weather conditions of every state and year in which max no. of accidents was held during that
#weather condition.

  us_weather <- reactive({
    data %>% 
      filter(State == input$State) %>%
      filter(Year==  input$Year) %>%
      group_by(State, Year, Weather_Condition) %>%
      dplyr::summarise(TOTAL_ACCIDENTS = n()) %>%
      arrange(desc(TOTAL_ACCIDENTS))%>%
      mutate(Types_of_weather = reorder(Weather_Condition, TOTAL_ACCIDENTS)) %>%  #arrange weathers in decreasing order
      head(9)
    
  })
  
  output$plot_us_weather <- plotly::renderPlotly({
    us_weather() %>%
      ggplot(aes(x = Types_of_weather, y = TOTAL_ACCIDENTS, color = Weather_Condition)) +
      geom_col() + coord_flip() + labs(title = "Showing the Top 9 weathers in which Higest no. of accidents happened for every state and year:") + 
      theme(title = element_text(family = "Calibri", 
                                 size = 13, 
                                 face = "bold"))
  })
}

shinyApp(ui, server)
