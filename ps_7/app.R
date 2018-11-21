
library(shiny)
library(tidyverse)
library(fs)
library(readr)
library(ggrepel)

app_data <- read_rds("ps_7.rds")

app_data <- app_data %>%
  mutate(region = case_when(str_detect(state, pattern ="CT") ~ "Northeast", 
                            str_detect(state, pattern ="ME") ~ "Northeast", 
                            str_detect(state, pattern ="MA") ~ "Northeast", 
                            str_detect(state, pattern ="NH") ~ "Northeast", 
                            str_detect(state, pattern ="RI") ~ "Northeast", 
                            str_detect(state, pattern ="VT") ~ "Northeast", 
                            str_detect(state, pattern ="NJ") ~ "Northeast", 
                            str_detect(state, pattern ="NY") ~ "Northeast", 
                            str_detect(state, pattern ="PA") ~ "Northeast", 
                            str_detect(state, pattern ="IN") ~ "Midwest", 
                            str_detect(state, pattern ="IL") ~ "Midwest", 
                            str_detect(state, pattern ="MI") ~ "Midwest", 
                            str_detect(state, pattern ="OH") ~ "Midwest", 
                            str_detect(state, pattern ="WI") ~ "Midwest", 
                            str_detect(state, pattern ="IA") ~ "Midwest", 
                            str_detect(state, pattern ="KS") ~ "Midwest", 
                            str_detect(state, pattern ="MN") ~ "Midwest", 
                            str_detect(state, pattern ="MO") ~ "Midwest", 
                            str_detect(state, pattern ="NE") ~ "Midwest", 
                            str_detect(state, pattern ="ND") ~ "Midwest", 
                            str_detect(state, pattern ="SD") ~ "Midwest", 
                            str_detect(state, pattern ="DE") ~ "Southwest", 
                            str_detect(state, pattern ="DC") ~ "Southwest", 
                            str_detect(state, pattern ="FL") ~ "Southwest", 
                            str_detect(state, pattern ="GA") ~ "Southwest", 
                            str_detect(state, pattern ="MD") ~ "Southwest", 
                            str_detect(state, pattern ="NC") ~ "Southwest", 
                            str_detect(state, pattern ="SC") ~ "Southwest", 
                            str_detect(state, pattern ="VA") ~ "Southwest", 
                            str_detect(state, pattern ="WV") ~ "Southwest", 
                            str_detect(state, pattern ="AL") ~ "Southwest", 
                            str_detect(state, pattern ="KY") ~ "Southwest", 
                            str_detect(state, pattern ="MS") ~ "Southwest", 
                            str_detect(state, pattern ="TN") ~ "Southwest", 
                            str_detect(state, pattern ="AR") ~ "Southwest", 
                            str_detect(state, pattern ="LA") ~ "Southwest", 
                            str_detect(state, pattern ="OK") ~ "Southwest", 
                            str_detect(state, pattern ="TX") ~ "Southwest", 
                            str_detect(state, pattern ="AZ") ~ "West", 
                            str_detect(state, pattern ="CO") ~ "West", 
                            str_detect(state, pattern ="ID") ~ "West", 
                            str_detect(state, pattern ="NM") ~ "West", 
                            str_detect(state, pattern ="MT") ~ "West", 
                            str_detect(state, pattern ="UT") ~ "West", 
                            str_detect(state, pattern ="NV") ~ "West", 
                            str_detect(state, pattern ="WY") ~ "West", 
                            str_detect(state, pattern ="AK") ~ "West", 
                            str_detect(state, pattern ="CA") ~ "West", 
                            str_detect(state, pattern ="HI") ~ "West", 
                            str_detect(state, pattern ="OR") ~ "West", 
                            str_detect(state, pattern ="WA") ~ "West"))
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Problem Set 7"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("region", "
                           Select Region:",
                           choices = c("West", "Southwest", "Midwest", "Northeast")
                    
                           #app_data$region,
                    
                    
                    
                             
                             )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )))
   
   


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     app_data %>%
       filter(region == input$region) %>%
     ggplot(aes(x = forecast, y = result)) + 
              geom_point() +
       ggtitle("Forcasted vs. Actual Democratic Advantage in 2018 Midterms Elections by State") +
       xlab("Forecast Democratic Advantage based upon Upshot Polls") +
       ylab("Result Democratic Advantage after Elections") +
       geom_label_repel(aes(label = Office), size = 3, force = 3) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

