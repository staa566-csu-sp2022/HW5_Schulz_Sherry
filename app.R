
library(shiny)
library("leaflet")
library("tidyverse")
library("dplyr")
library("leafpop")
library("tidyr")
library("ggplot2")
library("DT")
library("shinythemes")
library("readr")
library("shinydashboard")
library("rio")

setwd("C:/Users/syschulz/Documents/GitHub/HW5_Schulz_Sherry")
life<-read.csv("Human_life_Expectancy.csv")
location<-read.csv("World_Locator.csv")


location<-location[,2:4] #Keeping columns 2, 3, and 4 of the location file and replace the file.

life<-life[life$Level=="National",]
combined<-left_join(life, location, by = "Country") #For each row of national it is trying to match the location in the location file. Now it combines the 2 files.

combined<-replace(combined, combined == "Not Available", NA) #replace Not Available to NA
for (i in 5:ncol(combined)){
    combined[,i] <- as.numeric(combined[,i])
} #loop from i=5 onward in combined, take the ith row in combined and replace it with numeric category.


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("LIfe Expectancy Worldwide"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Year",
                        "Select a Year:",
                        choices=names(combined[,5:34]),
                                      selected="X2019"),
            selectInput("Country",
                        "Select a Country:",
                        choices=unique(combined[,"Country"]),
                        selected="Afghanistan",multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("Map",width="100%",height="500px"),
           DTOutput("DT"),
           plotOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    #location<-location[,2:4] #Keeping columns 2, 3, and 4 of the location file and replace the file.
    
   # life<-life[life$Level=="National",]
   # combined<-left_join(life, location, by = "Country") #For each row of national it is trying to match the location in the location file. Now it combines the 2 files.
    
   # combined<-replace(combined, combined == "Not Available", NA) #replace Not Available to NA
   # for (i in 5:ncol(combined)){
   #     combined[,i] <- as.numeric(combined[,i])
   # } #loop from i=5 onward in combined, take the ith row in combined and replace it with numeric category.
    

    output$Map<- renderLeaflet({
        combined %>%
            leaflet() %>%
            setView(lng=0, lat=0, zoom = 1.5) %>%
            addTiles() %>%
            addMarkers(lat=~latitude, lng=~longitude,
                       icon=list(
                           iconUrl="https://img.icons8.com/material/24/000000/arms-up.png",
                           iconSize=c(20,20)
                       ),popup=~as.character(combined[,input$Year]))
    })
    
    output$DT<-renderDT({
        mytable<-datatable(combined %>%
                               filter(Country==input$Country)%>%
                               select(c("Country","Country_Code","latitude","longitude",input$Year)))
        mytable
        #saveWidget(mytable, "mytable.html")
    })
    output$Plot<-renderPlot({
        combined %>% select(-c(2,3,4,35,36)) %>% pivot_longer(cols = 2:31,names_to = "Year", values_to = "Expect") %>% 
            mutate(Year = as.numeric(str_replace(Year, "X","")), Country = factor(Country)) %>% 
            filter(Country %in% input$Country) %>% 
            ggplot(., aes(x = Year, y = Expect, color = Country)) + geom_line() + 
            labs(title = "Life Expectancy over Time", ylab = "Life Expectancy") + 
            theme_classic()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
