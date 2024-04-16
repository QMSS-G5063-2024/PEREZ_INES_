#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(shinysurveys)
library(shinydashboard)
library(xtable)
library(shinyjs)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(


    # Sidebar with a slider input for number of bins 
    fluidRow(
        tags$h1("Billboard Top 100 Artist Data", style = "text-align:center;font-weight:bold;"),
        column(
            width = 12,
            style = "font-size:18px;color:black;background-color:lavender;padding:15px;border-radius:10px;margin-bottom:20px;margin-top:20px;text-align:center;",
            title = "Select Date range of the artist",
            sliderInput(inputId = "artist_year", label = "Artist Year:", min = 1900, max = 2020, value = 1970, step = 1)
        ),
        br(),
        
        column(
            width = 12,
            style="text-align:center;",
            DT::dataTableOutput('top_20_table')
        )
        
    ),
    
    fluidRow(
        
           # artist_list <- unique(billboard_data$performer)
             
             selectizeInput(
                 inputId = "artist_search", 
                 label = "Select an Artist",
                 multiple = FALSE,
                 choices = c("Search Bar" = "", unique_artists),
                 options = list(
                     create = FALSE,
                     placeholder = "Search Me",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                     onType = I("function (str) {if (str === \"\") {this.close();}}")
                 )
             )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(tidyverse)
    library(ggbump)

    #load in our music data
    billboard_data<-read.csv("/Users/inesperezalvarez-pallete/Data_Viz_HW4.csv",header = TRUE)
    music_data <- read.csv("//Users/inesperezalvarez-pallete/Data_Viz_HW4/audio_features.csv", header = TRUE)
    
    unique_artists <- unique(billboard_data$performer)
    
    
    output$top_20_table <- DT::renderDataTable({
        
        artist_list <- unique(billboard_data$performer)
        # Extract the selected year from the input
        selected_year <- input$artist_year
        billboard_select<-select(billboard_data,"week_id","song", "performer", "peak_position","weeks_on_chart")
        # Filter the data based on the selected year
        filtered_data <- subset(billboard_select, substr(week_id, nchar(week_id) - 3, nchar(week_id)) == selected_year)
        
        # Select specific columns and sort by 'weeks_on_chart' in descending order
        sorted_data <- filtered_data %>%
            arrange(desc(weeks_on_chart)) %>%
            distinct(song, .keep_all = TRUE)
        
        # Return the top 20 rows
        top_20_data <- head(sorted_data, 20)
        
        # Render the DataTable
        top_20_data
    })
    #output$bump_chart<-renderPlot(
        
        #merge with spotify data by song_id
   
        #artist_select<- 
     #   billboard_selects<-select(billboard_data,"week_id","song", "performer", "peak_position","weeks_on_chart")
        # Filter the data based on the selected year
      
    #  filtered_datas <- subset(billboard_selects, substr(week_id, nchar(week_id) - 3, performer== artist_search)
        
        # Select specific columns and sort by 'weeks_on_chart' in descending order
     #   sorted_datas <- filtered_datas %>%
     #       arrange(desc(weeks_on_chart)) %>%
       #     distinct(song, .keep_all = TRUE)
        
        # Return the top 20 rows
     #   top_20_datas <- head(sorted_datas, 20)
        
       
        
      #  ggplot(filtered_datas, aes(x = performer, y = weeks_on_chart, color = group)) +
         #   geom_bump(size = 1.5) +
        #    geom_point(size = 6) +
          #  geom_text(data = df %>% filter(x == min(x)),
           #           aes(x = x - 0.1, label = group),
            #          size = 5, hjust = 1) +
            #geom_text(data = df %>% filter(x == max(x)),
             #         aes(x = x + 0.1, label = group),
           #           size = 5, hjust = 0) +
           # scale_color_brewer(palette = "RdBu") +
            #theme_void() +
            #theme(legend.position = "none")
 #   )

}

# Run the application 
shinyApp(ui = ui, server = server)
