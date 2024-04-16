#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggbump)

#load in our music data
billboard_data<-read.csv("/Users/inesperezalvarez-pallete/Desktop/billboard.csv",header = TRUE)
music_data <- read.csv("/Users/inesperezalvarez-pallete/Desktop/audio_features.csv", header = TRUE)

unique_artists <- unique(billboard_data$performer)

# Define UI for application that draws a histogram

ui <- fluidPage(
    
    
    titlePanel("Billboard Ranking Visualization"),
    sidebarLayout(
        sidebarPanel(
            # Full text search input for selecting artist
            style = "font-size:18px;color:black;background-color:lavender;padding:15px;border-radius:10px;margin-bottom:20px;margin-top:20px;text-align:center;",
            textInput(inputId = "artist_search", label = "Search Artist", placeholder = "Type artist name..."),
            uiOutput("artist_suggestions") # Suggestions for artist searc
            
             ),
        mainPanel(
            plotOutput("rank_chart") # Visualization of Billboard ranking over time
        ),
       # column(
        #    width = 12,
          #  style="text-align:center;",
          #  DT::dataTableOutput('song_list')
       # )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$artist_suggestions <- renderUI({
        req(input$artist_search)
        artist_suggestions <- unique(billboard_data$performer[grep(input$artist_search, billboard_data$performer, ignore.case = TRUE)])
        if (length(artist_suggestions) > 0) {
            selectizeInput(inputId = "selected_artist", label = NULL, choices = artist_suggestions, multiple = FALSE)
        } else {
            NULL
        }
    })
    
    output$rank_chart <- renderPlot({
        
        req(input$selected_artist)
        selected_artist_data <- filter(billboard_data, performer == input$selected_artist)
        selected_artist_data$week_id <- as.Date(selected_artist_data$week_id, format = "%m/%d/%Y")
        
        ggplot(selected_artist_data, aes(x = week_id, y = peak_position, color = song, text = paste("Song:", song))) +
            geom_line() +
            geom_point() +
            labs(x = "Week", y = "Weeks Position", color = "Song") +
            theme_minimal() +
            scale_x_date(date_labels = "%m/%d/%Y", date_breaks = "2 years") +
            guides(color = guide_legend(title = "Song Title")) +  # Include legend label for song titles
            theme(legend.position = "right")  # Adjust legend position

    })
    
   # output$song_list <- DT::renderDataTable({
    #    req(input$selected_artist)
      #  artist_data <- filter(billboard_data, performer == input$selected_artist)
     #   artist_data$week_id <- as.Date(selected_artist_data$week_id, format = "%m/%d/%Y")
        
     #   table2_select <- select(selected_artist_data, week_id, song, performer, peak_position, weeks_on_chart)
        
     #   DT::datatable(table2_select)
  #  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
