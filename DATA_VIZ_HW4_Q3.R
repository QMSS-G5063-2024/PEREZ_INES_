library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggbump)
library(lubridate)
library(shiny)
library(plotly)

# Load in our music data
#Add a hover text that show the artist, song title, the year, and the billboard peak position.
billboard_data <- read.csv("/Users/inesperezalvarez-pallete/Data_Viz_HW4/billboard.csv", header = TRUE)
music_data <- read.csv("/Users/inesperezalvarez-pallete/Data_Viz_HW4/audio_features.csv", header = TRUE)

# Join the data
data_joined <- left_join(billboard_data, music_data,by = 'song_id', relationship="many-to-many")

# Mutate chart_status column
data_joined <- data_joined %>%
    mutate(chart_status = case_when(
        peak_position == 1 ~ "Top 1",
        peak_position <= 10 ~ "Top 10",
        peak_position <= 20 ~ "Top 20",
        TRUE ~ "Not Top 20"  # If the peak position is greater than 20
    ))

extract_genres <- function(combined_genre) {
    #genres <- gsub("\\[|\\]|'", "", combined_genre)
    genres <- unlist(strsplit(combined_genre, ","))
    return(genres)
}

filter_by_genre <- function(data, genre_selection) {
    # Check if the genre_selection is a single genre or a list of genres
    if (!is.character(genre_selection)) {
        stop("Genre selection must be a single character string")
    }
    
    # Filter rows based on genre_selection
    filtered_data <- data[grep(genre_selection, data), ]
    
    return(filtered_data)
}
# Define UI for application
ui <- fluidPage(
    sidebarLayout(
        titlePanel("Billboard Ranking Visualization"),
        fluidRow(
            column(
                width = 12,
                h3("Exploring select audio and billboard features"),
                p(HTML("With the provided selection, filter your choice of artist, time period, genre, and chart status to view our data."),                                
                  style = "font-size:18px;text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px;margin-bottom:20px;margin-top:20px"),
                br(),
                column(width = 4,
                       dateRangeInput("year_range", "Date range:",
                                      start = "1970-01-01",
                                      end   = "2010-12-31",
                                      format = "m/d/yyyy" ),
                       br(),
                       textInput(inputId = "artist_search", label = "Search Artist", placeholder = "Type artist name..."),
                       uiOutput("artist_suggestions"), # Suggestions for artist search
                       selectInput("Popularity", "Select the Ranking of the Artist", choices = c("Top 1", "Top 10", "Top 20", "Not Top 20")),
                       selectInput("genre","Genre",
                                   unique_genres, multiple = TRUE),
                       actionButton("plot", "Plot")
                )
            ),
            column(width = 10,
                   plotlyOutput("plot2")
            )
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    output$artist_suggestions <- renderUI({
        req(input$artist_search)
        artist_suggestions <- unique(data_joined$performer.x[grep(input$artist_search, data_joined$performer.x, ignore.case = TRUE)])
        if (length(artist_suggestions) > 0) {
            selectizeInput(inputId = "selected_artist", label = NULL, choices = artist_suggestions, multiple = FALSE)
        } else {
            NULL
        }
    })
    
    observeEvent(input$plot, {
        
        output$plot2 <- renderPlotly({
            
            # Convert start_date to Date format
            start_date <- as.Date(input$year_range[1], format = "%m/%d/%Y")
            
            # Assuming input$year_range[2] is already in the desired format "5/3/1997"
            end_date <- as.Date(input$year_range[2], format = "%m/%d/%Y")
            data_joined$new_week_id<-as.Date(data_joined$week_id, format = "%m/%d/%Y")
            artist <- input$selected_artist
            ranking <- input$Popularity
            genre <- input$genre
           
           # data_filtered_genre <- filter_by_genre(data_joined, genre)
            # Convert week_id in data_joined to Date format
          #  data_joined$week_id <- as.Date(data_joined$week_id, format = "%m/%d/%Y")
            
           # data_joined <- data_joined %>%
                #mutate(spotify_genre_cleaned = sapply(spotify_genre, extract_genres))
            
            
            sampled_data <- data_joined %>% 
                filter(
                    chart_status==ranking,
                    performer.x == artist,
                    new_week_id >= start_date,
                    new_week_id<end_date
                    #extract_genres(spotify_genre) %in% filter_by_genre(., genre)
                )
               
            
            #sampled_data <- data_joined %>% 
              #  filter(
               #        chart_status == ranking,
                #       performer.x == artist,
                 #      spotify_genre %in% genre,
                  #    new_week_id >= start_date,
                   #   new_week_id<end_date)
            
            if (nrow(sampled_data) > 500) {
                sampled_data <- sampled_data %>%
                    sample_n(500, replace = FALSE)
            }
            
            # Create the plotly scatter plot
            plot_ly(sampled_data, x = ~danceability, y = ~tempo, type = 'scatter', mode = 'markers', color = ~time_signature, text = ~paste("Artist:", performer.x, "<br>Date:", new_week_id, "<br>Peak Position:", peak_position))
            
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
