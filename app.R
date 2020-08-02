
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(fmsb)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)


access_token <- get_spotify_access_token()

pop <- get_genre_artists(genre = "pop", market = NULL, limit = 4,
                         offset = 0, authorization = access_token)


blues <- get_genre_artists(genre = "blues", market = NULL, limit = 4,
                           offset = 0, authorization = access_token)

wonky <- get_genre_artists(genre = "wonky", market = NULL, limit = 4,
                           offset = 0, authorization = access_token)

rock <- get_genre_artists(genre = "rock", market = NULL, limit = 4,
                          offset = 0, authorization = access_token)



blues <- blues %>% select(name,genre,popularity,followers.total)
pop <- pop %>% select(name,genre,popularity,followers.total)
wonky <-  wonky %>% select(name,genre,popularity,followers.total)
rock <- rock %>% select(name,genre,popularity,followers.total)

temp <- bind_rows(blues,pop, wonky, rock)
t1 <- temp %>% select(name) %>% slice(1) %>% pull()
dat <- get_artist_audio_features(t1,include_groups = "album") 
dat <- dat %>% bind_cols(temp %>% filter(name %in% t1) %>% select(genre:followers.total) %>% uncount(nrow(dat)))


for (i in 2: (temp %>% select(name) %>% nrow()) ){
    #ndat <- get_artist_audio_features(artistnames[i])
    #dat <- bind_rows(ndat)
    
    t1 <- temp %>% select(name) %>% slice(i) %>% pull()
    ndat <- get_artist_audio_features(t1,include_groups = "album") 
    ndat <- ndat %>% bind_cols(temp %>% filter(name %in% t1) %>% select(genre:followers.total) %>% uncount(nrow(ndat)))
    dat <- dat %>% bind_rows(ndat)
    
}
dat %>% select(valence) %>% summary()
library(shiny)


ui = fluidPage(
    sidebarPanel( #designates location of following items
        htmlOutput("genre_selector"),#add selectinput boxs
        htmlOutput("artist_selector"),# from objects created in server
        plotlyOutput("recommendedArtists"),
    ),
    
    mainPanel(
        tableOutput("summary"),
        plotOutput('summaryplot'),
        plotlyOutput('plot'),
        plotlyOutput("lengthPlot"),
        plotlyOutput("albumLoudnessPlot"),
        plotlyOutput("bubb"),
        plotlyOutput("bar")
        
    )
    
)
server = shinyServer(function(input, output){
    
    output$genre_selector = renderUI({ #creates State select box object called in ui
        selectInput(inputId = "genre", #name of input
                    label = "Genre:", #label displayed in ui
                    choices = as.character(unique(c("pop","blues", "wonky", "rock"))),
                    multiple = T,
                    # calls unique values from the State column in the previously created table
                    selected = "pop") #default choice (not required)
    })
    
    
    output$artist_selector = renderUI({#creates County select box object called in ui
        
        data_available = dat %>% filter(genre %in% input$genre) %>% select(artist_name) %>% pull() 
        #creates a reactive list of available counties based on the State selection made
        
        selectInput(inputId = "artist", #name of input
                    label = "Artist:", #label displayed in ui
                    choices = unique(data_available), #calls list of available counties
                    multiple = T,
                    selected = unique(data_available[1]) )
    })
    
    
    output$summary = renderTable({
        dat %>% 
            filter(artist_name %in% input$artist) %>% 
            group_by(artist_name,genre) %>% 
            mutate(duration_m = duration_ms/60000) %>% 
            select(artist_name, genre, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration_m) %>% 
            summarize_if(is.numeric,mean,na.rm = T) 
    })
    
    output$summaryplot = renderPlot({
        summaryplot = dat %>% 
            filter(artist_name %in% input$artist) %>% 
            # Scale to 0 to 1 by myself, based on spotify's developer information
            mutate(duration_m = duration_ms/60000, 
                   loudness = loudness/60 + 1, 
                   tempo = tempo/250) %>% 
            select(artist_name, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>% 
            group_by(artist_name) %>% 
            summarise_all(funs(mean))
        summaryplot = data.frame(artistname = rep(summaryplot$artist_name, 9), 
                                 character = rep(colnames(summaryplot)[2:10], times=1, each=nrow(summaryplot)), 
                                 value = c(as.matrix(summaryplot[,2:10])))
        ggplot(data = summaryplot,
               aes(x=character, 
                   y=value, 
                   fill=artistname)) + 
            geom_bar(stat="identity", position=position_dodge())
    })
    
    output$plot <- renderPlotly(
        
        plot1 <- dat %>% filter(artist_name %in% input$artist) %>%  plot_ly(
            x = ~valence,
            y = ~liveness, 
            color =~(as.factor(artist_name)),
            type = 'scatter',
            mode = 'markers')
    )
    
    
    
    output$lengthPlot <- renderPlotly({
        tracklength <- dat %>%  filter(artist_name %in% input$artist)  %>% 
            select(artist_name, duration_ms, album_release_year) 
        tracklength$artist_name <- as.factor(tracklength$artist_name)
        tracklength %>% plot_ly(x= ~(duration_ms / 60000), color = ~factor(artist_name),nbinsx = 10) %>% 
            add_histogram(histnorm = "", alpha = 0.7) %>%
            layout(barmode = "stack", title = "Histogram of Average Track length corresponding to Artist") 
        
        
    })
    
    output$albumLoudnessPlot <- renderPlotly({
        
        dat %>% filter(artist_name %in% input$artist ) %>% group_by(album_release_date,artist_name,album_name) %>% summarize(avg_tempo= mean(tempo)) %>% ungroup() %>% mutate(album_release_date = lubridate::as_date(album_release_date)) %>%
            plot_ly(x = ~album_release_date, y = ~avg_tempo, mode = 'lines',  color = ~factor(artist_name), text = ~factor(album_name))
        
    })
    
    output$recommendedArtists <- renderPlotly({
        tempID <- dat %>% filter(artist_name %in% input$artist) %>% 
            select(artist_id) %>% distinct()
        artistID <- tempID[[1:length(tempID)]]
        
        recommendation <- get_recommendations(limit = 15, seed_artists = artistID, authorization = ccess_token())
        
        zz <- recommendation %>% pull(artists) %>% bind_rows() %>% 
            filter(!(name %in% input$artists)) %>% pull(name) %>% str_c(collapse = ", ")
        
        zz <- rbind(rep(str_c(input$artist, collapse = ", "), times = length(zz)) , zz) 
        
        plot_ly(
            type = 'table',
            columnorder = c(1,2),
            columnwidth = c(80,400),
            header = list(
                values = c('<b>Artists</b><br>', '<b>Recommendations Based On Artists</b>'),
                line = list(color = '#506784'),
                fill = list(color = '#119DFF'),
                align = c('left','center'),
                font = list(color = 'white', size = 12),
                height = 40
            ),
            cells = list(
                values = zz,
                line = list(color = '#506784'),
                fill = list(color = c('#25FEFD', 'white')),
                align = c('left', 'center'),
                font = list(color = c('#506784'), size = 12),
                height = 30
            ))
        
        
    })
    
    
    output$bubb <- renderPlotly({
        dat  %>%  
            filter(artist_name %in% input$artist) %>% 
            mutate(valence = 100*valence, 
                   danceability = 100*danceability, 
                   diff = abs(danceability - valence)/10) %>% 
            plot_ly( x = ~danceability, 
                     y = ~valence, 
                     color = ~factor(artist_name)) %>% 
            layout(title = 'valence energy diff',
                   xaxis = list(showgrid = FALSE),
                   yaxis = list(showgrid = FALSE))
        
        
    })
    
    output$bar <- renderPlotly({
        
        dat  %>%  filter(artist_name %in% input$artist) %>% group_by(artist_name) %>% mutate(valence = as.integer(mean(100*valence)) , popularity = as.integer(popularity)) %>% 
            select(artist_name,valence, popularity) %>% distinct() %>% 
            plot_ly(x = ~factor(artist_name), y = ~valence, type = 'bar', name = 'valence', marker = list(color = 'rgb(49,130,189)')) %>% add_trace(y = ~popularity, name = 'popularity', marker = list(color = 'rgb(204,204,204)')) %>% layout(xaxis = list(title = "", tickangle = -45),
                                                                                                                                                                                                                                                yaxis = list(title = ""),
                                                                                                                                                                                                                                                margin = list(b = 100),
                                                                                                                                                                                                                                                barmode = 'group') 
        
    })
    
    
})

shinyApp(ui = ui, server = server)




