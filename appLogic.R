# application specific logic
# last update: 2017-03-01

# any record manipulations before storing a record
appData <- function(record){
        record
}

getRepoStruct <- function(repo){
        appStruct[[repo]]
}

repoData <- function(repo){
        data <- data.frame()
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                repo)
                data <- readItems(app, url)
        }
        data
}

appStart <- function(){
        
}

observeEvent(input$saveRelationInput, {
        app <- currApp()
        person <- input$personSelect
        myDate <- input$dateInput
        if(length(app) > 0){
                url <- itemsUrl(app[['url']], 
                                paste0(appKey, '.energy_', person))
                data <- list(date = as.character(myDate),
                             value = input$energyInput,
                             '_oydRepoName' = paste0('Energie #', person))
                writeItem(app, url, data)
                
                url <- itemsUrl(app[['url']], 
                                paste0(appKey, '.health_', person))
                data <- list(date = as.character(myDate),
                             value = input$healthInput,
                             '_oydRepoName' = paste0('Gesundheit #', person))
                writeItem(app, url, data)

                url <- itemsUrl(app[['url']], 
                                paste0(appKey, '.satisfaction_', person))
                data <- list(date = as.character(myDate),
                             value = input$satisfactionInput,
                             '_oydRepoName' = paste0('Zufriedenheit #', person))
                writeItem(app, url, data)

                url <- itemsUrl(app[['url']], 
                                paste0(appKey, '.relax_', person))
                data <- list(date = as.character(myDate),
                             value = input$relaxInput,
                             '_oydRepoName' = paste0('Entspannung #', person))
                writeItem(app, url, data)

                url <- itemsUrl(app[['url']], 
                                paste0(appKey, '.general_', person))
                data <- list(date = as.character(myDate),
                             value = input$generalInput,
                             '_oydRepoName' = paste0('Gesamt #', person))
                writeItem(app, url, data)
                
                if(!is.na(input$noteInput) &
                   (nchar(as.character(input$noteInput)) > 0)){
                        url <- itemsUrl(app[['url']], 
                                        paste0(appKey, '.diary_', person))
                        data <- list(
                                date = as.character(input$dateInput),
                                value = input$noteInput,
                                '_oydRepoName' = paste0('Notizen #', person))
                        writeItem(app, url, data)
                }
                
                output$relationInputStatus <- renderUI('Daten gespeichert')
        }
})

dateRangeSelect <- function(data){
        if(nrow(data) > 0){
                myRange <- c(mymin = as.Date(Sys.Date()),
                             mymax = as.Date(Sys.Date()))
                switch(input$dateSelect,
                       '1' = { myRange <- c(mymin = as.Date(Sys.Date()-7),
                                            mymax = as.Date(Sys.Date())) },
                       '2' = { myRange <- c(mymin = as.Date(Sys.Date() - months(1)),
                                            mymax = as.Date(Sys.Date())) },
                       '3' = { myRange <- c(mymin = as.Date(Sys.Date() - months(2)),
                                            mymax = as.Date(Sys.Date())) },
                       '4' = { myRange <- c(mymin = as.Date(Sys.Date() - months(6)),
                                            mymax = as.Date(Sys.Date())) },
                       '5' = { myRange <- c(mymin = as.Date(paste(year(Sys.Date()),'1','1',sep='-')),
                                            mymax = as.Date(paste(year(Sys.Date()),'12','31',sep='-'))) },
                       '6' = { myRange <- c(mymin = as.Date(Sys.Date() - months(12)),
                                            mymax = as.Date(Sys.Date())) },
                       '10'= { myRange <- c(mymin = as.Date('1970-01-01'),
                                            mymax = as.Date('2070-01-01')) },
                       {})
                mymin <- myRange['mymin']
                mymax <- myRange['mymax']
                daterange <- seq(mymin, mymax, 'days')
                data[as.Date(data$date) %in% daterange, ]
        } else {
                data.frame()
        }
}

output$relationGraph <- renderPlotly({
        data1 <- repoData(paste0(appKey, '.', 
                                 input$topicSelect, '_1'))
        data1 <- dateRangeSelect(data1)
        data1$marker_size <- 6
        data1Note <- repoData(paste0(appKey, '.diary_1'))
        data1Note <- dateRangeSelect(data1Note)
        if(nrow(data1Note) > 0){
                data1Note <- data1Note[, c('date', 'value')]
                colnames(data1Note) <- c('date', 'note')
                data1 <- merge(data1, data1Note, by='date', all=TRUE)
                data1[!is.na(data1$note), 'marker_size'] <- 10
        }

        data2 <- repoData(paste0(appKey, '.', 
                                 input$topicSelect, '_2'))
        data2 <- dateRangeSelect(data2)
        data2$marker_size <- 6
        data2Note <- repoData(paste0(appKey, '.diary_2'))
        data2Note <- dateRangeSelect(data2Note)
        if(nrow(data2Note) > 0){
                data2Note <- data2Note[, c('date', 'value')]
                colnames(data2Note) <- c('date', 'note')
                data2 <- merge(data2, data2Note, by='date', all=TRUE)
                data2[!is.na(data2$note), 'marker_size'] <- 10
        }

        pdf(NULL)
        outputPlot <- plotly_empty()
        if((nrow(data1) > 0) |
           (nrow(data2) > 0)){
                outputPlot <- plot_ly() %>%
                        add_lines(x = as.Date(data1$date),
                                  y = data1$value,
                                  line = list(
                                          color = 'blue',
                                          width = 2,
                                          shape = 'spline'),
                                  name = 'Person #1') %>%
                        add_markers(x = as.Date(data1$date),
                                    y = data1$value,
                                    marker = list(
                                            color='blue',
                                            size = data1$marker_size),
                                    text = data1$note,
                                    name = '', 
                                    showlegend = FALSE) %>%
                        add_lines(x = as.Date(data2$date),
                                  y = data2$value,
                                  line = list(
                                          color = 'orange',
                                          width = 2,
                                          shape = 'spline'),
                                  name = 'Person #1') %>%
                        add_markers(x = as.Date(data2$date),
                                    y = data2$value,
                                    marker = list(
                                            color='orange',
                                            size = data2$marker_size),
                                    name = '', 
                                    showlegend = FALSE) %>%
                        layout( xaxis = list(type = 'date',
                                             tickformat = '%Y-%m-%d'),
                                yaxis = list(range=c(0.5,6.5),
                                             title='< schlechter - besser >'))
        }
        dev.off()
        outputPlot
})