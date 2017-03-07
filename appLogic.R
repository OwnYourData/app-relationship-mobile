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

output$relationGraph <- renderPlotly({
        data1 <- repoData(paste0(appKey, '.', 
                                 input$topicSelect, '_1'))
        data2 <- repoData(paste0(appKey, '.', 
                                 input$topicSelect, '_2'))
        pdf(NULL)
        outputPlot <- plotly_empty()
        if(nrow(data) > 0){
                outputPlot <- plot_ly() %>%
                        add_lines(x = as.Date(data1$date),
                                  y = data1$value,
                                  line = list(
                                          color = 'blue',
                                          width = 3,
                                          shape = 'spline'),
                                  name = 'Person #1') %>%
                        add_markers(x = as.Date(data1$date),
                                    y = data1$value,
                                    marker = list(color='blue'),
                                    name = '') %>%
                        add_lines(x = as.Date(data2$date),
                                  y = data2$value,
                                  line = list(
                                          color = 'orange',
                                          width = 3,
                                          shape = 'spline'),
                                  name = 'Person #1') %>%
                        add_markers(x = as.Date(data2$date),
                                    y = data2$value,
                                    marker = list(color='orange'),
                                    name = '') %>%
                        layout(
                                yaxis = list(range=c(1,6),
                                             title='< schlechter - besser >'))
        }
        dev.off()
        outputPlot
})