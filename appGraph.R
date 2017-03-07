# mobile UI to show data
# last update: 2017-03-01

source('appSelect.R')

appGraph <- function(){
        tabPanel('Gesamt-Verlauf',
                 appSelect(),
                 bsAlert('dataStatus'),
                 plotlyOutput('relationGraph')
        )
}
                 