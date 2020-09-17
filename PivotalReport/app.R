library(shiny)
library(shinydashboard)
library(jsonlite)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

makePivotalTrackerRequest <- function(projectId, token, path) {
    url <- paste("https://www.pivotaltracker.com/services/v5/projects/", projectId, path, sep="")
    req <- httr::GET(url, httr::add_headers('X-TrackerToken' = token))
    json <- httr::content(req, as = "text")
    fromJSON(json, flatten = TRUE)
}

getIteration <- function(projectId, token, iterationNumber) {
    path <- paste("/iterations/", iterationNumber, sep="")
    makePivotalTrackerRequest(projectId, token, path)
}

getLabels <- function(iteration) {
    s <- iteration$stories$labels
    sort(do.call("rbind", s)$name)
}

createWordCloud <- function(labels) {
    freq <- table(labels)
    wordcloud(words = labels, freq = freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
}

plotFrequency <- function(labels) {
    freq <- table(labels)
    barplot(sort(freq, decreasing = TRUE)[1:5], las=2)

}

tryOrIgnore <- function(FUN) {
    tryCatch(
        {
            FUN()      
        }, error=function(err) {
           # cat(file = stderr(), err)
        }
    )
}

ui <- dashboardPage(
    
    dashboardHeader(title = "Pivotal Tracker Report"),
    
    dashboardSidebar(
        
        textInput("projectId", "Project ID"),
        textInput("token", "Token")
        
    ),
    
    dashboardBody(
        
        tabsetPanel(
            tabPanel("Overview", value="overviewTab",
                     plotOutput("wordCloud"),
                     plotOutput("frequency"))
        )
        
    )
    
)

server <- function(input, output) {

    currentIteration <- reactive({
        getIteration(input$projectId, input$token, 211)
    })
    
    currentIterationLabels <- reactive({
        getLabels(currentIteration())  
    })
    
    output$wordCloud <- renderPlot({
        tryOrIgnore(createWordCloud(currentIterationLabels()))
    })
    
    output$frequency <- renderPlot({
        tryOrIgnore(plotFrequency(currentIterationLabels()))
    })
    
}

shinyApp(ui = ui, server = server)
