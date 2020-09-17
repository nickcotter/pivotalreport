library(shiny)
library(shinydashboard)
library(jsonlite)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(config)

getDefaultProjectId <- function() {
    tryCatch({
        config::get("pivotal-tracker")$projectId
    }, error=function(err) {
    })
}

getDefaultToken <- function() {
    tryCatch({
        config::get("pivotal-tracker")$token
    }, error=function(err) {
    })
}

makePivotalTrackerRequest <- function(projectId, token, path) {
    url <- paste("https://www.pivotaltracker.com/services/v5/projects/", projectId, path, sep="")
    req <- httr::GET(url, httr::add_headers('X-TrackerToken' = token))
    json <- httr::content(req, as = "text")
    fromJSON(json, flatten = TRUE)
}

getIterations <- function(projectId, token) {
    makePivotalTrackerRequest(projectId, token, "/iterations?limit=50&offset=100")
}

getUnstartedBugs <- function(projectId, token) {
    makePivotalTrackerRequest(projectId, token, "/stories?with_state=unstarted&with_story_type=bug")
}

getUnscheduledBugs <- function(projectId, token) {
    makePivotalTrackerRequest(projectId, token, "/stories?with_state=unscheduled&with_story_type=bug")
}

getBugsFixedInIteration <- function(iteration) {
    
    acceptedBugs <- iteration$stories %>% 
        filter(current_state == 'accepted') %>% 
        filter(story_type == 'bug')
               
    length(acceptedBugs$id)
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
        
        textInput(inputId = "projectId", label = "Project ID", value = getDefaultProjectId()),
        textInput(inputId = "token", label = "Token", value = getDefaultToken()),
        textInput(inputId = "iterationNumber", label="Iteration")
        
    ),
    
    dashboardBody(
        
        tabsetPanel(
            tabPanel("Overview", value="overviewTab",
                     fluidRow(align="center",
                              helpText("Capacity"),
                              h2(textOutput("iterationCapacity"))),
                     plotOutput("wordCloud")
                     #plotOutput("frequency")
            ),
            tabPanel("Defects", value="defectsTab",
                     fluidRow(align="center",
                              helpText("Defects Fixed"),
                              h2(textOutput("fixedDefectCount"))),
                     fluidRow(align="center",
                              helpText("Unfixed Defects"),
                              h2(textOutput("unfixedDefectCount"))),
                     fluidRow(align="center",
                              helpText("Defect Removal Rate"),
                              h2(textOutput("defectRemovalRate")))
            )
        )
        
    )
    
)

server <- function(input, output) {

    currentIteration <- reactive({
        getIteration(input$projectId, input$token, input$iterationNumber)
    })
    
    currentIterationLabels <- reactive({
        getLabels(currentIteration())  
    })
    
    unfixedBugCount <- reactive({
        
        unstartedCount <- length(getUnstartedBugs(input$projectId, input$token)$id)
        unscheduledCount <- length(getUnscheduledBugs(input$projectId, input$token)$id)
        
        unstartedCount + unscheduledCount
    })
    
    
    output$wordCloud <- renderPlot({
        tryOrIgnore(createWordCloud(currentIterationLabels()))
    })
    
    output$frequency <- renderPlot({
        tryOrIgnore(plotFrequency(currentIterationLabels()))
    })
    
    output$fixedDefectCount <- renderText({
        tryCatch(
            {
                getBugsFixedInIteration(currentIteration())      
            }, error=function(err) {
                "-"
            }
        )
    })
    
    output$iterationCapacity <- renderText({
        if(input$iterationNumber > 0) {
            tryCatch(
                {
                    capacity <- currentIteration()$team_strength 
                    rate <- round(100 * capacity, digits = 0)
                    paste(rate, "%")
                }, error=function(err) {
                    "-"
                }
            )
        }
    })
    
    output$unfixedDefectCount <- renderText({
        tryCatch(
            {
                unfixedBugCount()      
            }, error=function(err) {
                "-"
            }
        )
    })
    
    output$defectRemovalRate <- renderText({
        tryCatch(
            {
                unfixed <- unfixedBugCount() 
                fixed <- getBugsFixedInIteration(currentIteration())
                total <- unfixed + fixed
                rate <- round(100 * fixed/total, digits = 0)
                paste(rate, "%")
            }, error=function(err) {
                "-"
            }
        )
    })
    
}

shinyApp(ui = ui, server = server)
