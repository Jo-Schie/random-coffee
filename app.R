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
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Remote-Arbeiten-Zufalls-Kaffeeklatsch-Generator (RAZKG)"),
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            # pre-defined inputs list
            selectizeInput(
                "vec1", "Geben Sie die Namen ein", 
                choices = NULL, 
                multiple = TRUE,
                options = list(create = TRUE)
            ),
            # Text
            "Keine Lust auf Kaffee alleine? Drück den Button!",
            # Button for creating the table
            actionButton("go", "Go"),
            # image
            img(src = "gato.gif",
                width = 250,
                height = 181)
        ),
        # Table output to the side
        mainPanel(textOutput("textwoche"),
                  tableOutput("zufallsteilnehmer"))
    ))


server <- function(input, 
                   output, 
                   session) {
    # observe user inputs
    
    inputnamendynamic <- reactive({
        input$vec1
    })
    
    # random table creation.
    randomVals <- eventReactive(input$go, {
        # check if number of participants is odd or even
        mylength <- length(inputnamendynamic()) / 2
        mylength <-
            ifelse(length(inputnamendynamic()) %% 2 == 0,
                   mylength,
                   mylength + 1)
        # print an error message for an even number of participants
        if(!length(inputnamendynamic()) %% 2 == 0){
            showNotification(paste("Aufgrund einer ungeraden Teilnehmerzahl führt der/die letzte heute leider ein Selbstgespräch, es sei denn, es lädt jemand spontan mit ein?"),
                             type="warning",duration = 120)}
        # create table with paticipants
        df.secret.santa.dynamic <- t(as.data.frame(split(
            sample(inputnamendynamic()),
            rep(1:mylength, each = 2)
        )))
        colnames(df.secret.santa.dynamic) <-
            c("Wer meldet sich?", "Bei wem?")
        df.secret.santa.dynamic
    })
    
    output$zufallsteilnehmer <- renderTable({
        randomVals()
    })
    
    output$textwoche <- renderText({
        paste("Und los gehts!")
    })
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
