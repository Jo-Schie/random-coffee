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
df.secret.santa<-data.frame(participants=c("Heike",
                                           "Anja B.",
                                           "Beate",
                                           "Franzi",
                                           "Nicola",
                                           "Jochen",
                                           "Doro",
                                           #"Tatjana",
                                           "Jörg",
                                           #"Vanessa",
                                           "Alex",
                                           "Vivien",
                                           "Lennart",
                                           "Anna Victoria",
                                           "Johannes",
                                           #"Christian",
                                           "Daniela",
                                           "Dilara",
                                           "Anja S.",
                                           "Luis",
                                           "Lotte",
                                           "Philip"),
                            secret_friends=NA)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Zufalls-Kaffee-Generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("inputnamen",
                        "Wer nimmt teil?",
                        df.secret.santa$participants,multiple = T,
                        selected=df.secret.santa$participants),
            textInput("caption", "Neuer Teilnehmer", ""),
            actionButton("add", "Add"),
            "Keine Lust auf Kaffee alleine? Drück den Button!",
            actionButton("go", "Go"),
            img(src="gato.gif",width=250,height=181)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("textwoche"),
           tableOutput("zufallsteilnehmer")
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output, session) {
    # observe user inputs
    
    # add a new name to the input list of "inputnamen". So far only works with one
    # additional name
    observeEvent(
        input$add,{
            if(input$caption %in% df.secret.santa){

            }
            else{
                df.secret.santa_new <- df.secret.santa %>%
                    add_row (participants = input$caption, secret_friends = NA)
                updateSelectInput(session, "inputnamen", label = "Wer nimmt teil?",
                                  choices  = df.secret.santa_new$participants,
                                  selected = df.secret.santa_new$participants)
            }
            df.secret.santa <- df.secret.santa_new
        })


    inputnamendynamic <- reactive({
        input$inputnamen
    })
    
    # random table creation. 
    randomVals <- eventReactive(input$go, {
        # check if number of participants is odd or even
        mylength<-length(inputnamendynamic())/2
        mylength<-ifelse(length(inputnamendynamic()) %% 2 == 0# is nr even?
                         ,mylength,mylength+1)
        # create table with paticipants
        df.secret.santa.dynamic<-t(as.data.frame(split(
            sample(inputnamendynamic()),
            rep(1:mylength,each=2))
            ))
        colnames(df.secret.santa.dynamic)<-c("Wer meldet sich?","Bei wem?")
        df.secret.santa.dynamic
    })
    output$zufallsteilnehmer <- renderTable({randomVals()
    })
    output$textwoche <- renderText({paste("Und los gehts!")
    })

    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
