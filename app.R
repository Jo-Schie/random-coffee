#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
df.secret.santa<-data.frame(participants=c(
    "Anna Victoria",
    "Alex",
    "Anja B.",
    "Anja S.",
    "Beate",
    "Daniela",
    "Dilara",
    "Franzi",
    "Heike",
    "Jochen",
    "Jörg",
    "Johannes",
    "Jota",
    "Lennart",
    "Luis",
    # "Inga",
    #"Lotte",
    "Nicola",
    "Doro",
    #"Tatjana",
    #"Vanessa",
    "Philip",
    "Vivien"
    ),
    secret_friends = NA)

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
            #textInput("caption", "Neuer Teilnehmer", ""),
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
server <- function(input, output) {
    # observe user inputs
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
