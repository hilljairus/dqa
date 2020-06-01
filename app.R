

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("cerulean"),
     navbarPage("DQA",id="tabs",


    tabPanel("home","Upload...",
             sidebarLayout(
                 sidebarPanel(
             fileInput("file",NULL,accept=".csv"),
             checkboxInput("head","Display Everything",TRUE),
             conditionalPanel(condition = "input.head==false",
                 numericInput("n","Number of rows to display",12)
             ),
                  numericInput("skip","Rows to skip",0),
                  
                  tags$hr(),
             
             
                 verbatimTextOutput("class")
                 ),
                mainPanel(
                    h3("Raw Data"),
                    tableOutput("preview"))
                   
                    
             )
             
    ),
    tabPanel("Clean&Filter","Clean & Filter",
             clean_filterUI("clnFt")
             )
        
        
 )
)

server <- function(input, output,session) {
    data<-reactive({
        req(input$file)
        ext<-tools::file_ext(input$file$name)
        switch(ext,
            csv=vroom::vroom(input$file$datapath,skip=input$skip),
            validate("Invalid File; Please Upload .csv file"))
    })
    
    output$preview<-renderTable(
        if(input$head){
            data()
        }else{
        head(data(),input$n)
        }
    )
    output$class<-renderPrint(preview_coltype(data()))
    
    callModule(clean_filterServer,"clnFt",tidied=data(),session=session)
}

# Run the application 
shinyApp(ui = ui, server = server)

