source("preview_summary.R")
source("tab2module.R")
source("tab3module.R")
source("file_input.R")
library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("cerulean"),
     navbarPage("DQA",id="tabs",


    tabPanel("home","Upload...",
            fileInput_UI("flnpt")
    ),
    tabPanel("Clean&Filter","Clean & Filter",
             clean_filterUI("clnFt")
             ),
    tabPanel("dqa1","dqa1",
             dimUI("cmplt")
             )
        
 )
)

server <- function(input, output,session) {
    
    callModule(fileInput_server, "flnpt")
    fileData<-callModule(fileInput_server, "flnpt")
    callModule(clean_filterServer,"clnFt",tidied=fileData$mydata(),session=session)
    dimData<-callModule(clean_filterServer,"clnFt",tidied=fileData$mydata(),session=session)
    callModule(dimServer,"cmplt",data=dimData)
    
}

# Run the application 
shinyApp(ui = ui, server = server)

