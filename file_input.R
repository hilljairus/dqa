fileInput_UI<-function(file){
  ns<-NS(file)
  formats<-c("DD/MM/YYYY"="%d/%m/%Y")
  tagList(sidebarLayout(
    sidebarPanel(
      #selectInput(ns("date_format"),"Date Format", choices =formats),
      numericInput(ns("skip"),"Rows to skip",0),
      fileInput(ns("file"),NULL,accept=".csv"),
      checkboxInput(ns("head"),"Display Everything",TRUE),
      conditionalPanel(condition = "input.head==false",ns=ns,
                       numericInput(ns("n"),"Number of rows to display",12)
      ),
      
      
      tags$hr(),
      
      
      tableOutput(ns("class"))
    ),
    mainPanel(
      h3("Raw Data"),
      tableOutput(ns("preview")))
  )
  
  )
}

fileInput_server<-function(input,output,session){
  data<-reactive({
    req(input$file)
    ext<-tools::file_ext(input$file$name)
    switch(ext,
           csv=vroom::vroom(input$file$datapath,skip=input$skip,delim = ","),
           validate("Invalid File; Please Upload .csv file"))
  })
  
  output$preview<-renderTable(
    if(input$head){
      data()
    }else{
      head(data(),input$n)
    }
  )
  output$class<-renderTable(tablerize(data()))
  return(list(mydata=reactive(data())))
}