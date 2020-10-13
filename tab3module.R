dims<-c("completeness","uniqueness","timeliness","validity","accuracy","consistency")
dimUI<-function(id2){
  ns<-NS(id2)
  tagList(
 
  h3("Completeness"),
  verbatimTextOutput(ns("comp")),
  h3("Uniqueness"),
  verbatimTextOutput(ns("unique")),
  tableOutput(ns("dot")),
  uiOutput(ns("fuzzy"))
  
  )
}
dimServer<-function(input,output,session,data){
  
  vars<-reactive(names(data$mydata()))
  
  
  output$comp<-renderPrint({
    ct<-unlist(map(vars(),~completeness(data$mydata()[[.x]],.x)))
  })
  output$unique<-renderPrint({
    uq<-unlist(map(vars(),~uniqueness(data$mydata()[[.x]],.x)))
  })
 #output$dot<-renderTable({data$mydata()})
  #observe({
 output$fuzzy<-renderUI({
    ns<-session$ns
    get_table_output(data$mydata(),ns)
    })
  
 # })
   
}
