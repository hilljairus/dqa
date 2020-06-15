dims<-c("completeness","uniqueness","timeliness","validity","accuracy","consistency")
dimUI<-function(id2){
  ns2<-NS(id2)
  list(
 
  h3("Completeness"),
  verbatimTextOutput(ns2("comp"))
  
  )
}
dimServer<-function(input,output,session,data){
  
  vars<-reactive(names(data$mydata()))
  
  
  output$comp<-renderPrint({
    ct<-unlist(map(vars(),~completeness(data$mydata()[[.x]],.x)))
    
  })
}
