library(shiny)
library(janitor)


#module1 for 2nd tab
clean_filterUI<-function(id1){
  ns<-NS(id1)
  list(sidebarLayout(
    sidebarPanel(
      checkboxInput(ns("clean"),"Clean Names"),
      checkboxInput(ns("separate"),"Separate",FALSE),
     uiOutput(ns("sep_by")),
      
     checkboxInput(ns("unite"),"Unite",FALSE),
     uiOutput(ns("unite_by"))
    
    ),
    
    mainPanel(h3("Preview"),
              tableOutput(ns("data"))
     )
    
    ),
    sidebarLayout(
      sidebarPanel(
        h3("FILTER"),
        uiOutput(ns("filters"))),
      mainPanel(tableOutput(ns("ftable")))
      )
  )
  
}
clean_filterServer<-function(input,output,session,tidied){
  raw<-reactive({
    
    if(input$clean){
      names(tidied)<-janitor::make_clean_names(names(tidied))
    }
    if(input$separate){
      req(input$sep_col)
      req(input$sep)
      req(input$n_parts)
      
      tidied<-separate(
        tidied,!!input$sep_col,into=paste0("x",seq_len(input$n_parts)),sep=input$sep)
      
    }
    if(input$unite){
      req(input$col_unite)
      
      tidied<-unite(tidied,!!input$new_col,!!input$col_unite)
      
    }
    
    tidied
  })
  ns<-session$ns
  #var1<-reactive(names(raw()))
  output$sep_by<-renderUI({
    if(input$separate){
      return(fluidRow(column(4,selectInput(ns("sep_col"),"Sep_Col",choices=names(raw()))),
               column(4, numericInput(ns("n_parts"),"Parts",NULL)),
                column(4,textInput(ns("sep"),"Separator"))))
    }
    
  })
  output$unite_by<-renderUI({unite_ui(ns,input$unite,names(raw()))})
  
    sep_union<-reactive({
    
    
    if(input$separate){
      req(input$sep_col)
      req(input$sep)
      req(input$n_parts)
      
     raw()<-separate(
     raw(),!!input$sep_col,into=paste0("x",seq_len(input$n_parts)),sep=input$sep)
     
    }
    if(input$unite){
      req(input$col_unite)
      
     raw()<-unite(raw(),!!input$new_col,!!input$col_unite)
     
    }
    raw()
      
  })
    output$data<-renderTable(head(raw()))
    
    vars<-reactive(names(raw()))
   output$filters<-renderUI({
      map(vars(),~filter_UI(raw()[[.x]],.x))
    })
   filtered<-reactive({
    selected<-sep_union()
     map(vars(),~filter_Server(selected[[.x]],input[[.x]],selected))
    })
    output$ftable<-renderTable(filtered())
}
  






