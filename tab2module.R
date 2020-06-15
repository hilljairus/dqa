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
  ns<-session$ns
  mydf<-reactiveValues(value=NULL)
  observe({mydf$value<-tidied})
  output$sep_by<-renderUI({
    if(input$separate){
      return(fluidRow(column(4,selectInput(ns("sep_col"),"Sep_Col",choices=names(raw()))),
                      column(4, numericInput(ns("n_parts"),"Parts",NULL)),
                      column(4,textInput(ns("sep"),"Separator"))))
    }
    
  })
  
  output$unite_by<-renderUI({unite_ui(ns,input$unite,names(sep_un()))})
  
  raw<-reactive({
    if(input$clean){
      names(tidied)<-janitor::make_clean_names(names(tidied))
    } 
      tidied
    })
  sep_un<-reactive({
    temp<-raw()
    if(input$separate){
      req(input$sep_col)
      req(input$sep)
      req(input$n_parts)
      
      temp<-separate(
        temp,!!input$sep_col,into=paste0("x",seq_len(input$n_parts)),sep=input$sep)
    }
    temp
  })
    
   uno<-reactive({
     tempo<-sep_un()
     if(input$unite){
      req(input$col_unite)
      tempo<-unite(tempo,!!input$new_col,!!input$col_unite)
      
    } 
    tempo
  })
 
 
  output$data<-renderTable(head(uno()))
  
  vars<-reactive(names(uno()))
  output$filters<-renderUI({
    map(vars(),~filter_UI(uno()[[.x]],.x,ns))
  })
  filtered<-reactive({
    selected<-map(vars(),~filter_Server(uno()[[.x]],input[[.x]]))
    reduce(selected,`&`)
  }) 
  output$ftable<-renderTable(head(uno()[filtered(), ]))
  
  return(list(
    mydata=reactive({
      x<-uno()[filtered(),]
      x
      })
    )
    )
  
}
  