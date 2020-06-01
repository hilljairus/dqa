library(tidyverse)
preview_coltype<-function(file){
 print(as.data.frame(sapply(file,class)))
}

#Function to make filter UI for module1--Add date input ui later
filter_UI<-function(x,var,ns){
  if(is.numeric(x)){
    rng<-range(x,na.rm=TRUE)
    sliderInput(ns(var),var,min=rng[1],max=rng[2], value = rng)
  }else if(is.factor(x)){
    levs<-levels(x)
    selectInput(ns(var),var,choices=levs,selected =levs,multiple =TRUE )
  } else {NULL}
}

filter_Server<-function(x,val){
  if(is.numeric(x)){
    !is.na(x) & x>=val[1] & x<=val[2]
  }else if(is.factor(x)){
    x %in% val
  } else {TRUE}
}

## Unite UI function for module 1
unite_ui<-function(name_space,unite_cond,var){
  if(unite_cond){
      fluidRow(
            column(6,selectInput(name_space("col_unite"),"Unite?",var,multiple = TRUE)),
            column(6,textInput(name_space("new_col"),"New Col_name"))
                 )}
                 
}


