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

##creating ui for tab 3
make_ui<-function(var,ns){
  h3
  verbatimTextOutput(ns(var))
}


reg<-"n/?a|not *available"
completeness<-function(x,var){
  if(is.numeric(x)){
    rng<-range(x,na.rm=TRUE)
    len<-length(x[!is.na(x)])
    prc<-(len/length(x))*100
    
    cat(paste0(var,": has values between ", rng[1], " and ", rng[2],"\n"))
    cat(sprintf("%s has %i non-missing values representing %1.2f%% completeness\n",var,len,prc))

  }else if(is.character(x)){
    len<-length(x[!is.na(x)])
    prc<-(len/length(x))*100
    sus<-str_subset(x,regex(reg,ignore_case = TRUE))
    if(length(sus)>0){
     list(cat(paste0("noteable mention: ",sus,"\n")))
      } else {cat("\n")}
    
    cat(sprintf("%s has %i non-missing values representing %1.2f%% completeness\n",var,len,prc))
    
  } else if(is.factor(x)){
    lev<-levels(x)
    cat(var,": has the following levels: ",lev," \n",sep ="\t")
  }
}
