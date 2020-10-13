library(tidyverse)

tablerize<-function(file){
  list=map(file,~class(.x))
  nas<-names(list)
  tab<-data.frame(nas)
  for (i in 1:length(nas)){
    tab[i,2]<-list[[i]]
  }
  colnames(tab)<-c("Variable","Type")
  tab
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

#Function for implementing completeness
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

#Function for implementing Uniqueness
uniqueness<-function(x,var){
  distinct<-length(base::unique(x))
  cat(sprintf("%s has %i unique values \n",var,distinct))
  
}

##Fuzzy Match
fuzzy<-function(x){
  if(is.character(x)){
    names<-tolower(unique(x))
    dfr<-data.frame(n1=names,
                    n2=names)
    ndf<-expand.grid(dfr,stringsAsFactors = FALSE)
    ndf <- ndf[ndf$n1>ndf$n2,]#Remove repeated combinations
    ndf$dist<-stringdist::stringdist(ndf$n1,ndf$n2)
    ndf<-ndf[ndf$dist<=5,] #Filter out combination with stringdist greater than 5
    method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
    for (i in method_list){
      ndf[,i]<-stringdist::stringdist(ndf$n1,ndf$n2,method=i)
    }
    suspicious_match <- ndf[ndf$cosine < 0.20 & ndf$cosine != 0 & ndf$qgram < 10, c(1,2) ]
    suspicious_match
   
  }
}
##Fuzzy Output

get_table_output<-function(data,ns){
  table_output_list<-lapply(names(data),function(x){
    if(is.character(data[[x]])){
      table_object<-tableOutput(ns(x))
      table_object<-renderTable({fuzzy(data[[x]])}) 
    }
  })
  
  do.call(tagList,table_output_list)  
  return(table_output_list)
}