precisionScore=function(act,prd){
  #treats the vectors like classes
  #act and prd must be whole numbers
  df=data.frame(act=act,prd=prd);
  scores=list();
  for(i in seq(min(act),max(act))){
    tp=nrow(df[df$prd==i & df$act==i,]);        
    fp=nrow(df[df$prd==i & df$act!=i,]);
    fn=nrow(df[df$prd!=i & df$act==i,]);
    
    precison=tp/(tp+fp)
    
    if(is.nan(precison)){
      precison<-0;
    }
    
    
    scores[[i]]=precison;
  }      
  print(scores)
  return(scores);
}
