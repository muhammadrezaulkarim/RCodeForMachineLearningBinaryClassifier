F1Score=function(act,prd){
  #treats the vectors like classes
  #act and prd must be whole numbers
  df=data.frame(act=act,prd=prd);
  scores=list();
  for(i in seq(min(act),max(act))){
    tp=nrow(df[df$prd==i & df$act==i,]);        
    fp=nrow(df[df$prd==i & df$act!=i,]);
    fn=nrow(df[df$prd!=i & df$act==i,]);
    f1=(2*tp)/(2*tp+fp+fn)
    
    if(is.nan(f1)){
      f1<-0;
    }
    
    scores[[i]]=f1;
  }      
  print(scores)
  return(scores);
}
