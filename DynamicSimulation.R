
prepare.dy.data <- function(ndata,pdatainicial){
  initial.ndata = round(ndata*pdatainitial)
  
  dy.data = as.matrix(sample(1:ndata,initial.ndata,replace=F))
  return(dy.data)
}



add.object <- function(dy.d,ndata){
  if(ndata==nrow(dy.d)){return(dy.d)}
  indexes.candidates = which(!c(1:ndata) %in% dy.d)
  if(length(indexes.candidates)>=2){
    dy.d = rbind(dy.d,sample(indexes.candidates,1))
  }else{
    dy.d = rbind(dy.d,indexes.candidates)
  }
  return(dy.d)
}


delete.object <- function(dy.d){
  if(nrow(dy.d)==1){return(dy.d)}
  dy.d = as.matrix(dy.d[-sample(1:nrow(dy.d),1),])
  return(dy.d)
}


trigger.change <- function(dy.d,ndata,padd = 0.5,pdelete = 0.5){
  pchange = c(padd,pdelete)
  pchange = pchange/sum(pchange)
  
  type.change = sample(c("add","delete"),1,prob=pchange)
  if(type.change=="add"){
    dy.d = add.object(dy.d,ndata)
  }else{
    if(type.change=="delete"){
      dy.d = delete.object(dy.d)
    }
  }
  return(dy.d)
}





