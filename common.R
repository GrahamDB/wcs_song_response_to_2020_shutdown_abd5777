# common functions


write_aictable <- function(.tab,file){
  stopifnot(inherits(x=.tab,what = "aictab"))
  
  output<- matrix(character(8),ncol=8, nrow=nrow(.tab))
  output[,1] = as.character(.tab$Modnames)
  output[,2] = as.character(.tab$K)
  output[,3] = as.character(round(.tab$AICc,4))
  output[,4] = as.character(round(.tab$Delta_AICc,4))
  output[,5] = as.character(round(.tab$ModelLik,6))
  output[,6] = as.character(round(.tab$AICcWt,6))
  output[,7] = as.character(round(.tab$LL,4))
  output[,8] = as.character(round(.tab$Cum.Wt,6))
  cN=c("Modnames", "K", "AICc", "Delta_AICc", "ModelLik", "AICcWt",  "LL", "Cum.Wt")
  
  if(missing(file)){
    write.table(output,quote=which(cN %in% c("fixed_effect")), sep=",",na="",row.names = F,col.names = cN)
  } else {
    write.table(output,file = file,quote=which(cN %in% c("fixed_effect")), sep=",",na="",row.names = F,col.names = cN)
  }
  
}

write_aictable_form <- function(.mod,file){
  .tab=aictab(.mod)
  
  output<- matrix(character(10),ncol=10, nrow=nrow(.tab))
  output[,1] = ind = as.character(.tab$Modnames)
  output[,2] = sapply(.mod[ind], function(m) format(as.list(m$call)$fixed) )
  output[,3] = sapply(.mod[ind], function(m) format(as.list(m$call)$random) )
  output[,4] = as.character(.tab$K)
  output[,5] = as.character(round(.tab$AICc,4))
  output[,6] = as.character(round(.tab$Delta_AICc,4))
  output[,7] = as.character(round(.tab$ModelLik,6))
  output[,8] = as.character(round(.tab$AICcWt,6))
  output[,9] = as.character(round(.tab$LL,4))
  output[,10] = as.character(round(.tab$Cum.Wt,6))
  cN=c("Modnames","fixed_effect","random_effect", "K", "AICc", "Delta_AICc", "ModelLik", "AICcWt",  "LL", "Cum.Wt")
  
  if(missing(file)){
    write.table(output,quote=which(cN %in% c("fixed_effect","random_effect")), sep=",",na="",row.names = F,col.names = cN)
  } else {
    write.table(output,file = file,quote=which(cN %in%c("fixed_effect","random_effect")), sep=",",na="",row.names = F,col.names = cN)
  }
  
}
