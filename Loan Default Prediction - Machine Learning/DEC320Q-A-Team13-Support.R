

#Version 1.4

scan<-function(mat){
  n<-ncol(mat)
  result<-NULL
  result2<-NULL
  dummy<-0
  for (i in 1:n){
    if (class(mat[,i])!="numeric" | all(unique(mat[,i]) %in% c(0,1)) ){
      result<-rbind(result,cbind(names(mat)[i],nlevels(factor(mat[,i])),sum(is.na(mat[,i])),sum(grepl("^\\s*$", mat[,i]))))
      dummy<-dummy+as.numeric(nlevels(factor(mat[,i])))
      
    }
    else{
      result2<-rbind(result2,cbind(names(mat)[i],t(round(as.matrix(summary(mat[,i])),0)),sum(mat[,i]==0),sum(is.na(mat[,i])),sum(grepl("^\\s*$", mat[,i]))))
    }
    
  }
  result<-data.frame(result)
  result2<-data.frame(result2)
    colnames(result)<-c("Variable","Levels","Null","Empty")
  colnames(result2)<-c("Variable","Min","1st.Quartile","Median","Mean","3rd.Quartile","Max","Zeros", "Null", "Empty")

  cat("\n")
  cat(paste("Total number of record is",nrow(mat)))
  cat("\n")
  cat(paste("Total number of categorical variables is",nrow(result),".","Number of dummies are",dummy))
  cat("\n")
  cat("\n")
  print(result,right=F)
  cat("\n")
  cat("\n")
  cat(paste("Total number of numeric variables is",ncol(mat)-nrow(result)))
  cat("\n")
  cat("\n")
  print(result2,right=F)
}

detail<-function(mat){
  n<-ncol(mat)
  for (i in 1:n){
    if (class(mat[,i])!="numeric" | all(unique(mat[,i]) %in% c(0,1)) ){
      cat(colnames(mat)[i])
      cat("\n")
      cat("\n")
      print(t(as.matrix(summary(factor(mat[,i])))),right=F)
      cat("--------------------------------------------------------------------------------------------")
      cat("\n")
    }
  }
  
  
}
