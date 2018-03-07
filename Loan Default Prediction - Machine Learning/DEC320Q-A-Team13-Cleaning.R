rm(list = ls())

load("loan.rda")

auto_drops<- NULL
for (i in 1:length(DT)){
  
  if (sum(is.na(DT[,i]))>80000){
    
    auto_drops<-c(auto_drops,names(DT[i]))
  }
}
auto_drops


auto_drops1<- NULL
for (i in 1:length(DT)){
  
  if (sum(is.na(DT[(DT[,"loan_status"]=="Default"),i]))==1219){
    
    auto_drops1<-c(auto_drops1,names(DT[i]))
  }
}




auto<-unique(append(auto_drops,auto_drops1))
DT<-DT[,!(names(DT) %in% auto)]

manu_drops<-c("url","desc","emp_title","policy_code", "title","zip_code","tot_coll_amt","total_rev_hi_lim","tot_cur_bal","revol_util","verification_status_joint")
DT<-DT[,!(names(DT) %in% manu_drops)]



m<-mean(DT$annual_inc,na.rm=T)
temp<-is.na(DT[,"annual_inc"])
DT[temp,]$annual_inc <- m



bool<-rep(T,times=nrow(DT))
for (i in 1:length(DT)){
  if(sum(is.na(DT[,i]))>=20){
    bool<-bool & is.na(DT[,i])
  }
}

DT<-DT[!bool,]


temp<-is.na(DT[,"collections_12_mths_ex_med"])
DT[temp,]$collections_12_mths_ex_med <- 0



DT$loan_status<-ifelse((DT$loan_status=="Charged Off" | DT$loan_status=="Default"), 1, 0 )

rm<-cbind("id","member_id","pymnt_plan","sub_grade","last_pymnt_d","next_pymnt_d","last_credit_pull_d","earliest_cr_line","addr_state")
new_DT<-DT[,!colnames(DT) %in% rm]

colnames(new_DT)[which(names(new_DT) == "loan_status")] <- "default"
new_DT<-new_DT[!new_DT[,"emp_length"]=="n/a",]
new_DT<-new_DT[,!colnames(new_DT) %in% "issue_d"]


rm(list=setdiff(ls(), "new_DT"))


save(new_DT,file="cleaned2.rda")



