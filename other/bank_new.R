rm(list=ls())

bank<-read.csv("bank-additional-full.csv", sep=";")
bank$y<-relevel(bank$y, "yes")

cases<-c(1:nrow(bank))

#-------------------------------------------------------------#
# Sunduasmos metavlitwn pdays kai previous

#previous->was there any previous contact

previous_new<-c(rep("no", nrow(bank)))
for (count in cases){   
     if(bank$pdays[count]!=999 | bank$previous[count]!=0){
         previous_new[count]="yes"
     } else {}
}

previous_new<-as.factor(previous_new)
#-------------------------------------------------------------#

#-------------------------------------------------------------#
# Aplopoiisi metavlitis poutcome

poutcome_new<-c(rep("not_success", nrow(bank)))

for (count in cases){
  if(bank$poutcome[count]=="success"){
    poutcome_new[count]="success"
  }
}

poutcome_new=as.factor(poutcome_new)
#-------------------------------------------------------------#


bank_new<-bank
bank_new$previous<-previous_new
bank_new$poutcome<-poutcome_new

bank_new<-bank_new[, -13]
