library(Hmisc)

#############################################

bank_new$age<-cut2(bank_new$age, g=5)
summary(bank_new$age)
#############################################

bank_new$duration<-cut2(bank_new$duration, g=5)
summary(bank_new$duration)

##############################################

bank_new$campaign<-cut2(bank_new$campaign, g=5)
summary(bank_new$campaign)

##############################################

bank_new$emp.var.rate<-cut2(bank_new$emp.var.rate, g=5)
summary(bank_new$emp.var.rate)

##############################################

bank_new$cons.price.idx<-cut2(bank_new$cons.price.idx, g=5)
summary(bank_new$cons.price.idx)

##############################################

bank_new$cons.conf.idx<-cut2(bank_new$cons.conf.idx, g=5)
summary(bank_new$cons.conf.idx)

##############################################

bank_new$euribor3m<-cut2(bank_new$euribor3m, g=5)
summary(bank_new$euribor3m)

################################################

bank_new$nr.employed<-cut2(bank_new$nr.employed, g=5)
summary(bank_new$nr.employed, g=5)

################################################
