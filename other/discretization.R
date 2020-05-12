library(Hmisc)

age_disc<-cut2(bank$age, g=5)
summary(age_disc)
#############################################

duration_disc<-cut2(bank$duration, g=5)
summary(duration_disc)

#############################################
# Proigoumeni epikoinwnia me ton pelati apo auti tin kampania

cases<-c(1:nrow(bank))
summary(cases)

campaign_disc<-c(rep("no", nrow(bank)))

for (count in cases){
    if (bank$campaign[count]!=1){
         campaign_disc[count]="yes"
         } else {
         campaign_disc[count]="no"
         }
}

campaign_disc<-as.factor(campaign_disc)

################################################
# Sunduasmos twn pdays & previous
# Proigoumeni epikoinwnia me ton pelati apo proigoumenes kampanies

cases<-c(1:nrow(bank))
previous_contacts_disc<-c(rep("no", nrow(bank)))

for (count in cases){   
     if(bank$pdays[count]!=999 | bank$previous[count]!=0){
         previous_contacts_disc[count]="yes"
     } else {}
}

previous_contacts_disc<-as.factor(previous_contacts_disc)
summary(previous_contacts_disc)

##############################################
# Aplopoiisi poutcome

poutcome_disc<-c(rep("not_success", nrow(bank)))

for (count in cases){
     if(bank$poutcome[count]=="success"){
         poutcome_disc[count]="success"
     }
}

poutcome_disc=as.factor(poutcome_disc)

##############################################

emp.var.rate_disc<-cut2(bank$emp.var.rate, g=5)
summary(emp.var.rate_disc)

##############################################

cons.price.idx_disc<-cut2(bank$cons.price.idx, g=5)
summary(cons.price.idx_disc)

##############################################

cons.conf.idx_disc<-cut2(bank$cons.conf.idx, g=5)
summary(cons.conf.idx_disc)

##############################################

euribor3m_disc<-cut2(bank$euribor3m, g=5)
summary(euribor3m_disc)

################################################

nr.employed_disc<-cut2(bank$nr.employed, g=5)
summary(nr.employed_disc)

################################################

job=bank$job 
marital=bank$marital 
education=bank$education 
default=bank$default 
housing=bank$housing 
loan=bank$loan 
contact=bank$contact 
month=bank$month 
day_of_week=bank$day_of_week 
y=bank$y



bank_disc<-cbind.data.frame(age_disc,
                            job,
                            marital,
                            education,
                            default,
                            housing,
                            loan,
                            contact,
                            month,
                            day_of_week,
                            duration_disc, 
                            campaign_disc, 
                            previous_contacts_disc, 
                            poutcome_disc, 
                            emp.var.rate_disc, 
                            cons.price.idx_disc, 
                            cons.conf.idx_disc, 
                            euribor3m_disc, 
                            nr.employed_disc,
                            y)