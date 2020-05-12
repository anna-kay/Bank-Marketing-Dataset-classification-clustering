# Exploring unknown values of the features

unkown_ages_indices<-which(bank$age=="unknown")
num_of_unknown_ages<-length(unkown_ages_indices)


# Unknown education, answer

length(which(bank$education=="unknown" & bank$y=="yes"))
length(which(bank$education=="unknown" & bank$y=="no"))

# Unknown housing + loan

length(which(bank$housing=="unknown" & bank$y=="no"))
length(which(bank$loan=="unknown" & bank$y=="no"))

unknown_loan_no<-which(bank$loan=="unknown" & bank$y=="no")
unknown_housing_no<-which(bank$housing=="unknown" & bank$y=="no")
unknown_housing_no==unknown_loan_no

#-->apo tous unknown housing + unknown loan, to 89,2% arni8ike