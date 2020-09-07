library(readr)
train_u6lujuX_CVtuZ9i_1_ <- read_csv("D:/MS/documents/NEU Course/Data mining/train_u6lujuX_CVtuZ9i(1).csv")

loan_prediction<- train_u6lujuX_CVtuZ9i_1_

library(dplyr)
summary(loan_prediction)

head(loan_prediction)

#initial na counts
sum(is.na(loan_prediction))
sum(is.na(loan_prediction$Loan_ID))
sum(is.na(loan_prediction$Gender))
sum(is.na(loan_prediction$Married))
sum(is.na(loan_prediction$Dependents))
sum(is.na(loan_prediction$Education))
sum(is.na(loan_prediction$Self_Employed))
sum(is.na(loan_prediction$ApplicantIncome))
sum(is.na(loan_prediction$CoapplicantIncome))
sum(is.na(loan_prediction$LoanAmount))
sum(is.na(loan_prediction$Loan_Amount_Term))
sum(is.na(loan_prediction$Credit_History))
sum(is.na(loan_prediction$Property_Area))
sum(is.na(loan_prediction$Loan_Status))

str(loan_prediction)
library(tidyverse)

#Since credtit history has more na, first let solve na for this
#loan_prediction1 is a filtered dataset which doesn't contain rows with credit history as NA
loan_prediction1<-loan_prediction %>% filter(Credit_History == 0 | Credit_History==1)
loan_prediction1$Credit_History<- as.factor(loan_prediction1$Credit_History)


library(ggplot2)
#finding relation between credit history and loan status
table(loan_prediction1$Credit_History, loan_prediction1$Loan_Status)
#Percentage of 1 in Yes category
378/(378+7)
#percentage of 0 in No category
82/(97+82)

#replacing NA with 1 for credit history where loan status = yes 
loan_prediction$Credit_History[is.na(loan_prediction$Credit_History)==T & loan_prediction$Loan_Status=="Y"]<-1 

#now total na is reduced to 112
sum(is.na(loan_prediction))


#finding the relationship between dependents and married 
str(loan_prediction$Dependents)
loan_prediction$Dependents<-as.factor(loan_prediction$Dependents)
table(loan_prediction$Dependents, loan_prediction$Married, loan_prediction$Gender)

dependents<- loan_prediction %>% filter(is.na(Dependents)==T) 

# the married female has a median 
median(c(rep(0,20),rep(1,6), rep(2,5), rep(3,0)))

# the unmarried female has a median 
median(c(rep(0,60),rep(1,13), rep(2,2), rep(3,2)))

# the married male has a median 
median(c(rep(0,147),rep(1,70), rep(2,86), rep(3,42)))

# the unmarried male has a median 
median(c(rep(0,108),rep(1,10), rep(2,6), rep(3,3)))

#replacing NA with 1 for married male for dependents otherwise 0 
loan_prediction$Dependents[is.na(loan_prediction$Dependents)==T & loan_prediction$Gender=="Male" & loan_prediction$Married=="Yes"]<-1 
loan_prediction$Dependents[is.na(loan_prediction$Dependents)==T & loan_prediction$Gender=="Male" & loan_prediction$Married=="No"]<-0
loan_prediction$Dependents[is.na(loan_prediction$Dependents)==T & loan_prediction$Gender=="Female"]<-0

#--------------------------------------------------------------------

#after second stage cleaning na counts
sum(is.na(loan_prediction))
sum(is.na(loan_prediction$Loan_ID))
sum(is.na(loan_prediction$Gender))
sum(is.na(loan_prediction$Married))
sum(is.na(loan_prediction$Dependents))
sum(is.na(loan_prediction$Education))
sum(is.na(loan_prediction$Self_Employed))
sum(is.na(loan_prediction$ApplicantIncome))
sum(is.na(loan_prediction$CoapplicantIncome))
sum(is.na(loan_prediction$LoanAmount))
sum(is.na(loan_prediction$Loan_Amount_Term))
sum(is.na(loan_prediction$Credit_History))
sum(is.na(loan_prediction$Property_Area))
sum(is.na(loan_prediction$Loan_Status))

#finding pattern of self employed with applicant income
ggplot()+geom_boxplot(aes(x=loan_prediction$Self_Employed, y=loan_prediction$ApplicantIncome) )+xlab("Self employed")+ylab("Applicant income")
#since NA and Not self employed have same applicant income pattern, we assign NA as No
loan_prediction$Self_Employed[is.na(loan_prediction$Self_Employed)==T]<-"No"

#after third stage cleaning na counts
sum(is.na(loan_prediction))
sum(is.na(loan_prediction$Loan_ID))
sum(is.na(loan_prediction$Gender))
sum(is.na(loan_prediction$Married))
sum(is.na(loan_prediction$Dependents))
sum(is.na(loan_prediction$Education))
sum(is.na(loan_prediction$Self_Employed))
sum(is.na(loan_prediction$ApplicantIncome))
sum(is.na(loan_prediction$CoapplicantIncome))
sum(is.na(loan_prediction$LoanAmount))
sum(is.na(loan_prediction$Loan_Amount_Term))
sum(is.na(loan_prediction$Credit_History))
sum(is.na(loan_prediction$Property_Area))
sum(is.na(loan_prediction$Loan_Status))


#We find 2 NA element has income above 20000 where only male has this amount.
ggplot()+geom_boxplot(aes(x=loan_prediction$Gender, y=loan_prediction$ApplicantIncome) )+xlab("Gender")+ylab("Applicant income")+theme(panel.background = element_rect(fill="lightblue"))

#so replacing with male
loan_prediction$Gender[is.na(loan_prediction$Gender)==T & loan_prediction$ApplicantIncome >20000] <- "Male"

#We could see dependents of 2 and 3+ is very rare in female. 
table(loan_prediction$Gender, loan_prediction$Dependents)

#So we assign male for 2 and 3+ dependents
loan_prediction$Gender[is.na(loan_prediction$Gender)==T & (loan_prediction$Dependents == "2" | loan_prediction$Dependents == "3+")] <- "Male"

#filtering the dataset with education graduated since we have only graduates in the gender dataset
gender_update<- loan_prediction %>% filter(Education=="Graduate" & Dependents==1)

#We found the dependent 1 and being graduate, the income lies close to IQR of Female
ggplot()+geom_boxplot(aes(x=gender_update$Gender, y=gender_update$ApplicantIncome) )

#So we assign female for 1 dependent
loan_prediction$Gender[is.na(loan_prediction$Gender)==T & (loan_prediction$Dependents == "1")] <- "Female"

M <-loan_prediction %>% filter(Gender=="Male") 
summary(M$ApplicantIncome)

F <-loan_prediction %>% filter(Gender=="Female") 
summary(F$ApplicantIncome)

#We could infer 4750 income lie within IQR of male than female
#So we assign male for 1 dependent
loan_prediction$Gender[is.na(loan_prediction$Gender)==T & (loan_prediction$ApplicantIncome == 4750)] <- "Male"

#filtering with unchanged variables
gender_update<- loan_prediction %>% filter(Education=="Graduate" & Dependents=="0" & Loan_Amount_Term==360)

ggplot()+geom_boxplot(aes(x=gender_update$Gender, y=gender_update$LoanAmount) )+xlab("Gender")+ylab("Loan amount")+theme(panel.background = element_rect(fill="purple"), panel.grid.major = element_line(colour = "white"))


M1 <- gender_update %>% filter(Gender=="Male")
summary(M1$LoanAmount)

F1 <- gender_update %>% filter(Gender=="Female")
summary(F1$LoanAmount)

#based on the IQR of both male and female loan amount, we assign 1 to female
loan_prediction$Gender[is.na(loan_prediction$Gender)==T & (loan_prediction$LoanAmount == 96)] <- "Female"
loan_prediction$Gender[is.na(loan_prediction$Gender)==T ] <- "Male"

#after fourth stage cleaning na counts
sum(is.na(loan_prediction))
sum(is.na(loan_prediction$Loan_ID))
sum(is.na(loan_prediction$Gender))
sum(is.na(loan_prediction$Married))
sum(is.na(loan_prediction$Dependents))
sum(is.na(loan_prediction$Education))
sum(is.na(loan_prediction$Self_Employed))
sum(is.na(loan_prediction$ApplicantIncome))
sum(is.na(loan_prediction$CoapplicantIncome))
sum(is.na(loan_prediction$LoanAmount))
sum(is.na(loan_prediction$Loan_Amount_Term))
sum(is.na(loan_prediction$Credit_History))
sum(is.na(loan_prediction$Property_Area))
sum(is.na(loan_prediction$Loan_Status))


#We could find only graduate occur above loan_income above 20000, 
ggplot(data=loan_prediction)+geom_point(aes(x=ApplicantIncome, y=LoanAmount, colour=Education ))

#Filtering above 20000 to find the middle value of loan amount
loan_amount_appl_high20k<- loan_prediction %>% filter(ApplicantIncome > 20000)

#median= 480 and mean=423.8
summary(loan_amount_appl_high20k$LoanAmount)

#seeing characteristics of loan_amount with na
loan_amount_na<- loan_prediction %>% filter(is.na(loan_prediction$LoanAmount)==T)

#Assigning loan_amount with applicant income greater than 20000 as 450
loan_prediction$LoanAmount[is.na(loan_prediction$LoanAmount)==T & loan_prediction$ApplicantIncome==20667]<-450

#Infering the relation of loanamount and applicant income 
ggplot(data=loan_prediction)+geom_point(aes(x=ApplicantIncome, y=LoanAmount, colour=Education, shape=Self_Employed ))+facet_grid(.~Property_Area)+xlim(0,14000)+ylim(0,300)+
  theme(panel.background = element_rect(fill="paleturquoise2"), panel.grid.major = element_line(colour = "grey93"), panel.grid.minor = element_line(colour="grey93"))


#Assigning loan_amount with applicant income greater than 20000 as 450
loan_prediction$LoanAmount[is.na(loan_prediction$LoanAmount)==T & loan_prediction$ApplicantIncome==20667]<-450

#From the plot we are assigning loan_amount to NAs
loan_prediction$LoanAmount[is.na(loan_prediction$LoanAmount)==T & (loan_prediction$ApplicantIncome==5849 | loan_prediction$ApplicantIncome==4945 | loan_prediction$ApplicantIncome==2395 | loan_prediction$ApplicantIncome==4652 | loan_prediction$ApplicantIncome== 4680) ]<-150
loan_prediction$LoanAmount[is.na(loan_prediction$LoanAmount)==T & (loan_prediction$ApplicantIncome==3010 | loan_prediction$ApplicantIncome==2213 | loan_prediction$ApplicantIncome== 2275) ]<-80
loan_prediction$LoanAmount[is.na(loan_prediction$LoanAmount)==T & (loan_prediction$ApplicantIncome==1830 | loan_prediction$ApplicantIncome==2000 | loan_prediction$ApplicantIncome== 2947) ]<-100
loan_prediction$LoanAmount[is.na(loan_prediction$LoanAmount)==T & (loan_prediction$ApplicantIncome==6633 | loan_prediction$ApplicantIncome==6782)] <-170
loan_prediction$LoanAmount[is.na(loan_prediction$LoanAmount)==T & (loan_prediction$ApplicantIncome==7451 | loan_prediction$ApplicantIncome==3865 | loan_prediction$ApplicantIncome==3992 | loan_prediction$ApplicantIncome==3601 | loan_prediction$ApplicantIncome== 2492 | loan_prediction$ApplicantIncome==2400) ]<-125
loan_prediction$LoanAmount[is.na(loan_prediction$LoanAmount)==T & loan_prediction$ApplicantIncome==13650 ]<-50
loan_prediction$LoanAmount[is.na(loan_prediction$LoanAmount)==T & loan_prediction$ApplicantIncome==10047 ]<-230


#after fifth stage cleaning na counts
sum(is.na(loan_prediction))
sum(is.na(loan_prediction$Loan_ID))
sum(is.na(loan_prediction$Gender))
sum(is.na(loan_prediction$Married))
sum(is.na(loan_prediction$Dependents))
sum(is.na(loan_prediction$Education))
sum(is.na(loan_prediction$Self_Employed))
sum(is.na(loan_prediction$ApplicantIncome))
sum(is.na(loan_prediction$CoapplicantIncome))
sum(is.na(loan_prediction$LoanAmount))
sum(is.na(loan_prediction$Loan_Amount_Term))
sum(is.na(loan_prediction$Credit_History))
sum(is.na(loan_prediction$Property_Area))
sum(is.na(loan_prediction$Loan_Status))


#Loan_term has dataset with loan_term as NA
Loan_term<-loan_prediction %>% filter(is.na(Loan_Amount_Term)==T)


unique(loan_prediction$Loan_Amount_Term)

#relation of loan_prediction and loan term with education and gender as consideration
ggplot(data=loan_prediction)+geom_point(aes(x=Loan_Amount_Term, y=LoanAmount, colour=Gender, shape=Education) )+ylim(75,200)+xlab("Loan Amount Term")+
  theme(panel.background = element_rect(fill="gray97"), panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour="white"))

#assigning values
loan_prediction$Loan_Amount_Term[is.na(loan_prediction$Loan_Amount_Term)==T & loan_prediction$LoanAmount==96]<-480
loan_prediction$Loan_Amount_Term[is.na(loan_prediction$Loan_Amount_Term)==T ]<-360

#after sixth stage cleaning na counts
sum(is.na(loan_prediction))
sum(is.na(loan_prediction$Loan_ID))
sum(is.na(loan_prediction$Gender))
sum(is.na(loan_prediction$Married))
sum(is.na(loan_prediction$Dependents))
sum(is.na(loan_prediction$Education))
sum(is.na(loan_prediction$Self_Employed))
sum(is.na(loan_prediction$ApplicantIncome))
sum(is.na(loan_prediction$CoapplicantIncome))
sum(is.na(loan_prediction$LoanAmount))
sum(is.na(loan_prediction$Loan_Amount_Term))
sum(is.na(loan_prediction$Credit_History))
sum(is.na(loan_prediction$Property_Area))
sum(is.na(loan_prediction$Loan_Status))


#loan_credit is dataset with credit as NA
loan_credit<-loan_prediction %>% filter(is.na(Credit_History)==T)

#Filtering the dataset with common characteristics of loan_credit to analyse the credit history
loan_update_credit<-loan_prediction %>% filter(is.na(Credit_History) !=T &  Gender=="Male" & Loan_Status=="N" & Loan_Amount_Term==360 & Property_Area !="Urban" )

#determinig the relation of credit history from loan amount vs applicant income 
ggplot(data=loan_update_credit)+geom_point(aes(x=ApplicantIncome, y=LoanAmount, colour=Credit_History))+xlim(1000,11500)+ylim(0,300)+
  theme(panel.background = element_rect(fill="white"), panel.grid.major = element_line(colour = "grey93"), panel.grid.minor = element_line(colour="grey93"))

#assigning values
loan_prediction$Credit_History[is.na(loan_prediction$Credit_History)==T & (loan_prediction$LoanAmount==128 | loan_prediction$LoanAmount==190 | loan_prediction$LoanAmount==196) ]<-0
loan_prediction$Credit_History[is.na(loan_prediction$Credit_History)==T ]<-1

#after final stage cleaning na counts
sum(is.na(loan_prediction))
sum(is.na(loan_prediction$Loan_ID))
sum(is.na(loan_prediction$Gender))
sum(is.na(loan_prediction$Married))
sum(is.na(loan_prediction$Dependents))
sum(is.na(loan_prediction$Education))
sum(is.na(loan_prediction$Self_Employed))
sum(is.na(loan_prediction$ApplicantIncome))
sum(is.na(loan_prediction$CoapplicantIncome))
sum(is.na(loan_prediction$LoanAmount))
sum(is.na(loan_prediction$Loan_Amount_Term))
sum(is.na(loan_prediction$Credit_History))
sum(is.na(loan_prediction$Property_Area))
sum(is.na(loan_prediction$Loan_Status))


loan_married<-loan_prediction %>% filter(is.na(Married)==T)

#Finding relation of married with loan amount and income
ggplot(data=loan_prediction)+geom_point(aes(x=LoanAmount, y=ApplicantIncome, color=Married ))+xlim(150,250)+ylim(3000,11000)+
  theme(panel.background = element_rect(fill="white"), panel.grid.major = element_line(colour = "gray93"), panel.grid.minor = element_line(colour="gray97"))

#From the plot we could determine the relation of married based on their income and loan amount
loan_prediction$Married[is.na(loan_prediction$Married)==T & loan_prediction$Gender=="Male" ]<-'Yes'
loan_prediction$Married[is.na(loan_prediction$Married)==T & loan_prediction$Gender=="Female" ]<-'No'

sum(is.na(loan_prediction))
sum(is.na(loan_prediction$Loan_ID))
sum(is.na(loan_prediction$Gender))
sum(is.na(loan_prediction$Married))
sum(is.na(loan_prediction$Dependents))
sum(is.na(loan_prediction$Education))
sum(is.na(loan_prediction$Self_Employed))
sum(is.na(loan_prediction$ApplicantIncome))
sum(is.na(loan_prediction$CoapplicantIncome))
sum(is.na(loan_prediction$LoanAmount))
sum(is.na(loan_prediction$Loan_Amount_Term))
sum(is.na(loan_prediction$Credit_History))
sum(is.na(loan_prediction$Property_Area))
sum(is.na(loan_prediction$Loan_Status))

#replacing NA with 1 for married male for dependents otherwise 0 
loan_prediction$Dependents[is.na(loan_prediction$Dependents)==T & loan_prediction$Gender=="Male" & loan_prediction$Married=="Yes"]<-1 
loan_prediction$Dependents[is.na(loan_prediction$Dependents)==T & loan_prediction$Gender=="Male" & loan_prediction$Married=="No"]<-0
loan_prediction$Dependents[is.na(loan_prediction$Dependents)==T & loan_prediction$Gender=="Female"]<-0

sum(is.na(loan_prediction))
sum(is.na(loan_prediction$Loan_ID))
sum(is.na(loan_prediction$Gender))
sum(is.na(loan_prediction$Married))
sum(is.na(loan_prediction$Dependents))
sum(is.na(loan_prediction$Education))
sum(is.na(loan_prediction$Self_Employed))
sum(is.na(loan_prediction$ApplicantIncome))
sum(is.na(loan_prediction$CoapplicantIncome))
sum(is.na(loan_prediction$LoanAmount))
sum(is.na(loan_prediction$Loan_Amount_Term))
sum(is.na(loan_prediction$Credit_History))
sum(is.na(loan_prediction$Property_Area))
sum(is.na(loan_prediction$Loan_Status))

summary(loan_prediction)

#Analysing the outlier in applicant income, coapplicant income, Loan amount
ggplot()+geom_boxplot(aes(x='',y=loan_prediction$ApplicantIncome ))+xlab("Applicant income")+ylab("Applicant income")+ggtitle("Box plot of Applicant income")
ggplot()+geom_boxplot(aes(x='',y=loan_prediction$CoapplicantIncome))+xlab("Coapplicant income")+ylab("Coapplicant income")+ggtitle("Box plot of Coapplicant income")
ggplot()+geom_boxplot(aes(x='',y=loan_prediction$LoanAmount))+xlab("Loan Amount")+ylab("Loan Amount")+ggtitle("Box plot of Loan Amount")

#Additing applicant and coapplicant income together
loan_prediction$total_income<- loan_prediction$ApplicantIncome+ loan_prediction$CoapplicantIncome

ggplot()+geom_point(aes(x=loan_prediction$total_income, y=loan_prediction$LoanAmount, color=loan_prediction$Loan_Status))+xlim(0,20000)


#correlation of numerical variables
cor(loan_prediction[,7:9])
#We cannot apply PCA since only 2 variables are correlated to an extent.

# 1) Does the marital status of the customer impact his/her loan eligibility?

table(loan_prediction$Married, loan_prediction$Loan_Status)

#Percentage of married people who are loan approved 
287/ (287+113)
#So we can find 71% of the married people are approved loan.

#Percentage of unmarried people who are loan approved 
135/ (135+79)
#So we can find 63% of the unmarried people are approved loan.
#We can infer that married people have slightly higher chance of getting loan approved, but not to a greater extent.

# 2) Does the gender of the customer influence his/her loan eligibility?

table(loan_prediction$Gender, loan_prediction$Loan_Status)

#Percentage of Male who are loan approved 
346/ (346+154)
#So we can find 69% of the male are approved loan.

#Percentage of female who are loan approved 
76/ (76+38)
#So we can find 66% of the female are approved loan.
#We can infer that male have slightly higher chance of getting loan approved, but not to a greater extent.

# 3) Does the number of dependents have an impact on one's loan eligibility?

table(loan_prediction$Dependents, loan_prediction$Loan_Status)

#Percentage of 0 dependent who are loan approved 
241/ (109+241)
#So we can find 69% of the people who have 0 dependent are approved loan.

#Percentage of people who have 1 dependent are loan approved 
72/ (72+40)
#So we can find 64% of the people who have 1 dependent are approved loan.

#Percentage of 2 dependent who are loan approved 
76/ (76+25)
#So we can find 75% of the people who have 2 dependent are approved loan.

#Percentage of 3 dependent who are loan approved 
33/ (33+18)
#So we can find 64% of the people who have 3 dependent are approved loan.
#We can infer that people who have 2 dependents have higher chance of getting loan approved than other people.

# 4) Is an educated customer more likely to be eligible for a loan?


table(loan_prediction$Education, loan_prediction$Loan_Status)

#Percentage of graduate who are loan approved 
340/ (340+140)
#So we can find 71% of the graduate are approved loan.

#Percentage of ungradauted who are loan approved 
82/ (82+52)
#So we can find 61% of the ungraduated people are approved loan.
#We can infer graduated poeple have higher chance of getting loan approved.

# 5) Does the income of the customer play a major role in his/her loan eligibility?

ggplot()+geom_histogram(aes(x=loan_prediction$ApplicantIncome, fill=loan_prediction$Loan_Status))+xlim(0,20000)+xlab("Applicant Income")+ggtitle("Applicant income distribution based on Loan status")+labs(fill="Loan status")+
  theme(panel.background = element_rect(fill="grey93"), panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour="white"))

#We could infer that as the loan applicant income increases the percentage of loan approved varies.

# 6) Does being self-employed decrease the chances of being eligible for a loan?

table(loan_prediction$Self_Employed, loan_prediction$Loan_Status)

#Percentage of self_employed whose are loan approved 
366/ (366+166)
#So we can find 69% of the self_employed are approved loan.

#Percentage of not self_employed whose are loan approved 
56/ (56+26)
#So we can find 68% of the not self_employed are approved loan.

# 7) Does the co-applicant's income augment the chance of being eligible for a loan?

ggplot()+geom_histogram(aes(x=loan_prediction$CoapplicantIncome, fill=loan_prediction$Loan_Status))+xlim(0,7500)+ylim(0,75)+xlab("Coapplicant Income")+ggtitle("Coapplicant income distribution based on Loan status")+labs(fill="Loan status")+
  theme(panel.background = element_rect(fill="grey93"), panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour="white"))
#We could infer that as the loan applicant income increases the percentage of loan approved varies.

#8) Is a customer more likely to be eligible for a loan, when he requests for a small loan amount?

ggplot()+geom_histogram(aes(x=loan_prediction$LoanAmount, fill=loan_prediction$Loan_Status))+xlab("Loan Amount")+ggtitle("Loan Amount distribution based on Loan status")+labs(fill="Loan status")+
  theme(panel.background = element_rect(fill="grey93"), panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour="white"))

#Yes, we could infer that till loan amount of 75, loan rejected status is less.

#9) Does the loan repayment term influence the applicant's loan eligibility?

table(loan_prediction$Loan_Amount_Term, loan_prediction$Loan_Status)

#We could find loan status is approved more for 180,360 terms

#10) Do customers with high repaying capacity (high income) not eligible for big loan amounts?

ggplot()+geom_point(aes(x=loan_prediction$ApplicantIncome, y=loan_prediction$LoanAmount, color=loan_prediction$Loan_Status))+xlim(0,20000)+xlab("Applicant income")+ylab("Coapplicant income")+ggtitle("Applicant income vs Loan Amount")+labs(color="Loan status")

#We could infer that there is a higher chance for people with high income to get loan approved for high loan amount

# 11)Does the customer's credit history have any impact on his/her loan eligibility?

table(loan_prediction$Loan_Status, loan_prediction$Credit_History)

#Percentage of people with credit history 1 whose loan are approved 
455/(455+107)
#So we can find 81% people with credit history 1 are loan approved 

#Percentage of people with credit history 0 whose loan are approved 
7/(85+7)
#So we can find 8% people with credit history 0 are loan approved 

#We can find people who have credit history 1 have a higher chance of getting loan approved.

# 12)Does the location of the customer's property impact his/her loan eligibility?

table(loan_prediction$Property_Area, loan_prediction$Loan_Status)

#Percentage of chance of loan approval for people who live in rural area 

110/(110+69)
# 61% of rural people can get loan approval

#Percentage of chance of loan approval for people who live in Semiurban area 

179/(179+54)
# 77% of semiurban people can get loan approval

#Percentage of chance of loan approval for people who live in urban area 

133/(133+69)
# 66% of semiurban people can get loan approval

#We could find semiurban people have higher chance of loan approval than other two.

str(loan_prediction)
summary(loan_prediction)
loan_prediction$Gender <-factor( loan_prediction$Gender, levels= c('Male','Female'), labels= c(1,0))

loan_prediction$Married <-factor( loan_prediction$Married, levels= c('Yes','No'), labels= c(1,0))

loan_prediction$Dependents <-factor( loan_prediction$Dependents, levels= c('0','1','2','3+'), labels= c(0,1,2,3))

loan_prediction$Education <-factor( loan_prediction$Education, levels= c('Graduate','Not Graduate'), labels= c(1,0))

loan_prediction$Self_Employed <-factor( loan_prediction$Self_Employed, levels= c('Yes','No'), labels= c(1,0))

loan_prediction$Property_Area <-factor( loan_prediction$Property_Area, levels= c('Rural','Semiurban','Urban'), labels= c(1,2,3))

loan_prediction$Loan_Status <-factor( loan_prediction$Loan_Status, levels= c('Y','N'), labels= c(1,0))


loan_prediction<-loan_prediction[,-1]
loan_prediction<-loan_prediction[,-13]

str(loan_prediction)
loan_prediction$Credit_History<-factor(loan_prediction$Credit_History)
loan_prediction$Loan_Amount_Term<-factor(loan_prediction$Loan_Amount_Term)

ggplot()+geom_boxplot(aes(x='',y=loan_prediction$ApplicantIncome ))+xlab("Applicant income")+ylab("Applicant income")+ggtitle("Box plot of Applicant income")
summary(loan_prediction$LoanAmount)

ggplot()+geom_boxplot(aes(x='',y=loan_prediction$CoapplicantIncome))+xlab("Coapplicant income")+ylab("Coapplicant income")+ggtitle("Box plot of Coapplicant income")
ggplot()+geom_boxplot(aes(x='',y=loan_prediction$LoanAmount))+xlab("Loan Amount")+ylab("Loan Amount")+ggtitle("Box plot of Loan Amount")

library(OneR)
loan_prediction$CoapplicantIncome<-bin(loan_prediction$CoapplicantIncome, method = "content")


loan_prediction$LoanAmount<-bin(loan_prediction$LoanAmount, method = "content")

loan_prediction$ApplicantIncome<-bin(loan_prediction$ApplicantIncome, method = "content")

str(loan_prediction)

library(caTools)
set.seed(123)
split<-sample.split(loan_prediction$Loan_Status, SplitRatio = 2/3)
train_set<- subset(loan_prediction, split==TRUE)
test_set<- subset(loan_prediction, split==FALSE)

#Oversampling

library(ROSE)

oversample<-ovun.sample(Loan_Status~., data=train_set, p=0.5, seed=110, method="over")$data
table(oversample$Loan_Status)
train_set<- oversample

library(e1071)

#naive bayes
#Training the model

classifier4 = naiveBayes(x = train_set[-12],
                         y = train_set$Loan_Status)

#predicting the value for test data

y_pred3 = predict(classifier4,  newdata = train_set[,-12])


#Computing the confusion matrix

library(caret)
cm_train<- confusionMatrix(y_pred3, train_set$Loan_Status, positive = "1")
cm_train
cm_train$byClass[7]

#predicting the value for test data

y_pred4 = predict(classifier4,  newdata = test_set[,-12], type = "raw")

test_set$propensity<- y_pred4[,1]


#Arranging the predict_value in descending order

test_set<-test_set %>% arrange(desc(propensity))

test_set$Predicted_loan_status<-as.factor(ifelse(test_set$propensity >0,1,0))

#Computing the confusion matrix for cutoff 0 with success class as 1.
confusion_0 <- confusionMatrix(test_set$Predicted_loan_status,test_set$Loan_Status, positive = "1")
confusion_0
confusion_0$byClass[7]
#accuracy=0.68
#Sensitivity=1.0
#Pos Pred value=0.6878
#F1= 0.8150

test_set$Predicted_loan_status<-as.factor(ifelse(test_set$propensity >0.25,1,0))

confusion_25 <- confusionMatrix(test_set$Predicted_loan_status,test_set$Loan_Status, positive = "1")
confusion_25
confusion_25$byClass[7]
#accuracy=0.82
#Sensitivity=0.97
#Pos Pred value=0.81
#F1= 0.88

test_set$Predicted_loan_status<-as.factor(ifelse(test_set$propensity >0.5,1,0))

confusion_50 <- confusionMatrix(test_set$Predicted_loan_status,test_set$Loan_Status, positive = "1")
confusion_50
confusion_50$byClass[7]
#accuracy=0.75
#Sensitivity=0.766
#Pos Pred value=0.85
#F1= 0.806

test_set$Predicted_loan_status<-as.factor(ifelse(test_set$propensity >0.75,1,0))

confusion_75 <- confusionMatrix(test_set$Predicted_loan_status,test_set$Loan_Status, positive = "1")
confusion_75
confusion_75$byClass[7]
#accuracy=0.49
#Sensitivity=0.32
#Pos Pred value=0.84
#F1= 0.47

test_set$Predicted_loan_status<-as.factor(ifelse(test_set$propensity >1,1,0))

confusion_100 <- confusionMatrix(test_set$Predicted_loan_status,test_set$Loan_Status, positive = "1")
confusion_100
confusion_100$byClass[7]
#accuracy=0.31
#Sensitivity=0
#Pos Pred value=nan
#F1= NA

#Considering the accuracy, sensitivity and F1 score since missclassification might account we could predict good values at cut off 0.25. 
#ploting the ROC curve

library(pROC)
r2<-roc(as.numeric(test_set$Loan_Status),as.numeric(test_set$propensity))
plot.roc(r2, main="ROC Curve of Naive Bayes (AUC=0.78)")
auc(r2)

#The area under the curve is 0.7816, which implies the model is a perfect classifier.

library(gains)

gain1<-gains(as.numeric(as.character(test_set$Loan_Status)),as.numeric(test_set$propensity), groups = 10)
barplot(gain1$mean.resp/mean(as.numeric(as.character(test_set$Loan_Status))),names.arg = gain1$depth, xlab = "Percentile", ylab="Mean Response", main="Naive Bayes Decile-wise lift chart")

#From decile chart we could consider first 6 decile to get a loan_status data.
