#DataScienceChallenge_Training <- read.csv("~/DataScienceChallenge_Training.csv")
DataScienceChallenge_Training <- read.csv(file.choose())

## take first 10000 customers
first_10000_customer <- subset(x = DataScienceChallenge_Training,DataScienceChallenge_Training$Cust_map <=10000)
write.csv(file = "first_10000_customer.csv",x = first_10000_customer)

library('plyr')
count(DataScienceChallenge_Training,'Cust_map')
#write.csv(customer1_data,file="customer1.csv")
count(DataScienceChallenge_Training,'Merch_Map_final')
customer_table<-table(DataScienceChallenge_Training$Cust_map)
#customer_no=nrow(customer_table)
customer_no=1000
#write.csv(customer_table,"customer_list.csv")
merchant_table<-table(DataScienceChallenge_Training$Merch_Map_final)
#write.csv2(merchant_table,"merchant_list.csv")
merchant_information<- DataScienceChallenge_Training[!duplicated(DataScienceChallenge_Training[,c(2,4)]),c(2,4)]
#write.csv2(merchant_information,"merchant_category.csv")
library('compare')
category_name<-unique(DataScienceChallenge_Training$SPND_CATGY)
no_of_category=length(category_name)

HU_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[1])
Grocery_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[2])
Retail_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[3])
FB_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[4])
Clothing_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[5])
Servicestation_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[6])
Recreation_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[7])
Medical_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[8])
Tech_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[9])
Lodging_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[10])
Travel_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[11])
Business_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[12])
other_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[13])
Insurance_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[14])
Education_merchants_data=subset(DataScienceChallenge_Training,SPND_CATGY==category_name[15])

freq_HU=aggregate(HU_merchants_data$NumTrans,by=list(Merch_Map_final=HU_merchants_data$Merch_Map_final),FUN=sum)
top100_HU=head(freq_HU,100)

freq_grocery=aggregate(Grocery_merchants_data$NumTrans,by=list(Merch_Map_final=Grocery_merchants_data$Merch_Map_final),FUN=sum)
top100_grocery=head(freq_grocery,100)

freq_retail=aggregate(Retail_merchants_data$NumTrans,by=list(Merch_Map_final=Retail_merchants_data$Merch_Map_final),FUN=sum)
top100_retail=head(freq_retail,100)

freq_FB=aggregate(FB_merchants_data$NumTrans,by=list(Merch_Map_final=FB_merchants_data$Merch_Map_final),FUN=sum)
top100_FB=head(freq_FB,100)

freq_clothing=aggregate(Clothing_merchants_data$NumTrans,by=list(Merch_Map_final=Clothing_merchants_data$Merch_Map_final),FUN=sum)
top100_clothing=head(freq_clothing,100)

freq_servicestation=aggregate(Servicestation_merchants_data$NumTrans,by=list(Merch_Map_final=Servicestation_merchants_data$Merch_Map_final),FUN=sum)
top100_servicestation=head(freq_servicestation,100)

freq_recreation=aggregate(Recreation_merchants_data$NumTrans,by=list(Merch_Map_final=Recreation_merchants_data$Merch_Map_final),FUN=sum)
top100_recreation=head(freq_recreation,100)

freq_medical=aggregate(Medical_merchants_data$NumTrans,by=list(Merch_Map_final=Medical_merchants_data$Merch_Map_final),FUN=sum)
top100_medical=head(freq_medical,100)

freq_techno=aggregate(Tech_merchants_data$NumTrans,by=list(Merch_Map_final=Tech_merchants_data$Merch_Map_final),FUN=sum)
top100_techno=head(freq_techno,100)

freq_lodging=aggregate(Lodging_merchants_data$NumTrans,by=list(Merch_Map_final=Lodging_merchants_data$Merch_Map_final),FUN=sum)
top100_lodging=head(freq_lodging,100)

freq_travel=aggregate(Travel_merchants_data$NumTrans,by=list(Merch_Map_final=Travel_merchants_data$Merch_Map_final),FUN=sum)
top100_travel=head(freq_travel,100)

freq_business=aggregate(Business_merchants_data$NumTrans,by=list(Merch_Map_final=Business_merchants_data$Merch_Map_final),FUN=sum)
top100_business=head(freq_business,100)

freq_other=aggregate(other_merchants_data$NumTrans,by=list(Merch_Map_final=other_merchants_data$Merch_Map_final),FUN=sum)
top100_other=head(freq_other,100)

freq_insurance=aggregate(Insurance_merchants_data$NumTrans,by=list(Merch_Map_final=Insurance_merchants_data$Merch_Map_final),FUN=sum)
top100_insurance=head(freq_insurance,100)

freq_education=aggregate(Education_merchants_data$NumTrans,by=list(Merch_Map_final=Education_merchants_data$Merch_Map_final),FUN=sum)
top100_education=head(freq_education,100)

top_100=c()
top_100=append(top_100,c(top100_HU,top100_grocery,top100_retail,top100_FB,top100_clothing,top100_servicestation,
         +top100_recreation,top100_medical,top100_techno,top100_lodging,top100_travel,top100_business,
         +top100_other,top100_insurance,top100_education))

global_weightage=c()
j=1
for(j in 1:no_of_category){
  w1=2*sum(subset(DataScienceChallenge_Training,SPND_CATGY==category_name[j] & TXN_MTH<=201311 )$NumTrans)
  w2=sum(subset(DataScienceChallenge_Training,SPND_CATGY==category_name[j] & TXN_MTH>201311 )$NumTrans)
  global_weightage=append(global_weightage,w1+w2)
}

submission=c()
####Customer-1
for(i in 1:100 ){
print("Customer Number:")
print(i)
time_before <- Sys.time();
customer1_data<-subset(DataScienceChallenge_Training,Cust_map==names(customer_table)[i])
comp=rbind(merchant_information,customer1_data[,c(2,4)])
potential_merchant_1<-comp[!duplicated(comp,fromLast = TRUE) & seq(nrow(comp))<=nrow(merchant_information),]
no_of_merchant_cust1<-c()
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="HOUSEHOLD & UTILITIES")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="GROCERY STORES")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="RETAIL")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="F&B")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="CLOTHING & ACCESSORIES")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="SERVICE STATIONS")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="RECREATION")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="MEDICAL")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="TECHNOLOGY")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="LODGING")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="TRAVEL & TRANSPORT")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY==" BUSINESS SERVICES")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="OTHERS")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="INSURANCE SERVICES")$Merch_Map_final)))
no_of_merchant_cust1=append(no_of_merchant_cust1,nrow(table(subset(customer1_data,SPND_CATGY=="EDUCATION")$Merch_Map_final)))

#delete
category_weightage_1=c()
for(j in 1:no_of_category){
w1=2*sum(subset(customer1_data,SPND_CATGY==category_name[j] & TXN_MTH<=201311 )$NumTrans)
w2=sum(subset(customer1_data,SPND_CATGY==category_name[j] & TXN_MTH>201311 )$NumTrans)
category_weightage_1=append(category_weightage_1,w1+w2)
}
category_weightage_1=10*category_weightage_1/sum(category_weightage_1)
#####current algo
for( k in 1:10){
index=c()
max_weightage=max(category_weightage_1)
index=append(index,match(max_weightage,category_weightage_1))
counter=1
while (max_weightage %in% category_weightage_1[-index])
{ index=append(index,match(max_weightage,category_weightage_1[-index])+counter)
counter=counter+1

}
if(length(index)==1){
  selected_index=index[1]
  likely_sector=category_name[selected_index]
  }else
  {selected_index=match(max(global_weightage[index]),global_weightage)
  likely_sector=category_name[selected_index]
}
psbl_merchant=merge(top_100[2*selected_index-1],subset(potential_merchant_1,SPND_CATGY==likely_sector),by="Merch_Map_final")
choosen_one=head(psbl_merchant[,1],1)
row_to_delete=which(potential_merchant_1$Merch_Map_final==choosen_one)
potential_merchant_1=potential_merchant_1[-row_to_delete,]
print(c(selected_index,choosen_one))
print(likely_sector)
category_weightage_1[selected_index]=category_weightage_1[selected_index]-1
output=data.frame(i,k,choosen_one,likely_sector)
submission=rbind(submission,output)
}
time_spent <- Sys.time() - time_before
cat("Time spent for a customer", time_spent)
}
submission=as.data.frame(submission)
write.csv(file = "first_100_cust_prediction.csv",x = submission)

#####previous algo
index=match(max(no_of_merchant_cust1),no_of_merchant_cust1)
likely_sector=category_name[index]
psbl_merchant=merge(top_100[2*index-1],subset(potential_merchant_1,SPND_CATGY==likely_sector),by="Merch_Map_final")
top10=head(psbl_merchant[,1],10)

cus=rep(i,times=10)
rnk=rep(1:10)
output=data.frame(cus,rnk,top10)
submission=rbind(submission,output)