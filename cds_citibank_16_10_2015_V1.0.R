DataScienceChallenge_Training <- read.csv("~/DataScienceChallenge_Training.csv")
#DataScienceChallenge_Training[,2]=as.character(DataScienceChallenge_Training[,2])


merchant_information<- DataScienceChallenge_Training[!duplicated(DataScienceChallenge_Training[,c(2,4)]),2]
#list of all merchant in sorted order 1,2,3,4.....
merchant_list=sort(merchant_information)
#
no_of_merchant=nrow((table(DataScienceChallenge_Training$Merch_Map_final)))
#take list of customers
customer_list<-DataScienceChallenge_Training[!duplicated(DataScienceChallenge_Training[,1]),1]
customer_list=sort(customer_list)

no_of_customer=nrow(table(DataScienceChallenge_Training$Cust_map))

transaction=c()
no_of_customer=10#for experimental purpose..delete it later
## create a matrix of 10 customer, with all merchants, where each items represents no of transactions
## from customer with the merchants
for( j in 1:no_of_customer){
customer1_data<-subset(DataScienceChallenge_Training,Cust_map==customer_list[j])
merchant_present=customer1_data[!duplicated(customer1_data[,c(2,4)]),2]
merchant_present=sort(merchant_present)
no_merchant_1=nrow(table(customer1_data$Merch_Map_final))
c1_transaction=matrix(0,1,no_of_merchant)
for( i in 1:no_merchant_1){
c1_transaction[1,merchant_present[i]] =sum(subset(customer1_data,Merch_Map_final==merchant_present[i])$NumTrans)
}
transaction=rbind(transaction,c1_transaction)
}
##create 3 kmeans cluster of customers.
no_of_cluster=3
kc=kmeans(transaction,no_of_cluster)#3 is no of clusters
###cluster formation


## list of which customers are there in which clusters.
cust_cluster=vector(mode="list",length=no_of_cluster)
for( i in 1:no_of_customer){
  t=kc$cluster[i]
  cust_cluster[[t]]=append(cust_cluster[[t]],i)
}

####calculate top 100 merchant for each cluster
top_100=list()
for( i in 1:no_of_cluster)
{
  s_c=transaction[as.vector(cust_cluster[[i]]),]
  if(!is.null(nrow(s_c))){
  merchant_wise=apply(s_c,2,sum)
  }else{
    merchant_wise=s_c
  }
  l=c()
  count=0
  while(count<=100){
  index=match(max(merchant_wise),merchant_wise)
  l=append(l,index+count)
  merchant_wise=merchant_wise[-index]
  count=count+1
  }
  top_100[[i]]=l
}

## suggest 10 merchants for each customer.
submission=c()
for (i in 1:no_of_customer)
  
{
  potential_merchant_1<-which(transaction[i,]==0,arr.ind=TRUE)
  cluster_no=kc$cluster[i]
  pssbl_merchant_1=top_100[[cluster_no]]
  suggested=head(intersect(pssbl_merchant_1,potential_merchant_1),10)
  output=cbind(rep(i,times=10),suggested)
  submission=rbind(submission,output)
}
submission=as.data.frame(submission)


  
  
