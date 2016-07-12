find_index<-function(L,x)
{
  if(length(L)==1)
  {
    if(L[1]>=x)
    {return (1)
    }else
    {
      return(2)
    }
  }else
  {
    mid=as.integer( (1+length(L))/2)
    if(L[mid]>=x)
    {
      return( find_index(L[1:mid],x))
    }else{
      return(mid+find_index(L[(mid+1):length(L)],x))
    }
    
  }
}

DataScienceChallenge_Training <- read.csv("~/DataScienceChallenge_Training.csv")
#DataScienceChallenge_Training[,2]=as.character(DataScienceChallenge_Training[,2])

category_name<-unique(DataScienceChallenge_Training$SPND_CATGY)
no_of_category=length(category_name)


merchant_information<- DataScienceChallenge_Training[!duplicated(DataScienceChallenge_Training[,c(2,4)]),c(2,4)]
no_of_merchant=length(merchant_information[,1])
###category-wise divison
merchant_by_category=vector(mode="list",length=no_of_category)
for(i in 1:no_of_merchant)
{
 index=match(merchant_information[i,2],category_name)
 merchant_by_category[[index]]=append(merchant_by_category[[index]],merchant_information[i,1])
}
category_length=c()
for(i in 1:no_of_category)
{
  category_length[i]=length(merchant_by_category[[i]])
  
}
category_length=cumsum(category_length)


customer_list<-DataScienceChallenge_Training[!duplicated(DataScienceChallenge_Training[,1]),1]
customer_list=sort(customer_list)
no_of_customer=length(customer_list)

transaction=c()


for( j in 1:no_of_customer){
customer1_data<-subset(DataScienceChallenge_Training,Cust_map==customer_list[j])
merchant_present=customer1_data[!duplicated(customer1_data[,c(2,4)]),2]
c1_transaction=matrix(0,1,no_of_merchant)
c=0
for(k in 1:no_of_category)
{
  lim=length(merchant_by_category[[k]])
  for( i in 1:lim)
  {
    if(!is.na(match(merchant_by_category[[k]][i],merchant_present)))
    {
      c1_transaction[1,c+i]=sum(subset(customer1_data,Merch_Map_final==merchant_by_category[[k]][i])$NumTrans)
    }
  }
  c=c+lim
}


transaction=rbind(transaction,c1_transaction)
}
#the matrix is built up
#now SVD
transfomed_transaction=c()
nmbr_of_pca=5#try for different number
k=1
for(i in 1:no_of_category)
{
  m1=transaction[,k:(k-1+length(merchant_by_category[[i]]))]
  S=svd(m1)
  T1=S$u[,1:nmbr_of_pca] %*% diag(S$d[1:nmbr_of_pca]) %*% t(S$v[,1:nmbr_of_pca])
  transfomed_transaction=cbind(transfomed_transaction,T1)
  k=k+length(merchant_by_category[[i]])
}
#svd stored in transfomed_transaction
submission=c()
for (i in 1:no_of_customer)
{
  suggested=c()
  potential_merchant_1<-which(transaction[i,]==0,arr.ind=TRUE)
  row_1=transfomed_transaction[i,]-transaction[i,]#diff btwn k-rank approximate and original for customer
  customers_picked=0
  index_counter=0
  while(customers_picked!=10){
  index=match(max(row_1),row_1)+index_counter
  #this part is to find out the merchant_no of the column for which maximum deviation is observed between 
  #the k-rank and original
  fi=find_index(category_length,index)
  if(fi>1){find_i=index-category_length[fi-1]
  }else
  {
    find_i=index
  }
  
  merchant_no=merchant_by_category[[fi]][find_i]#get the merchant_no
  if(!is.na(match(merchant_no,potential_merchant_1)))#included in the output if he's not already on the buying list of the customer
  {
    suggested=append(suggested,merchant_no)
    customers_picked=customers_picked+1
  }
  
  row_1=row_1[-index]#remove that index from that row..search for next biggest index
  index_counter=index_counter+1
  }
  output=cbind(rep(i,times=10),suggested)
  submission=rbind(submission,output)
  
}
submission=as.data.frame(submission)




