DataScienceChallenge_Training <- read.csv(file.choose())
category_name<-unique(DataScienceChallenge_Training$SPND_CATGY)
no_of_category=length(category_name)
merchant_information<- DataScienceChallenge_Training[!duplicated(DataScienceChallenge_Training[,c(2,4)]),c(2,4)]
no_of_merchant=length(merchant_information[,1])
merchant_list=merchant_information[,1]
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

##
category_length=cumsum(category_length)

customer_list<-DataScienceChallenge_Training[!duplicated(DataScienceChallenge_Training[,1]),1]
customer_list=sort(customer_list)#sort them in ascending order
no_of_customer=length(customer_list)
customer_absent=setdiff(c(1:max(DataScienceChallenge_Training$Cust_map)),customer_list)
require('Matrix')
transaction_matrix=sparseMatrix(DataScienceChallenge_Training$Cust_map,DataScienceChallenge_Training$Merch_Map_final,x=DataScienceChallenge_Training$NumTrans)
require('irlba')


svd_vector=vector(mode="list",length=no_of_category)
reloaded_index=c()
for(i in 1:no_of_category){
index=merchant_by_category[[i]]
reloaded_index=append(reloaded_index,index)
a=transaction_matrix[-customer_absent,index]
svd_vector[[i]]=irlba(a,nv=1)#k=10
print(i)
}
transaction_matrix=transaction_matrix[-customer_absent,reloaded_index]


submission=c()
for(j in 1:100){
  print(j)
cust_vector=c()
for(i in 1:no_of_category)
{
  
  vec=svd_vector[[i]]$u[j,]%*%diag(svd_vector[[i]]$d)%*%t(svd_vector[[i]]$v)
  cust_vector=cbind(cust_vector,vec)
}
original_vector=transaction_matrix[j,]
difference_vector=cust_vector-original_vector
merchants_picked=0
index_collection=c()
suggested=c()
while(merchants_picked!=10){
  index=match(max(difference_vector),difference_vector)
  difference_vector=difference_vector[-index]#remove that index from that row..search for next biggest index
  if(original_vector[index]==0){
  index=index_return(index_collection,index)
  #this part is to find out the merchant_no of the column for which maximum deviation is observed between 
  #the k-rank and original
  fi=find_index(category_length,index)
  if(fi>1){find_i=index-category_length[fi-1]
  }else
  {
    find_i=index
  }
  merchant_no=merchant_by_category[[fi]][find_i]#get the merchant_no
  suggested=append(suggested,merchant_no)
    
  
  index_collection=append(index,index_collection)
  merchants_picked=merchants_picked+1
}
}
output=cbind(rep(customer_list[j],times=10),c(1:10),suggested)
submission=rbind(submission,output)
}

submission=as.data.frame(submission)
names(submission)=c("Cust_map","Rank","Merchant")
write.table(submission, file="submission_40.csv", row.names=FALSE, col.names=TRUE, sep=",")
m=matrix(c(0,2,-3,9,19,23,-7,9,11,14,-32,14,33,11,8),byrow=F,nrow=3)
index_return<-function(L,x)
{
  l1=sort(L)
  len=length(L)
  if(len==0)
  {return (x)
  }else
  {
    
    i=1
    while((x>=l1[i])&(i<=len))
    {
      x=x+1
      i=i+1
    }
    return(x)}
}
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

