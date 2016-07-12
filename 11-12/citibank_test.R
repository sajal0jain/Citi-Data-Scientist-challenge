Data_9_months=subset(DataScienceChallenge_Training,TXN_MTH<=201405)
length(Data_9_months[!duplicated(Data_9_months[,c(2,4)]),c(2,4)])
test_merchant_data=Data_9_months[!duplicated(Data_9_months[,c(2,4)]),c(2,4)]
test_merchant_list=test_merchant_data[,1]
test_merchant_no=length(test_merchant_list)
test_merchant_by_category=vector(mode="list",length=no_of_category)
test_absent_merchant=setdiff(c(1:max(Data_9_months$Merch_Map_final)),test_merchant_list)
test_customer=Data_9_months[!duplicated(Data_9_months[,1]),1]
test_customer=sort(test_customer)
test_cust_no=length(test_customer)
test_cust_absent=setdiff(c(1:max(Data_9_months$Cust_map)),test_customer)
for(i in 1:test_merchant_no)
{
  index=match(test_merchant_data[i,2],category_name)
  test_merchant_by_category[[index]]=append(test_merchant_by_category[[index]],test_merchant_data[i,1])
}

test_category_length=c()
for(i in 1:no_of_category)
{
  test_category_length[i]=length(test_merchant_by_category[[i]])
  
}


test_category_length=cumsum(test_category_length)


require('Matrix')


test_data_matrix=sparseMatrix(Data_9_months$Cust_map,Data_9_months$Merch_Map_final,x=Data_9_months$NumTrans)
test_data_matrix=test_data_matrix[-test_cust_absent,]
require('irlba')
test_svd_vector=vector(mode="list",length=no_of_category)
test_reloaded_index=c()
for(i in 1:no_of_category){
  index=test_merchant_by_category[[i]]
  test_reloaded_index=append(test_reloaded_index,index)
  a=test_data_matrix[,index]
  test_svd_vector[[i]]=irlba(a,nv=20)#k=2
  print(i)
}
test_data_matrix=test_data_matrix[,test_reloaded_index]
submission=c()
for(j in 1:1000){
  print(j)
  cust_vector=c()
  for(i in 1:no_of_category)
  {
    
    vec=test_svd_vector[[i]]$u[j,]%*%diag(test_svd_vector[[i]]$d)%*%t(test_svd_vector[[i]]$v)
    cust_vector=cbind(cust_vector,vec)
  }
  original_vector=test_data_matrix[j,]
  difference_vector=cust_vector-original_vector
  merchants_picked=0
  index_collection=c()
  suggested=c()
  while(merchants_picked!=10){
    index=match(max(difference_vector),difference_vector)
    difference_vector=difference_vector[-index]#remove that index from that row..search for next biggest index
    if(original_vector[index]==0){
    index=index_return(index_collection,index)
    fi=find_index(test_category_length,index)
    if(fi>1){find_i=index-test_category_length[fi-1]
    }else
    {
      find_i=index
    }
    merchant_no=test_merchant_by_category[[fi]][find_i]
    
    
    suggested=append(suggested,merchant_no)
      
    
    index_collection=append(index,index_collection)
    merchants_picked=merchants_picked+1
    }
  }
  output=cbind(rep(test_customer[j],times=10),c(1:10),suggested)
  submission=rbind(submission,output)
  }
submission=as.data.frame(submission)
names(submission)=c("Cust_map","Rank","Merchant")
write.table(submission, file="test_submission_rank_20.csv", row.names=FALSE, col.names=TRUE, sep=",")


DATA_3_MONTHS=subset(DataScienceChallenge_Training,TXN_MTH>201405)
####TEST DATA
test_data_matrix=sparseMatrix(Data_9_months$Cust_map,Data_9_months$Merch_Map_final,x=Data_9_months$NumTrans)
test_data_matrix=test_data_matrix[-test_cust_absent,]
Three_month_matrix=sparseMatrix(DATA_3_MONTHS$Cust_map,DATA_3_MONTHS$Merch_Map_final,x=DATA_3_MONTHS$NumTrans)
Three_month_matrix=Three_month_matrix[test_customer,]
Three_month_matrix=cbind(Three_month_matrix,as.matrix(rep(0,times=test_cust_no)))
test_rslt=c()
for( i in 1:5000)
{
  print(i)
#   merchant_1=Three_month_matrix[i,]
#   potential_merchant_1<-which(test_data_matrix[i,]==0,arr.ind=TRUE)
#   potential_merchant_1=setdiff(potential_merchant_1,test_absent_merchant)
#   comparison_vector=Three_month_matrix[i,potential_merchant_1]
#   top50=rev(tail(sort(comparison_vector),50))
#   top50=top50[top50!=0]
#   top50_match=find_match(top50,merchant_1)
#   l=length(top50)
#   if(l!=0){
#   write_out=cbind(rep(test_customer[i],times=l),c(1:l),top50_match)
#   
#   
#   test_rslt=rbind(test_rslt,write_out)}
  comparison_vector=Three_month_matrix[i,]-test_data_matrix[i,]
  index_collection=c()
  while(max(comparison_vector)!=0){
  highest=max(comparison_vector)
  index=match(highest,comparison_vector)
  comparison_vector=comparison_vector[-index]
  index=index_return(index_collection,index)
  if(test_data_matrix[i,index]==0)
  {
    index_collection=append(index,index_collection)
  }
  }
  l=length(index_collection)
  if(l!=0){
  write_out=cbind(rep(test_customer[i],times=l),c(1:l),index_collection)
  }
  test_rslt=rbind(test_rslt,write_out)
 # gc()
}
test_rslt=as.data.frame(test_rslt)
names(test_rslt)=c("Cust_map","Rank","Merchant")
write.table(test_rslt, file="TEST_DATA_5000customers.csv", row.names=FALSE, col.names=TRUE, sep=",")


find_match<-function(vect,Li)
{
len=length(vect)
index_list=c()
count=0
if(len!=0){
for(i in 1:len)
{
  index=match(vect[i],Li)
  Li=Li[-index]
  index=index_return(index_list,index)
  index_list=append(index_list,index)
}
}
return(index_list)
}

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





