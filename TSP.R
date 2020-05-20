### 2020/05/20 keonwoo Park

## Data Structure 2

## 최단거리구하기

####### Distance Table
data_distance<-matrix(,nrow=5,ncol=5)
data_distance[,]=Inf
data_distance[1,2]=1
data_distance[1,3]=3
data_distance[1,5]=2
data_distance[2,1]=1
data_distance[2,3]=1
data_distance[3,2]=1
data_distance[3,1]=3
data_distance[3,4]=2
data_distance[4,3]=2
data_distance[4,5]=2
data_distance[5,1]=2
data_distance[5,4]=2

### Shortest path(최단거리)
data_distance_1<-NULL
temp_distance<-data.frame(Origin=0,Destination=0,Distance=0)

## index table
index_distance<-data.frame(Origin=1:5,Index=rep(0,time=5))

i1=0
pre_i=0
for(i in 1:5){
  for(j in 1:5){
    if(is.finite(data_distance[i,j])){
      temp_distance$Origin=i
      temp_distance$Destination=j
      temp_distance$Distance<-data_distance[i,j]
      data_distance_1<-rbind(data_distance_1,temp_distance)
      ## index table
      i1=i1+1
      if(pre_i!=i){
        index_distance$Index[i]=i1
        pre_i=i
      }
    }
  }
}



Random_Sequence<-function(x,k,rep=F){
  n=length(x)
  if(rep==F){
    for(i in 1:k){
      j=floor(runif(1,min=i,max=(n+1)))
      temp_value=x[j]
      x[j]=x[i]
      x[i]=temp_value
    }
  }else{
    x=x[floor(runif(k,min=1,max=(n+1)))]
  }
  return(x[1:k])
}
Random_Sequence(1,2)
##################################################################################
Random_Sequence_2<-function(d){
  size_d=length(d)
  for(i in 1:(size_d-1)){
    random_index = ceiling(i+runif(1)*(size_d+1-i)-1)
    
    temp_value=d[random_index]
    d[random_index]=d[i]
    d[i]=temp_value
  }
  return(d)
}



Selection_Sort_df<- function(d,col_num=1,decreasing=FALSE){
  size_d <- length(d[,1])
  if(decreasing == FALSE){
    for(i1 in 1:(size_d-1)){
      min_value = d[i1, col_num]
      min_index=i1
      for(i2 in (i1+1):size_d){
        if(d[i2,col_num]<min_value){
          min_value=d[i2,col_num]
          min_index=i2
        }
      }
      ### Swap
      tem_value = d[i1,]
      d[i1,] = d[min_index,]
      d[min_index,] = tem_value
    }
    
  }else{
    for(i1 in 1:(size_d-1)){
      max_value = d[i1,col_num]
      max_index=i1
      for(i2 in (i1+1):size_d){
        if(d[i2,col_num]>max_value){
          max_value=d[i2,col_num]
          max_index=i2
        }
      }
      ### Swap
      tem_value = d[i1,]
      d[i1,] = d[max_index,]
      d[max_index,] = tem_value
    }
    
  }
  return(d)
}

### TSP
n= 100
result<-data.frame(x1=rep(0,n),x2=rep(0,n),x3=rep(0,n),
                   x4=rep(0,n), x5=rep(0,n), distance=rep(0,n))
for(i1 in 1:n){
  Sequence=Random_Sequence_2(1:5)
  distance=0
  for(i2 in 1:4){
    distance=distance + data_distance[Sequence[i2],Sequence[i2+1]]
  }
  distance=distance + data_distance[Sequence[5],Sequence[1]]
  result[i1,1:5] =Sequence
  result$distance[i1]=distance
  }


Selection_Sort_df(result,6)


### TSP
set.seed(1234)
x=runif(100,0,100)
y=runif(100,0,100)
data_distance<-matrix(nrow=100,ncol=100)
for(i1 in 1:99){
  for(i2 in (i1+1):100){
    data_distance[i1,i2]=((x[i1]-x[i2])^2+(y[i1]-y[i2])^2)^(1/2)
    data_distance[i2,i1]=data_distance[i1,i2]
    }
}
data_distance
summary(data_distance)

#######################################
n= 100
result<-data.frame(x1=rep(0,n),x2=rep(0,n),x3=rep(0,n),
                   x4=rep(0,n), x5=rep(0,n), distance=rep(0,n))
for(i1 in 1:n){
  Sequence=Random_Sequence_2(1:5)
  distance=0
  for(i2 in 1:4){
    distance=distance + data_distance[Sequence[i2],Sequence[i2+1]]
  }
  distance=distance + data_distance[Sequence[5],Sequence[1]]
  result[i1,1:5] =Sequence
  result$distance[i1]=distance
}


Selection_Sort_df(result,6)