# Setting Working Directory
setwd("G:\\Current\\SAS\\SASUniversityEdition\\myfolders\\MEGA Case\\Clusterinng- K means in R")
getwd()
options(java.parameters = "- Xmx1024m")

# Importing File
credit<-read.csv("CC GENERAL.csv",stringsAsFactors = FALSE)

# Identifying Outliers
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+2*s
  LC <- m-2*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}


Num_Vars <- c(
  "BALANCE",
  "BALANCE_FREQUENCY",
  "PURCHASES",
  "ONEOFF_PURCHASES",
  "INSTALLMENTS_PURCHASES",
  "CASH_ADVANCE",
  "PURCHASES_FREQUENCY",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "CASH_ADVANCE_FREQUENCY",
  "CASH_ADVANCE_TRX",
  "PURCHASES_TRX",
  "CREDIT_LIMIT",
  "PAYMENTS",
  "MINIMUM_PAYMENTS",
  "PRC_FULL_PAYMENT",
  "TENURE")

Outliers<-t(data.frame(apply(credit[Num_Vars], 2, mystats)))
View(Outliers)


# Outlier Treatment
credit$BALANCE[credit$BALANCE>5727.53]<-5727.53
credit$BALANCE_FREQUENCY[credit$BALANCE>1.3510787]<-1.3510787
credit$PURCHASES[credit$BALANCE>5276.46]<-5276.46
credit$ONEOFF_PURCHASES[credit$BALANCE>3912.2173709]<-3912.2173709
credit$INSTALLMENTS_PURCHASES[credit$BALANCE>2219.7438751]<-2219.7438751
credit$CASH_ADVANCE[credit$BALANCE>5173.1911125]<-5173.1911125
credit$PURCHASES_FREQUENCY[credit$BALANCE>1.2930919]<-1.2930919
credit$ONEOFF_PURCHASES_FREQUENCY[credit$BALANCE>0.7991299]<-0.7991299
credit$PURCHASES_INSTALLMENTS_FREQUENCY[credit$BALANCE>1.1593329]<-1.1593329
credit$CASH_ADVANCE_FREQUENCY[credit$BALANCE>0.535387]<-0.535387
credit$CASH_ADVANCE_TRX[credit$BALANCE>16.8981202]<-16.8981202
credit$PURCHASES_TRX[credit$BALANCE>64.4251306]<-64.4251306
credit$CREDIT_LIMIT[credit$BALANCE>11772.09]<-11772.09
credit$PAYMENTS[credit$BALANCE>7523.26]<-7523.26
credit$MINIMUM_PAYMENTS[credit$BALANCE>5609.1065423]<-5609.1065423
credit$PRC_FULL_PAYMENT[credit$BALANCE>0.738713]<-0.738713
credit$TENURE[credit$BALANCE>14.19398]<-14.19398


# Identifying Missing Value
Missing_Values<-t(data.frame(apply(credit[Num_Vars], 2, mystats)))
View(Missing_Values)

# Missing Value Imputation
credit$MINIMUM_PAYMENTS[which(is.na(credit$MINIMUM_PAYMENTS))] <- 721.9256368
credit$CREDIT_LIMIT[which(is.na(credit$CREDIT_LIMIT))] <- 4343.62

# Checking Missing Value
check_Missing_Values<-t(data.frame(apply(credit[Num_Vars], 2, mystats)))
View(check_Missing_Values)


# Variable Reduction (Factor Analysis)
Step_nums <- credit[Num_Vars]
corrm<- cor(Step_nums)    
eigen(corrm)$values

require(dplyr)

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))

write.csv(eigen_values, "EigenValues.csv")

require(psych)
FA<-fa(r=corrm, 5, rotate="varimax", fm="ml")  
#SORTING THE LOADINGS
FA_SORT<-fa.sort(FA)                                         
FA_SORT$loadings


Loadings<-data.frame(FA_SORT$loadings[1:ncol(Step_nums),])
write.csv(Loadings, "loadings.csv")


# standardizing the data
segment_prepared <-credit[Num_Vars]

segment_prepared = scale(segment_prepared)

#building clusters using k-means clustering 
cluster_three <- kmeans(segment_prepared,3)
cluster_four <- kmeans(segment_prepared,4)
cluster_five <- kmeans(segment_prepared,5)
cluster_six <- kmeans(segment_prepared,6)

credit_new<-cbind(credit,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(credit_new)

# Profiling

Num_Vars2 <- c(
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "BALANCE"
)

require(tables)
tt <-tabulr(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
                     factor(km_clust_6)~Heading()*mean*All(credit_new[Num_Vars2]),
                   data=credit_new)

tt1 <- as.data.frame.matrix(tt)
View(tt1)

rownames(tt1)<-c(
  "ALL",
  "KM3_1",
  "KM3_2",
  "KM3_3",
  "KM4_1",
  "KM4_2",
  "KM4_3",
  "KM4_4",
  "KM5_1",
  "KM5_2",
  "KM5_3",
  "KM5_4",
  "KM5_5",
  "KM6_1",
  "KM6_2",
  "KM6_3",
  "KM6_4",
  "KM6_5",
  "KM6_6")
  

colnames(tt1)<-c(
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "BALANCE"
  )
  

cluster_profiling <- t(tt1)

write.csv(cluster_profiling,'cluster_profiling.csv')
