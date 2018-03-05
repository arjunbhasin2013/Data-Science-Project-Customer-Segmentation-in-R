##Clear The Buffer By Removing Pre-Built Variables

rm(list=ls())

#Import File - There Are Many Ways To Import CSV File From Local Server

file_cc <- file.choose()
cc <- read.csv(file_cc,header=TRUE)

#View File and See Underlying Structure of Variables

View(cc)
str(cc)


#Removing the Cust_ID Column as It Is Categorical Variable (Non-Numeric)

cc <- cc[-1]
View(cc)
names(cc)

#Writing Function To Compute Statistics and Apply On Dataset

cc_func <- function(x){
  nmiss = sum(is.na(x))
  a = x[!is.na(x)]
  n = length(a)
  m = mean(a)
  min = min(a)
  max = max(a)
  s = sd(a)
  p1 = quantile(a, 0.95)
  p2 = quantile(a, 0.99)
  UL = m+3*s
  LL = m-3*s
  return(c(n=n, nmiss=nmiss, Mean=m, Min=min, Max=max, StDev=s, P1=p1, P2=p2, 'Upper Limit'=UL, 'Lower Limit'=LL))
}

#Taking the variable names on which function will be applied

vars <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",          
          "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY",
          "CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX","PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",
          "MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE")

#Applying function on the variables selected above

describe_stats <- t(data.frame(apply(cc[vars],2,cc_func)))
View(describe_stats)

#Treating te Outliers and filling them with UL (Upper Limit) values
#UL (Upper Limit) = Mean + 3*Standard Deviation

cc$BALANCE[cc$BALANCE > 7809.060] <- 7809.060
cc$PURCHASES[cc$PURCHASES > 7413.090] <- 7413.090
cc$ONEOFF_PURCHASES[cc$ONEOFF_PURCHASES > 5572.107] <- 5572.107
cc$INSTALLMENTS_PURCHASES[cc$INSTALLMENTS_PURCHASES > 3124.082] <- 3124.082
cc$CASH_ADVANCE[cc$CASH_ADVANCE > 7270.351] <- 7270.351
cc$CASH_ADVANCE_FREQUENCY[cc$CASH_ADVANCE_FREQUENCY > 0.736] <- 0.736
cc$CASH_ADVANCE_TRX[cc$CASH_ADVANCE_TRX > 23.723] <- 23.723
cc$PURCHASES_TRX[cc$PURCHASES_TRX > 89.283] <- 89.283
cc$CREDIT_LIMIT[cc$CREDIT_LIMIT > 15410.910] <- 15410.910
cc$PAYMENTS[cc$PAYMENTS > 10418.320] <- 10418.320
cc$MINIMUM_PAYMENTS[cc$MINIMUM_PAYMENTS > 7981.557] <- 7981.557

# Missing Value Treatment (Imputation)

cc$CREDIT_LIMIT[is.na(cc$CREDIT_LIMIT)] <- 4494.4
cc$MINIMUM_PAYMENTS[is.na(cc$MINIMUM_PAYMENTS)] <- 864.20
inputdata <- cc[vars]

# Correlation Matrix-computing values among the Data

corr_cc <- cor(inputdata)
View(corr_cc)

require(psych)
require(GPArotation)

#Plotting Data on Scree Plot

scree(corr_cc, factors = T, pc = T, main = "Scree Plot", hline = NULL, add = F)

#Applying Factor Analysis on Dataset

FA <- fa(r = corr_cc, 5, rotate = "varimax", fm = "ml")
print(FA)

#Computing Eigen Values to Select Potential Factors

eigen_value <- eigen(corr_cc)$values

#Writing Eigen Values Data to External CSV File

write.csv(eigen_value,"Eigen_Values.csv")

#Sorting Factor Analysis Data and Computing Factor Loadings

FA_Sorted <- fa.sort(FA)
ls(FA_Sorted)
loading <- FA_Sorted$loadings
print(loading)

#Writing Loading Data to External CSV File

write.csv(loading,"Factor Loadings.csv")

#Taking Potential or Intelligent KPIs in another Vector for further usage

vars1 <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","PURCHASES_FREQUENCY",
           "PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_TRX","MINIMUM_PAYMENTS")

#Changing name of dataset with new KPI to inputdata1

inputdata1 <- cc[vars1]

#Scaling variables of the new dataset for clustering

inputdata_final <- scale(inputdata1)

#Applying Kmeans clustering on 3,4,5,6 clusters to check different values at each segment

Clus_3 <- kmeans(inputdata_final,3)
Clus_4 <- kmeans(inputdata_final,4)
Clus_5 <- kmeans(inputdata_final,5)
Clus_6 <- kmeans(inputdata_final,6)

table(Clus_3$cluster)
table(Clus_4$cluster)
table(Clus_5$cluster)
table(Clus_6$cluster)

#Taking previous clusters and combining them with new dataset with different names

cc_new <- cbind(cc, Km_Clus_3 = Clus_3$cluster, Km_Clus_4 = Clus_4$cluster, Km_Clus_5 = Clus_5$cluster, Km_Clus_6 = Clus_6$cluster)
cc_new$Km_Clus_3 <- factor(cc_new$Km_Clus_3)
cc_new$Km_Clus_4 <- factor(cc_new$Km_Clus_4)
cc_new$Km_Clus_5 <- factor(cc_new$Km_Clus_5)
cc_new$Km_Clus_6 <- factor(cc_new$Km_Clus_6)

table(cc_new$Km_Clus_3)

View(cc_new)

#Final output with out dataset is taken in CSV file - This is final output 

write.csv(cc_new,"Final_Output_With_Dataset.csv")

#Taking all the variables and puttin them with clusters * Mean to show deviation from Mean of centroids - Additional Step \

require(tables)

#
Pro <- tabular(BALANCE+BALANCE_FREQUENCY+PURCHASES+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+
                 CASH_ADVANCE+PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+
                 CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+PURCHASES_TRX+CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+
                 PRC_FULL_PAYMENT+TENURE ~ mean+(mean*Km_Clus_3)+(mean*Km_Clus_4)+(mean*Km_Clus_5)+(mean*Km_Clus_6), data = cc_new)

Pro1 <- as.matrix(Pro)
Pro1 <- data.frame(Pro1)
View(Pro1)

Profile <- tabular(1~length+(length*Km_Clus_3)+(length*Km_Clus_4)+(length*Km_Clus_5)+(length*Km_Clus_6), data = cc_new)

Pro2 <- as.matrix(Pro)
Pro2 <- data.frame(Pro2)
View(Pro2)
