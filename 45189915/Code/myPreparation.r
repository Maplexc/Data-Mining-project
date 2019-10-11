# Xin Chen 45189915

# code to complete Task 1

# 1.1 extract data into an R data frame
ILPD <- read.csv(file="./Data/Indian Liver Patient Dataset (ILPD).csv", header = FALSE)

# 1.2 assign the column name to 11 different columns
names(ILPD)<- c("Age","Gender","TB","DB","Alkphos","Sgpt","Sgot","TP","Albumin","AG_Ratio","Class")

# 1.3 fill missing value in "AG_Ratio" column with median of this column
ILPD$AG_Ratio[is.na(ILPD$AG_Ratio)] <- mean(ILPD$AG_Ratio, na.rm = TRUE)

# 1.4 replace "2" in "class" column with "0" to indicate "no_patient"
ILPD$Class[ILPD$Class==2] <- 0

# 1.5 change "class" column type from integer to factor
ILPD[,"Class"]<-factor(ILPD[,"Class"])
typeof(ILPD$Class)
class(ILPD$Class)

# 1.6 save the dataframe into a file called "ilpd_processed.Rda"
saveRDS(ILPD,file = "./Data/ilpd_processed.Rda")
