### Read tabular data into R
# test
subject_test<-read.table("subject_test.txt", header = FALSE, sep = "", dec = ".")
X_test<-read.table("X_test.txt", header = FALSE, sep = "", dec = ".")
y_test<-read.table("y_test.txt", header = FALSE, sep = "", dec = ".")

#train
subject_train<-read.table("subject_train.txt", header = FALSE, sep = "", dec = ".")
X_train<-read.table("X_train.txt", header = FALSE, sep = "", dec = ".")
y_train<-read.table("y_train.txt", header = FALSE, sep = "", dec = ".")

#features
features<-read.table("features.txt", header = FALSE, sep = "", dec = ".")
mean_var<-grep("mean()",features$V2)
std_var<-grep("std()",features$V2)


###cleaning variables
colnames(y_test) <- c("Activity")
colnames(subject_test) <- c("Subject")
colnames(y_train) <- c("Activity")
colnames(subject_train) <- c("Subject")

col.names1<-features[mean_var,2]
col.names1<-gsub("-","",col.names1)
col.names1<-gsub(pattern="mean()",replacement =".Mean." ,col.names1)
col.names1<-gsub("[()]","",col.names1)

col.names2<-features[std_var,2]
col.names2<-gsub("-","",col.names2)
col.names2<-gsub(pattern="std()",replacement =".std." ,col.names2)
col.names2<-gsub("[()]","",col.names2)

#relabels the names of datasets.
colnames(X_test)[mean_var] <- col.names1
colnames(X_test)[std_var] <- col.names2
colnames(X_train)[mean_var] <- col.names1
colnames(X_train)[std_var] <- col.names2



#Merge the test dataset

data_test<-cbind(subject_test, y_test, X_test)

#Merge the train dataset

data_train<-cbind(subject_train, y_train, X_train)

#Merge the train + test dataset

finaldata<-rbind(data_test, data_train)


#Extract only the measurements 

new_mean<-mean_var+2
new_std<-std_var+2

new_data<-finaldata[, c(1, 2, new_mean,new_std)]

#Uses descriptive activity names 

acttivitylabel<-read.table("activity_labels.txt", header = FALSE, sep = "", dec = ".")
labels_act <- acttivitylabel[,2]

new_data$Activity <- labels_act[new_data$Activity]

#5. Tidy final data set 

final_data <- new_data[order(new_data$Subject, new_data$Activity),]


output<-NULL

for (i in 1:30){
  outcome<-final_data %>% filter(Subject== i) %>% group_by(Activity) %>% summarise_all(funs(mean))
  output<-rbind(output, outcome)
}
print(output)
#write dataset into txt files
write.table(output, "output.txt", row.name=FALSE)


