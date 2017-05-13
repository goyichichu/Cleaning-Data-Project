#
# Getting and Cleaning Data Assignment
# Wu Chua
#

# load libraries
# install.packages("dplyr")
require(dplyr)

###
# 
# read decode data
#
###
features = read.table("features.txt",stringsAsFactors=FALSE)
activity = read.table("activity_labels.txt",stringsAsFactors=FALSE)

###
# 
# assemble test data
#
###

# load test data
testX = read.table("test\\X_test.txt")
testY = read.table("test\\Y_test.txt")
testSubject = read.table("test\\subject_test.txt")

# combine test data
test = cbind(testSubject,testY,testX)

###
# 
# assemble training data
#
###

# load training data
trainX = read.table("train\\X_train.txt")
trainY = read.table("train\\y_train.txt")
trainSubject = read.table("train\\subject_train.txt")

# combine training data
train = cbind(trainSubject,trainY,trainX)

###
# 
# format working data
#
###

# merge test and training sets
complete = rbind(test,train)

# assign column names
v_colnames = as.vector(as.matrix(rbind("SubjectId","ActivityId",features[2])))
colnames(complete) = v_colnames 

# get rid of duplicate columns
measures = complete[ , !duplicated(colnames(complete))] %>%

# extract mean and std measurements
select(matches("SubjectId"),matches("ActivityId"),contains("std()"),contains("mean()")) %>%

# add descriptive activity names
merge(activity, by.x = "ActivityId", by.y="V1", all.x=TRUE, sort=FALSE) %>%

# reorder columns for clarity
select(SubjectId,ActivityId,ActivityDesc=V2,everything())

# label the dataset with descriptive variable names
renameF = sub("^f","Fourier",colnames(measures)) 
renameAcc = sub("Acc","Acceleration",renameF)
renameGyro = sub("Gyro","Gyroscope",renameAcc)
renameT = sub("^t","",renameGyro)
renameZ = sub("Z$","Z-Axis",renameT)
renameX = sub("X$","X-Axis",renameZ)
renameY = sub("Y$","Y-Axis",renameX)
renameMag = sub("Mag","Magnitude",renameY)
renameBodyBody = sub("BodyBody","Body",renameMag)
renameMean = sub("-mean\\(\\)","Mean",renameBodyBody)
renameStd = sub("-std\\(\\)","StdDev",renameMean)

colnames(measures) = renameStd
###
# 
# create average dataset
#
###

measure_means = group_by(measures,SubjectId,ActivityId,ActivityDesc) %>%
summarise_each(funs(mean(.,na.rm = TRUE)))

# update headers
colnames(measure_means) = sub("^","Mean_",colnames(measure_means))

# write out the data set
write.table(measure_means,file="measure_means.txt",row.name=FALSE,col.names=TRUE)
