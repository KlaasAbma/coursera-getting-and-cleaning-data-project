require(plyr)
# Harddisk Dir's and files
uci_harddisk_dir <- "UCI\ HAR\ Dataset"
feature_file <- paste(uci_harddisk_dir, "/features.txt", sep = "")
activity_labels_file <- paste(uci_harddisk_dir, "/activity_labels.txt", sep = "")
x_activityfile <- paste(uci_harddisk_dir, "/train/X_train.txt", sep = "")
y_activityfile <- paste(uci_harddisk_dir, "/train/y_train.txt", sep = "")
subject_activityfile <- paste(uci_harddisk_dir, "/train/subject_train.txt", sep = "")
x_test_file <- paste(uci_harddisk_dir, "/test/X_test.txt", sep = "")
y_test_file <- paste(uci_harddisk_dir, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_harddisk_dir, "/test/subject_test.txt", sep = "")
# Load the raw data
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_activityfile)
y_train <- read.table(y_activityfile)
subject_train <- read.table(subject_activityfile)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)
# 1. Merge the training and the test sets to create one big data set.
# Binding the sensor data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)
# Label the columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]
# 3. Uses descriptive activity names to name the activities in the data set
mean_std <- join(mean_std, activity_labels, by = "ActivityId", match = "first")
mean_std <- mean_std[,-1]
# 4. Label the data set with descriptive names.
# Remove parentheses
names(mean_std) <- gsub('\\(|\\)',"",names(mean_std), perl = TRUE)
# Make syntactically valid names
names(mean_std) <- make.names(names(mean_std))
# Make clearer names
names(mean_std) <- gsub('Acc',"Acceleration",names(mean_std))
names(mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(mean_std))
names(mean_std) <- gsub('Gyro',"AngularSpeed",names(mean_std))
names(mean_std) <- gsub('Mag',"Magnitude",names(mean_std))
names(mean_std) <- gsub('^t',"TimeDomain.",names(mean_std))
names(mean_std) <- gsub('^f',"FrequencyDomain.",names(mean_std))
names(mean_std) <- gsub('\\.mean',".Mean",names(mean_std))
names(mean_std) <- gsub('\\.std',".StandardDeviation",names(mean_std))
names(mean_std) <- gsub('Freq\\.',"Frequency.",names(mean_std))
names(mean_std) <- gsub('Freq$',"Frequency",names(mean_std))
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
sensor_avg_by_act_sub = ddply(mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "tidy.txt", row.name=FALSE )

