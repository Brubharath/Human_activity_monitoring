#Master of data science
#Bharath Shivakumar
#Data Capture and Preparations

#########################################################################################################

#Question 1
#Load dataset. Load each of the files you captured in Part 1 as a data frame, 
#use computational methods to achieve this. Use names that are meaningful and
#consistent (e.g., subj1_act1). You should have created 12 data frames at the end of
#this task. (6 points)


#In this task we use 2 for loops as we have two conditions to satisfy
#first is the number of the activity and then the number of the subject
#The code picks up the pattern to make the file name and is assigned to a variable.
#then we use the file name to extract the file/data.
#Assign function is assigning the value to the function so that it read the csv file.
for (activity_no in 1:4) {
  for (subject_no in 1:3) {
    varname <- paste0("Sub",subject_no,"_act", activity_no)
    path <- paste0("data/", varname,".csv")
    assign(varname,read.csv(path,header = FALSE))
  }
}

##########################################################################################################


#Question 2
#Using computational methods, find the data markers which determine the actual
#data (human activity) needed for this assignment. Obtain the indexes for the start
#and end markers. (8 points)

#First we make an empty in which we are going to store our start and end indexes
marker_indexes_list <- list()

#We use the same for loop used in the previous question.
for (activity_no in 1:4) {
  for (subject_no in 1:3) {
    
    ##we use the same method to make the string to get our file name
    varname <- paste0("Sub",subject_no,"_act", activity_no)  
    
    #we use get function to convert our string to the actual file. So that it can extract the data off the file.
    df <- get(varname)
    
    #In this case, all of our data has the markers in the same column, which is V1, so we use the which
    #function to retrive the data where we can find the string "sample"
    marker_indexes <- which(df$V1 == "Sample")
    
    #This is just to create a copy of the data
    df_copy <- df
    
    #This is to pick the starting and ending index where the data actually exists
    start_index <- marker_indexes[2]
    end_index <- marker_indexes[3]
    
    #We use this list to store our index's data for all the files.
    marker_indexes_list[[varname]] <- list(start_index = start_index,end_index = end_index)
  }
} 

#we print the data to see if we have the desired output.
print(marker_indexes_list)


##########################################################################################################


#Question 3
#Obtain the desire data. Using computational methods, use the markers obtained in
#point 2, get the data between these markers only. Save the new data in separate
#data frames. (8 points)


#We use the same loop which was used in the prvious question to access the datasets.
for (activity_no in 1:4) {
  for (subject_no in 1:3) {
    
    #We use this function to gather the name or the string of the file name
    varname <- paste0("Sub",subject_no,"_act", activity_no)
    
    #Once the file name is setup, we need to access the file 
    #using the string which we created in the last function. So we use the get function.
    df1 <- get(varname)
    
    #We use this flag variable, as we are not able to access the data inside the file directly as the
    #filename keeps changing whenever this loop runs. so we have mentioned the file as varname.
    #This function accessess the file inside the list.
    marker_indexes <- marker_indexes_list[[varname]]
    
    #we then mark our starting index and the ending index to remove the 
    #unwanted data and the markers as well, and we save them in a new variable
    new_df <- df1[(marker_indexes$start_index+2):(marker_indexes$end_index-1),]
    
    #This flag variable finds the pattern of our files and replaces the new files names
    #with a new name, but still following the same pattern. Ex - Sub3_act3_new
    #this makes us understand which is our clean data
    new_varname <- paste0(gsub("_", "_", varname), "_new")
    
    #we use the assign variable as the loop runs, it keeps putting the computed data only 
    #into one data frame, but by using this function we create new data frames, which is given new names using the previous function.
    assign(new_varname, new_df)
  }
}


##########################################################################################################


#Question 4
#Name Variables. Once you have stored your data into data frames and identified the
#data markers, name each of your variables. Using computational methods (e.g., a for loop), 
#name each variable using the data marker. You should have named each of the variables 
#from each data frame. (8 points)

#We use the same for loop to access the data
for (activity_no in 1:4) {
  for (subject_no in 1:3) {
    
    #we use the same method to make the string and get the data using get function
    varname <- paste0("Sub",subject_no,"_act", activity_no,"_new")
    df2 <- get(varname) 
    
    #We introduce the column name values for our data
    variables_names <- c("Sample","aX","aY","aZ","Temp","gX","gY","gZ")
    
    #This function setNames is a method where it sets the name for an object 
    #and returns it to the object that we assign it to.
    #I have used this to rename our column names in the datasets.
    df2 <- setNames(df2, variables_names)
    
    #We assign the column names to the datasets
    assign(varname, df2)
  }
}

#we print to see if we have the desired output.
print(Sub1_act1_new)


##########################################################################################################


#Question 5
#Remove Specific Data. For this assignment we only need the columns containing the 
#number of samples and gyroscope’s data (gX,gY,gz). Thus, maintain only the gyroscope data 
#and drop the undesired data. At the end, each data frame should have four columns only. (3 points)

#We use the same for loop to access the data
for (activity_no in 1:4) {
  for (subject_no in 1:3) {
    
    #We use the paste0 function to find the pattern of the file and get function to access the file
    varname <- paste0("Sub",subject_no,"_act", activity_no,"_new")
    df3 <- get(varname)
    
    #This variable, creates a vector where we only mention the colnames which we want to retain in the data
    keep_column <- c("Sample","gX","gY","gZ")
    
    #We use this function to keep our column names as mentioned above.
    #anything before the comma has to deal with rows and after the comma we deal with columns
    df3 <- df3[,keep_column]
    
    #We assign the column names to our datasets
    assign(varname, df3)
  }
}

#we print to see if the data is right
print(Sub3_act1_new)


##########################################################################################################


#Question 6
#Remove variables. Remove from the environment variables that are not needed, 
#maintain only the data frames after Step 5. (2 points)

#I have removed all the environment variables, now we just have the data frames to work with.
rm(activity_no,end_index,keep_column,new_varname,path,start_index,subject_no,variables_names,varname)


##########################################################################################################


#Question 7
#Remove empty cells or NAs. Make sure that there are not empty cells or NAs in your variables. 
#If so, remove the whole row that contains an empty cell or a NA; if not deeded, 
#inform that within your code using comments. (4 points)


#we pass a for loop to access the files
for (activity_no in 1:4) {
  for (subject_no in 1:3) {
    
    #We use this method to create the file name and access the file
    varname <- paste0("Sub",subject_no,"_act", activity_no,"_new")
    df_NA <- get(varname)
    
    #We have to consider that we have both NA values and empty values. First we take the empty values
    # and we change them into NA, and then we remove those NA values using the na.omit function.
    df_NA <- replace(df_NA,df_NA == "", NA )
    df_NA <- na.omit((df_NA))
    
    #We assign the variable to the file.
    assign(varname, df_NA)
  }
}

#we can print and see if our data has any NA values at all.
#Since we receive none we are good to go with the data which we have.
print(sum(is.na(Sub1_act1_new)))



##########################################################################################################



#Question 8
#Check data Structure. Make sure that the variables in the data frame are numeric, 
#if not, convert the variables into a numeric data type; you might need to remove any 
#character/symbol within the observations using computational methods. (3 points)


#We use the for loop to access the variables.
for (activity_no in 1:4) {
  for (subject_no in 1:3) {
    
    #We create the file name and then we access them
    varname <- paste0("Sub",subject_no,"_act", activity_no,"_new")
    df_class <- get(varname)
    
    #We print the structure of our files to have an idea if all the files 
    #are corrected to the desired data type.
    print(str(df_class))
    
    #In this case, since we have a lot of files that we are accessing. We use lapply function to
    #convert all our columns to numeric data type.
    #NOTE: this will give only NA values by coercion if the CSV file is not UTF 8 encoded.
    #We have to make sure that our file is UTF 8 encoded and the data is right.
    df_class[] <- lapply(df_class, as.numeric)
    assign(varname, df_class)
    
    #I did receive 2 NA values, from 2 datasets. So i have removed those values using na.omit function.
    df_class <- na.omit((df_class))
    
    #We again print to see if we have any NA values in any of the data frames.
    print(sum(is.na(df_class)))
    
  }
}


#Also changing it numeric, takes out the decimal values.


##########################################################################################################



#Question 9
#Signal Calibration. Use the first rest period of each activity to remove the 
#offset (the amount in which the signal is above the 0.0 horizontal line) of each variable. 
#You will need to compute the arithmetic mean of the rest period (e.g., mean(rest)) 
#and then subtract the mean value from the entire variable. (8 points)


#We use the for loop to access the data sets
for (activity_no in 1:4) {
  for (subject_no in 1:3) {
    
    #We use this method to create the file name and access the data set
    varname <- paste0("Sub",subject_no,"_act", activity_no,"_new")
    df_sig_cal <- get(varname)
    
    #This is the formula to identify the signal calibration, where we multiply 
    #each of our variable with 0.2
    end_rest <- as.integer(nrow(df_sig_cal)*0.2)
    
    #We define rest period, where we enter the end_rest values to the data
    rest_period <- df_sig_cal[1:end_rest, ]
    
    #We take mean value of our rest period
    gX_rest_mean <- mean(as.numeric(rest_period$gX))
    gY_rest_mean <- mean(as.numeric(rest_period$gY))
    gZ_rest_mean <- mean(as.numeric(rest_period$gZ))
    
    #Then we subtract the mean value from the entire variable.
    df_sig_cal$gX <- as.numeric(df_sig_cal$gX) - gX_rest_mean
    df_sig_cal$gY <- as.numeric(df_sig_cal$gY) - gY_rest_mean
    df_sig_cal$gZ <- as.numeric(df_sig_cal$gZ) - gZ_rest_mean
    
    #We create a new series for our data frames as we want to store the signal calbration
    #in a new data frame.
    new_varname <- paste0(gsub("_", "_", varname), "_sig_cal")
    
    #We assign this data to new data frames, so that we can compute further.
    #The new data frames are named in the following way, Sub1_act1_new_sig_cal
    assign(new_varname, df_sig_cal)
  }
}


##########################################################################################################



#Question 10
#Plot data. Explore the data by visualising its content. 
#Using the data from the four activities plot these activities in a single plot. 
#Each variable (x,y,z) should be represented with a different colour, 
#label the plot accordingly. Repeat the same procedure for the rest of the subjects, 
#at the end you should have three plots (one for each subject). Example below. (12 points)


#We create an empty data frame to store all our data of each subject
combined <- data.frame()

#we use the for loop to access the data according to our subjects
for (subject_no in 1:3) {
  
  #We create two variables to keep track of the sample numbers and the end of each activity.
  sample_no <- 0
  activity_end <- 1
  
  #we create another variable which contains all 4 activities
  activity_list <- c("Activity 1","Activity 2", "Activity 3", "Activity 4")
  
  #We create another for loop to access the activities
  for (activity_no in 1:4) {
    
    #We again use this method to create the file name and access it.
    varname <- paste0("Sub",subject_no,"_act", activity_no,"_new_sig_cal")
    df_plot <- get(varname)
    
    #We combine all the activities of the subject in a single data frame to plot them
    combined <- rbind(combined, transform(df_plot, Sample = Sample + sample_no))
    
    #We make sure that we have the maximum values of the sample number.
    sample_no <- max(combined$Sample, na.rm = TRUE)
    activity_end <- c(activity_end,sample_no)
  }
  
  #We plot the line graph, we make sure our lines are of different colors so that it is easy to distinguish them.
  #We add in aesthetic features as well.
  plot(combined$Sample, combined$gX, type = "l", col ="red", 
       main = paste0("Subject ", subject_no," - All Activities"),xlab = "Sample",
       ylab = "Rotational Velocity")
  lines(combined$Sample, combined$gY, type = "l", col ="green")
  lines(combined$Sample, combined$gZ, type = "l", col ="blue")
  
  #We use the abline function to create vertical dashes to distinguish between the activities.
  for (i in 2:4) {
    abline(v=activity_end[i],lty=2)
  }
  
  #The text function is to add labels for each activity at the midpoint of each activity.
  for (i in 1:4) {
    text(x = (activity_end[i]+activity_end[i+1])/2,y = -30000, labels = activity_list[i],cex =1 )
    abline(v=activity_end[i],lty=2)
  }
  
  #The legend function is to add the legend, so that we understand what the color of the line represents.
  legend("topleft",legend = c("gX","gY","gZ"),col = c("red","green","blue"), lty =1, cex = 0.8)
  
  #This is to reset and empty the data frame for the next subject to be performed in the loop.
  combined <-  data.frame()
}



#####################################################################################################



#Question 11
#Get only the ~30 seconds of activity. 
#Remove the rest data (~10sec) before and after the activity for each activity. (8 points)


#we use the for loop to access the data set
for (subject_no in 1:3) {
  for (activity_no in 1:4) {
    
    #We use this function to create the file name and then access it.
    varname <- paste0("Sub",subject_no,"_act", activity_no,"_new_sig_cal")
    df <- get(varname)
    
    #using nrows, we are trying to find the number of rows in each of the data set.
    df_rows <- nrow(df)
    
    #Resting rows indicate the number of rows of the rest period which we exclude. 0.2 indicates
    #20 percent of the data which we exclude from the beginning.
    resting_rows <- df_rows*0.2
    
    #The acivity rows is where the actual activity has happened, so we take 80% of that data set, excluding the end 20%
    #of the rest activity which we do not need.
    activity_rows <- df_rows*0.8
    
    #we select the data from the start to the end according to our requirement i.e, resting and activity rows.
    df <- df[resting_rows:activity_rows, ]
    
    #We print the dimension of the data to make sure of how much data we have
    print(dim(df))
    
    #We create new data sets with the obtained data and assign the new file names to it.
    new_varname <- paste0(gsub("_", "_", varname), "_only_activity")
    assign(new_varname,df)
  }
}


###############################################################################################



#Question 12
#Simple statistical metrics. Compute the mean and standard deviation for each activity 
#and for each participant. (8 points)


#We create a new data frame to store all of our mean and SD values for each variable of all activities
df_stats <- data.frame(Subject = character(),
                       Activity = character(),
                       Mean_gX = numeric(),
                       Mean_gY = numeric(),
                       Mean_gZ = numeric(),
                       SD_gX = numeric(),
                       SD_gY = numeric(),
                       SD_gZ = numeric())


#We create the same for loop to access all the files
for (subject_no in 1:3) {
  for (activity_no in 1:4) {
    
    #We create the file name and then access the data set using the get function
    varname <- paste0("Sub",subject_no,"_act", activity_no,"_new_sig_cal_only_activity")
    df <- get(varname)
    
    #First we compute the mean
    gX_mean <- mean(df$gX)
    gY_mean <- mean(df$gY)
    gZ_mean <- mean(df$gZ)
    
    #Then we compute our SD
    gX_sd <- sd(df$gX)
    gY_sd <- sd(df$gY)
    gZ_sd <- sd(df$gZ)
    
    #Once we have done the computation, we put all the data in our new file which have created.
    #We should have 12 observations in it.
    df_stats <- rbind(df_stats, data.frame(Subject = paste0("Subject ", subject_no),
                                           Activity = paste0("Activity ", activity_no),
                                           Mean_gX = gX_mean,
                                           Mean_gY = gY_mean,
                                           Mean_gZ = gZ_mean,
                                           SD_gX = gX_sd,
                                           SD_gY = gY_sd,
                                           SD_gZ = gZ_sd))
  }
    
}


#We print the data set.
print(df_stats)


####################################################################################################



#Question 13
#Plot the statistical metrics. Using a boxplot, build a plot using the mean values 
#from the subjects for each activity. You should have three boxes (x,y,z) per activity 
#in a single plot. Repeat the process and build another plot using the standard deviation. 
#Example below. (12 points for both plots)

#This box plot is for the mean data

#We create a data frame where we put data from our previous question's stat data.
#We subset the data to what we require
mean_boxplot <- subset(df_stats, select = c(Subject, Activity, Mean_gX, Mean_gY, Mean_gZ))

#We specify the columns that we are interested in and we only choose those.
#We also make sure that the data type is numeric.
cols_mean <- c("Mean_gX", "Mean_gY", "Mean_gZ")
mean_boxplot[cols_mean] <- lapply(mean_boxplot[cols_mean], as.numeric)

#We create an empty list to store the values
df_list <- list()

#We take out the unique values in the column activity
activities <- unique(mean_boxplot$Activity)

#we use a for loop to take out all the rows and columns that we are interested in and
#put them into the list.
for (activity in activities) {
  rows_mean <- mean_boxplot$Activity == activity
  df_sub <- mean_boxplot[rows_mean, cols_mean]
  df_list[[activity]] <- df_sub
}

#Once we have all the data in the list, we combine all of that into a single data frame
comb <- do.call(cbind, df_list)

#We create the box plot with the necessary aesthetics.
boxplot(comb, main = "Boxplot of Means", ylab = "Rotational Velocity", xlab = "Mean Values", col = c("red", "blue", "green"),
        legend = c("gX", "gY", "gZ"), boxwex = 0.5)
legend("topleft", legend = c("gX", "gY", "gZ"), fill = c("red", "blue", "green"), cex = 0.5)


##########################

#This box plot is for the SD data

#We create a data frame where we put data from our previous question's stat data.
#We subset the data to what we require
sd_boxplot <- subset(df_stats, select = c(Subject, Activity, SD_gX, SD_gY, SD_gZ))

#We specify the columns that we are interested in and we only choose those.
#We also make sure that the data type is numeric.
cols_sd <- c("SD_gX", "SD_gY", "SD_gZ")
sd_boxplot[cols_sd] <- lapply(sd_boxplot[cols_sd], as.numeric)

#We create an empty list to store the values
df_list <- list()

#We take out the unique values in the column activity
activities <- unique(sd_boxplot$Activity)

#we use a for loop to take out all the rows and columns that we are interested in and
#put them into the list.
for (activity in activities) {
  rows_sd <- sd_boxplot$Activity == activity
  df_sub <- sd_boxplot[rows_sd, cols_sd]
  df_list[[activity]] <- df_sub
}

#Once we have all the data in the list, we combine all of that into a single data frame
comb <- do.call(cbind, df_list)

#We create the box plot with the necessary aesthetics.
boxplot(comb, main = "Boxplot of SD", ylab = "Rotational Velocity", xlab = "SD Values", col = c("red", "blue", "green"),
        legend = c("gX", "gY", "gZ"), boxwex = 0.5)
legend("topleft", legend = c("gX", "gY", "gZ"), fill = c("red", "blue", "green"))


#########################################################################################################



#Question 14
#Using the collected data and your analysis, do you see any difference between 
#the activities? Explain your observations. Add your observations in the code 
#and not in a separated file. 

#In this data we have 12 CSV files which we have used. There were 3 subjects, who performed 4 activities each.
#The activities are listed as follows.
#Sitting
#Standing
#Typing
#walking

#in this project we mainly focus on data capture and preparation. Since the data has already been captured, we
#focus on the preparation part.
.
#In this data we have used markers to differentiate our data from the rest activity to the actual activity,
#our first task is to remove the data which is not useful to us and to remove these markers as well.
#This data consists of 8 columns, containing gyroscopic data, counts/observations, temperature etc
#For this project we need only the gyroscopic data with the number of observations.

#We are supposed to make a lot of plot for our data and also this data needs computing, so the basic thing that
#we do is remove blank data and NA values so that it becomes easy for computation

#We do deal with signal calibration well. In a simple manner, this could mean that it is technique
#depending on type of sensor or the nature that we use in. It's just adjusting the sensor signals to reomve
#errors or bias in the data.
#In this case we remove the offset from the gyroscopic data. 

#When we look at our plots, especially in the beginning where we saw the differences in 
#fluctuation in activities we noticed. Usually Activity 1 and 3 had pretty much the same results 
#as the subject had very less body movement.
#Activity 4 is where most of the activity is noticed when the subject is walking. These 
#show us a higher fluctuation in the gyroscopic data.
#It is important to note that all of these observations which we look at are based off visualization
#and they have to be interpreted cautiously.


#when it comes to these boxplots, the mean values are comparatively low for standing and sitting for gX
#and they are low for gZ as well. Only for gY they are relatively higher when it comes to standing and sitting.

#Understanding the data in question 11, where we tried to remove data according to seconds, can also be tried
#in a different manner with different combinations. As we have collected our data mostly based on observations,
#We can try other combinations to take out maybe even 15% of the data as a rest period to see how our mean and
#standard deviation is affected by it and how much of a difference it makes to our visualizations.

