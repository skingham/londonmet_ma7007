#=======================================
# Pie Chart, Bar Chart, Histogram
#=======================================

##Pie Chart with annotated %
#=======================================
slices <- c(12, 18, 5, 10)
names <- c("A", "B", "AB", "O")
percent <- round(slices/sum(slices)*100)
names <- paste(names, percent)
names <- paste(names,"%",sep="")
pie(slices,labels = names, col=rainbow(length(names)),
    main="Pie Chart of Blood Group")


# 3D Pie Chart
#=============
install.packages("plotrix")
library(plotrix)
slices <- c(12, 18, 5, 10)
names <- c("A", "B", "AB", "O")
pie3D(slices,labels=names,explode=0.1,
      main="Pie Chart of Blood Group")

# Bar Chart
#==============
# Simple Bar Chart

# Create the data for the chart
Income <- c(4,6,12,8,5,18,14,9,7,4,3,1)
# assign/give names
Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept",
       "Oct","Nov","Dec")
# Plot the bar chart 
barplot(Income,names.arg=Month,xlab="Month",ylab="Income",col="blue",
        main="Family's Monthly Income",border="red")

# Stacked Bar Chart

# Create the input vectors.
colors = c("green","blue","red")
blood_group <- c("A","B","AB","O")
class <- c("Children","Youth","Adult")

# Create the matrix of the values.
patients <- matrix(c(2,9,11,8,10,11,9,4,8,7,12,5),
                 nrow = 3, ncol = 4, byrow = TRUE)
# Create the bar chart
barplot(patients, main = "Blood Group of Patients", 
        names.arg = blood_group, xlab = "Blood Group", 
        ylab = "No of Patients", col = colors)

# Add the legend to the chart
#legend("topleft", class, cex = 1.3, fill = colors)

# Histogram
#==================

# enter the values in R
Age_in_years<-c(60,68,63,56,90,64,74,57,65,85,83,88,74,60,
          85,72,89,64,65,75,65,70,66,65,55,75,82,54,
          60,68,60,59,55,52,55,60,57,80,45,44,57,80,
          53,56,55,57,55,48,60,82,65,45,49,55,75,62,
          60,60,68,58,48,60,62,70,56,49,60,50,76,54,
          74,66,48,51,55,50,69,52,42,50,65,58,54)
# Draw the histogram
hist(Age_in_years)

# Draw a colored histogram with specifications
hist(Age_in_years,
     main="Age of Patients in a Doctor's Clinic",
     xlab="Age of Patients (in Years)",
     xlim=c(40,90),
     col="darkmagenta",
     freq=TRUE)
