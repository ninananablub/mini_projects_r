setwd("/Users/Acer/Documents/CMSC 197/CMSC197_R")
getwd()

##PROBLEM NUMBER ONE
pollutantMean <- function(directory, pollutant, id = 1:332){
  ans <- numeric()
  ##creating an empty numeric vector "ans". 
  data <- list.files(path = "/Users/Acer/Documents/CMSC 197/CMSC197_R/specdata", full.names=T)
  ##identifying the list of files in the directory "specdata"
  ##assigning the list of csv files in the specdata directory in the variable "data" using the list.files function, which takes, the path of the directory of the files as the first argument, and the logical value full.names=TRUE which gets path and file in the  directory given in the first argument. 
  
  for(i in id){ ##for loop runs through monitor IDs
    read <-  (read.csv(data[i])) ## as loop runs through monitor IDs it is continuously reading the particulate matter data.
    ans <- c(ans, read[[pollutant]])##updating variable "ans" with the pollutant element accessed/read from the file.
  }
  mean(ans, na.rm=T) ##computing the mean for variable "ans" in [,pollutant] the single bracket is used to access a list within a single element, argument na.rm=T tells the code to not include NAs in getting the mean.
}

pollutantMean("specdata", "sulfate", 1:10)
pollutantMean("specdata", "nitrate", 70:72) 
pollutantMean("specdata", "nitrate", 23)



##PROBLEM NUMBER TWO
complete <- function(directory, id = 1:332){
  nobs <- numeric() ##creating empty numeric vector "nobs" to be stored by number of observations later on.
  data <- list.files(path = "/Users/Acer/Documents/CMSC 197/CMSC197_R/specdata", full.names=TRUE)
  for(i in id){ 
    add <- sum(complete.cases(read.csv(data[i]))) ##as loop runs through monitoring location (ID) this will access observations for which both sulfate and nitrate have been recorded.
    nobs <- c(nobs, add)##updating vector nobs that contains the number of complete observations
  }
  data.frame(id,nobs) ##code will return an output of a data frame that displays the monitoring location (id) and the number of complete observations(both sulfate and nitrate have been recorded.)
}

complete("specdata", 1)
complete("specdata", 30:25)
complete("specdata", c(2,4,8,10,12))
complete("specdata", 3)




##PROBLEM NUMBER THREE
corr <- function(directory, threshold = 0){
  result<-c() ##creating empty numeric vectore
  data <- list.files(path = "/Users/Acer/Documents/CMSC 197/CMSC197_R/specdata", full.names=TRUE)
  frame <- complete("/Users/Acer/Documents/CMSC 197/CMSC197_R/specdata")##calling complete function and assigning it to variable frame. using complete function in accessing completely observed cases by monitoring IDs
  
  for(i in id){ ##as for loop runs through IDs it reads csv files and make a selection of only those complete cases within the accessed data file  
    read<-read.csv(data[i])[complete.cases(read.csv(data[i])),]
    result<-c(result,cor(read$sulfate,read$nitrate))##using the cor() function in computing the correlation between sulfate and nitrate, by first getting the subset sulfate and nitrate from the data file accessed in the variable read
  } 
  return(result)
}
cr <- corr("specdata", 150)
head(cr); summary(cr)

cr <- corr("specdata", 400)
head(cr); summary(cr)

cr <- corr("specdata", 5000)
head(cr); summary(cr); length(cr)

cr <- corr("specdata")
head(cr); summary(cr); length(cr)



##PROBLEM NUMBER FOUR
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")##reading/accessing csv file 'outcome-of-care-measures.csv'
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11]) ##in the csv file "outcome-of-care-measures" the data for Hospital 30-Day Death (Mortality) Rates from Heart Attack are listed on column 11, using the is.numeric this line  takes  object that needs to be coerced and returns the converted numeric value.
hist(outcome[, 11], main="Hospital 30-Day Death (Mortality) Rates From Heart Attacks ",xlab="Deaths", border="black", col="sky blue")
##"main = "Hospital 30-Day Death (Mortality) Rates From Heart Attacks"" will display the header of the title of the histogram which is, "Hospital 30-Day Death (Mortality) Rates From Heart Attacks ", "xlab = "Deaths" " defines that the x-axis of the graph are number of deaths, "border = black" creates a color black border of the the columns of the histogram, and "col="sky blue"" fills the columns of the histogram with the a sky blue color.
