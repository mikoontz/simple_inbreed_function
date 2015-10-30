### Title: Test case for using inbreeding function
###
### Author: Michael Koontz
### Email: mikoontz@gmail.com
###
### Date Created: 20150902
### Last Updated: 20150902
###
### Purpose: Demonstrate the inbreeding function

# This data frame represents the starting population size for 20 populations with different introduction regimes. Column titles are the filial generation number with 0 being the parental generation. Totals include any migrants

# E.g. population 1 was founded with 20 beetles, the population size dropped to 8, and 8 beetles were used to start generation 1. Population 61 was introduced with 10 beetles, the population size dropped to 10, 10 more beetles were added, and 20 indivuals were used to start generation 1.

rm(list=ls())
source("/Users/mikoontz/GitHub/simple_inbreed_function/simple inbreed function.R")

Nt <- structure(
list(
  ID = c(1L, 2L, 3L, 4L, 5L, 61L, 62L, 63L, 64L, 65L, 121L, 122L, 123L, 124L, 125L, 181L, 182L, 183L, 184L, 185L), 
  `0` = c(20L, 20L, 20L, 20L, 20L, 10L, 10L, 10L, 10L, 10L, 5L, 5L, 5L, 5L, 5L, 4L, 4L, 4L, 4L, 4L),
  `1` = c(8, 15, 10, 12, 8, 20, 22, 10, 10, 21, 15, 11, 7, 10, 10, 6, 4, 5, 6, 7),
  `2` = c(19, 29, 16, 29, 17, 27, 51, 6, 9, 33, 45, 28, 9, 55, 53, 20, 19, 16, 19, 17), 
  `3` = c(29, 14, 28, 33, 25, 30, 31, 13, 7, 55, 28, 57, 10, 39, 32, 17, 30, 19, 19, 9),
  `4` = c(36, 8, 10, 26, 21, 21, 23, 5, 6, 9, 20, 5, 15, 6, 27, 15, 13, 31, 10, 15), 
  `5` = c(40,17, 4, 51, 37, 41, 25, 4, 2, 14, 34, 5, 13, 6, 27, 20, 19, 18, 13, 19),
  `6` = c(79, 4, 0, 117, 44, 58, 43, 2, 0, 31, 65, 8, 52, 33, 23, 56, 64, 43, 31, 51),
  `7` = c(89, 8, 0, 44, 43, 37, 62, 2, 0, 33, 66, 16, 56, 63, 29, 32, 41, 78, 31, 45),
  `8` = c(29, 3, 0, 31, 22, 20, 28, 0, 0, 7, 31, 0, 25, 13, 43, 10, 16, 34, 14, 40),
  `9` = c(37, 9, 0, 47, 26, 48, 64, 0, 0, 0, 42, 0, 15, 8, 41, 6, 24, 6, 8, 30)), 
.Names = c("ID", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), 
row.names = c(1L, 2L, 3L, 4L, 5L, 61L, 62L, 63L, 64L, 65L, 121L, 122L, 123L, 124L, 125L, 181L, 182L, 183L, 184L, 185L), 
class = "data.frame")

#------------------------#

head(Nt)

# Get the attributes of each population

attributes <- structure(
list(
  ID = c(1L, 2L, 3L, 4L, 5L, 61L, 62L, 63L, 64L, 65L, 121L, 122L, 123L, 124L, 125L, 181L, 182L, 183L, 184L, 185L
), 
  number = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L), 
  size = c(20L, 20L, 20L, 20L, 20L, 10L, 10L, 10L, 10L, 10L, 5L, 5L, 5L, 5L, 5L, 4L, 4L, 4L, 4L, 4L), 
  environment = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
    .Label = c("fluctuating", "stable"), class = "factor")), 
.Names = c("ID", "number", "size", "environment"), 
row.names = c(1L, 2L, 3L, 4L, 5L, 61L, 62L, 63L, 64L, 65L, 121L, 122L, 123L, 124L, 125L, 181L, 182L, 183L, 184L, 185L), 
class = "data.frame")

head(attributes)

#-----------------------#


# Set up the storage data frame. Just copy all of the ID's over to all of the columns for now; the columns other than the ID column will be overwritten
inbreed.mat <- matrix(Nt$ID, nrow=nrow(Nt), ncol=ncol(Nt))
inbreed.mat <- as.data.frame(inbreed.mat)
names(inbreed.mat) <- names(Nt)

# Go through each row of the Nt dataframe and calculate the inbreeding coefficient through the time series. Subtract from 1 to get expected amount of heterozygosity remaining

inbreed.mat[, 2:ncol(inbreed.mat)] <- 1 - inbreed_noMigrants(N=Nt[, 2:ncol(Nt)])
head(inbreed.mat)



# Combine the attributes data with the inbreeding coefficient data
data <- merge(attributes, inbreed.mat, by="ID")
head(data)

#----------
# Aggregate as desired
#----------
# Aggregate the expected loss of heterozygosity values in inbreed.mat (take away first column representing the ID)

inbred.mean <- aggregate(
  inbreed.mat[,-1], 
  by=list(propagule.number=data$'number'), 
  FUN=function(x) mean(x, na.rm=TRUE) )

inbred.mean
