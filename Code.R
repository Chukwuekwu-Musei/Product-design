

##################################################
## Conjoint Analysis in R #
##################################################

# Load library and Set Seed to an integer
library(conjoint)
set.seed(1845)

## Set up attributes and levels as a list from the data given
attrib.level <- list(Profile = c("Apple", "Lenovo", "Dell", "Acer"),
                     HardDrive = c("128 GB", "256 GB", "512 GB"),
                     RAM = c("2 GB", "4 GB", "8 GB", "16 GB"),
                     Screensize = c("12.1 in", "15.4 in", "17.3 in"),
                     Price = c("$900", "$1200", "$1500", "$2000"))

####import already generated product profiles
design <-read.csv(file.choose())


preff <- read.csv(file.choose()) ## Choose the file named conjoint_preferences.csv

#transpose survey data
pref <- t(preff)


attrib.vector <- data.frame(unlist(attrib.level,use.names=FALSE))
colnames(attrib.vector) <- c("levels")
part.worths <- NULL
for (i in 1:ncol(pref)){
  temp <- caPartUtilities(pref[,i], design, attrib.vector)
  Base_Profile <- temp[,"Apple"]; Base_HardDrive <- temp[,"128 GB"]; Base_RAM <- temp[,"2 GB"]
  Base_Screensize <- temp[,"12.1 in"]; Base_Price <- temp[,"$900"]
  temp[,"intercept"] <- temp[,"intercept"] - Base_Brand - Base_HardDrive - Base_RAM -
    Base_Screensize - Base_Price
  L1 <- length(attrib.level$Brand) + 1 ## Add 1 for the intercept
  for (j in 2:L1){temp[,j] <- temp[,j] - Base_Brand}
  L2 <- length(attrib.level$HardDrive) + L1
  for (k in (L1+1):L2){temp[,k] <- temp[,k] - Base_HardDrive}
  L3 <- length(attrib.level$RAM) + L2
  for (l in (L2+1):L3){temp[,l] <- temp[,l] - Base_RAM}
  L4 <- length(attrib.level$Screensize) + L3
  for (m in (L3+1):L4){temp[,m] <- temp[,m] - Base_Screensize}
  L5 <- length(attrib.level$Price) + L4
  for (n in (L4+1):L5){temp[,n] <- temp[,n] - Base_Price}
  part.worths <- rbind.data.frame(part.worths,temp)
}

rownames(part.worths) <- colnames(pref)

## Export part-worths from analysis
write.csv(part.worths, file.choose(new=TRUE), row.names = FALSE) ## Name the file conjoint_partworths.csv



##########################################
## Principal Component Analysis in R #
############################################
install.packages("data.table")

## Load Packages and Set Seed
library(data.table)
set.seed(1845)

## Read in perception and preference data
perc <- read.csv(file.choose()) ## Choose perceptions.csv file
prefc <- read.csv(file.choose()) ## Choose preferences.csv file

## Run Princple Components Analysis on Perceptions
pca <- prcomp(perc[,2:length(perc)], retx=TRUE, scale=TRUE)

## Perceptual Map Data - Attribute Factors and CSV File
attribute <- as.data.table(colnames(perc[,2:length(perc)])); setnames(attribute, 1, "Attribute")
factor1 <- pca$rotation[,1]*pca$sdev[1]; 
factor2 <- pca$rotation[,2]*pca$sdev[2]; 
path <- rep(1, nrow(attribute))

pca_factors <- subset(cbind(attribute, factor1, factor2, path), select = c(Attribute, factor1, factor2, path))

pca_origin <- cbind(attribute, factor1 = rep(0,nrow(attribute)), factor2 = rep(0,nrow(attribute)), path = rep(0,nrow(attribute)))

#combining two pca files
pca_attributes1 <- rbind(pca_factors, pca_origin)

#writing and creating a csv file for perception attribute
write.csv(pca_attributes1, file = file.choose(new=TRUE), row.names = FALSE) ## Name file perceptions_attributes1.csv

###############################################
## Perceptual and Preference Mapping #
################################################


## Perceptual Map Data - Brand Factors and CSV File
score1 <- (pca$x[,1]/apply(abs(pca$x),2,max)[1])
score2 <- (pca$x[,2]/apply(abs(pca$x),2,max)[2])
pca_scores1 <- subset(cbind(perc, score1, score2), select = c(Profile, score1, score2))
write.csv(pca_scores1, file = file.choose(new=TRUE), row.names = FALSE) ## Name file perceptions_scores1.csv

## Preference Map Data - Respondent Preferences and CSV File
prefa <- data.matrix(prefc[,2:ncol(prefc)])%*% (cbind(score1,score2))

prefa[,1] <- (prefa[,1]/max(abs(prefa[,1]))); prefa[,2] <- (prefa[,2]/max(abs(prefa[,2])))
preferences1 <- subset(cbind(prefc, prefa, preference = rep(1,nrow(prefc))), select = c(Customer, score1, score2))
write.csv(preferences1, file = file.choose(new=TRUE), row.names = FALSE) ## Name file preference_scores1.csv
