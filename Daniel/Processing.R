library(dplyr)
library(readstata13)
######Processing the data#############
####Adding a variable for the size of the firm####
df <- read.dta13("wbesDEC09.dta")

##l1 is the variable name for the number of employees the firm had##
n_employ <- df$l1

#Classifying firms#
  #Any firm with less than 20 is small, 20-99 is medium, and 100+ is large
firmsize <- rep(NA, length(n_employ))

for (i in 1:length(n_employ)){
  cf <- n_employ[i]
  
  if (cf < 20 & is.na(cf) !=T){
    firmsize[i] <- 1
  } else if (cf > 19 & cf < 100 & is.na(cf) !=T){
    firmsize[i] <- 2
  } else if (cf > 99  & is.na(cf) !=T) {
    firmsize[i] <- 3
  } else if (is.na(cf)==T){
    firmsize[i] <- 0
  }
}

###Now adding the firmsize variable into the original data set 
df$firmsize <- firmsize


###Creating a function to add columns of dummy variables to the dataset 
make_dummy_per <- function(df, var, cutoff){
  per <- df[var]
  per <- per[,1]

  variable <- rep(NA, length(per))

  for (i in 1:length(variable)){
    cf <- per[i]
  
    if (cf > cutoff & is.na(cf)==F){
      variable[i] <- 1
    } else if (cf <= cutoff & is.na(cf)==F){
      variable[i] <- 0
    } else {
      variable[i] <- NA
    }
  }
  return(variable)
}

####Creating a dummy variable for if a firm is foreign owned or not####
##Variable b2b designated the percentage of the firm owned by foreign entities 
  #So here I classify anything where b2b > 0 as foreign owned 
df$for_own <- make_dummy_per(df, "b2b", 0)

##Now for if the firm is an exporter 
df$exporter <- make_dummy_per(df, "d3c", 5)

###Creating a dummy variable for if a firm has experienced any crime###
##Variable i3 designates if a firm has experienced crime or not 
crime <- df$i3 %>% as.vector()

output <- rep(NA, length(crime))

for (i in 1:length(crime)){
  cf <- crime[i]

  if (cf=="Yes" & is.na(cf)==F){
    output[i] <- 1
  } else if (cf=="No" & is.na(cf)==F) {
    output[i] <- 0
  } else {
    output[i] <- NA
  }
}
df$victim <- output

#############Working in the polity scores#####################
library(tidyr)
polity <- read.csv("Polity.csv", header=T, stringsAsFactors = F)
polity <- polity[polity$year==2017,]

polity_s <- polity$polity

###Adding the polity scores based on country###
country_survey <- df$countryname
Polity_Score <- rep(NA, length(country_survey))

for (i in 1:length(polity_s)){
  cc <- polity$country[i]
  
  cpolity <- polity[polity$country==cc,]
  cpolity <- cpolity[["polity"]]
  
  for (k in 1:length(country_survey)){
    cur_country <- country_survey[k]
    
    if (cur_country==cc){
      Polity_Score[k] <- cpolity
    } else {}
  }
}
df$Polity <- as.numeric(Polity_Score)

#########Function for making an interaction term#########
make_int_term <- function(df, var1, var2){
  variable1 <- df[[var1]]
  variable2 <- df[[var2]]
  
  int_term <- variable1*variable2
  return(int_term)
}

###Making a foreign own and victim interaction term###
df$ForxPol <- as.numeric(make_int_term(df, "for_own", "Polity"))

####Writing in the data to the processed csv####
write.csv(df, "Processed_WBES.csv")