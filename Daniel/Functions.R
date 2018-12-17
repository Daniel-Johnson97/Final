######Package for reading in Stata .dta files######
library(readstata13)

#####Function to create an object of the survey data######
get_survey_df <- function(){
  df <- read.csv("Processed_WBES.csv", header=T, stringsAsFactors = F)
  
  return(df)
}

######Function that returns all the data for a given country in the survey#######
get_country.Survey <- function(df, country){
  df <- df[df$countryname==country,]
  return(df)
}

######Function that returns all the data for a given region in the survey#######
get_region.Survey <- function(df, region){
  df <- df[df$region==region,]
  return(df)
}

######Function that returns all the data for a given year in the survey#########
get_year.Survey <- function(df, s_year){
  df <- df[df$year==s_year,]
  return(df)
}

######Function that returns a vector of the observations for all the firms in a given country in a given year for a given variable##########
get_obs_country_year.Survey <- function(df, s_year, country, var){
  df_temp <- get_country.Survey(df, country)
  df_temp <- get_year.Survey(df_temp, s_year)
  
  df_temp <- df_temp[var]
  return(df_temp)
}

######Function that returns a vector of the observations for all the firms in a given region in a given year for a given variable##########
get_obs_region_year.Survey <- function(df, s_year, region, var){
  df_temp <- get_region.Survey(df, region)
  df_temp <- get_year.Survey(df_temp, s_year)
  
  df_temp <- df_temp[var]
  return(df_temp)
}

######Calculating the mean value of a variable for a given country and year#########
get_mean_variable_country.Survey <- function(df, s_year, country, var){
  df_temp <- get_obs_country_year.Survey(df, s_year, country, var)
  vec_temp <- df_temp[,1]
  
  mean <- mean(vec_temp, na.rm=T)
  return(mean)
}

######Calculating the mean value of a variable for a given region and year#########
get_mean_variable_region.Survey <- function(df, s_year, region, var){
  df_temp <- get_obs_region_year.Survey(df, s_year, region, var)
  vec_temp <- df_temp[,1]
  
  mean <- mean(vec_temp, na.rm=T)
  return(mean)
}


#######Function for conducting a multivariate linear regression#########
get_lm_multi.Survey <- function(df, group_var, group_val, y_var, x_var1, x_var2, x_var3, x_var4, x_var5, x_var6){
  df_group <- df[df[[group_var]]==group_val,]
  
  #Making the x and y variables for the regression
  y <- df_group[y_var]
  y <- y[,1]
  x1 <- df_group[x_var1]
  x1 <- x1[,1]
  
  x2 <- df_group[x_var2]
  x2 <- x2[,1]
  
  x3 <- df_group[x_var3]
  x3 <- x3[,1]
  
  x4 <- df_group[x_var4]
  x4 <- x4[,1]
  
  x5 <- df_group[x_var5]
  x5 <- x5[,1]
  
  x6 <- df_group[x_var6]
  x6 <- x6[,1]
  
  df_reg <- data.frame("x1"=x1,
                       "y"=y,
                       "x2"=x2,
                       "x3"=x3,
                       "x4"=x4,
                       "x5"=x5,
                       "x6"=x6,
                       stringsAsFactors = F) %>%
    drop_na()
  #Running the regression
  fit <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data=df_reg)
  
  fit_sum <- summary(fit)
  
  return(fit_sum)
}

####A function that finds the regressions for every member of a particular group####
get_every_reg.Survey <- function(df, group, y, x1, x2, x3, x4, x5, x6){
  u_values <- df[[group]] %>% unique
  
  output <- list()
  
  for (i in 1:length(u_values)){
    cm <- u_values[i]
    
    output[[i]] <- get_lm_multi.Survey(df, group, cm, y, x1, x2, x3, x4, x5, x6)
  }
  names(output) <- u_values
  return(output)
}

###A function that runs a regression on the entire data set###
get_overall_reg <- function(df, y, x1, x2, x3, x4, x5, x6, x7){
  #df_temp <- df[c(y, x1, x2, x3, x4, x5, x6, x7)] %>% drop_na
  Y<- df[y]
  Y <- Y[,1]
  X1 <- df[x1]
  X1 <- X1[,1]
  
  X2 <- df[x2]
  X2 <- X2[,1]
  
  X3 <- df[x3]
  X3 <- X3[,1]
  
  X4 <- df[x4]
  X4 <- X4[,1]
  
  X5 <- df[x5]
  X5 <- X5[,1]
  
  X6 <- df[x6]
  X6 <- X6[,1]
  
  X7 <- df[x7]
  X7 <- X7[,1]
  df_reg <- data.frame("X1"=X1,
                       "Y"=Y,
                       "X2"=X2,
                       "X3"=X3,
                       "X4"=X4,
                       "X5"=X5,
                       "X6"=X6,
                       "X7"=X7,
                       stringsAsFactors = F) %>%
    drop_na()
  fit <- lm(Y ~ X1 + X2 + X3 +X4 + X5 + X6 + X7, data=df_reg)
  fit_sum <- summary(fit)
  return(fit_sum)
}