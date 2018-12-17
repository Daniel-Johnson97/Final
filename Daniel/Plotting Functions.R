library(dplyr)
library(ggplot2)
library(tidyr)

#####Plotting the change in the mean of a variable across years in the survey for a given country######
get_means_graph_country.Survey <- function(df, country, var){
  u_years <- df$year %>% unique 
  
  #Make a for loop for the mean of every year in a given variable
  mean_vec <- rep(NA, length(u_years))
  
  for (i in 1:length(u_years)){
    cy <- u_years[i]
    
    mean <- get_mean_variable_country.Survey(df, cy, country, var)
    mean_vec[i] <- mean
  }
  
  #Now graphing the results
  mean_df <- data.frame("Year"=u_years,
                        "Mean"=mean_vec,
                        stringsAsFactors = F)
  p <- ggplot()
  p <- p + geom_point(mapping=aes(x=Year, y=Mean), data=mean_df)
  p <- p + geom_line(mapping=aes(x=Year, y=Mean), data=mean_df)
  p <- p + ggtitle("Mean by Year")
  print(p)
}

#####Plotting the change in the mean of a variable across years in the survey for a given region######
get_means_graph_region.Survey <- function(df, region, var){
  u_years <- df$year %>% unique 
  
  #Make a for loop for the mean of every year in a given variable
  mean_vec <- rep(NA, length(u_years))
  
  for (i in 1:length(u_years)){
    cy <- u_years[i]
    
    mean <- get_mean_variable_region.Survey(df, cy, region, var)
    mean_vec[i] <- mean
  }
  #Making a title 
  title <- paste("Mean by Year for", var)
  
  #Now graphing the results
  mean_df <- data.frame("Year"=u_years,
                        "Mean"=mean_vec,
                        stringsAsFactors = F)
  p <- ggplot()
  p <- p + geom_col(mapping=aes(x=Year, y=Mean), data=mean_df, fill="orange")
  p <- p + ggtitle(title)
  print(p)
}

#######Function that plots and returns a regression table for a regression of a given value within some kind of group##########
  #Group examples may be region, income group, country, etc. 
get_regression.Survey <- function(df, group_var, group_val, y_var, x_var){
  #Reduce the data set to only those in the desired group
  df_group <- df[df[[group_var]]==group_val,]
  
  #Making the x and y variables for the regression
   y <- df_group[y_var]
   y <- y[,1]
   x <- df_group[x_var]
   x <- x[,1]
   
   df_reg <- data.frame("x"=x,
                        "y"=y,
                        stringsAsFactors = F) %>%
            drop_na(x,y)
  
  #Regressing and returning statistical significance
  fit <- lm(y ~ x, data=df_reg)
  fit_sum <- summary(fit)
  
  #Creating a title for the graph 
  title <- paste("Regression of", y_var, "") 
  title <- paste(title, "and", x_var)

  #Plotting the values 
  lmp <- ggplot()
  lmp <- lmp + geom_point(mapping=aes(x=x, y=y), data=df_reg)
  lmp <- lmp + geom_smooth(mapping=aes(x=x, y=y), data=df_reg, method="lm")
  lmp <- lmp + ylab(y_var) + xlab(x_var) + ggtitle(title)
  return(list(fit_sum, lmp))
}

#########Plotting two variables against each other, by a given group#############
graph_two_variables.Survey <- function(df, yvar, xvar, group){
  x <- df[[xvar]]
  y <- df[[yvar]]
  group <- df[[group]]

  graph_df <- data.frame("x"=x,
                         "y"=y,
                         "group"=group,
                         stringsAsFactors = F) %>% 
              drop_na()

  title <- paste(yvar, "vs.", xvar)
  
  p <- ggplot()
  p <- p + geom_col(mapping=aes(x=x, y=y, col=group), data=graph_df)
  p <- p + ggtitle(title) + xlab(xvar) + ylab(yvar)
  print(p)
}