library(ggplot2)

#1.Create a frequency table for every categorical and logical variable
freq_table <- function(data){
  data_cat <- c(data[sapply(data,is.factor)],data[sapply(data,is.logical)])
  #select categorical and logical variable
  return (sapply(data_cat,table)) #create a table
}

#2
#a).Create a summary statistics table for each numerical variable
summary_num <- function(data){
  data_num=data[sapply(data,is.numeric)] #select numeric data
  return (summary(data_num)) #summary statistics
}

#b).Create a data frame that contains each pair of column names in the first column (name the column "Variable Pairs") and the
#associated r-square value in the second column (name the column "R-Square").
r_squared <- function(data) {
  data_num <- data[sapply(data, is.numeric)] # Select numeric data
  colname <- colnames(data_num) # extract column names
  pairwise_rsquared <- c() # new empty r-squre data
  pairwise_names <- c() # new empty pairnames
  for (i in 1:(length(colname)-1)) {
    for (j in (i+1):length(colname)) { #two column names
      num_rsqaured <- summary(lm(data_num[,i]~data_num[,j]))$r.squared
      # get r-squared data using linear model r.squared
      pairwise_names <- c(pairwise_names, paste(colname[i], colname[j], sep="-"))
      # add pairnames to pairwise_names
      pairwise_rsquared <- c(pairwise_rsquared, num_rsqaured)
      # add r-squared data to pairwise_r_squared
    }
  }
  data_rsquared <- data.frame(pairwise_names, pairwise_rsquared)
  colnames(data_rsquared) <- c("Variable Pairs", "R-squared")
  return (data_rsquared)
}  

#c).Create Pearson Correlation Coefficient
pearson_coe <- function(data, threshold = 0) {
  data_num <- data[sapply(data, is.numeric)] # select numeric data
  comb_names <- combn(colnames(data_num), 2) # combinations of all names
  pairwise_names <- paste(comb_names[1,], comb_names[2, ], sep = "-") 
  # add "-" in names e.g. x-y
  temp <- cor(data_num, method = "pearson")
  # derive pearson correlation coefficient data using cor function
  cor_data <- temp[which(lower.tri(temp))]  
  # use data in lower triangular of matrix to aviod double-use same data
  dfm_new <- data.frame(pairwise_names, cor_data)
  # create a new dataframe data_coe
  dfm_new <- subset(dfm_new, abs(cor_data) > threshold)
  # select absolute value of correlation greater than threshold
  colnames(dfm_new) <- c("Variable Pairs", "Pearson Exceeds Threshold")
  return(dfm_new)
}
