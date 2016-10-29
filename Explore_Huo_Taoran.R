library(ggplot2)
library(grid)

#Write	a	function	called	explore	that	accepts the	following	
#parameters,	with	defaults	for	the	last	three	parameters:
#1. A dataframe
#2. A	plot	switch	that	can	accept	three	values:	off,	on,	or	grid
#3. A	threshold	cut-off	value between	0	and	1 for	correlations
#4. An optional	vector	that	contains	one	or	more	integers	that	
#represent	the	numbers	of	bins	to	use	for	a	histogram.	If	the	
#vector	is	not	provided,	then	let	ggplot	use	it’s	default.

explore <- function(data, plotswitch = "off", threshold = 0, vector = NULL) {
  
  Freq_table <- freq_table (data) #frequency table
  Summary_num <- summary_num (data) #statistics table
  R_squared <- r_squared (data) #r-sqaured values
  Pearson_coe <- pearson_coe (data, threshold) #pearson correlation coefficient
  plot_density_count(data,plotswitch,vector) #plot density and count histograms
  plot_gray (data, plotswitch) #plot gray bar
  new_result=list(Freq_table, Summary_num, R_squared, Pearson_coe) #combine data results
  
  return (new_result)
  
}


#1
freq_table <- function(data){
 
  data_cat <- c(data[sapply(data,is.factor)],data[sapply(data,is.logical)])
  #select categorical and logical variable
  return (sapply(data_cat,table)) #make a table
}

##2
#a)
summary_num <- function(data){

  data_num=data[sapply(data,is.numeric)] #select numeric data
  return (summary(data_num)) #summary statistics
}

#b).
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

#c).
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

#3.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  plots <- c(list(...), plotlist)
  # Create a list 'plots' using ... and plotlist
  numPlots = length(plots)
  if (is.null(layout)) {
    # If layout is NULL, then use 'cols' to determine layout
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    # ncol=number of columns in plots
    # nrow=number of rows needed, calculated from # of cols
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Plot each in the correct location
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      #the position that contain this subplot
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


plot_density_count <- function(data,plotswitch='off',vector=NULL){

  num=data[sapply(data,is.numeric)]
  if(plotswitch == "on"){
    if(!is.null(vector)){ # if vector is NULL
      for(j in 1:length(vector)){ 
        for(i in 1:ncol(num)){
          mean <- mean(num[,i]) 
          # caculate the mean of each numeric column
          p1 <- ggplot(num,aes(x=num[i]),color = "blue")+ 
            #draw the histogram of count
            geom_histogram(fill="blue",bins=vector[j])+
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red")  
          #geom_vline can add red line on it
          
          p2 <- ggplot(num,aes(x=num[i],..density..))+
            #draw the density histogram
            geom_histogram(fill="blue",bins=vector[j])+
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red") 
          
          grid.newpage()
          #new page
          pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
          title <- paste(colnames(num[i]),vector[j],sep=" bin=")
          grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
          print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
          print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))#print p1 and p2 two histograms
          
        }
      }
    }else{ #if vector isn't NULL
      for(i in 1:ncol(num)){
        mean <- mean(num[,i]) 
        #caculate the mean of each numeric column
        p1 <- ggplot(num,aes(x=num[i]),color = "blue")+  
          geom_histogram(fill="blue")+
          #draw the histogram of count
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        p2 <- ggplot(num,aes(x=num[i],..density..))+
          #draw the density histogram
          geom_histogram(fill="blue")+
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        title <- paste(colnames(num[i]),"default bins",sep=" bins=")
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))#print p1 and p2 two histograms
        
      }
      
    }
    
  }else{
    if(plotswitch == "grid"){#  plotswitch can also = 'grid'
      for(j in 1:length(vector)){
        grid.newpage()
        his_count <-list()   
        his_density <- list()  
        #create two empty list
        for(i in 1:ncol(num)){
          his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(fill="blue", bins = vector[j])+ 
            labs(title= paste(vector[j], "bins")) 
          #draw histograms of count and add them to list his_count
        }
        multiplot(plotlist = his_count, cols = 2)  
        #draw all histogram with same bins in one page
        for(i in 1:ncol(num)){
          his_density[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(aes(y= ..density..), fill="blue", bins = vector[j])+ 
            labs(title= paste(vector[j], "bins")) 
          #draw histograms of density and add them to list his_density 
        }
        multiplot(plotlist = his_density, cols = 2)  
        #similar to above, draw all histogram of density with same bins in one page
      }
    }
  }
}


#4.
is.binary <- function(v) {

  x <- unique(v)                    
  #x contains all unique values in v
  length(x) - sum(is.na(x)) == 2L         
  #check to see if x only contains 2 distinct values
}


plot_gray <- function(data, plotswitch='off') {

  dfm_cb <- data[,sapply(data,is.factor)|sapply(data,is.logical)|sapply(data,is.binary)]
  #select categorical and binary variable
  if(plotswitch=="on"|plotswitch=="grid"){
    for(i in 1:ncol(dfm_cb)){
      p <- ggplot(dfm_cb,aes(x=dfm_cb[,i]),colour="gray")+
        geom_bar()+ xlab(colnames(dfm_cb[i]))
      #plot gray bar for every categorial and binary variable
      print(p)
    }
  }
}