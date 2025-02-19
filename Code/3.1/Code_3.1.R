start_time <- Sys.time() 

################### Load the packages ###################
library(MonteCarloSEM)
library(polycor)
library(psych)
library(dplyr)
library(ggplot2)

################### Function Defination ###################

## Define a function used to generate the threshold values corresponding to different situations
divide_func <- function(sign, mean_value, std_value, num_categories){
  
  
  if(sign == 3){ # if sign == 3, the function is used to deal with symmetric situation
    threshold_points_results <- c(mean_value-3 * std_value) # the range between the negative infinity and (miu - 3 * std) as the the first interval
    interval <- 6*std_value / (num_categories-2)
    for(k in 1:(num_categories-2-1)){ # num_categories-2-1 is used to get the number of internal threshold points
      threshold_points_results <- c(threshold_points_results,mean_value-3 * std_value + k*interval )
    }
    threshold_points_results <- c(threshold_points_results,mean_value+3 * std_value)
  }else if(sign == 1){# if sign == 1, the function is used to deal with positive asymmetric situation
    threshold_points_results <- c(mean_value-3 * std_value) # the range between the negative infinity and (miu - 3 * std) as the the first interval
    interval <- 3*std_value / (num_categories-2)
    for(k in 1:(num_categories-2-1)){ # num_categories-2-1 is used to get the number of internal threshold points
      threshold_points_results <- c(threshold_points_results,mean_value-3 * std_value + k*interval )
    }
    threshold_points_results <- c(threshold_points_results,mean_value)
  }else if(sign == 2){ # if sign == 2, the function is used to deal with negative asymmetric situation
    threshold_points_results <- c(mean_value)
    interval <- 3*std_value / (num_categories-2)
    for(k in 1:(num_categories-2-1)){ # num_categories-2-1 is used to get the number of internal threshold points
      threshold_points_results <- c(threshold_points_results,mean_value + k*interval )
    }
    threshold_points_results <- c(threshold_points_results,mean_value+3 * std_value)
  }
  
  
  return (threshold_points_results)
}



## Define a function used to transform continuous data into categories.

transform <- function(value,threshold_points,num_categories){
  for(i in 1: (num_categories-1)){
    if(value < threshold_points[i]){
      return(i)
    }
  }
  return(num_categories)
}


## Define a function used to get the accurat EFA Performance

RMSE_func <- function(matrix_1, matrix_2){
  dimension <- dim(matrix_1)
  seq_vec <- c()
  rmse_vec <- c()
  meta_vector <- c(1:dimension[2])
  
  for(i in 1:dimension[2]){
    
    rest_columns <- setdiff(meta_vector, seq_vec)
    
    for(j in 1:length(rest_columns)){
      rmse_vec[j] <-  sqrt(mean((matrix_1[,i]-matrix_2[,rest_columns[j]])^2))
    }
    seq_vec <- c(seq_vec,rest_columns[which.min(rmse_vec)])
    rmse_vec <- c()
  }  
  
  rmse_result <- sqrt(mean((matrix_1 - matrix_2[,seq_vec])^2))
  print(seq_vec)
  return(rmse_result)
}



#################### Parameters Settings ######################


## Define which items should be changed to asymmetric
pos_asym_vec <- c(1,2,3,4,5,6,7,8,9,10,11,12)
neg_asym_vec <- c()


## Define the number of simulations
sim_num <- 100

## Define how many factors would be used in factor analysis
nfactors = 3

RMSE_result <- list() # Polychoric Matrix with Ordinal Data
RMSE_result_2 <- list() # Pearson Matrix with Continuous Data
RMSE_result_3 <- list() # Pearson Matrix with Ordinal Data
#################### Implementation Part ######################
for(k in 5:10){
  
  ## the default setting is 5
  num_categories <- k
  RMSE_vec <- c() # Polychoric Matrix with Ordinal Data
  RMSE_vec_2 <- c() # Pearson Matrix with Continuous Data
  RMSE_vec_3 <- c() # Pearson Matrix with Ordinal Data
  
  for(l in 1:sim_num){
    
    print(paste0("The category number is ",k," and the simulation number is ",l))
    
    
    # First step: Generate the data, but after that the normality should be tested.
    
    ## Data needed to be generated at the first step. 
    
    ### The following two lines are corresponding to 3 factors
    fc <- fcors.value(nf=3, cors=c(1, 0.556, 0.773, 0.556, 1, 0.409, 0.773, 0.409, 1))
    fl <- loading.value(nf=3, fl.loads=c(0.8,0.7,0.6,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0.75,0.65,0.55,0.45,0,0,0,0,0,0,0,0,0,0,0,0,0.4,0.3,0.2,0.1)) 
    
  
    
    sim.normal(nd=12, ss=2000, fcors=fc, loading<-fl, f.loc=tempdir())
    
    ## List files in the temporary directory to find the simulated data
    list.files(tempdir())
    
    ## Assuming the data files are named consistently, read the first simulated data
    data_file <- file.path(tempdir(), "Data_1.dat")
    simulated_data <- read.table(data_file, header=FALSE)
    
    ## View the first few rows of the simulated data
    head(simulated_data)
    
    df <- simulated_data # Continuous Data Used for Transformation
    df_2 <- simulated_data[,-1] # Continuous Data Used directly
    
    # Second step: the data should be categorized into certain types.
    
    ## Get the "mean value" and "std" of each item in the above data. 
    mean_vec <- c()
    std_vec <- c()
    
    for(m in 2:13){
      col <- paste0("V",m)
      mean_vec <- c(mean_vec,mean(df[[col]]))
      std_vec <- c(std_vec, sqrt(var(df[[col]])))
    }
    
    ## use the mean value and std value to get the threshold
    
    
    threshold_points_list <- list()
    
    
    ### Get the threshold points for each item 
    for(j in 2:13){
      if((j-1) %in% pos_asym_vec){
        threshold_points_list[[j-1]] <- divide_func(1,mean_vec[j-1],std_vec[j-1],num_categories)
      }else if((j-1) %in% neg_asym_vec){
        threshold_points_list[[j-1]] <- divide_func(2,mean_vec[j-1],std_vec[j-1],num_categories)
      }else{
        threshold_points_list[[j-1]] <- divide_func(3,mean_vec[j-1],std_vec[j-1],num_categories)
      }
    }
    
    
    ## Use the threshold to change the original data set into categories.
    
    ### transform the original continuous data into categories in each item.
    df_new <- list()
    
    for(i in 2:13){
      col <- paste0("V",i)
      df_new[[i-1]] <- sapply(df[[col]],function(x) transform(x,threshold_points_list[[i-1]],num_categories))
    }
    
    new_data_df <- data.frame(lapply(df_new, as.factor))
    colnames(new_data_df) <- c(1:12)
    
    
    # Third step: Factor Analysis Part
    
    # -------- Polychoric Matrix with Ordinal Data
    
    ## Compute the Polychoric correlation matrix by using Ordinal Data
    cor_matrix <- hetcor(new_data_df)$correlations
    
    ## Print the Polychoric correlation matrix
    #print("Polychoric Correlation Matrix:")
    #print(cor_matrix)
    
    cor_matrix[is.na(cor_matrix)] <- 0
    ## Perform factor analysis (It is the oblique rotation.)
    fa_result <- fa(cor_matrix, nfactors, rotate = "Promax",fm="mle")
    
    ## Print the factor analysis results
    loadings <- fa_result$loadings
    loadings_df <- as.matrix(loadings[1:ncol(new_data_df),]) ## this is used to only extract the rows relevant to loadings.
    print(loadings_df)
    
    
    ## Calculate the RMSE(rooted mean square error)
    RMSE <- RMSE_func(fl,loadings_df)
    RMSE_vec <- c(RMSE_vec,RMSE)
    
    
    
    
    
    
    # --------- Pearson Matrix with Continuous Data
    
    
    ## Compute the Pearson correlation matrix by using continuous data
    cor_matrix_2 <- cor(df_2)
    cor_matrix[is.na(cor_matrix_2)] <- 0
    
    ## Perform factor analysis (It is the oblique rotation.)
    fa_result_2 <- fa(cor_matrix_2, nfactors, rotate = "Promax",fm="mle")
    
    ## Print the factor analysis results
    loadings_2 <- fa_result_2$loadings
    loadings_df_2 <- as.matrix(loadings_2[1:ncol(new_data_df),]) ## this is used to only extract the rows relevant to loadings.
    print(loadings_df_2)
    
    ## Calculate the RMSE(rooted mean square error)
    RMSE_2 <- RMSE_func(fl,loadings_df_2)
    RMSE_vec_2 <- c(RMSE_vec_2,RMSE_2)
    
    
    
    
    
    # --------- Pearson Matrix with Ordinal Data
    
    ## Compute the Pearson correlation matrix by using Ordinal data
    new_data_df_2 <- sapply(new_data_df,as.numeric)
    cor_matrix_3 <- cor(new_data_df_2)
    cor_matrix[is.na(cor_matrix_3)] <- 0
      
    
    ## Perform factor analysis (It is the oblique rotation.)
    fa_result_3 <- fa(cor_matrix_3, nfactors, rotate = "Promax",fm="mle")
    
    
    ## Print the factor analysis results
    loadings_3 <- fa_result_3$loadings
    loadings_df_3 <- as.matrix(loadings_3[1:ncol(new_data_df),]) ## this is used to only extract the rows relevant to loadings.
    print(loadings_df_3)
    
    ## Calculate the RMSE(rooted mean square error)
    RMSE_3 <- RMSE_func(fl,loadings_df_3)
    RMSE_vec_3 <- c(RMSE_vec_3,RMSE_3)
    
    
    
    
    
    
    
  }
  
  RMSE_result[[k-4]] <- RMSE_vec
  RMSE_result_2[[k-4]] <- RMSE_vec_2
  RMSE_result_3[[k-4]] <- RMSE_vec_3
}

end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)





# Save the files into the local computer
## 1
result_1 <- data.frame(matrix(unlist(RMSE_result), nrow = 100, byrow = FALSE))
write.csv(result_1, "RMSE_result_1.csv", row.names = FALSE)

## 2
result_2 <- data.frame(matrix(unlist(RMSE_result_2), nrow = 100, byrow = FALSE))
write.csv(result_2, "RMSE_result_2.csv", row.names = FALSE)

## 3
result_3 <- data.frame(matrix(unlist(RMSE_result_3), nrow = 100, byrow = FALSE))
write.csv(result_3, "RMSE_result_3.csv", row.names = FALSE)





