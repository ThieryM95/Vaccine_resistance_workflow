#######################################################################################
# Post process results of the sensitivity analysis                                    #
#                                                                                     #
# Task : Reorganise the result of the sensitivity analysis from a list to a table     #
# Input: Output of the sensitivity analysis in a list format                          #
# Output: Output of the sensitivity analysis in a table format                        #
#                                                                                     #
# Author:  thiery.masserey@swisstph.ch                                                #                                      
#######################################################################################

#-----------------------------------------------------------------
# function to organize the Sobol indices   from a list to a table.
#-----------------------------------------------------------------
Post_process_sensitivity <- function(Results_SA) {

  # select the parameter names
  parameters <- pm$prog$Parameter[pm$prog$Parameter != "decay_efficacy"]
  
  if(pm$opts$Type_analysis!="Vaccine_R_adult_AIV"){
    pm$settings$Age<-1
    pm$settings$Blood_coverage<-1
  }
  
  if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
    pm$settings$Vaccine_type<-"PEV"
  }

  # ---- Define the different variable of our table and their length ----

  # Number of row
  Repetition<-length(parameters) * 2 * length(pm$settings$Coverage_reduced)* length(pm$settings$Vaccine_type) * length(pm$settings$seasonality)* length(pm$settings$Age) * length(pm$settings$Blood_coverage)
  
  
  # Setting names
  Setting_names <- rep(0, Repetition)

  # Parameter names
  factors <- rep(parameters, (Repetition/length(parameters)))

  # if first/ total order indicies
  First <- rep(0, Repetition)

  # value of the sobol indices
  Effect <- rep(0, Repetition)

  # maximum values of the sobol indices
  MAX <- rep(0, Repetition)

  # Minimum values of the sobol indices
  MIN <- rep(0, Repetition)

  # define the value for the factors that were constrained
  Coverage_reduced <- rep(0, Repetition)
  Vaccine_type <- rep(0, Repetition)
  Seasonality <- rep(0, Repetition)
  Age <- rep(0, Repetition)
  Blood_coverage <- rep(0, Repetition)
  
  
  # merge all the information into a table
  if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
    data <- cbind(Setting_names, as.character(factors), First, Effect, MAX, MIN, Coverage_reduced, Seasonality, Age, Blood_coverage)
    colnames(data) <- c("Setting", "Factor", "First", "Effect", "MAX", "MIN","Coverage_reduced", "Seasonality", "Age", "Blood_coverage")
  }else{
    data <- cbind(Setting_names, as.character(factors), First, Effect, MAX, MIN, Coverage_reduced,Vaccine_type, Seasonality)
    colnames(data) <- c("Setting", "Factor", "First", "Effect", "MAX", "MIN","Coverage_reduced","Vaccine_type", "Seasonality")
  }
 
  #---- Restructure the table ----

  # define the number of setting post processed for the loop
  n_settings <- 1
  
  # loop across each arms
   for (this_vaccine in pm$settings$Vaccine_type){
      for (this_age in pm$settings$Age){
        for (this_blood in pm$settings$Blood_coverage){
          for (this_reduction in pm$settings$Coverage_reduced) {
            for (this_season in names(pm$settings$seasonality)) {

              # Define the arm name
              if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
                setting_name <- paste(pm$opts$om_outcome, this_season, "Coverage_reduced", this_reduction,"Age", this_age, "Blood_coverage", this_blood,  sep = "_")
              }else{
                setting_name = paste(pm$opts$om_outcome, this_season, this_vaccine, "Coverage_reduced",this_reduction, sep = "_") 
              }      
              
              SA_file <- paste0(pm$pth$results, setting_name, "_gp.RData")
              SA <- readRDS(SA_file)

              # other options:
              # SA<-Results_SA[setting_name]
              # SA<-list.ungroup(SA, level = 1L)

              # transfer the  data from the sensitivity analysis output into the data frame
              data[c(n_settings:(n_settings + length(parameters) * 2 - 1)), 1] <- setting_name
              data[n_settings:(n_settings + length(parameters) - 1), 3] <- SA$S$original[1:length(parameters)]
              data[(n_settings + length(parameters)):(n_settings + length(parameters) * 2 - 1), 3] <- SA$T$original[1:length(parameters)]
              data[c(n_settings:(n_settings + length(parameters) - 1)), 4] <- "First"
              data[c((n_settings + length(parameters)):(n_settings + length(parameters) * 2 - 1)), 4] <- "Total"
              data[n_settings:(n_settings + length(parameters) - 1), 5] <- SA$S$`max. c.i.`[1:length(parameters)]
              data[(n_settings + length(parameters)):(n_settings + length(parameters) * 2 - 1), 5] <- SA$T$`max. c.i.`[1:length(parameters)]
              data[n_settings:(n_settings + length(parameters) - 1), 6] <- SA$S$`min. c.i.`[1:length(parameters)]
              data[(n_settings + length(parameters)):(n_settings + length(parameters) * 2 - 1), 6] <- SA$T$`min. c.i.`[1:length(parameters)]
              data[c(n_settings:(n_settings + length(parameters) * 2 - 1)), 7] <- this_reduction
              data[c(n_settings:(n_settings + length(parameters) * 2 - 1)), 8] <- this_vaccine
              data[c(n_settings:(n_settings + length(parameters) * 2 - 1)), 9] <- this_season
              
              if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
                data[c(n_settings:(n_settings + length(parameters) * 2 - 1)), 8] <- this_season
                data[c(n_settings:(n_settings + length(parameters) * 2 - 1)), 9] <- this_age
                data[c(n_settings:(n_settings + length(parameters) * 2 - 1)), 10] <- this_blood
              }

              # update the number of setting we went across with the post processing
              n_settings <- n_settings + length(parameters) * 2
            }
          }
        }
      }
   }
  
  # finalize the dataframe
  data <- as.data.frame(data)

  data$First <- as.numeric(data$First)
  data$MAX <- as.numeric(data$MAX)
  data$MIN <- as.numeric(data$MIN)
  data$First[data$First <= 0] <- 0

  # save the dataframe of sobol indices for each arm and parameter
  name_file <- paste0(pm$pth$results, "Sobol_indices", ".txt")
  write.table(data,
    file = name_file, sep = "\t",
    quote = FALSE, col.names = TRUE, row.names = FALSE)

  # return the  dataframe of sobol indices for each arm and parameter
  return(data)
}

#------------------------------------------- ---------------------------
# function to organise the effect of each factor from a list to a table.
#-----------------------------------------------------------------------
Post_process_sensitivity_2 <- function(Results_SA) {
  
  # select the parameter name 
  parameters <- pm$prog$Parameter[pm$prog$Parameter != "decay_efficacy"]

  # creat a parameter table that will contains the estimate selection coefficient over the parameter range for each arm
  Quantil_final_final <- matrix(NA, nrow = 0, ncol = 5)
  colnames(Quantil_final_final) <- c("L", "M", "U", "x", "G")

  if(pm$opts$Type_analysis!="Vaccine_R_adult_AIV"){
    pm$settings$Age<-1
    pm$settings$Blood_coverage<-1
  }
  
  if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
    pm$settings$Vaccine_type<-"PEV"
  }
  
  
  # loop across each arms
  for (this_vaccine in pm$settings$Vaccine_type){
  for (this_age in pm$settings$Age){
   for (this_blood in pm$settings$Blood_coverage){
    for (this_reduction in pm$settings$Coverage_reduced) { 
      for (this_season in names(pm$settings$seasonality)) {

              # Define the name of the arm
              if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
                 setting_name <- paste(pm$opts$om_outcome, this_season, "Coverage_reduced", this_reduction,"Age", this_age, "Blood_coverage", this_blood,  sep = "_")
              }else{
                setting_name = paste(pm$opts$om_outcome, this_season,this_vaccine, "Coverage_reduced",this_reduction, sep = "_") 
              }
        
              # load the results of the sensitivity analyis for this arm
              SA_file <- paste0(pm$pth$results, setting_name, "_gp.RData")
              SA <- readRDS(SA_file)

              # other options:
              # SA<-Results_SA[setting_name]
              # SA<-list.ungroup(SA, level = 1L)

              # Select the predicted selection coefficient (output) into a vector
              Y <- SA$y
              
              # Select the input parameter into a table
              X<-SA$X
              
              # Define the number of fraction
              N <- 30
              NN <- 30
              
              # Creat a vector that will contain the boundary of fraciton of the parameter space + median selection coeffcient 
              Quantil_final <- matrix(NA, nrow = 0, ncol = 8)

              # Loop across each parameters
              for (i in 1:length(SA$X)) {
                
                # Creat a datafram of parameter value and  selection coefficient
                DF <- as.data.frame(cbind(X[, i], Y))
                colnames(DF)[1:2] <- c("X", "Y")
                
                # cut the parameter space into N piece
                splitFitness <- seq(min(X[, i]), max(X[, i]), length = N)
                
                # creat a table for this specific parameter
                Quantiles_1 <- matrix(NA, nrow = N, ncol = 3)

                # estimate the median selection coeffcient in each fraction of the parameter space
                for (k1 in 1:(NN - 1)) {
                  thisXY <- DF[X[, i] > splitFitness[k1] & X[, i] < splitFitness[k1 + 1], ]
                  Quantiles_1[k1, ] <- quantile(thisXY$Y, c(0.25, 0.5, 0.75))
                }

                # transform data into dataframe
                Quantiles_1 <- as.data.frame(cbind(Quantiles_1, 1:NN))
                
                # select the parameter name
                Quantiles_1$G <- colnames(X)[i]
                
                # add columns name
                colnames(Quantiles_1) <- c("L", "M", "U", "x", "G")
                
                # Update the table to contain the data of all parameters
                Quantil_final <- rbind(Quantil_final, Quantiles_1)
              }
              
              # add information about the arm
              Quantil_final$Coverage_reduced <- this_reduction
              Quantil_final$Seasonality <- this_season
              
              
              if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
                Quantil_final$Age <- this_age
                Quantil_final$Blood_coverage <- this_blood              
              }else{
                Quantil_final$Vaccine_type <- this_vaccine
              }
              
              

              # Update the table to contain the data of all arm
              Quantil_final_final <- rbind(Quantil_final_final, Quantil_final)
            }
          }
        }
      }
    }

  # Save the table of mean seleciton coeffcient over the parameter range of each paramter, for each arm
  name_file <- paste0(pm$pth$results, "Factors_effect", ".txt")
  write.table(Quantil_final_final,
    file = name_file, sep = "\t",
    quote = FALSE, col.names = TRUE, row.names = FALSE)
  
  # Return the table of mean selection coeffcient over the parameter range of each paramter, for each arm
  return(Quantil_final_final)
}
