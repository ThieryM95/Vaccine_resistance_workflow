###################################################################################
# Perform the sensitivity analysis                                                #
#                                                                                 #
# Task : Perfom the global sensitivity analysis of the GP                         #
# Input: fitted GP                                                                #
# Output: sobol indices + direction of effect                                     #
#                                                                                 #
# authors: thiery.masserey@swisstph.ch adapted from monica.golumbeanu@swisstph.ch #                                      #
###################################################################################

# load the package
library("sensitivity")
library("multisensi")
library("lhs")
library("hetGP")
library("tgp")
library("dplyr")
library("ggplot2")
library("reshape2")
library("gridExtra")
library("hrbrthemes")
library("grid")
library("gridExtra")
library("rlist")

#--------------------------------------------------------------------
# function to preapre data for sensitivity analysis and save results.
#--------------------------------------------------------------------
sensitivity_analysis <- function(pm) {

  # creat a liste to store the results of the SA
  indices <- list()

  # define the parameter range
  param_ranges <- cbind(pm$prog$min, pm$prog$max)

  # define the name of the paramter
  row.names(param_ranges) <- pm$prog$prog_names

  # define the column names
  colnames(param_ranges) <- c("min", "max")
  param_ranges <- param_ranges[rownames(param_ranges) != "decay_efficacy", ]
  
  # number of sampled points
  sa_n <- 100000

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

            # Define the arm name
            if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
              setting_name <- paste(pm$opts$om_outcome, this_season, "Coverage_reduced", this_reduction,"Age", this_age, "Blood_coverage", this_blood,  sep = "_")
            }else{
              setting_name = paste(pm$opts$om_outcome, this_season,this_vaccine, "Coverage_reduced",this_reduction, sep = "_") 
            }
        
            # Message to the console
            message("  - ", setting_name)

            # Load the GP of the define arm
            gp_file <- paste0(pm$pth$gp_models, setting_name, "_gp.RData")
            trained_model <- readRDS(gp_file)
            trained_model <- trained_model$gp_model

            # Do the sensitivity analysis
            sobol_indices <- calc_sobol_idx(trained_model, param_ranges, sa_n) # see function bellow

            # Save the results in a dataframe
            SA_file <- paste0(pm$pth$results, setting_name, "_gp.RData")
            saveRDS(sobol_indices, file = SA_file)

            # Save the results as an output of this function
            indices$setting_name <- sobol_indices
            names(indices)[names(indices) == "setting_name"] <- setting_name
          }
        }      
     }
    }
   }

  # return the sobole indice
  return(indices)
  
}

#----------------------------------------------
# function to perform the sensitivity analysis.
#----------------------------------------------
calc_sobol_idx <- function(trained_model, param_ranges, sa_n) {

  # wrapper function for the GP_model prediction
  GP_f <- function(X) {
    out <- predict(x = as.matrix(X), object = trained_model)
    return(out$mean)
  }

  # construct the two random lhs samples
  X1 <- as.data.frame(lhs(sa_n, as.matrix(param_ranges)))
  X2 <- as.data.frame(lhs(sa_n, as.matrix(param_ranges)))

  # define the columns name
  colnames(X1) <- pm$prog$Parameter[pm$prog$Parameter != "decay_efficacy"]
  colnames(X2) <- pm$prog$Parameter[pm$prog$Parameter != "decay_efficacy"]

  # estimate the Sobol indices
  SA <- soboljansen(model = GP_f, as.data.frame(X1), as.data.frame(X2), nboot = (10000))
  
  # Return the sobole indices + direction of effect
  return(SA)
}
