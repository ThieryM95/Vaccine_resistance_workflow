#######################################################################
# Post process of the GP prediction for a  better visualization       #
#                                                                     #
# Task : Reorganise the gp accuracy result from a list to a dataframe #
# Input: GP prediction and true value from  the train dataset         #
# output: Table of the GP test and prediction for each arm            #
#                                                                     #
# Author:  thiery.masserey@swisstph.ch                                #
#######################################################################

# Load package
#library(SummarizedExperiment)

#--------------------------------------------------------------------------
# function to Reorganise the gp accuracy result from a list to a dataframe.
#--------------------------------------------------------------------------
Post_process_GP<-function(Results_gp){

  # Create the data frame
  if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
    precision_3=precision_2<-matrix(NA, nrow =0, ncol = 7)
    colnames(precision_2)[1:7]<-c("Test_True","Test_predicted","iteration","seasonality","Coverage_reduced","Age","Blood_coverage")
  }else{
    precision_3=precision_2<-matrix(NA, nrow =0, ncol = 6)
    colnames(precision_2)[1:6]<-c("Test_True","Test_predicted","iteration","seasonality","Vaccine_type","Coverage_reduced")
  } 
  
  # Loop across the different ith iteration and arms
  for(iter in 1:length(Results_gp)){
    
    # select data of the ith iteration
    z<-Results_gp[[iter]]
        
    if(pm$opts$Type_analysis!="Vaccine_R_adult_AIV"){
      pm$settings$Age<-1
      pm$settings$Blood_coverage<-1
    }
    
    if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
      pm$settings$Vaccine_type<-"PEV"
    }
    
    
    # Loop across constrain variable
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
              
              
              # Select the data from the arm
              precision<-z[setting_name]
              
              # Unlist the results
              precision<-list.ungroup(precision, level = 1L)
              
              # Select the variable true value and predicted value
              Test_True<-precision$actual_test
              Test_predicted<-precision$predict_test
             
              # Create the data set with all variables
              if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
                precision_2<-data.frame(Test_True,Test_predicted,iter,this_season,this_reduction, this_age, this_blood)
              }else{
                precision_2<-data.frame(Test_True,Test_predicted,iter,this_season,this_vaccine,this_reduction)
              }
              
              # Create a data set that contain all the data from all the arms
              precision_3<-rbind(precision_3,precision_2)
         
       }
      }
     }
    }
    }
  }
  
  # Name the columns
  if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
    colnames(precision_3)[1:7]<-c("Test_True","Test_predicted","iteration","seasonality","Coverage_reduced","Age","Blood_coverage")
  }else{
    colnames(precision_3)[1:6]<-c("Test_True","Test_predicted","iteration","seasonality","Vaccine_type","Coverage_reduced")
  }
  
  # Save the data set
  name_file = paste0(pm$pth$gp_models, "Precision", ".txt") 
  write.table(precision_3, file = name_file,sep = "\t", 
              quote = FALSE, col.names = TRUE, row.names = FALSE)
  
  # Return the data set
  return(precision_3)
}
