##################################################################
# Estimate the outpout of choice                                 #
#                                                                #
# Task: Calculate the selection coefficient for each simulation  #
# Input: Post processed data output of OpenMalaria               #
# Output: Table of parameter input of each simulation and        #
#         the estimated selection coefficient                    #
#                                                                #
# Important: need to define here the number of time step         #
# of the survey that was used                                    #
# Authors: thiery.masserey@swisstph.ch                           #
##################################################################

# Load function
library(grDevices)
library(pkr)

# ------------------------------------------------------------------------------
# Estimate the selection coefficient or prophylactic period for each simulation.
# ------------------------------------------------------------------------------
SummaryResults <- function(pm) {
  
  # Message in the console
  message("  - Calulate the spread or esthablihsment ")
  
  # Load the list of scenario
  Scenario_liste <- read.table(file.path(pm$pth$sim_sed, "scenarios_.txt"), header = T)
  
  # Load the list of timing of deployment to make sure it occurred as expected to fit seasonality pattern
  # TIMING <- read.table("/scicore/home/penny/masthi00/smc_resistance/Data_time_2.txt", sep = "\t", header = T)
  
  # Load the list of Output files
  Output <- list.files(pm$pth$processed)
  
  # Define the time step
  time_step <- 5 # define time step of monitoring
  Number_survey_generation <- 60 / time_step
  Number_survey_years <- 365 / time_step
  
  # Initiate progress bar
  pb <- txtProgressBar(min = 0, max = length(Output), initial = 0, width = 100, style = 3)
  
  # Loop across each simulation
  for (i in 1:length(Output)) {# i<-1
    
    # Download the data
    Output_data_name <- Output[i]
    Output_data_file_path <- file.path(pm$pth$processed, Output_data_name)
    
    Output_data <- read.table(Output_data_file_path, sep = ";")
    name_scenario <- gsub("PostProcess_", "", Output_data_name)
    name_scenario <- gsub("_out", "", name_scenario)
    
    # Variable estimate when we estimate the selection coefficient for vaccine deployed to children in perenial setting
    if (pm$opts$Type_analysis == "Vaccine_R_perennial") {
      
      # Estimate the selection coefficient
      Scenario_liste$Indicator_10[Scenario_liste$scenario_name == name_scenario] <- spread_R10_0(Output_data) # see function bellow
      
      #Scenario_liste$Prevalance_b[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (29 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (29 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_32[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (32 * Number_survey_years) & Output_data$Survey <= (33 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (32 * Number_survey_years) & Output_data$Survey <= (33 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_35[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (34 * Number_survey_years) & Output_data$Survey <= (35 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (34 * Number_survey_years) & Output_data$Survey <= (35 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_37[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (36 * Number_survey_years) &  Output_data$Survey <= (37 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (36 * Number_survey_years) & Output_data$Survey <= (37 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_40[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (38 * Number_survey_years) & Output_data$Survey <= (39 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (38 * Number_survey_years) & Output_data$Survey <= (49 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_42[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (40 * Number_survey_years) & Output_data$Survey <= (41 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (40 * Number_survey_years) & Output_data$Survey <= (41 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_42[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (42 * Number_survey_years) & Output_data$Survey <= (43 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (42 * Number_survey_years) & Output_data$Survey <= (43 * Number_survey_years)])
      #Scenario_liste$Prevalance_a[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (44 * Number_survey_years) & Output_data$Survey <= (45 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (44 * Number_survey_years) & Output_data$Survey <= (45 * Number_survey_years)])
      
      
      # Estimate the input EIR
      Scenario_liste$Input_EIR[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$inputEIR[Output_data$Survey >= (20 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)] * 365 / 5)
      
      # Estimate the simulated EIR
      Scenario_liste$Simulated_EIR[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (20 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)] * 365 / 5)
      
    }
    
    if (pm$opts$Type_analysis == "Vaccine_R_seasonal") {
      
      # Estimate the selection coefficient
      Scenario_liste$Indicator_10[Scenario_liste$scenario_name == name_scenario] <- spread_R3_99(Output_data) # see function bellow

      # Estimate the input EIR
      Scenario_liste$Input_EIR[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$inputEIR[Output_data$Survey >= (20 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)] * 365 / 5)
      
      # Estimate the simulated EIR
      Scenario_liste$Simulated_EIR[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (20 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)] * 365 / 5)
      

      # Prevalacne before and after
      #Scenario_liste$Prevalance_b[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (29 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (29 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_32[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (32 * Number_survey_years) & Output_data$Survey <= (33 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (32 * Number_survey_years) & Output_data$Survey <= (33 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_35[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (34 * Number_survey_years) & Output_data$Survey <= (35 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (34 * Number_survey_years) &  Output_data$Survey <= (35 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_37[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (36 * Number_survey_years) & Output_data$Survey <= (37 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (36 * Number_survey_years) & Output_data$Survey <= (37 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_40[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (38 * Number_survey_years) & Output_data$Survey <= (39 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (38 * Number_survey_years) & Output_data$Survey <= (39 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_42[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (40 * Number_survey_years) & Output_data$Survey <= (41 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (40 * Number_survey_years) & Output_data$Survey <= (41 * Number_survey_years)])
      #Scenario_liste$Prevalance_a_42[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (42 * Number_survey_years) & Output_data$Survey <= (43 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (42 * Number_survey_years) & Output_data$Survey <= (43 * Number_survey_years)])
      #Scenario_liste$Prevalance_a[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (44 * Number_survey_years) & Output_data$Survey <= (45 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (44 * Number_survey_years) & Output_data$Survey <= (45 * Number_survey_years)])
      
      # Estimate when the EIR reach it maximum
      #Scenario_liste$time_eir[Scenario_liste$scenario_name == name_scenario] <- Time_eir_max(Output_data) # see function bellow
      
      # Estimate the time at which it was since one month that the vaccine was deployed
      #Scenario_liste$time_eir_2[Scenario_liste$scenario_name == name_scenario] <- TIMING$time_2[TIMING$EIR == 1] - 0.7
      
      
      # If the peak of EIR is more than one month away from the time at which it was since one month that the vaccine was deployed
      # if (abs(Scenario_liste$time_eir[Scenario_liste$scenario_name == name_scenario] - Scenario_liste$time_eir_2[Scenario_liste$scenario_name == name_scenario]) >= 1) {
      #   Scenario_liste$Indicator[Scenario_liste$scenario_name == name_scenario] <- NA
      # }
      
    }
    
    if (pm$opts$Type_analysis == "Vaccine_R_adult_AIV" | pm$opts$Type_analysis == "Vaccine_R_adult_AIV_TBV") {
      # Estimate the selection coefficient
      
      if (Scenario_liste$seasonality[Scenario_liste$scenario_name == name_scenario] == "sesonality1") {
        Scenario_liste$Indicator_10[Scenario_liste$scenario_name == name_scenario] <- spread_adult(Output_data) # see function bellow
      }
      
      if (Scenario_liste$seasonality[Scenario_liste$scenario_name == name_scenario] == "sesonality2") {
        Scenario_liste$Indicator_10[Scenario_liste$scenario_name == name_scenario] <- spread_adult_seasonal(Output_data) # see function bellow
      }
      
      # Estimate the input EIR
      Scenario_liste$Input_EIR[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$inputEIR[Output_data$Survey >= (20 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)] * 365 / 5)
      
      # Estimate the simulated EIR
      Scenario_liste$Simulated_EIR[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (20 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)] * 365 / 5)
      
      # Estimate the simulated EIR
      # Scenario_liste$Simulated_EIR_after[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (30 * Number_survey_years) & Output_data$Survey <= (40 * Number_survey_years)] * 365 / 5)
      # Prevalence before and after
      # Scenario_liste$Prevalance_b[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (29 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (29 * Number_survey_years) & Output_data$Survey <= (30 * Number_survey_years)])
      # Scenario_liste$Prevalance_a_32[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (32 * Number_survey_years) & Output_data$Survey <= (33 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (32 * Number_survey_years) & Output_data$Survey <= (33 * Number_survey_years)])
      # Scenario_liste$Prevalance_a_34[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (34 * Number_survey_years) & Output_data$Survey <= (35 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (34 * Number_survey_years) & Output_data$Survey <= (35 * Number_survey_years)])
      # Scenario_liste$Prevalance_a_36[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (36 * Number_survey_years) & Output_data$Survey <= (37 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (36 * Number_survey_years) & Output_data$Survey <= (37 * Number_survey_years)])
      # Scenario_liste$Prevalance_a_38[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (38 * Number_survey_years) & Output_data$Survey <= (39 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (38 * Number_survey_years) & Output_data$Survey <= (49 * Number_survey_years)])
      # Scenario_liste$Prevalance_a_40[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (40 * Number_survey_years) & Output_data$Survey <= (41 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (40 * Number_survey_years) & Output_data$Survey <= (41 * Number_survey_years)])
      # Scenario_liste$Prevalance_a_42[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (42 * Number_survey_years) & Output_data$Survey <= (43 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (42 * Number_survey_years) & Output_data$Survey <= (43 * Number_survey_years)])
      # Scenario_liste$Prevalance_a[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$nPatent_8[Output_data$Survey >= (44 * Number_survey_years) & Output_data$Survey <= (45 * Number_survey_years)] / Output_data$nHost_8[Output_data$Survey >= (44 * Number_survey_years) & Output_data$Survey <= (45 * Number_survey_years)])
      # Scenario_liste$EIR_a_32[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (32 * Number_survey_years) & Output_data$Survey <= (33 * Number_survey_years)] * 365 / 5)
      # Scenario_liste$EIR_a_34[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (34 * Number_survey_years) & Output_data$Survey <= (35 * Number_survey_years)] * 365 / 5)
      # Scenario_liste$EIR_a_36[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (36 * Number_survey_years) & Output_data$Survey <= (37 * Number_survey_years)] * 365 / 5)
      # Scenario_liste$EIR_a_38[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (38 * Number_survey_years) & Output_data$Survey <= (39 * Number_survey_years)] * 365 / 5)
      # Scenario_liste$EIR_a_40[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (40 * Number_survey_years) & Output_data$Survey <= (41 * Number_survey_years)] * 365 / 5)
      # Scenario_liste$EIR_a_42[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (42 * Number_survey_years) & Output_data$Survey <= (43 * Number_survey_years)] * 365 / 5)
      # Scenario_liste$EIR_a[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$simulatedEIR[Output_data$Survey >= (44 * Number_survey_years) &  Output_data$Survey <= (45 * Number_survey_years)] * 365 / 5)
 
    }
    
    if (pm$opts$Type_analysis == "Vaccine_efficacy_children") {
      
      if (Scenario_liste$seasonality[Scenario_liste$scenario_name == name_scenario] == "sesonality1") {
        
        Output_data$Prevalance <- (Output_data$nPatent_1 + Output_data$nPatent_2 + Output_data$nPatent_3 + Output_data$nPatent_4 + Output_data$nPatent_5 + Output_data$nPatent_6) /
                                  (Output_data$nHost_1 + Output_data$nHost_2 + Output_data$nHost_3 + Output_data$nHost_4 + Output_data$nHost_5 + Output_data$nHost_6)
        
        Output_data$Incidence <-(Output_data$nUncomp_1 + Output_data$nUncomp_2 + Output_data$nUncomp_3 + Output_data$nUncomp_4 + Output_data$nUncomp_5 + Output_data$nUncomp_6) /
                                (Output_data$nHost_1 + Output_data$nHost_2 + Output_data$nHost_3 + Output_data$nHost_4 + Output_data$nHost_5 + Output_data$nHost_6)
        
        
        Time_control_1 <- Output_data$Survey[Output_data$nEPIVaccinations_3 >= 1][1] / 73 - 1
        Time_control_2 <- Output_data$Survey[Output_data$nEPIVaccinations_3 >= 1][1] / 73 # Time at which first vaccine is deployed
        Time_vaccine_1 <- Output_data$Survey[Output_data$nEPIVaccinations_3 >= 1][1] / 73 + 5 # five year later all children under 5 are vaccinated, so can estimate effectivness
        Time_vaccine_2 <- Output_data$Survey[Output_data$nEPIVaccinations_3 >= 1][1] / 73 + 6
      }
      
      if (Scenario_liste$seasonality[Scenario_liste$scenario_name == name_scenario] == "sesonality2") {
        
        Output_data$Prevalance <- (Output_data$nPatent_1 + Output_data$nPatent_2 + Output_data$nPatent_3 + Output_data$nPatent_4 + Output_data$nPatent_5 + Output_data$nPatent_6 + Output_data$nPatent_7) /
                                  (Output_data$nHost_1 + Output_data$nHost_2 + Output_data$nHost_3 + Output_data$nHost_4 + Output_data$nHost_5 + Output_data$nHost_6 + Output_data$nHost_7)
        
        Output_data$Incidence <- (Output_data$nUncomp_1 + Output_data$nUncomp_2 + Output_data$nUncomp_3 + Output_data$nUncomp_4 + Output_data$nUncomp_5 + Output_data$nUncomp_6 + Output_data$nUncomp_7) /
                                 (Output_data$nHost_1 + Output_data$nHost_2 + Output_data$nHost_3 + Output_data$nHost_4 + Output_data$nHost_5 + Output_data$nHost_6 + Output_data$nHost_7)
        
        
        Time_control_1 <- Output_data$Survey[Output_data$nMassVaccinations_4 >= 1][1] / 73 - 2
        Time_control_2 <- Output_data$Survey[Output_data$nMassVaccinations_4 >= 1][1] / 73 - 1 # One year before first booster is given
        Time_vaccine_1 <- Output_data$Survey[Output_data$nMassVaccinations_4 >= 1][1] / 73 + 3 # Year at which last booster is given 
        Time_vaccine_2 <- Output_data$Survey[Output_data$nMassVaccinations_4 >= 1][1] / 73 + 4
      
      }
      
    
      
      # Look at prevalence in control
      Scenario_liste$Prevalance_control[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Prevalance[Output_data$Survey / 73 >= Time_control_1 & Output_data$Survey / 73 <= Time_control_2])
      
      # Look at prevalence in Vaccine (after 5 years)
      Scenario_liste$Prevalance_vaccine[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Prevalance[Output_data$Survey / 73 >= Time_vaccine_1 & Output_data$Survey / 73 <= Time_vaccine_2])
      
      # Look at incidence in control
      Scenario_liste$Incidence_control[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Incidence[Output_data$Survey / 73 >= Time_control_1 & Output_data$Survey / 73 <= Time_control_2])
      
      # Look at incidence in vaccine
      Scenario_liste$Incidence_vaccine[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Incidence[Output_data$Survey / 73 >= Time_vaccine_1 & Output_data$Survey / 73 <= Time_vaccine_2])
      
    }
    
    if (pm$opts$Type_analysis == "Vaccine_efficacy_adult") {
      
      Output_data$Prevalance <- (Output_data$nPatent_1 + Output_data$nPatent_2 + Output_data$nPatent_3 +Output_data$nPatent_4 + Output_data$nPatent_5 + Output_data$nPatent_6 +Output_data$nPatent_7 + Output_data$nPatent_8) / 
                                (Output_data$nHost_1 + Output_data$nHost_2 + Output_data$nHost_3 + Output_data$nHost_4 + Output_data$nHost_5 + Output_data$nHost_6 + Output_data$nHost_7 + Output_data$nHost_8)
      
      Output_data$Incidence <- (Output_data$nUncomp_1 + Output_data$nUncomp_2 + Output_data$nUncomp_3 + Output_data$nUncomp_4 + Output_data$nUncomp_5 + Output_data$nUncomp_6 + Output_data$nUncomp_7 + Output_data$nUncomp_8) /
                               (Output_data$nHost_1 + Output_data$nHost_2 + Output_data$nHost_3 + Output_data$nHost_4 + Output_data$nHost_5 + Output_data$nHost_6  + Output_data$nHost_7 + Output_data$nHost_8)
      

      
      Time_control_1 <- Output_data$Survey[Output_data$nMassVaccinations_8 >= 1][1] / 73 - 2
      Time_control_2 <- Output_data$Survey[Output_data$nMassVaccinations_8 >= 1][1] / 73
      Time_vaccine_1 <- Output_data$Survey[Output_data$nMassVaccinations_8 >= 1][1] / 73
      Time_vaccine_2 <- Output_data$Survey[Output_data$nMassVaccinations_8 >= 1][1] / 73 + 2
      Time_vaccine_1_s <- Output_data$Survey[Output_data$nMassVaccinations_8 >= 1][1] / 73 + 12
      Time_vaccine_2_s <- Output_data$Survey[Output_data$nMassVaccinations_8 >= 1][1] / 73 + 14
      
      # Look at prevalence in control
      Scenario_liste$Prevalance_control[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Prevalance[Output_data$Survey / 73 >= Time_control_1 & Output_data$Survey / 73 <= Time_control_2])
      
      # Look at prevalence in Vaccine (after 5 years)
      Scenario_liste$Prevalance_vaccine[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Prevalance[Output_data$Survey / 73 >= Time_vaccine_1 & Output_data$Survey / 73 <= Time_vaccine_2])
      
      # Look at prevalence in Vaccine (after 12 years)
      Scenario_liste$Prevalance_vaccine_s[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Prevalance[Output_data$Survey / 73 >= Time_vaccine_1_s & Output_data$Survey / 73 <= Time_vaccine_2_s])
      

      # Look at incidence in control
      Scenario_liste$Incidence_control[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Incidence[Output_data$Survey / 73 >= Time_control_1 & Output_data$Survey / 73 <= Time_control_2])
      
      # Look at incidence in vaccine
      Scenario_liste$Incidence_vaccine[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Incidence[Output_data$Survey / 73 >= Time_vaccine_1 & Output_data$Survey / 73 <= Time_vaccine_2])
      
      # Look at incidence in vaccine (after 12 years)
      Scenario_liste$Incidence_vaccine_s[Scenario_liste$scenario_name == name_scenario] <- mean(Output_data$Incidence[Output_data$Survey / 73 >= Time_vaccine_1_s & Output_data$Survey / 73 <= Time_vaccine_2_s])
      
      
    }
    
    
    Scenario_liste$Liste[Scenario_liste$scenario_name == name_scenario] <- i
    
    # Update the progress bar
    setTxtProgressBar(pb, i)
    
    # end the loop
  }
  
  # close the progress bar
  close(pb)
  
  # Save the  dataset in the summarized folder
  name_sumarised <- param_set_name(sample_num = pm$sample_num)
  summary_file <- paste0(name_sumarised, ".txt")
  summary_path <- paste0(pm$pth$summarised, summary_file)
  
  write.table(Scenario_liste, file = summary_path, sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
  
  # Merge the summarised results of each iterations
  full_table_name <- param_set_name(all_samples = TRUE)
  
  # Construct file name and path
  full_summary_file <- paste0(full_table_name, "_", ".txt")
  full_summary_path <- paste0(pm$pth$summarised, full_summary_file)
  
  # Initiate 'all sample' table
  full_param_table <- NULL
  
  # Process only necessary for adaptive sampling summary
  if (pm$sample_num > 0) {
    
    # Loop through previously generated samples
    for (i in (1:pm$sample_num - 1)) {
      
      # Construct sample results summary file name and path
      sample_file <- paste0(param_set_name(sample_num = i), ".txt")
      
      # Load up the param table associated with this sample number
      sample_param_table <- read.table(paste0(pm$pth$summarised, sample_file), sep = "\t", header = TRUE, as.is = TRUE)
      
      full_param_table <- rbind(full_param_table, sample_param_table)
    }
  }
  
  # Concatenate the param table we have just summarised
  full_param_table <- rbind(full_param_table, Scenario_liste)
  
  # Overwrite scenario names in this 'all samples' file
  full_param_table$scenario_name <- paste0("scenario_", 1:nrow(full_param_table))
  
  # (Over)-write full result tables across all samples
  write.table(full_param_table, full_summary_path, sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)

  } # end the function

#--------------------------------------------------------------
# function to estimate the time at which the EIR reach it peak.
#--------------------------------------------------------------
# Time_eir_max <- function(Output_data_2) {
#   
#   # Define the time step
#   time_step <- 5
#   Number_survey_generation <- 60 / time_step
#   Number_survey_years <- 365 / time_step
#   
#   # Select the data over a year
#   TIME <-Output_data_2[Output_data_2$Survey >= (29 * Number_survey_years) & Output_data_2$Survey < (30 * Number_survey_years),]
#   
#   # select the time at which the EIR is at it maximum
#   TIME_MAX <- TIME$Survey[TIME$simulatedEIR == max(TIME$simulatedEIR)]
#   
#   # convert the data into month
#   TIME_MAX <- (TIME_MAX / Number_survey_years - 29)
#   TIME_MAX_2 <- TIME_MAX * 365 / 30.4
#   
#   # return the time
#   return(TIME_MAX_2)
# }

#----------------------------------------------------------------------------------
# Estimate the selection coefficient of the resistant genotype in perennial setting.
#-----------------------------------------------------------------------------------
spread_R10_0 <- function(Output_data) {
  
  # define the time step
  time_step <- 5
  Number_survey_generation <- 60 / time_step # 1 generation equal 60 days
  Number_survey_years <- 365 / time_step
  
  # Select the time at which the last dose of vaccin is first deployed
  Time_Vaccine <- Output_data$Survey[Output_data$nEPIVaccinations_5 >= 1][1] # time start after one generation
  
  # Time to start the estimation of the selection coefficient
  time_start <- Time_Vaccine + (60 / 5) # add two month for the new parasite generation
  
  # select the end of the regression 
  time_end <- time_start + 15 * Number_survey_years # define time to stop the regression
  
  # select the data within this boundary
  time_spread <- Output_data$Survey[Output_data$Survey > time_start & Output_data$Survey < time_end] / Number_survey_years # time spread is converted in years
  
  R_num <- Output_data$innoculationsPerAgeGroup_R1[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R2[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R3[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R4[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R5[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R6[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R7[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R8[Output_data$Survey > time_start & Output_data$Survey < time_end]
  
  S_num <- Output_data$innoculationsPerAgeGroup_S1[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S2[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S3[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S4[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S5[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S6[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S7[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S8[Output_data$Survey > time_start & Output_data$Survey < time_end]
  
  # Estimate the frequency of resistant genotype
  Inoculation_R <- R_num / (S_num + R_num)
  
  Measurment_R <- Inoculation_R
  # Delet measurment that have to low or high frequency
  if (max(Inoculation_R) >= 0.99) {
    Measurment_R <- Inoculation_R[1:which(Inoculation_R ==  Inoculation_R[Inoculation_R >= 0.99][1])]
    time_spread <- time_spread[1:length(Measurment_R)]
  }
  
  if (min(Inoculation_R) <= 0.3) {
    Measurment_R <- Inoculation_R[1:which(Inoculation_R ==  Inoculation_R[Inoculation_R <= 0.3][1])]
    time_spread <- time_spread[1:length(Measurment_R)]
  }
  
  
  # estimate the selection coefficient if we have at least three measurement
  if (length(Measurment_R) >= 3 & is.na(sum(Measurment_R)) == F) {
    # Estimate the slope fo the regression
    slope <- lm(log(Measurment_R / (1 - Measurment_R)) ~ time_spread)$coefficients[2]
    
    # Adjust the slpoe to be in number of parasite generation
    slope <- slope / 6
    
  } else {
    slope <- NA
  }
  
  # return the slope
  return(slope)
}


#----------------------------------------------------------------------------------
# Estimate the selection coefficient of the resistant genotype in seasonal setting.
#----------------------------------------------------------------------------------
spread_R3_99 <- function(Output_data) {
  
  # Define the time step
  time_step <- 5
  Number_survey_generation <- 60 / time_step # 1 generation equal 60 days
  Number_survey_years <- 365 / time_step
  
  # Select the time at which the last dose of vaccin is first deployed
  Time_Vaccine <- Output_data$Survey[Output_data$nMassVaccinations_7 >= 1][1] # time start after one generation
  
  # Time to start the estimation of the selection coefficient
  time_start <- Time_Vaccine + (60 / 5) # add 6.5 years (make sur selection is constant 5 year that all are vaccinate+1.5 years for maximum half-life) month for the new parasite generation
  
  # Select the end of the regression
  time_end <- time_start + 9 * Number_survey_years # define time to stop the regression
  
  # Select the data within this boundary
  time_spread <- Output_data$Survey[Output_data$Survey > time_start & Output_data$Survey < time_end] / Number_survey_years # time spread is converted in years
  
  R_num <- Output_data$innoculationsPerAgeGroup_R1[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R2[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R3[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R4[Output_data$Survey > time_start & Output_data$Survey < time_end] +
           Output_data$innoculationsPerAgeGroup_R5[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R6[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R7[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R8[Output_data$Survey > time_start & Output_data$Survey < time_end] 
  
  S_num <- Output_data$innoculationsPerAgeGroup_S1[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S2[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S3[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S4[Output_data$Survey > time_start & Output_data$Survey < time_end] +
           Output_data$innoculationsPerAgeGroup_S5[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S6[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S7[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S8[Output_data$Survey > time_start & Output_data$Survey < time_end]
    
  
  # Estimate the frequency of resistant genotype
  Inoculation_R <- R_num / (S_num + R_num)
  Measurment_R <- Inoculation_R
  
  # Delet measurment that have to low frequency
  if (max(Inoculation_R) >= 0.99) {
    Measurment_R <- Inoculation_R[1:which(Inoculation_R ==  Inoculation_R[Inoculation_R >= 0.99][1])]
    time_spread <- time_spread[1:length(Measurment_R)]
  }
  
  if (min(Inoculation_R) <= 0.3) {
    Measurment_R <- Inoculation_R[1:which(Inoculation_R ==  Inoculation_R[Inoculation_R <= 0.3][1])]
    time_spread <- time_spread[1:length(Measurment_R)]
  }
  
  # Estimate the selection cofficient if we have at least one year of measurment
  if (length(Measurment_R) >= 72 & is.na(sum(Measurment_R)) == F) {
    # Estimate the slope fo the regression
    slope <- lm(log(Measurment_R / (1 - Measurment_R)) ~ time_spread)$coefficients[2]
    
    # Adjust the slpoe to be in number of parasite generation
    slope <- slope / 6
    
  } else {
    slope <- NA
  }
  
  # Return the slope
  return(slope)
}

#--------------------------------------------------------------------------------------------------------
# Estimate the selection coefficient of the resistant genotype when deploy to adults in perenial setting.
#--------------------------------------------------------------------------------------------------------
spread_adult <- function(Output_data) {
  
  # define the time step
  time_step <- 5
  Number_survey_generation <- 60 / time_step # 1 generation equal 60 days
  Number_survey_years <- 365 / time_step
  
  # Select the time at which the last dose of vaccin is first deployed
  Time_Vaccine <- Output_data$Survey[Output_data$nMassVaccinations_8 >= 1][1] # time start after one generation
  
  # Time to start the estimation of the selection coefficient
  time_start <- Time_Vaccine + (60 / 5) # add two month for the new parasite generation
  
  # select the end of the regression
  time_end <- time_start + 15 * Number_survey_years # define time to stop the regression
  
  # select the data within this boundary
  time_spread <- Output_data$Survey[Output_data$Survey > time_start & Output_data$Survey < time_end] / Number_survey_years # time spread is converted in years
  R_num <- Output_data$innoculationsPerAgeGroup_R1[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R2[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R3[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R4[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R5[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R6[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R7[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R8[Output_data$Survey > time_start & Output_data$Survey < time_end]
  
  S_num <- Output_data$innoculationsPerAgeGroup_S1[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S2[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S3[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S4[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S5[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S6[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S7[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S8[Output_data$Survey > time_start & Output_data$Survey < time_end]
  
  # Estimate the frequency of resistant genotype
  Inoculation_R <- R_num / (S_num + R_num)
  
  Measurment_R <- Inoculation_R
  
  # Delete measurement that have to low or high frequency
  if (max(Inoculation_R) >= 0.99) {
    Measurment_R <- Inoculation_R[1:which(Inoculation_R ==  Inoculation_R[Inoculation_R >= 0.99][1])]
    time_spread <- time_spread[1:length(Measurment_R)]
  }
  if (min(Inoculation_R) <= 0.1) {
    Measurment_R <- Inoculation_R[1:which(Inoculation_R ==  Inoculation_R[Inoculation_R <= 0.3][1])]
    time_spread <- time_spread[1:length(Measurment_R)]
  }
  
  # Estimate the selection coefficient if we have at least two measurement
  if (length(Measurment_R) >= 3 & is.na(sum(Measurment_R)) == F) {
    # Estimate the slope fo the regression
    slope <- lm(log(Measurment_R / (1 - Measurment_R)) ~ time_spread)$coefficients[2]
    
    # Adjust the slope to be in number of parasite generation
    slope <- slope / 6
    
  } else {
    slope <- NA
  }
  
  # return the slope
  return(slope)
}

#--------------------------------------------------------------------------------------------------------
# Estimate the selection coefficient of the resistant genotype when deploy to adults in seasonal setting.
#--------------------------------------------------------------------------------------------------------
spread_adult_seasonal <- function(Output_data) {
  
  # define the time step
  time_step <- 5
  Number_survey_generation <- 60 / time_step # 1 generation equal 60 days
  Number_survey_years <- 365 / time_step
  
  # Select the time at which the last dose of vaccin is first deployed
  Time_Vaccine <- Output_data$Survey[Output_data$nMassVaccinations_8 >= 1][1] # time start after one generation
  
  # Time to start the estimation of the selection coefficient
  time_start <- Time_Vaccine + (60 / 5) # add two month for the new parasite generation
  
  # select the end of the regression
  time_end <- time_start + 15 * Number_survey_years # define time to stop the regression
  
  # select the data within this boundary
  time_spread <- Output_data$Survey[Output_data$Survey > time_start & Output_data$Survey < time_end] / Number_survey_years # time spread is converted in years
  R_num <- Output_data$innoculationsPerAgeGroup_R1[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R2[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R3[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R4[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R5[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R6[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R7[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_R8[Output_data$Survey > time_start & Output_data$Survey < time_end]
  
  S_num <- Output_data$innoculationsPerAgeGroup_S1[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S2[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S3[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S4[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S5[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S6[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S7[Output_data$Survey > time_start & Output_data$Survey < time_end] + 
           Output_data$innoculationsPerAgeGroup_S8[Output_data$Survey > time_start & Output_data$Survey < time_end]
  
  # Estimate the frequency of resistant genotype
  Inoculation_R <- R_num / (S_num + R_num)
  
  Measurment_R <- Inoculation_R
  
  # Plot(time_spread,Inoculation_R)
  # Delet measurment that have to low or high frequency
  if (sum(Inoculation_R) == "NaN") {
    slope <- NA
  } else{
    if (max(Inoculation_R) >= 0.99) {
      Measurment_R <- Inoculation_R[1:which(Inoculation_R ==  Inoculation_R[Inoculation_R >= 0.99][1])]
      time_spread <- time_spread[1:length(Measurment_R)]
    }
    if (min(Inoculation_R) <= 0.3) {
      Measurment_R <- Inoculation_R[1:which(Inoculation_R ==  Inoculation_R[Inoculation_R <= 0.3][1])]
      time_spread <- time_spread[1:length(Measurment_R)]
    }
    
    # estimate the selection cofficient if we have at least two measurment
    if (length(Measurment_R) >= 72 & is.na(sum(Measurment_R)) == F) {
      # Estimate the slope fo the regression
      slope <- lm(log(Measurment_R / (1 - Measurment_R)) ~ time_spread)$coefficients[2]
      
      # Adjust the slpoe to be in number of parasite generation
      slope <- slope / 6
      
    } else {
      slope <- NA
    }
  }
  # return the slope
  return(slope)
}
