######################################################################################
#     Post process of the data                                                       #
#                                                                                    #
# Task : reorganise the output of Openmalaria in a table                             #
#        that display all the measurment at each survey time                         #
# Input: row outpout file form Openmalaria                                           #
# output: Table of each measurment value at each time step                           #
#                                                                                    #
# authors: thiery.masserey@swisstph.ch adapted from andrewjames.shattock@swisstph.ch #                                      
######################################################################################

#--------------------------------------------------------
# function to post-process the outputdata of OpenMalaria.
#--------------------------------------------------------
Postprocess <- function(pm) {
  
  # Message to the console
  message("  - Post Processing ")
  
  # Define list of output files
  Output <- list.files(pm$pth$sim_out) # Make a list of all the output file
  
  # Initiate progress bar
  pb <- txtProgressBar(
    min = 0,
    max = length(Output),
    initial = 0,
    width = 100,
    style = 3)
  
  # Start the loop that will do the post process for each output file
  for (i in 1:length(Output)) {
    
    # define file name, file pathe, and download it i<-1
    Output_data_name <- Output[i] # select the file name
    Output_data_file_path <- file.path(pm$pth$sim_out, Output_data_name) # define the path to the file
    Output_data <- read.table(Output_data_file_path) # open the file
    
    # Extract the survey Time in a new dataframe
    a <- as.data.frame(table(Output_data$V1))
    Survey <- as.numeric(a$Var1)
    Output_data_2 <- as.data.frame(Survey)
    
    # Extract the survey measure number
    Indicator <- Output_data$V3[Output_data$V1 == 1]
    
    # Extract information about age group and resistant/sensitive parasite
    Age_groupe <- Output_data$V2[Output_data$V1 == 1]
    
    
    # Define the meaning of each measurement
    Indicators_meaning <- c(
      "nHost_1", # 0
      "nHost_2",
      "nHost_3",
      "nHost_4",
      "nHost_5", # 0
      "nHost_6",
      "nHost_7",
      "nHost_8",
      
      "nInfect_1", # 1                   The number of human hosts with an infection
      "nInfect_2",
      "nInfect_3",
      "nInfect_4",
      "nInfect_5", # 1                   The number of human hosts with an infection
      "nInfect_6",
      "nInfect_7",
      "nInfect_8",
      
      "nExpectd_1", # 2                  The expected number of infected hosts
      "nExpectd_2",
      "nExpectd_3",
      "nExpectd_4",
      "nExpectd_5", # 2                  The expected number of infected hosts
      "nExpectd_6",
      "nExpectd_7",
      "nExpectd_8",
      
      "nPatent_1", #  3                 The number of human hosts whose total (blood-stage) parasite density is above the detection threshold
      "nPatent_2",
      "nPatent_3",
      "nPatent_4",
      "nPatent_5", # 3                 The number of human hosts whose total (blood-stage) parasite density is above the detection threshold
      "nPatent_6",
      "nPatent_7",
      "nPatent_8",
      
      "nTransmit", # 7              Infectiousness of human population to mosquitoes: sum(p(transmit_i)) across humans i, weighted by availability to mosquitoes.
      
      "nTreatments1_1", # 11           number of treatments (1st line)
      "nTreatments1_2",
      "nTreatments1_3",
      "nTreatments1_4",
      "nTreatments1_5",
      "nTreatments1_6",
      "nTreatments1_7",
      "nTreatments1_8",
      
      "nUncomp_1", # 14 number of episodes uncomplicated1
      "nUncomp_2",
      "nUncomp_3",
      "nUncomp_4",
      "nUncomp_5", # 14 number of episodes uncomplicated1
      "nUncomp_6",
      "nUncomp_7",
      "nUncomp_8",
      
      "nEPIVaccinations_1",# 20
      "nEPIVaccinations_2",
      "nEPIVaccinations_3",
      "nEPIVaccinations_4",
      "nEPIVaccinations_5",
      "nEPIVaccinations_6",
      "nEPIVaccinations_7",
      "nEPIVaccinations_8",
      
      "nMassVaccinations_1",# 22
      "nMassVaccinations_2",
      "nMassVaccinations_3",
      "nMassVaccinations_4",
      "nMassVaccinations_5",
      "nMassVaccinations_6",
      "nMassVaccinations_7",
      "nMassVaccinations_8",
      
      "innoculationsPerAgeGroup_S1",# 30
      "innoculationsPerAgeGroup_R1",
      "innoculationsPerAgeGroup_S2",
      "innoculationsPerAgeGroup_R2",
      "innoculationsPerAgeGroup_S3",
      "innoculationsPerAgeGroup_R3",
      "innoculationsPerAgeGroup_S4",
      "innoculationsPerAgeGroup_R4",
      "innoculationsPerAgeGroup_S5",
      "innoculationsPerAgeGroup_R5",
      "innoculationsPerAgeGroup_S6",
      "innoculationsPerAgeGroup_R6",
      "innoculationsPerAgeGroup_S7",
      "innoculationsPerAgeGroup_R7",
      "innoculationsPerAgeGroup_S8",
      "innoculationsPerAgeGroup_R8",
      
      "Vector_Nv",# 32                Host seeking mosquito population size at this time step, species 1
      
      "inputEIR", # 35
      "simulatedEIR", # 36              EIR generated by transmission model as measured by inoculations recieved by adults
      
      "nMDAs_1", # 52
      "nMDAs_2",
      "nMDAs_3",
      "nMDAs_4",#  Number of drug doses given via mass deployment (MDA or screen&treat)
      "nMDAs_5", # 52
      "nMDAs_6",
      "nMDAs_7",
      "nMDAs_8",
      
      "nTreatDiagnostics_1",#   64      Number of diagnostic tests performed (if in the health system description, useDiagnosticUC="true").
      "nTreatDiagnostics_2",
      "nTreatDiagnostics_3",
      "nTreatDiagnostics_4",
      "nTreatDiagnostics_5",#   64      Number of diagnostic tests performed (if in the health system description, useDiagnosticUC="true").
      "nTreatDiagnostics_6",
      "nTreatDiagnostics_7",
      "nTreatDiagnostics_8",
      
      "nInfectByGenotype_S1",# 69
      "nInfectByGenotype_R1",
      "nInfectByGenotype_S2",
      "nInfectByGenotype_R2",
      "nInfectByGenotype_S3",
      "nInfectByGenotype_R3",
      "nInfectByGenotype_S4",
      "nInfectByGenotype_R4",
      "nInfectByGenotype_S5", # 69
      "nInfectByGenotype_R5",
      "nInfectByGenotype_S6",
      "nInfectByGenotype_R6",
      "nInfectByGenotype_S7",
      "nInfectByGenotype_R7",
      "nInfectByGenotype_S8",
      "nInfectByGenotype_R8",
      
      "nPatentByGenotype_S1", # 70
      "nPatentByGenotype_R1",
      "nPatentByGenotype_S2",
      "nPatentByGenotype_R2",
      "nPatentByGenotype_S3",
      "nPatentByGenotype_R3",
      "nPatentByGenotype_S4",
      "nPatentByGenotype_R4",
      "nPatentByGenotype_S5", # 70
      "nPatentByGenotype_R5",
      "nPatentByGenotype_S6",
      "nPatentByGenotype_R6",
      "nPatentByGenotype_S7",
      "nPatentByGenotype_R7",
      "nPatentByGenotype_S8",
      "nPatentByGenotype_R8",
      
      "nHostDrugConcNonZero_A1", #  72    For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
      "nHostDrugConcNonZero_B1",
      
      "nHostDrugConcNonZero_A2", # For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
      "nHostDrugConcNonZero_B2",
      
      "nHostDrugConcNonZero_A3", # For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
      "nHostDrugConcNonZero_B3",
      
      "nHostDrugConcNonZero_A4", # For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
      "nHostDrugConcNonZero_B4",
      
      "nHostDrugConcNonZero_A5", # For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
      "nHostDrugConcNonZero_B5",
      
      "nHostDrugConcNonZero_A6", # For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
      "nHostDrugConcNonZero_B6",
      
      "nHostDrugConcNonZero_A7", # For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
      "nHostDrugConcNonZero_B7",
      
      "nHostDrugConcNonZero_A8", # For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
      "nHostDrugConcNonZero_B8",
      
      
      "sumLogDrugConcNonZero_A1", # 73 For each drug type in the pharmacology section of the XML, report the sum of the natural logarithm of the drug concentration in hosts with non-zero concentration.
      "sumLogDrugConcNonZero_B1",
      
      "sumLogDrugConcNonZero_A2", # For each drug type in the pharmacology section of the XML, report the sum of the natural logarithm of the drug concentration in hosts with non-zero concentration.
      "sumLogDrugConcNonZero_B2",
      
      "sumLogDrugConcNonZero_A3", # For each drug type in the pharmacology section of the XML, report the sum of the natural logarithm of the drug concentration in hosts with non-zero concentration.
      "sumLogDrugConcNonZero_B3",
      
      "sumLogDrugConcNonZero_A4", # For each drug type in the pharmacology section of the XML, report the sum of the natural logarithm of the drug concentration in hosts with non-zero concentration.
      "sumLogDrugConcNonZero_B4",
      
      "sumLogDrugConcNonZero_A5", # For each drug type in the pharmacology section of the XML, report the sum of the natural logarithm of the drug concentration in hosts with non-zero concentration.
      "sumLogDrugConcNonZero_B5",
      
      "sumLogDrugConcNonZero_A6", # For each drug type in the pharmacology section of the XML, report the sum of the natural logarithm of the drug concentration in hosts with non-zero concentration.
      "sumLogDrugConcNonZero_B6",
      
      "sumLogDrugConcNonZero_A7", # For each drug type in the pharmacology section of the XML, report the sum of the natural logarithm of the drug concentration in hosts with non-zero concentration.
      "sumLogDrugConcNonZero_B7",
      
      "sumLogDrugConcNonZero_A8", # For each drug type in the pharmacology section of the XML, report the sum of the natural logarithm of the drug concentration in hosts with non-zero concentration.
      "sumLogDrugConcNonZero_B8")
    
    if (pm$opts$Type_analysis == "Vaccine_R_adult_AIV" | pm$opts$Type_analysis == "Vaccine_R_adult_AIV_TBV" | pm$opts$Type_analysis == "Vaccine_efficacy_adult") {
      Indicators_meaning <- c(
        "nHost_1", # 0
        "nHost_2",
        "nHost_3",
        "nHost_4",
        "nHost_5", # 0
        "nHost_6",
        "nHost_7",
        "nHost_8",
        
        "nInfect_1", # 1                   The number of human hosts with an infection
        "nInfect_2",
        "nInfect_3",
        "nInfect_4",
        "nInfect_5",
        "nInfect_6",
        "nInfect_7",
        "nInfect_8",
        
        "nExpectd_1", #2                  The expected number of infected hosts
        "nExpectd_2",
        "nExpectd_3",
        "nExpectd_4",
        "nExpectd_5",
        "nExpectd_6",
        "nExpectd_7",
        "nExpectd_8",
        
        "nPatent_1",#  3                 The number of human hosts whose total (blood-stage) parasite density is above the detection threshold
        "nPatent_2",
        "nPatent_3",
        "nPatent_4",
        "nPatent_5",
        "nPatent_6",
        "nPatent_7",
        "nPatent_8",
        
        "nTransmit", #   7              Infectiousness of human population to mosquitoes: sum(p(transmit_i)) across humans i, weighted by availability to mosquitoes.
        
        "nTreatments1_1", #   11           number of treatments (1st line)
        "nTreatments1_2",
        "nTreatments1_3",
        "nTreatments1_4",
        "nTreatments1_5",#  11           number of treatments (1st line)
        "nTreatments1_6",
        "nTreatments1_7",
        "nTreatments1_8",
        
        "nUncomp_1",# 14 number of episodes uncomplicated1
        "nUncomp_2",
        "nUncomp_3",
        "nUncomp_4",
        "nUncomp_5",
        "nUncomp_6",
        "nUncomp_7",
        "nUncomp_8",
        
        "nEPIVaccinations_1",# 20
        "nEPIVaccinations_2",
        "nEPIVaccinations_3",
        "nEPIVaccinations_4",
        "nEPIVaccinations_5",
        "nEPIVaccinations_6",
        "nEPIVaccinations_7",
        "nEPIVaccinations_8",
        
        "nMassVaccinations_1",# 22
        "nMassVaccinations_2",
        "nMassVaccinations_3",
        "nMassVaccinations_4",
        "nMassVaccinations_5",
        "nMassVaccinations_6",
        "nMassVaccinations_7",
        "nMassVaccinations_8",
        
        "innoculationsPerAgeGroup_S1",# 30
        "innoculationsPerAgeGroup_R1",
        "innoculationsPerAgeGroup_S2",
        "innoculationsPerAgeGroup_R2",
        "innoculationsPerAgeGroup_S3",
        "innoculationsPerAgeGroup_R3",
        "innoculationsPerAgeGroup_S4",
        "innoculationsPerAgeGroup_R4",
        "innoculationsPerAgeGroup_S5",
        "innoculationsPerAgeGroup_R5",
        "innoculationsPerAgeGroup_S6",
        "innoculationsPerAgeGroup_R6",
        "innoculationsPerAgeGroup_S7",
        "innoculationsPerAgeGroup_R7",
        "innoculationsPerAgeGroup_S8",
        "innoculationsPerAgeGroup_R8",
        
        "Vector_Nv", # 32                Host seeking mosquito population size at this time step, species 1
        
        "inputEIR", # 35
        
        "simulatedEIR",# 36              EIR generated by transmission model as measured by inoculations recieved by adults
        
        "nMDAs_1",# 52 Number of drug doses given via mass deployment (MDA or screen&treat)
        "nMDAs_2",
        "nMDAs_3",
        "nMDAs_4",
        "nMDAs_5",
        "nMDAs_6",
        "nMDAs_7",
        "nMDAs_8",
        
        "nTreatDiagnostics_1", #   64      Number of diagnostic tests performed (if in the health system description, useDiagnosticUC="true").
        "nTreatDiagnostics_2",
        "nTreatDiagnostics_3",
        "nTreatDiagnostics_4",
        "nTreatDiagnostics_5",
        "nTreatDiagnostics_6",
        "nTreatDiagnostics_7",
        "nTreatDiagnostics_8",
        
        "nInfectByGenotype_S1",# 69
        "nInfectByGenotype_R1",
        "nInfectByGenotype_S2",
        "nInfectByGenotype_R2",
        "nInfectByGenotype_S3",
        "nInfectByGenotype_R3",
        "nInfectByGenotype_S4",
        "nInfectByGenotype_R4",
        "nInfectByGenotype_S5",
        "nInfectByGenotype_R5",
        "nInfectByGenotype_S6",
        "nInfectByGenotype_R6",
        "nInfectByGenotype_S7",
        "nInfectByGenotype_R7",
        "nInfectByGenotype_S8",
        "nInfectByGenotype_R8",
        
        "nPatentByGenotype_S1",# 70
        "nPatentByGenotype_R1",
        "nPatentByGenotype_S2",
        "nPatentByGenotype_R2",
        "nPatentByGenotype_S3",
        "nPatentByGenotype_R3",
        "nPatentByGenotype_S4",
        "nPatentByGenotype_R4",
        "nPatentByGenotype_S5",
        "nPatentByGenotype_R5",
        "nPatentByGenotype_S6",
        "nPatentByGenotype_R6",
        "nPatentByGenotype_S7",
        "nPatentByGenotype_R7",
        "nPatentByGenotype_S8",
        "nPatentByGenotype_R8",
        
        "nHostDrugConcNonZero_A1", #  72    For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
        "nHostDrugConcNonZero_B1",
        "nHostDrugConcNonZero_C1",
        "nHostDrugConcNonZero_D1",
        
        "nHostDrugConcNonZero_A2",
        "nHostDrugConcNonZero_B2",
        "nHostDrugConcNonZero_C2",
        "nHostDrugConcNonZero_D2",
        
        "nHostDrugConcNonZero_A3", 
        "nHostDrugConcNonZero_B3",
        "nHostDrugConcNonZero_C3",
        "nHostDrugConcNonZero_D3",
        
        "nHostDrugConcNonZero_A4",
        "nHostDrugConcNonZero_B4",
        "nHostDrugConcNonZero_C4",
        "nHostDrugConcNonZero_D4",
        
        "nHostDrugConcNonZero_A5",
        "nHostDrugConcNonZero_B5",
        "nHostDrugConcNonZero_C5",
        "nHostDrugConcNonZero_D5",
        
        "nHostDrugConcNonZero_A6",
        "nHostDrugConcNonZero_B6",
        "nHostDrugConcNonZero_C6",
        "nHostDrugConcNonZero_D6",
        
        "nHostDrugConcNonZero_A7",
        "nHostDrugConcNonZero_B7",
        "nHostDrugConcNonZero_C7",
        "nHostDrugConcNonZero_D7",
        
        "nHostDrugConcNonZero_A8",
        "nHostDrugConcNonZero_B8",
        "nHostDrugConcNonZero_C8",
        "nHostDrugConcNonZero_D8",
        
        "sumLogDrugConcNonZero_A1", 
        "sumLogDrugConcNonZero_B1",
        "sumLogDrugConcNonZero_C1",
        "sumLogDrugConcNonZero_D1",
        
        "sumLogDrugConcNonZero_A2",
        "sumLogDrugConcNonZero_B2",
        "sumLogDrugConcNonZero_C2",
        "sumLogDrugConcNonZero_D2",
        
        "sumLogDrugConcNonZero_A3",
        "sumLogDrugConcNonZero_B3",
        "sumLogDrugConcNonZero_C3",
        "sumLogDrugConcNonZero_D3",
        
        "sumLogDrugConcNonZero_A4",
        "sumLogDrugConcNonZero_B4",
        "sumLogDrugConcNonZero_C4",
        "sumLogDrugConcNonZero_D4",
        
        "sumLogDrugConcNonZero_A5",#     
        "sumLogDrugConcNonZero_B5",
        "sumLogDrugConcNonZero_C5",
        "sumLogDrugConcNonZero_D5",
        
        "sumLogDrugConcNonZero_A6",
        "sumLogDrugConcNonZero_B6",
        "sumLogDrugConcNonZero_C6",
        "sumLogDrugConcNonZero_D6",
        
        "sumLogDrugConcNonZero_A7",
        "sumLogDrugConcNonZero_B7",
        "sumLogDrugConcNonZero_C7",
        "sumLogDrugConcNonZero_D7",
        
        "sumLogDrugConcNonZero_A8",
        "sumLogDrugConcNonZero_B8",
        "sumLogDrugConcNonZero_C8",
        "sumLogDrugConcNonZero_D8"
      )
      
    }
    
    if (pm$pth$xml_base== "/scicore/home/penny/masthi00/vaccine_resistance/SIM_FOLDER/Vaccine_resistance_seasonal_hybride_efficacy_SPAQ.xml") {
      
      Indicators_meaning <- c(
        "nHost_1",# 0
        "nHost_2",
        "nHost_3",
        "nHost_4",
        "nHost_5",
        "nHost_6",
        "nHost_7",
        "nHost_8",
        
        "nInfect_1",# 1                   The number of human hosts with an infection
        "nInfect_2",
        "nInfect_3",
        "nInfect_4",
        "nInfect_5",
        "nInfect_6",
        "nInfect_7",
        "nInfect_8",
        
        "nExpectd_1",#2                  The expected number of infected hosts
        "nExpectd_2",
        "nExpectd_3",
        "nExpectd_4",
        "nExpectd_5",
        "nExpectd_6",
        "nExpectd_7",
        "nExpectd_8",
        
        "nPatent_1",#  3                 The number of human hosts whose total (blood-stage) parasite density is above the detection threshold
        "nPatent_2",
        "nPatent_3",
        "nPatent_4",
        "nPatent_5",
        "nPatent_6",
        "nPatent_7",
        "nPatent_8",
        
        "nTransmit",#   7              Infectiousness of human population to mosquitoes: sum(p(transmit_i)) across humans i, weighted by availability to mosquitoes.
        
        "nTreatments1_1",#11           number of treatments (1st line)
        "nTreatments1_2",
        "nTreatments1_3",
        "nTreatments1_4",
        "nTreatments1_5",
        "nTreatments1_6",
        "nTreatments1_7",
        "nTreatments1_8",
        
        "nUncomp_1", # 14 number of episodes uncomplicated1
        "nUncomp_2",#
        "nUncomp_3",
        "nUncomp_4",
        "nUncomp_5",
        "nUncomp_6",
        "nUncomp_7",
        "nUncomp_8",
        
        "nEPIVaccinations_1",# 20
        "nEPIVaccinations_2",
        "nEPIVaccinations_3",
        "nEPIVaccinations_4",
        "nEPIVaccinations_5",
        "nEPIVaccinations_6",
        "nEPIVaccinations_7",
        "nEPIVaccinations_8",
        
        "nMassVaccinations_1",# 22
        "nMassVaccinations_2",
        "nMassVaccinations_3",
        "nMassVaccinations_4",
        "nMassVaccinations_5",
        "nMassVaccinations_6",
        "nMassVaccinations_7",
        "nMassVaccinations_8",
        
        "innoculationsPerAgeGroup_S1",# 30
        "innoculationsPerAgeGroup_R1",
        "innoculationsPerAgeGroup_S2",
        "innoculationsPerAgeGroup_R2",
        "innoculationsPerAgeGroup_S3",
        "innoculationsPerAgeGroup_R3",
        "innoculationsPerAgeGroup_S4",
        "innoculationsPerAgeGroup_R4",
        "innoculationsPerAgeGroup_S5",
        "innoculationsPerAgeGroup_R5",
        "innoculationsPerAgeGroup_S6",
        "innoculationsPerAgeGroup_R6",
        "innoculationsPerAgeGroup_S7",
        "innoculationsPerAgeGroup_R7",
        "innoculationsPerAgeGroup_S8",
        "innoculationsPerAgeGroup_R8",
        
        "Vector_Nv",# 32                Host seeking mosquito population size at this time step, species 1
        
        "inputEIR", # 35
        "simulatedEIR", # 36              EIR generated by transmission model as measured by inoculations recieved by adults
        
        "nMDAs_1",# 52
        "nMDAs_2",
        "nMDAs_3",
        "nMDAs_4",#  Number of drug doses given via mass deployment (MDA or screen&treat)
        "nMDAs_5",# 52
        "nMDAs_6",
        "nMDAs_7",
        "nMDAs_8",
        
        "nTreatDiagnostics_1",#64      Number of diagnostic tests performed (if in the health system description, useDiagnosticUC="true").
        "nTreatDiagnostics_2",
        "nTreatDiagnostics_3",
        "nTreatDiagnostics_4",
        "nTreatDiagnostics_5",
        "nTreatDiagnostics_6",
        "nTreatDiagnostics_7",
        "nTreatDiagnostics_8",
        
        "nInfectByGenotype_S1",# 69
        "nInfectByGenotype_R1",
        "nInfectByGenotype_S2",
        "nInfectByGenotype_R2",
        "nInfectByGenotype_S3",
        "nInfectByGenotype_R3",
        "nInfectByGenotype_S4",
        "nInfectByGenotype_R4",
        "nInfectByGenotype_S5",# 69
        "nInfectByGenotype_R5",
        "nInfectByGenotype_S6",
        "nInfectByGenotype_R6",
        "nInfectByGenotype_S7",
        "nInfectByGenotype_R7",
        "nInfectByGenotype_S8",
        "nInfectByGenotype_R8",
        
        "nPatentByGenotype_S1",# 70
        "nPatentByGenotype_R1",
        "nPatentByGenotype_S2",
        "nPatentByGenotype_R2",
        "nPatentByGenotype_S3",
        "nPatentByGenotype_R3",
        "nPatentByGenotype_S4",
        "nPatentByGenotype_R4",
        "nPatentByGenotype_S5",# 70
        "nPatentByGenotype_R5",
        "nPatentByGenotype_S6",
        "nPatentByGenotype_R6",
        "nPatentByGenotype_S7",
        "nPatentByGenotype_R7",
        "nPatentByGenotype_S8",
        "nPatentByGenotype_R8",
        
        "nHostDrugConcNonZero_A1", #  72    For each drug type in the pharmacology section of the XML, report the number of humans with non-zero concentration
        "nHostDrugConcNonZero_B1",
        "nHostDrugConcNonZero_C1",
        "nHostDrugConcNonZero_D1",
        "nHostDrugConcNonZero_E1",
        
        "nHostDrugConcNonZero_A2", 
        "nHostDrugConcNonZero_B2",
        "nHostDrugConcNonZero_C2",
        "nHostDrugConcNonZero_D2",
        "nHostDrugConcNonZero_E2",
        
        "nHostDrugConcNonZero_A3", 
        "nHostDrugConcNonZero_B3",
        "nHostDrugConcNonZero_C3",
        "nHostDrugConcNonZero_D3",
        "nHostDrugConcNonZero_E3",
        
        "nHostDrugConcNonZero_A4", 
        "nHostDrugConcNonZero_B4",
        "nHostDrugConcNonZero_C4",
        "nHostDrugConcNonZero_D4",
        "nHostDrugConcNonZero_E4",
        
        "nHostDrugConcNonZero_A5", 
        "nHostDrugConcNonZero_B5",
        "nHostDrugConcNonZero_C5",
        "nHostDrugConcNonZero_D5",
        "nHostDrugConcNonZero_E5",
        
        "nHostDrugConcNonZero_A6",
        "nHostDrugConcNonZero_B6",
        "nHostDrugConcNonZero_C6",
        "nHostDrugConcNonZero_D6",
        "nHostDrugConcNonZero_E6",
        
        "nHostDrugConcNonZero_A7",
        "nHostDrugConcNonZero_B7",
        "nHostDrugConcNonZero_C7",
        "nHostDrugConcNonZero_D7",
        "nHostDrugConcNonZero_E7",
        
        "nHostDrugConcNonZero_A8",
        "nHostDrugConcNonZero_B8",
        "nHostDrugConcNonZero_C8",
        "nHostDrugConcNonZero_D8",
        "nHostDrugConcNonZero_E8",
        
        "sumLogDrugConcNonZero_A1",
        "sumLogDrugConcNonZero_B1",
        "sumLogDrugConcNonZero_C1",
        "sumLogDrugConcNonZero_D1",
        "sumLogDrugConcNonZero_E1",
        
        "sumLogDrugConcNonZero_A2",
        "sumLogDrugConcNonZero_B2",
        "sumLogDrugConcNonZero_C2",
        "sumLogDrugConcNonZero_D2",
        "sumLogDrugConcNonZero_E2",
        
        "sumLogDrugConcNonZero_A3",
        "sumLogDrugConcNonZero_B3",
        "sumLogDrugConcNonZero_C3",
        "sumLogDrugConcNonZero_D3",
        "sumLogDrugConcNonZero_E3",
        
        "sumLogDrugConcNonZero_A4",
        "sumLogDrugConcNonZero_B4",
        "sumLogDrugConcNonZero_C4",
        "sumLogDrugConcNonZero_D4",
        "sumLogDrugConcNonZero_E4",
        
        "sumLogDrugConcNonZero_A5",
        "sumLogDrugConcNonZero_B5",
        "sumLogDrugConcNonZero_C5",
        "sumLogDrugConcNonZero_D5",
        "sumLogDrugConcNonZero_E5",
        
        "sumLogDrugConcNonZero_A6",
        "sumLogDrugConcNonZero_B6",
        "sumLogDrugConcNonZero_C6",
        "sumLogDrugConcNonZero_D6",
        "sumLogDrugConcNonZero_E6",
        
        "sumLogDrugConcNonZero_A7",
        "sumLogDrugConcNonZero_B7",
        "sumLogDrugConcNonZero_C7",
        "sumLogDrugConcNonZero_D7",
        "sumLogDrugConcNonZero_E7",
        
        "sumLogDrugConcNonZero_A8",
        "sumLogDrugConcNonZero_B8",
        "sumLogDrugConcNonZero_C8",
        "sumLogDrugConcNonZero_D8",
        "sumLogDrugConcNonZero_E8")
      
    }
    
    
    # Extract the information for each measurement at each timestep, and save it in the new dataframe (Output_data_2)
    for (j in 1:(length(Indicator))) {
      Output_data_2[j + 1] <- Output_data$V4[Output_data$V3 == Indicator[j] & Output_data$V2 == Age_groupe[j]] # put in the j +1 colone (due that we have one colone of the time survey), the value of the jth indicator
      colnames(Output_data_2)[j + 1] <- Indicators_meaning[j] # give the name of the indicators
    }
    
    # Save the new dataset in the postprocess folder
    Postprocess_data_name <- paste0("PostProcess_", Output_data_name)
    Postprocess_data_path <- file.path(pm$pth$processed, Postprocess_data_name)
    write.table(Output_data_2, file = Postprocess_data_path, sep = ";")
    
    # Update progress bar
    setTxtProgressBar(pb, i)
    
  }
  
  # close the progress bar
  close(pb)
}
