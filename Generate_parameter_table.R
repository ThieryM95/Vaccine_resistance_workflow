######################################################################################
#      Generate the parameter tables for the simulation that will be run             #
#                                                                                    #
#                                                                                    #
# Task: This function create the table of input parameter for each simulation        #
# Input: number of simulation, number of seed, contrained parameter values,          #
#        parameter space for parameter that are not constrained (see Option.R)       #
# Output: A table of the parameter value of each parameter for each simulation       #
#                                                                                    #
# authors: thiery.masserey@swisstph.ch adapted from andrewjames.shattock@swisstph.ch #
######################################################################################

# --------------------------------------------
# Generate the input table for the simulation.
# --------------------------------------------
generate_param_table <- function(pm, param_table = NULL) {
  
  # message in console
  message("* Generating parameter table")
  
  # File name for this parameter table (see adaptive_sampling.R)
  param_table_name <- param_set_name(sample_num = pm$sample_num)
  param_file <- paste0(pm$pth$param_table, param_table_name, ".txt")
  
  # if sample number is == 0 the parameter table is generate via latin hypercube sampling
  if (pm$sample_num == 0) {
    # ---- Generate latin hypercube samples for intervention coverages ----
    
    # Load the define parameter  ranges into a matrix
    cov_ranges <- matrix(c(pm$prog$min, pm$prog$max), ncol = 2)
    
    # Generate latin hypercube sample between the coverage limits
    param_table <- as.data.frame(lhs(pm$opts$lhc_samples, cov_ranges))
    
    # Variable names of parameters must match @parameter@ fields in base xml
    colnames(param_table) <- paste(pm$prog$prog_names)
    
    
    # ---- Define the parameter that were constrained (the different arm) ----
    
    # if aimed to estimate the selection coefficient for vaccine deployed to children
    if (pm$opts$Type_analysis == "Vaccine_R_perennial" | pm$opts$Type_analysis == "Vaccine_R_seasonal") {
      
      # Define the level and value for each fixed parameter
      Vaccine_type <- pm$settings$Vaccine_type
      Coverage_reduced <- pm$settings$Coverage_reduced
      season <- pm$settings$seasonality
      
      # Merge into full factorial table
      setting_table <- Reduce(merge, list(names(season), 
                                          as.data.frame(Vaccine_type),
                                          as.data.frame(Coverage_reduced)))
      
      colnames(setting_table) <- c("seasonality","Vaccine_type", "Coverage_reduced")
    }
    
    # if aimed to estimate the selection coefficient for vaccine deployed to adults
    if (pm$opts$Type_analysis == "Vaccine_R_adult_AIV" | pm$opts$Type_analysis == "Vaccine_R_adult_AIV_TBV") {
      
      # Define the level and value for each parameter
      Coverage_reduced <- pm$settings$Coverage_reduced
      season <- pm$settings$seasonality
      Age <- pm$settings$Age
      Blood_coverage <- pm$settings$Blood_coverage
      
      # Merge into full factorial table
      setting_table <-Reduce(merge, list(names(season),
                                         as.data.frame(Coverage_reduced),
                                         as.data.frame(Age),
                                         as.data.frame(Blood_coverage)))
      
      # names columns
      colnames(setting_table) <- c("seasonality", "Coverage_reduced", "Age", "Blood_coverage") 
    }
    
    # if aimed to estimate the protective effectiveness of vaccine deployed to children
    if (pm$opts$Type_analysis == "Vaccine_efficacy_children") {
      
      # Define the level and value for each parameter
      Vaccine_type <- pm$settings$Vaccine_type
      season <- pm$settings$seasonality
      Coverage <- pm$settings$Coverage
      initialEfficacy <- pm$settings$initialEfficacy
      half_life <- pm$settings$half_life
      Degree_resistance <- pm$settings$Degree_resistance
      Access <- pm$settings$Access
      eir <- pm$settings$eir
      Coverage_reduced <- pm$settings$Coverage_reduced
      
      # Merge into full factorial table
      setting_table <- Reduce(merge, 
                              list(names(season),
                                   as.data.frame(Vaccine_type),
                                   as.data.frame(Coverage),
                                   as.data.frame(initialEfficacy),
                                   as.data.frame(half_life),
                                   as.data.frame(Degree_resistance),
                                   as.data.frame(Access),
                                   as.data.frame(eir),
                                   as.data.frame(Coverage_reduced)))
      
      colnames(setting_table) <- c("seasonality", "Vaccine_type", "Coverage", "initialEfficacy", "half_life", "Degree_resistance", "Access", "eir", "Coverage_reduced")
    }
    
    # if aimed to estimate the protective effectiveness of vaccine deployed to adults
    if (pm$opts$Type_analysis == "Vaccine_efficacy_adult") {
      
      # Define the level and value for each parameter
      Age <- pm$settings$Age
      Blood_coverage <- pm$settings$Blood_coverage
      season <- pm$settings$seasonality
      Coverage <- pm$settings$Coverage
      initialEfficacy <- pm$settings$initialEfficacy
      half_life <- pm$settings$half_life
      Degree_resistance <- pm$settings$Degree_resistance
      Access <- pm$settings$Access
      eir <- pm$settings$eir
      Coverage_reduced <- pm$settings$Coverage_reduced
      
      # Merge into full factorial table
      setting_table <- Reduce(merge, list(names(season),
                                          as.data.frame(Age),
                                          as.data.frame(Blood_coverage),
                                          as.data.frame(Coverage),
                                          as.data.frame(initialEfficacy),
                                          as.data.frame(half_life),
                                          as.data.frame(Degree_resistance),
                                          as.data.frame(Access),
                                          as.data.frame(eir),
                                          as.data.frame(Coverage_reduced)))
      # colnames
      colnames(setting_table) <- c("seasonality", "Age", "Blood_coverage", "Coverage", "initialEfficacy", "half_life", "Degree_resistance", "Access", "eir", "Coverage_reduced")
    }
    
    # Merge the setting and parameter table
    param_table <- merge(setting_table, param_table)
    
    # Incorporate a scenario name column
    scenario_name <- paste("scenario", 1:nrow(param_table), sep = "_")
    param_table <- cbind(scenario_name, param_table)
    
    
    # Repeat the scenarios for a predefined number of seeds
    seed <- 1:pm$opts$n_seeds
    param_table <- merge(param_table, as.data.frame(seed))
    
    # Write table to file
    write.table(param_table, file = param_file, sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
    
  } else {
    # Case where sample number is greater than 0 the parameter table is generate via adaptative sampling
    
    # Write extended parameter table to a new file
    write.table(param_table, file = param_file, sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
  }
}
