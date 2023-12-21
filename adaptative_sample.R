###############################################################################################
# ADAPTIVE SAMPLING                                                                           #
#                                                                                             #
# Functions to prepare for each adaptive sampling iteration.                                  #
#                                                                                             #
# The process:                                                                                #
# 1) Loop through the different arm to assess each GP model                                   #
# 2) Check if adaptive sampling is necessary by assessing GP performance                      #
# 3) Resample as necessary based on variance of GP emulator                                   #
# 4) Generate new parameter tables and xml files                                              #
# 5) Rerun model and post-processing                                                          #
# 6) Repeat these steps until we're happy (or max iterations reached)                         #
#                                                                                             #
# authors: thiery.masserey@swisstph.ch adapted from andrewjames.shattock@swisstph.ch          #
###############################################################################################

# ----------------------------------------------------
# Parent function for all adaptive sampling processes.
# ----------------------------------------------------
run_adaptive_sampling <- function(pm) {

  # Creat a list vector to save the model and correlation of each round of GP for easy visualization
  outpout <- list()
  all_outpout <- list()

  # Repeat adaptive sampling UP TO sampling_max_iter times
  for (i in 1:(pm$opts$sampling_max_iter+1)) { # i<-1
    
    # Define sample number
    pm$sample_num <- i

    # Message to console
    message("* Performing adaptive sampling iteration ", pm$sample_num)

    # ---- For each arm, if the accuracy of the gp is not good enough resample parameter  ----

    # Reset param table on each iteration
    as_param_table <- NULL
  
    if(pm$opts$Type_analysis!="Vaccine_R_adult_AIV"){
      pm$settings$Age<-1
      pm$settings$Blood_coverage<-1
    }
    
    if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
      pm$settings$Vaccine_type<-"PEV"
    }
    
    # Loop through all of the different arms
    for (this_vaccine in pm$settings$Vaccine_type){
    for (this_age in pm$settings$Age){
      for (this_blood in pm$settings$Blood_coverage){
        for (this_reduction in pm$settings$Coverage_reduced) { 
          for (this_season in names(pm$settings$seasonality)) { 

            # Define the parameter range that we can sample between
            cov_ranges <- matrix(c(pm$prog$min, pm$prog$max), ncol = 2)
            rownames(cov_ranges) <- pm$prog$prog_names
            cov_ranges <- cov_ranges[rownames(cov_ranges) != "decay_efficacy", ]

            # Construct file name based on arm values
            if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
              setting_name <- paste(pm$opts$om_outcome, this_season, "Coverage_reduced", this_reduction,"Age", this_age, "Blood_coverage", this_blood,  sep = "_")
            }else{
              setting_name <- paste(pm$opts$om_outcome, this_season,this_vaccine, "Coverage_reduced", this_reduction, sep = "_") #
            }
            
            sample_name <- paste(setting_name, "sample", (pm$sample_num - 1), sep = "_")
            
            # Load the GP of the arm
            lala <- paste0(pm$pth$gp_samples, sample_name, "_gp.RData")
            gp_file <- readRDS(lala)
            gp_model <- gp_file$gp_model

            # Save the content of gp
            outpout$setting_name <- gp_file
            names(outpout)[names(outpout) == "setting_name"] <- setting_name

            # Estimate the correlation betwen the predicted and true selection coeffcient of the testing dataset
            CORELATION <- cor(gp_file$predict_test, gp_file$actual_test, use = "pairwise.complete.obs")

            # If corelation is low, have another round of adaptative sampling
            if (CORELATION <= 0.9999) {

              # Sample the parameters in the region of the parameter in which we are less confident in the prediciton
              new_samples <- resample_points(pm, gp_model, cov_ranges) # see function bellow

              # Concatenate information from each arm into one parameter table + add arm information
                if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
                  as_param_table <- bind_param_table_2(as_param_table, new_samples, this_season, this_reduction, this_age, this_blood) # See function bellow
                }else{
                  as_param_table <- bind_param_table(as_param_table, new_samples, this_season, this_vaccine, this_reduction) # See function bellow
                }    
            }
          }
        }
      }
    }
    }
        
    # Save the GP fit for visualizing the result in an easy way later
    all_outpout[[i]] <- outpout
    outpout <- list()

    # Null table => all GP models satisfy stopping criteria => we're done
    if (is.null(as_param_table)) break()

    # ---- Add finishing touches to parameter table ----
    
  # Conditional to prevent an additional round of adaptive sampling to be done
   if(pm$opts$sampling_max_iter<=pm$opts$sampling_max_iter){
    
      # Redefin the name of the variable that are constrained
    colnames(as_param_table)[1] <- "seasonality"
    colnames(as_param_table)[2] <- "Vaccine_type"
    colnames(as_param_table)[3] <- "Coverage_reduced"
    
    if(pm$opts$Type_analysis=="Vaccine_R_adult_AIV"){
      colnames(as_param_table)[1] <- "seasonality"
      colnames(as_param_table)[2] <- "Coverage_reduced"
      colnames(as_param_table)[3] <- "Age"
      colnames(as_param_table)[4] <- "Blood_coverage"  
      }
    
    # Append scenario name column to data frame
    scenario_name <- paste0("scenario_", 1:nrow(as_param_table))
    as_param_table <- cbind(as.data.frame(scenario_name), as_param_table)

    # Repeat the scenarios for a predefined number of seeds
    as_param_table <- merge(as_param_table, data.frame(seed = 1:pm$opts$n_seeds))

    # Update number of jobs to run
    pm$opts$n_total_jobs <- nrow(as_param_table)

    # Message to console
    message(" - Additional simulations: ", format(pm$opts$n_total_jobs, big.mark = ","))

    # ---- Redo all processes, including GP fitting ----

    # Create new sub-directories for latest sample number
    pm <- set_dirs(pm,pm$opts$Type_analysis)

    # 1) Generate parameter table
    generate_param_table(pm, param_table = as_param_table)

    # 2) Generate seed patterns
    n_jobs_unique <- simulation_sed_patterns(pm) 
    message("   ~ Total number of scenarios: ", thou_sep(n_jobs_unique), " (with seed)")

    # 3) Generate xml file
    generate_xml_files(pm,
      n_jobs = n_jobs_unique,
      file_path = pm$pth$xml_base,
      sed_path = pm$pth$sim_sed,
      xml_path = pm$pth$sim_xml)

    # 4) Run OpenMalaria
    run_model(pm, n_jobs_unique,
      xml_path = pm$pth$sim_xml,
      sim_path = pm$pth$sim_out)

    # 5) Post-processing and summary
    Postprocess(pm)
    SummaryResults(pm)

    # 6) Perform Gaussian Process on model outcome
    run_gp(pm)

    }
  }

  # Save all_output
  gp_file <- paste0(pm$pth$gp_models, "All_gp.RData")
  saveRDS(all_outpout, file = gp_file)

  return(all_outpout)
}

# --------------------------------------------------------------------------
# Function to samples a set of points to be rerun based on largest variance.
# --------------------------------------------------------------------------
resample_points <- function(pm, gp_model, cov_ranges) {

  # Resample new points across the parameter space using LHC sampling
  new_points <- lhs(2000, cov_ranges)

  # Predict the output with the GP model on the new points
  pred_obj <- predict(x = new_points, object = gp_model)
  new_points <- dataFrame(new_points, colNames = rownames(cov_ranges))
 
  # Order points by decreasing variance and nugs
  new_points <- new_points[order(pred_obj$sd2 + pred_obj$nugs, decreasing = TRUE), ]

  # Select set of points with largest posterior predictive variance
  new_samples <- new_points[1:pm$opts$n_adaptive_samples, ]

  # Add the parameter value for the dosage
  new_samples$decay_efficacy <- 0
  
  # Re-order to match parameter table
  new_samples <- new_samples[, pm$prog$prog_names]

  return(new_samples)
}

# ----------------------------------------------------
# Add the constrained variable to the parameter table.
# ----------------------------------------------------
bind_param_table <- function(as_param_table, new_samples, this_season, this_vaccine, this_reduction) {

  # For each analysis add the constrained variable to the new parameter table
  updated_table <- cbind.data.frame(this_season,this_vaccine, this_reduction, new_samples, row.names = NULL)
  updated_table <- rbind(as_param_table, updated_table)

  # Return the table with all parameters
  return(updated_table)
}

bind_param_table_2 <- function(as_param_table, new_samples, this_season, this_reduction, this_age, this_blood) {
  
  # For each analysis add the constrained variable to the new parameter table
  updated_table <- cbind.data.frame(this_season, this_reduction,this_age, this_blood, new_samples, row.names = NULL)
  updated_table <- rbind(as_param_table, updated_table)
  
  # Return the table with all parameters
  return(updated_table)
}

# ---------------------------------------------------------------------
# Define the parameter table name for each adaptive sampling iteration.
# ---------------------------------------------------------------------
param_set_name <- function(sample_num = NA, all_samples = FALSE) {
  
  # Default name for all samples
  if (all_samples == TRUE) {
    
    name <- "parameter_table_all_samples"
  
    } else {
    
    # Set in one place so can be more easily changed
    name <- paste0("parameter_table_sample_", sample_num)
  }
  
  # Return name of table
  return(name)
}
