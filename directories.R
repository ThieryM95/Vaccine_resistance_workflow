######################################################################################
# SET DIRECTORIES                                                                    #
#                                                                                    #
# Set and get directories in one place in the name of consistency and ease.          #
# Creates any directories that do not currently exist.                               #
#                                                                                    #
# outputs: A list of relevant directories (within pm$pth) which can be referenced    #
# elsewhere                                                                          #
#                                                                                    #
# authors: thiery.masserey@swisstph.ch adapted from andrewjames.shattock@swisstph.ch #
######################################################################################

# Load the functions
source("myRfunctions.R")

# --------------------------------------------
# Define paths for project inputs and outputs.
# --------------------------------------------
set_dirs <- function(pm, type_analysis) {
  
  # Initiate file path lists
  pth <- out <- list()
  
  # Sample number
  sample <- paste0("sample_", pm$sample_num)
  
  # ---- Code and resource locations ----
  
  # Base path to code repositories
  base_stm <- file.path("/scicore/home/penny/masthi00")
  
  # Parent paths to all input files relating to this project
  pth$code <- file.path(base_stm, "vaccine_resistance") # working directory
  
  
  # ---- select the good base XML files based on analysis ----
  
  # Folder where XML are sotred
  input <- file.path(pth$code, "SIM_FOLDER") # file with input in the working floder
  
  # Named of based file for each analysis
  if (type_analysis == "Vaccine_R_perennial") {
    pth$xml_base <- file.path(input, "Vaccine_resistance_non_seasonal.xml")
  }
  
  if (type_analysis == "Vaccine_R_seasonal") {
    pth$xml_base <- file.path(input, "Vaccine_resistance_seasonal_hybride.xml")
  }
  
  
  if (type_analysis == "Vaccine_R_adult_AIV") {
    pth$xml_base <- file.path(input, "Vaccine_resistance_AIV_adult.xml")
  }
  
  if (type_analysis == "Vaccine_R_adult_AIV_TBV") {
    pth$xml_base <- file.path(input, "Vaccine_resistance_AIV_TBV_adult.xml")
  }
  
  if (type_analysis == "Vaccine_efficacy_children") {
    pth$xml_base <- file.path(input, "Vaccine_resistance_non_seasonal_efficacy.xml")
  }
  
  if (type_analysis == "Vaccine_efficacy_adult") {
    pth$xml_base <- file.path(input, "Vaccine_resistance_AIV_adult_efficacy.xml")
  }
  
  # Path to OpenMalaria resource files
  pth$om_files <- file.path(base_stm, "OM_schema44_0")
  
  # ---- Output directories and files ----
  
  # path to output  (if simulation run or not)
  pth$log_files <- file.path(pth$code, "JOB_OUT")
  
  # Parent path to all output files relating to this project
  file_stem <- file.path(base_stm, "out_vaccine_learning_session", "Spread_AIV_children_non_seasonal")
  
  # Path to parameter table
  out$param_table <- file.path(file_stem, "0_parameters")
  
  # Paths to  simulation folders
  out$sim_files <- file.path(file_stem, "1_sim_files")
  
  # Paths to seed files
  out$sim_sed <- file.path(out$sim_files, "sed_files", sample)
  
  # Paths to xml files
  out$sim_xml <- file.path(out$sim_files, "xml_files", sample)
  
  # Paths to Outputs folders
  sim_output <- file.path(file_stem, "2_sim_outputs")
  
  # Path to OpenMalaria raw outputs
  out$sim_out <- file.path(sim_output, "raw_output", sample)
  
  # Path to Post processed Outputs
  out$processed <- file.path(sim_output, "processed", sample)
  
  # Path to summary outputs
  out$summarised <- file.path(sim_output, "summarised")
  
  # Paths to  GP fitted during every round of adaptative sampling
  out$gp_samples <- file.path(file_stem, "3_gp_models")
  
  # Path to the GP fitted during the last round of adaptative sampling (use for the sensitivity analysis)
  out$gp_models <- file.path(file_stem, "3_gp_models", "models")
  
  # Paths to sensitivity analysis
  out$results <- file.path(file_stem, "4_results")
  
  # Make all output directories
  make_out_dirs(out)  # see bellow
  
  # Append paths to pm list
  pm <- append_dirs(pm, pth, out)  # see bellow
  
  # return pm
  return(pm)
}

# ---------------------------------------------------------
# Make all output directories if they do not already exist.
# ---------------------------------------------------------
make_out_dirs <- function(out) {
  
  # Extract all path names in list
  pth_names <- names(out)
  
  # Loop through these path names
  for (pth_name in pth_names) {
    this_pth <- out[[pth_name]]
    
    # If it does not already exist, create it
    if (!dir.exists(this_pth)) {
      dir.create(this_pth, recursive = TRUE)
    }
  
  } # Close directory loop
}

# ---------------------------------------------------------
# Concatenate separators and append directories to pm list.
# ---------------------------------------------------------
append_dirs <- function(pm, pth, out) {
  
  # Extract all path names in list
  pth_names <- names(out)
  
  # Loop through these path names
  for (pth_name in pth_names) {
    this_pth <- out[[pth_name]]
    
    # Add a file separator to end of output paths
    out[[pth_name]] <- paste0(this_pth, file_sep())
  }
  
  # Concatenate lists
  pm$pth <- c(pth, out)
  
  return(pm)
}
