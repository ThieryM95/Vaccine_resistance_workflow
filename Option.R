######################################################################################
# Option.R                                                                           #
#                                                                                    #
# Task: Set key simulation options: type of analysis, parameter spaces,              #
# constrained parameter values, number of simulations and seeds, directories         #                                       #
#                                                                                    #
# authors: thiery.masserey@swisstph.ch adapted from andrewjames.shattock@swisstph.ch #                                      #
######################################################################################

# download function in the environments
source("myRfunctions.R")
require(plyr)
require(dplyr)
require(xlsx)


# ---------------------------------------------------------
# Set options for a locally called analysis.
# ---------------------------------------------------------
set_options <- function(do_step = NA, sample_num = 0, quiet = FALSE) {
  
    #----general information----
    
    pm <- list()
    
    # Define the user name
    pm$user <- "masthi00"
    
    # iteration
    pm$sample_num <- sample_num
    
    # ---- Sampling options ----
    
    opts <- list()
    
    # Number of Latin Hypercube samples to generate (prior to GP)
    opts$lhc_samples <- 100
    
    # Number of different seeds to simulate
    opts$n_seeds <- 5
    
    # ---- Post processing  ----
    
    # Define the type of analysis
    opts$Type_analysis <- "Vaccine_R_perennial" # options are: 
                                                          # Vaccine_R_perennial: assess impact on selection coefficient of AIV or BSV deployed to children in perennial setting
                                                          # Vaccine_R_seasonal: assess impact on selection coefficient of AIV deployed to children in seasonal setting
                                                          # Vaccine_R_adult_AIV: assess impact on selection coefficient of AIV deployed to adults
                                                          # Vaccine_R_adult_AIV_TBV: assess impact on selection coefficient of AIV+ TBV deployed to adults/Whole population
                                                          # Vaccine_efficacy_children: assess impact on spread on protective effectiveness of vaccine deployed to children
                                                          # Vaccine_efficacy_adult: assess impact on spread on protective effectiveness of vaccine deployed to adults/Whole population
                                                  
    # Define the outcome
    opts$om_outcome <- "Spread"
    
    # ---- Gaussian process  ----
    
    # Select GP kernel function
    opts$gp_kernel <-"Gaussian" # option: "Gaussian", "Matern5_2", or "Matern3_2"
    
    # Proportion of data withheld from training set
    opts$gp_test_split <- 0.2
    

    # ---- Adaptive sampling  ----
    
    # Maximum number of adaptive sampling attempts
    opts$sampling_max_iter <- 5
    
    # Number of points to re-simulate per arm
    opts$n_adaptive_samples <- 50
    
    # Save opts into pm
    pm$opts <- opts
    
    # ---- Define parameter values and ranges ---- 
    
    # Specify parameter values for parameters that are constrained
    pm <- constrained_parameter(pm, opts$Type_analysis) # see function bellow
    
    # Specify parameter range for parameter of interests
    pm <- variable_parameter(pm, opts$Type_analysis) # see function bellow
    
    
    # ---- Define directory ---- 
    
    # define the directory
    pm <- set_dirs(pm, pm$opts$Type_analysis) # see directories.R
    
    # ---- Display key options and return ----
    
    # Number of scenarios defined - just to inform the user
    n_param <- 0
    
    for (i in 1:length(pm$settings)) {
      n_param[i] <- length(pm$settings[[i]])
    }
    
    n_settings <- prod(n_param)
    pm$opts$n_total_jobs <- pm$opts$lhc_samples * n_settings * pm$opts$n_seeds
    
    # Only display is flag is on
    if (quiet == FALSE) {
      message(" - Total number of arms: ", format(n_settings, big.mark = ","))
      message(" - Total number of simulations: ", format(pm$opts$n_total_jobs, big.mark = ","))
    }
    
    return(pm)
  }

# ------------------------------------------------------
# Define the parameter values of constrained parameters.
# ------------------------------------------------------
constrained_parameter <- function(pm, type_analysis) {
  
  # Creat a liste with all the arm to simulate
  settings <- list()
  
  # Define the seasonality profile (via fourrier selection coefficient)
  sesonality1 <- c(0, 0, 0, 0)  #  no seasonality
  sesonality2 <- c(-1.1688319842020671, -1.9682456652323406, -0.23417717218399048, 0.3833462397257487) # 4 months seasonality
  
  # If aimed to identify the key driver of parasite resistant to vaccine targeting children in perennial setting
  if (type_analysis == "Vaccine_R_perennial") {
    
    # Vaccine type
    settings$Vaccine_type <- unique(c("PEV"))
    
    # Reduction in % of coverage at the fourth vaccine doses compare to the third dose
    settings$Coverage_reduced <- unique(c(0))
    
    # Seasonality profile
    settings$seasonality <- data.frame(sesonality1)
  }
  
  # If aimed to identify the key driver of parasite resistant to vaccine targeting children in seasonal setting
  if (type_analysis == "Vaccine_R_seasonal") {
    
    # Vaccine type
    settings$Vaccine_type <- unique(c("PEV", "BSV"))
    
    # Reduction in % of coverage at the fourth vaccine doses comapre to the third dose
    settings$Coverage_reduced <-unique(c(0, 0.2))
    
    # Seasonality profile
    settings$seasonality <- data.frame(sesonality2)
  }
  
  # If aimed to identify the key driver of parasite resistant to vaccine targeting the whole population
  if (type_analysis == "Vaccine_R_adult_AIV") {
    
    # Reduction in % of coverage at the fourth vaccine doses comapre to the third dose
    settings$Coverage_reduced <- unique(c(0, 0.2))
    
    # Minimum age target by the vaccine
    settings$Age <- unique(c(0.75, 18))
    
    # Blood stage clearance
    settings$Blood_coverage <- unique(c(0, 1))
    
    # Seasonality profile
    settings$seasonality <- data.frame(sesonality2)
  }
  
  # If aimed to identify the key driver of parasite resistant to vaccine targeting the whole population when combined AIV +TBV
  if (type_analysis == "Vaccine_R_adult_AIV_TBV") {
    
    # Reduction in % of coverage at the fourth vaccine doses comapre to the third dose
    settings$Coverage_reduced <- unique(c(0))
    
    # Minimum age target by the vaccine
    settings$Age <- unique(c(0.75))
    
    # Blood stage clearance
    settings$Blood_coverage <- unique(c(0))
    
    # Seasonality profile
    settings$seasonality <- data.frame(sesonality2)
  }
  
  # If aimed to assess efficacy of vaccine in children
  if (type_analysis == "Vaccine_efficacy_children") {
    
    # Vaccine type
    settings$Vaccine_type <- unique(c("PEV", "BSV"))
    
    # Seasonality profile
    settings$seasonality <- data.frame(sesonality1)
    
    # Coverage
    settings$Coverage <- unique(c(0.9))
    
    # Initial_Efficacy
    settings$initialEfficacy <- unique(c(0.85, 0.95))
    
    # half_life
    settings$half_life <- unique(c(1, 1.5))
    
    # Degree_resistance
    settings$Degree_resistance <-unique(c(0, 0.25, 0.5, 0.75, 1))
    
    # Access
    settings$Access <- unique(c(0.1, 0.3))
    
    # Eir
    settings$eir <- unique(c(5, 50, 150))
    
    # Reduction in % of coverage at the fourth vaccine doses compare to the third dose
    settings$Coverage_reduced <- unique(c(0))
  }
  
  # If aimed to assess efficacy of vaccine in adults
  if (type_analysis == "Vaccine_efficacy_adult") {
    
    # Minimum age target by the vaccine
    settings$Age <- unique(setting_data$Age)
    
    # Blood stage clearance
    settings$Blood_coverage <- unique(c(0, 1))
    
    # Seasonality profile
    settings$seasonality <- data.frame(sesonality1, sesonality2)
    
    # Coverage
    settings$Coverage <- unique(c(0.9))
    
    # Initial_Efficacy
    settings$initialEfficacy <- unique(c(0.85, 0.95))
    
    # half_life
    settings$half_life <- unique(c(1, 1.5))
    
    # Degree_resistance
    settings$Degree_resistance <- unique(c(0, 0.25, 0.5, 0.75, 1))
    
    # Access
    settings$Access <- unique(c(0.1, 0.3))
    
    # Eir
    settings$eir <- unique(c(5, 50, 150))
    
    # Reduction in % of coverage at the fourth vaccine doses compare to the third dose
    settings$Coverage_reduced <- unique(c(0))
  }
  
  # Append settings to pm list
  pm$settings <- settings
  
  return(pm)
}

# -------------------------------------------------------------------
# Define the parameter space for parameters that are not constrained.
# -------------------------------------------------------------------
variable_parameter <- function(pm, type_analysis) {
  
  # Parameter space if aimed to identify the key driver of parasite resistant to vaccine targeting children in perennial setting
  if (type_analysis == "Vaccine_R_perennial") {
    
    # Name parameters 
    Parameter <- c("Coverage", "initialEfficacy", "decay_efficacy", "Degree_resistance", "Access", "eir", "half_life") # NB: dosae long do not vary at the end
    
    # Maximum 
    max <- c(1, 1, 0, 0.99, 0.5, 500, 1.5)
    
    # Minimum
    min <- c(0.6, 0.5, 0, 0, 0.04, 5, 0.5)
  }
  
  # Parameter space if aimed to identify the key driver of parasite resistant to vaccine targeting children in seasonal setting
  if (type_analysis == "Vaccine_R_seasonal") {
    
    # Name parameters 
    Parameter <- c("Coverage", "initialEfficacy", "decay_efficacy", "Degree_resistance", "Access", "eir", "half_life") # NB: dosae long do not vary at the end
    
    # Maximum 
    max <-c(1, 1, 0, 0.99, 0.5, 500, 1.5)
    
    # Minimum
    min <-c(0.6, 0.5, 0, 0, 0.04, 5, 0.5)
  }
  
  # Parameter space if aimed to identify the key driver of parasite resistant to vaccine targeting whole population
  if (type_analysis == "Vaccine_R_adult_AIV") {
    
    # Name parameters 
    Parameter <-c("Coverage", "initialEfficacy", "decay_efficacy", "Degree_resistance", "Access", "eir", "half_life") # NB: dosage long do not vary at the end
    
    # Maximum 
    max <- c(         1,          1,              0,                 0.99,             0.5,   500,        5)
    
    # Minimum
    min <- c(         0.6,        0.5,             0,                   0,             0.04,   5,       0.5)
  
  }
  
  
  # Parameter space if aimed to identify the key driver of parasite resistant to vaccine targeting whole population when combined AIV +TBV
  if (type_analysis == "Vaccine_R_adult_AIV_TBV") {
    
    # Name parameters 
    Parameter <- c( "Coverage", "initialEfficacy", "decay_efficacy", "Degree_resistance", "Access", "eir", "half_life", "TBV_coverage", "half_life_TBV", "Efficacy_TBV" ) # NB: dosage long do not vary at the end
    
    # Maximum 
    max <- c(0.9,           0.95,             0,                 0.1,              0.3,   10,         2,             1,              1,               0.75)
    
    # Minimum
    min <- c(0.9,           0.95,             0,                 0.1,              0.3,   10,         2,             1,              1,               0.75)
  }
  
  
  # Parameter space if aimed to assess efficacy of vaccine in children
  if (type_analysis == "Vaccine_efficacy_children") {
    
    # Name parameters 
    Parameter <- c("decay_efficacy")
    
    # Maximum 
    max <- c(0)
    
    # Minimum
    min <- c(0)
  }
  
  # Parameter space if aimed to assess efficacy of vaccine in adults
  if (type_analysis == "Vaccine_efficacy_adult") {
    
    # Name parameters 
    Parameter <- c("decay_efficacy")
    
    # Maximum 
    max <- c(0)
    
    # Minimum
    min <- c(0)
  }
  
  # Merge the information into a data frame
  program_data <- data.frame(Parameter, max, min)
  
  # Convert data frame to list
  prog <- as.list(program_data)
  
  # Names
  prog$prog_names <- Parameter
  
  # Easy access number of programs
  prog$n_progs <- length(prog$prog_names)
  
  # Append program details to pm list
  pm$prog <- prog
  
  # Return pm
  return(pm)
}
