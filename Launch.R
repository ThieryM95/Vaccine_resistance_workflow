################################################################################### 
# Analyse workflow to estimate the impact of factors on the spread of genotype    #
# resistant to vaccine and assess the effects of the spread of resistance on      #
# vaccine effectiveness                                                           #
#                                                                                 #
# Need: Need a basefile, precise the parameter range, and constrained variables   #
# Run on OpenMalaria V43.0                                                        #
#                                                                                 #
# author: thiery.masserey@swisstph.ch,                                            #
# adapted from andrewjames.shattock@swisstph.ch and monica.golumbeanu@swisstph.ch #
###################################################################################

# Clear global environment
rm(list = ls())

# set the working directory
setwd("/scicore/home/penny/masthi00/vaccine_resistance")

# download functions in the environment
library("tgp")
library("styler")
source("myRfunctions.R")
source("Option.R")
source("directories.R")
source("Generate_parameter_table.R")
source("openmalaria_setup_seed.R")
source("openmalaria_run.R")
source("Post_process_AG.R")
source("summary_results.R")
source("GP.R")
source("adaptative_sample.R")
source("Post_process_GP.R") #
source("sensitivity_analysis.R")
source("Post_process_sensitivity_analysis.R")

# Tidy up
# Clear console
if (interactive()) clc() # see myRfunctions.R
# Close figures
if (interactive()) clf() # see myRfunctions.R

#########################
# Start of the workflow #
#########################

# 1) Creat directory, and options :
pm <- set_options(sample_num = 0) # See Option.R

# 2) Generate latin hypercube sample and list of scenario
generate_param_table(pm, param_table = as_param_table) # see generate_parameter_table_2.R

# 3) Generate seed paterns
n_jobs_unique <- simulation_sed_patterns(pm) # see Open_malaria_setup.R
message("   ~ Total number of scenarios: ", thou_sep(n_jobs_unique), " (with seed)")

# 4) Generate scenario xml files from the base xml
generate_xml_files(pm, n_jobs = n_jobs_unique, file_path = pm$pth$xml_base, sed_path = pm$pth$sim_sed, xml_path = pm$pth$sim_xml) # see Open_malaria_setup.R

# 5) Run simulation
run_model(pm, n_jobs_unique, xml_path = pm$pth$sim_xml, sim_path = pm$pth$sim_out) # see Openmalaria_run.R

# 6) Post processing
Postprocess(pm) # see Post_process_AG.R

# 7) Summarized the results (stop here if aim to assess effectivness)
SummaryResults(pm) # see Summarized_results.R

# 8) Fit the Gaussian process
Results_gp_1 <- run_gp(pm) # see run GP.R

# 9) Run adaptive sampling
Results_gp <- run_adaptive_sampling(pm) # see adaptive_sample.R

# 10) Post process to visualize the results of the GP in a table format
Precision_final <- Post_process_GP(Results_gp) # see Post_process_GP.R

# Sensitivity analysis
Results_SA <- sensitivity_analysis(pm) # see sensitivity_analysis.R

# 12) Post process the result of the sensitivity analysis in a table format
# Function to transform the Sobol indies into a table format
data <- Post_process_sensitivity(Results_SA) # see Post_process_sensitivity_analysis.R

# Function to transform the direction of the effect of parameter on the selection coeffcient in a table format
Quantil_final_final <- Post_process_sensitivity_2(Results_SA) # see Post_process_sensitivity_analysis.R

# See the visualize folder to visualize the results