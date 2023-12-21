#!/bin/bash

#SBATCH --job-name=make_xml_files
#SBATCH --account=penny
#SBATCH --time=00:10:00
#SBATCH --qos=30min
#SBATCH --mem=1G
#SBATCH --cpus-per-task=1
#SBATCH --output=/scicore/home/penny/masthi00/smc_resistance/JOB_OUT/scicore_output%A%a.txt

############################################################
# BASH MAKE XML FILES
#
# Creates OpenMalaria-ready xml input files using a base xml file and a txt file
# that associates variable names and variable values. Uses sed pattern replacement.
#
# Arguments:
#     N_SEQ: Number of xml files to create per job
#	 BASE_XML: File path of base xml that contains parameter fields to be populated
#		SED_DIR: Full path to the directory that contains all the sed replacement patterns
#	  XML_DIR: Full path of the directory where all the scenario xml files will be saved
#  LOG_FILE: Text log file that stores names of completed scenarios
#
# Created by M.Golumbeanu
# Adapted by A.J.Shattock
############################################################

# Define input variables
N_SEQ=$1
BASE_XML=$2
SED_DIR=$3
XML_DIR=$4
LOG_FILE=$5

# All files containing sed replacement patterns
SED_FILES=(${SED_DIR}*.txt)

# We'll create a subset of these sequentially
for i in $(seq 1 $N_SEQ);
do

  # Construct ID based on SLURM_ARRAY_TASK_ID, N_SEQ, and i
  INT_IDX=$(( ${SLURM_ARRAY_TASK_ID} - 1 ))
  FILE_IDX=$(( $INT_IDX * $N_SEQ + $i - 1 ))
  
  # Select sed file according to value of FILE_IDX
  SED_FILE=${SED_FILES[$FILE_IDX]}
  
  # Check SED_FILE is non-empty
  if ! [ -z "$SED_FILE" ];
  then
  
    # Use scenario base name to construct xml file name
    SCENARIO_NAME=$(basename $SED_FILE ".txt")
    SCENARIO_FILE=$XML_DIR$SCENARIO_NAME".xml"

    # Replace all sed patterns and save to a new xml file
    sed -f $SED_FILE $BASE_XML > $SCENARIO_FILE

    # Write scenario name to log file
    echo "Input xml file created for $SCENARIO_NAME" >> $LOG_FILE
  fi
done