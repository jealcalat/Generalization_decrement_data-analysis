# Process data
# Load libraries
library(tidyverse) # dplyr,tidyr,readr, etc
library(data.table)
options(stringsAsFactors = F)
# ==============================================================================
# Subjects
SS_list = seq(326,333,1)
# Baseline 
# Path to acquisition mult IF
path_mif = "~/Documentos/2019A/paper_gen_decrement/Data/raw/mult_IF_acq/"

# Save path for BL
path_save_mif = "~/Documentos/2019A/paper_gen_decrement/Data/processed/mult_IF_acq/"

MED2txt(SS_list, path_mif, path_save_mif)

