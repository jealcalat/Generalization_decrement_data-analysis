# Process data
# Load libraries
library(tidyverse) # dplyr,tidyr,readr, etc
library(data.table)
options(stringsAsFactors = F)
# ==============================================================================
# Subjects
SS_list = seq(326,333,1)
# Baseline 
# Path to baseline. Change accordingly
path_BL = "~/Documentos/2019A/paper_gen_decrement/Data/raw/baseline"
# Path to experimental condition
path_Exp = "~/Documentos/2019A/paper_gen_decrement/Data/raw/experimental"

# Save path for BL
path_save_BL = "~/Documentos/2019A/paper_gen_decrement/Data/processed/baseline/"
# Save path  for exp
path_save_Exp = "~//Documentos/2019A/paper_gen_decrement/Data/processed/experimental/"

MED2txt(SS_list, path_BL, path_save_BL)

MED2txt(SS_list, path_Exp, path_save_Exp)
