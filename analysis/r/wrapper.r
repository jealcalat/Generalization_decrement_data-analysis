## Wrapper of functions and pcks
#
# Erase all in memory
rm(list = ls())
# ==============================================================================
# WARNING: this will install packages (laste a few minutes)

list.of.packages <- c(
  "tidyverse", "data.table", "cluster", "gridExtra",
  "ggthemes", "grid", "magrittr", "latex2exp"
)

new.packages <- list.of.packages[!(list.of.packages %in%
  installed.packages()[, "Package"])]

# Install packages

if (length(new.packages)) {
  install.packages(new.packages)
}

# Load packages at once
sapply(list.of.packages, library, character.only = T)

# ==============================================================================

## Source custom functions ----

# Special function to use + as a string concatenator

"+" <- function(x, y) {
  if (is.character(x) || is.character(y)) {
    return(paste(x, y, sep = ""))
  } else {
    .Primitive("+")(x, y)
  }
}

# Change this to match the parent directory of main folder
home <- ""
path_functions <- home + "analysis/r/"

## For data wrangling and transformation
source(path_functions + "last_n_ses.r")
source(path_functions + "bind_all_ss.r")
source(path_functions + "read_sep_by_lever.r")

## Plotting utils
source(path_functions + "pub_theme.r")
source(path_functions + "axis_ticks.r")
source(path_functions + "sem_ul_lwl.r")
source(path_functions + "GeomStepHist.r")
source(path_functions + "resp_rate_df.r")
source(path_functions + "resp_norm_plot.r")

# Other utils
source(path_functions + "f_table.r")
source(path_functions + "get_bins.r")
source(path_functions + "expand-grid-jc.r")
source(path_functions + "resp_times.r")
## FWHM
source(path_functions + "fwhm.r")

## Kullback-Leibler divergence
source(path_functions + "kld_d.r")
## QQ-IRI
## Optimal cluster computation with pam
source(path_functions + "qq_xy.r")
source(path_functions + "bp_km.r") # principal
source(path_functions + "bp_km_wrap.r") # wrapper for subject-session-trial
source(path_functions + "pooled_ind_ana_km_df.r") # second wrapper for all data

## Individual trial analysis with Church et al 1994
source(path_functions + "lhl.r") # principal
source(path_functions + "lhl_wrapper.r") # wrapper per session
source(path_functions + "ind_ana_lhl.r") # for all data
## Individual trials for FI (single breakpoint)

source(path_functions + "bp_fi.r") # principal
source(path_functions + "bp_fi_wrp.r") # wrapper per session
source(path_functions + "ind_ana_bp1.r") # for all data
