This code accompanies, "When are novel methods for analyzing complex chemical mixtures in epidemiology beneficial?" By Nate Wiecha, Emily Griffith, Brian J. Reich, and Jane A. Hoppin. The manuscript is submitted to Epidemiology but is not yet publicly available.

The scripts titled "run_sims_*.R" are the files submitted to a cluster that call the other scripts to run the full simulations. 
They directly call scripts from "simulation_functions.R" which in turn call functions in "gen_data.R" to generate simulated data sets, and then "fit_all.R" to fit all the methods and perform tests based on a dataset.
"process_sim_outs.R" creates figures. "create_tables.Rmd" outputs tables in LaTeX format.
