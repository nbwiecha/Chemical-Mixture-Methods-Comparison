#!/bin/tcsh
#BSUB -n 32
#BSUB -W 36:00
#BSUB -R "span[hosts=1]"
#BSUB -J mix_sims_lin_n400
#BSUB -o stdout.%J
#BSUB -e stderr.%J
#BSUB -q stat
cd /share/$GROUP/$USER/mixtures_sims
module load R
conda activate env_R421
Rscript run_sims_lin_n400.R