# Synthrade: Synthetic model simulator of World Trade Network with mitigation strategies


Based on: A stochastic generative model of the World Trade Network

https://www.nature.com/articles/s41598-019-54979-1

Authors: Javier Garcia-Algarra (U-TAD, Spain)
         Mary Luz Mouronte-Lopez (UFV, Spain)
         Javier Galeano (UPM, Spain)


## Description

This repository contains the code of Synthrade plus simulator and input data.

### Prerequisites

R 3.4 or later
Python 3.0 or later
git bash installed


### Reproducibility

- Download or Clone the repository
- Extract `results_synthrade.rar` to a folder `results` under the repository root folder
- Run scripts from command line with Rscript

- To create matrixes from raw data you should read the documentation of Synthrade
https://github.com/jgalgarra/synthrade

#### Run the synthetic model

- Run script Rscript synthrade_plus.R
                    iniseq : Initial year
                    finseq : Final year
                    numexper: Number of experiments
                    fbal: function to balance trade (if NONE there is no balance)
                    cutoff: percentage of trade NOT improved
                    boost : trade boost
                    scope : REGIONAL or global

Invocation example: `Rscript synthrade_plus.R 2011 2011 10 BOOST 97 100 REGIONAL`
We provide 2 examples files (2017_REGIONAL, 2017_GLOBAL) with the set of commands to run 20 experiments. They may be used as a Linux script or a .BAT file in Windows.

#### Compute Ginis
- Run `Rscript compute_ginis.r initial_year final_year`
- Run `Rscript compute_ginis_region.r initial_year final_year`
- Run `calc_ginis_allyears_differences.r`

#### Plots
- Run `Rscript paint_matrix_plus.R year`
- Run `Rscript paint_density_plots_plus.R year` 
- Run `Rscript paint_ginis.R`
- Run `Rscript paint_ginis_region.R initial_year final_year`