#!/bin/bash

chmod +x *.R
chmod +x *.sh


# Rscript s_run_hyperparameters.R 1 200
# Rscript s_run_hyperparameters.R 2 50
Rscript s_run_hyperparameters.R 3 20

