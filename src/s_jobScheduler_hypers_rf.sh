#!/bin/bash

chmod +x *.R
chmod +x *.sh


Rscript s_run_hyperparameters_rf.R 1 200
Rscript s_run_hyperparameters_rf.R 2 50
Rscript s_run_hyperparameters_rf.R 3 20
Rscript s_run_hyperparameters_rf.R 4 20

