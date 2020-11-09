#!/bin/bash

chmod +x *.R
chmod +x *.sh


Rscript s_run_ml_hypers_set.R 1 100
Rscript s_run_ml_hypers_set.R 2 100
Rscript s_run_ml_hypers_set.R 3 50
Rscript s_run_ml_hypers_set.R 4 30

