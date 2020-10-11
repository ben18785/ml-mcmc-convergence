#!/bin/bash

chmod +x *.R
chmod +x *.sh


Rscript s_run_ml.R 1 2
Rscript s_run_ml.R 2 2
Rscript s_run_ml.R 3 2
Rscript s_run_ml.R 4 2
Rscript s_run_ml.R 5 2

