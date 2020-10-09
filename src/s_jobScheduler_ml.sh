#!/bin/bash

chmod +x *.R
chmod +x *.sh


Rscript s_run_ml.R 1 50
Rscript s_run_ml.R 2 20
Rscript s_run_ml.R 3 20

