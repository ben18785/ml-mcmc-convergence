#!/bin/bash

chmod +x *.R
chmod +x *.sh


sh s_jobScheduler_hypers.sh
sh s_jobScheduler_ml.sh

