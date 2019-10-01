#!/bin/bash

Rscript 02_binning_data.R

wait

Rscript 03_recomb_loop.R

wait

Rscript 04_ancestry_final.R

