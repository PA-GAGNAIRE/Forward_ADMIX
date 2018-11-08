#!/bin/bash

dir=/home/pagagnaire/Documents/Flavia/hybrid_index/trial_2
run=("run_1" "run_2" "run_3" "run_4" "run_5" "run_6" "run_7" "run_8" "run_9" "run_10" "run_11" "run_12" "run_13" "run_14" "run_15" "run_16" "run_17" "run_18" "run_19" "run_20" "run_21" "run_22" "run_23" "run_24" "run_25" "run_26" "run_27" "run_28" "run_29" "run_30" "run_31" "run_32" "run_33" "run_34" "run_35" "run_36" "run_37" "run_38" "run_39" "run_40" "run_41" "run_42" "run_43" "run_44" "run_45" "run_46" "run_47" "run_48" "run_49" "run_50")

for i in "${run[@]}"; do
    cd $(echo $dir)
    mkdir -p $(echo $dir)/"${i}"
    cp ./launch_sim_alfa.py ./"${i}"
    cp ./ancestry_auto.py ./"${i}"
    cp ./hyb_2018.slim ./"${i}"
    cd $(echo $dir)/"${i}"  
    python3.4 ./launch_sim_alfa.py nPop1=1 nPop2=1 nPop3=1000 Lchro=10000 Alfa=1 outputFile="${i}" slimScript=hyb_2018.slim nSampleAncestry=0 > "${i}"_log.txt
    python3.4 ./ancestry_auto.py run="${i}" > "${i}"_log_ancestry.txt
    cd $(echo $dir)
done