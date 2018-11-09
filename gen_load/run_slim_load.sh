#!/bin/bash

dir=/home/pagagnaire/Documents/Flavia/gen_load
run=("run_1" "run_2" "run_3" "run_4" "run_5" "run_6" "run_7" "run_8" "run_9" "run_10") #"run_11" "run_12" "run_13" "run_14" "run_15" "run_16" "run_17" "run_18" "run_19" "run_20" "run_21" "run_22" "run_23" "run_24" "run_25" "run_26" "run_27" "run_28" "run_29" "run_30" "run_31" "run_32" "run_33" "run_34" "run_35" "run_36" "run_37" "run_38" "run_39" "run_40" "run_41" "run_42" "run_43" "run_44" "run_45" "run_46" "run_47" "run_48" "run_49" "run_50")
#pop sizes
p1=500
p2=125
p3=500

#time to evolve without migrations
t=1000
#time of the end of simulation
f=2000 
#step size of sampling 
s=10

for i in "${run[@]}"; do
    cd $(echo $dir)
    mkdir -p $(echo $dir)/"${i}"
    cp ./launch_sim_load.py ./"${i}"
    cp ./ancestry_auto_load.py ./"${i}"
    cp ./gen_load.slim ./"${i}"
    cd $(echo $dir)/"${i}"  
    python3.4 ./launch_sim_load.py nPop1=$(echo $p1) nPop2=$(echo $p2) nPop3=$(echo $p3) Lchro=10000 outputFile="${i}" slimScript=gen_load.slim > "${i}"_log.txt
    python3.4 ./ancestry_auto_load.py run="${i}" Time=1100 Finish=2100 Step=100 > "${i}"_log_ancestry.txt
    cd $(echo $dir)
done