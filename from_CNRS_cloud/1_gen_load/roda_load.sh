#!/bin/bash


dir=$(echo $PWD)
run=("run_1" "run_2" "run_3" "run_4" "run_5" "run_6" "run_7" "run_8" "run_9" "run_10" "run_11" "run_12" "run_13" "run_14" "run_15" "run_16" "run_17" "run_18" "run_19" "run_20" "run_21" "run_22" "run_23" "run_24" "run_25" "run_26" "run_27" "run_28" "run_29" "run_30" "run_31" "run_32" "run_33" "run_34" "run_35" "run_36" "run_37" "run_38" "run_39" "run_40" "run_41" "run_42" "run_43" "run_44" "run_45" "run_46" "run_47" "run_48" "run_49" "run_50")
 
#pop sizes
p1=1000
p2=500
p3=1000

#poportions contribution to pop 3
prop1=0.2
prop2=0.8

#size chromosome
len=100000

#time to evolve without migrations
t=__adx__
#time of the end of simulation
f=_._fim_._ 

#step size of sampling 
taxfifty=$(echo $((t+50)))
taxhund=$(echo $((t+100)))
s1=5 
s2=10

for i in "${run[@]}"; do
    cd $(echo $dir)
    mkdir -p $(echo $dir)/"${i}"
    cp ./launch_sim_load.py ./"${i}"
    cp ./ancestry_load.py ./"${i}"
    cp ./gen_load.slim ./"${i}"
    cd $(echo $dir)/"${i}"
    sed -i "s/:_time_ early()/:$t early()/g" ./gen_load.slim
    sed -i "s/:_time_ late()/:$t late()/g" ./gen_load.slim
    sed -i "s/_time_ late()/$t late()/g" ./gen_load.slim
    sed -i "s/_time1_/$(echo $(($t+1)))/g" ./gen_load.slim
    sed -i "s/_time1_:/$(echo $(($t+1))):/g" ./gen_load.slim
    sed -i "s/_finish_/$(echo $f)/g" ./gen_load.slim
    sed -i "s/_step_/$(echo $s)/g" ./gen_load.slim
    sed -i "s/_prop1_/$(echo $prop1)/g" ./gen_load.slim
    sed -i "s/_prop2_/$(echo $prop2)/g" ./gen_load.slim
    sed -i "s/_admxplusfifty_ late/$(echo $taxfifty) late/g" ./gen_load.slim
    sed -i "s/_admxplusfifty_:/$(echo $taxfifty):/g" ./gen_load.slim
    sed -i "s/func_el/$(echo $load)/g" ./gen_load.slim
    python3.4 ./launch_sim_load.py nPop1=$(echo $p1) nPop2=$(echo $p2) nPop3=$(echo $p3) Lchro=$(echo $len) outputFile="${i}" prop1=$(echo $prop1) prop2=$(echo $prop2) slimScript=gen_load.slim > "${i}"_log.txt
    python3.4 ./ancestry_load.py run="${i}" tadmix=$(echo $t) timestep=$(echo $(($taxfifty+$s2))) fim1=$(echo $(($taxfifty+$s1))) fim2=$(echo $(($f+$s2))) Step1=$(echo $s1) Step2=$(echo $s2)> "${i}"_log_ancestry.txt
    rm *.trees
    cd $(echo $dir)
done