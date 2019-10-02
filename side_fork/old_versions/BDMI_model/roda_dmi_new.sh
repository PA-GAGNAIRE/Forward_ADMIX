#!/bin/bash

#ONLY THING YOU CHANGE TO FIDDLE WITH DIVERGENCE TIME
#time admix
tadx=5000

#same folder you are calling the script from
dir=$(echo $PWD)

#replicates names
run=("run_1" "run_2" "run_3" "run_4" "run_5" "run_6" "run_7" "run_8" "run_9" "run_10" "run_11" "run_12" "run_13" "run_14" "run_15" "run_16" "run_17" "run_18" "run_19" "run_20" "run_21" "run_22" "run_23" "run_24" "run_25" "run_26" "run_27" "run_28" "run_29" "run_30" "run_31" "run_32" "run_33" "run_34" "run_35" "run_36" "run_37" "run_38" "run_39" "run_40" "run_41" "run_42" "run_43" "run_44" "run_45" "run_46" "run_47" "run_48" "run_49" "run_50")
 
#pop sizes
p1=500
p2=500
p3=1000

#poportions contribution to pop 3
prop1=0.8
prop2=0.2

#mutation rate
mut=0.000001

#size chromosome
len=100000

#time of generation intervals to get data (tree seq or fitness)
taxfifty=$(echo $((tadx+50)))
taxhund=$(echo $((tadx+100)))

#max number of incompatibilities (for the BDMI model)
dmi=2000
#number max loci that can evolve under hybrid load model
load=20000

#time of the end of simulation
f=$(echo $((tadx+300)))

#step size of sampling 
s1=2 
s2=10

for i in "${run[@]}"; do
    cd $(echo $dir)
    mkdir -p $(echo $dir)/"${i}"
    cp ./launch_sim_avdqnf.py ./"${i}"
    cp ./ancestry_avdqnf.py ./"${i}"
    cp ./dmi_avdqnf.slim ./"${i}"
    cd $(echo $dir)/"${i}"
    sed -i "s/_b4admx_ /$(echo $(($tadx-1))) /g" ./dmi_avdqnf.slim
    sed -i "s/_admx_:/$tadx:/g" ./dmi_avdqnf.slim      
    sed -i "s/_admx_ late/$tadx late/g" ./dmi_avdqnf.slim
    sed -i "s/_admxplusone_ /$(echo $(($tadx+1))) /g" ./dmi_avdqnf.slim
    sed -i "s/_admxplusone_:/$(echo $(($tadx+1))):/g" ./dmi_avdqnf.slim 
    sed -i "s/_admixplustwo_:/$(echo $(($tadx+2))):/g" ./dmi_avdqnf.slim
    sed -i "s/_admxplusfifty_ late/$(echo $taxfifty) late/g" ./dmi_avdqnf.slim
    sed -i "s/_admxplusfifty_:/$(echo $taxfifty):/g" ./dmi_avdqnf.slim
    sed -i "s/_admxplushund_ late/$(echo $taxhund) late/g" ./dmi_avdqnf.slim
    sed -i "s/_admxplushund_ early/$(echo $taxhund) early/g" ./dmi_avdqnf.slim
    sed -i "s/_admxplushund_:/$(echo $taxhund):/g" ./dmi_avdqnf.slim          
    sed -i "s/_final_ late/$f late/g" ./dmi_avdqnf.slim
    #                                 size P1           size P2            size P3          size chromosome   num incomp loci      output file rep name  num loci hyb load  mut rate      proportion P1        Proportion P2          sim script                  log file    
    python3.4 ./launch_sim_avdqnf.py nPop1=$(echo $p1) nPop2=$(echo $p2) nPop3=$(echo $p3) Lchro=$(echo $len) incomp=$(echo $dmi) outputFile="${i}" func_el=$(echo $load) mu=$(echo $mut) prop1=$(echo $prop1) prop2=$(echo $prop2) slimScript=dmi_avdqnf.slim > "${i}"_sim_log.txt
                                   #name files #gen of admixture   #second samp. interval start        #end first samp interval  #end second samp interval   #samp step 1  #samp step 2    #log of time ancestry  
    python3.4 ./ancestry_avdqnf.py run="${i}" tadmix=$(echo $tadx) timestep=$(echo $(($taxfifty+$s2))) fim1=$(echo $(($taxfifty+$s1))) fim2=$(echo $(($f+$s2))) Step1=$(echo $s1) Step2=$(echo $s2)> "${i}"_log_ancestry.txt
    #occupy a lot of memory, so just remove it after ancestry analysis
    rm ./*.trees
    cd $(echo $dir)
done
