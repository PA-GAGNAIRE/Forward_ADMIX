#!/bin/bash

# this copies and pastes all necessary files into the correct folders
# it also changes the allopatry time, in this case, 18k and 20k.

dir=$(echo $PWD) #this is the v_9 folder


#batch=("18000_dmi" "20000_dmi")

bb=("18000" "20000")


for i in "${bb[@]}"; do

   cd $(echo $dir)

   mkdir -p $(echo $dir)/"${i}"

   cp ./roda_load.sh ./"${i}"

   cp ./launch_sim_load.py ./"${i}"

   cp ./ancestry_load.py ./"${i}"

   cp ./gen_load.slim ./"${i}"

   cd ./"${i}"

   sed -i "s/__adx__/"${i}"/" ./roda_load.sh
   sed -i "s|_._fim_._|$(echo $((${i}+300)))|g" ./roda_load.sh


done

