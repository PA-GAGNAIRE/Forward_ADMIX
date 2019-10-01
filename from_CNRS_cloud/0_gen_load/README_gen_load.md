#####          ~~~ README ~~~

# Hybrid Load Model


#### In this folder, there are the following scripts:

* *set_up_load.sh*
  * this sets up the folder organization/layout to run the simulations in SLiM and calculate ancestry proportions.
    - requires a separate folder for each different divergence time
    - requires to copy all tamplate scripts in that folder
      - *ancestry_load.py*
      - *gen_load.slim*
      - *launch_sim_load.py*
      - *roda_load.sh*
  
* *roda_load.sh*
  * deploys the SLiM script to run the simulation (*gen_load.slim*) and then the python script to calculate ancestry proportions (*ancestry_load.py*).
    - It defines important variables:
      - pop sizes
      - admixture proportions
      - time in allopatry (divergence)
      - time end of simulation
      - sampling intervals of TreeSeq files (these can be quite large, thus we opted to collect them every 5 or 10 generations, for example).
      - name and number of replicates
  
* *launch_sim_load.py*
  * just lauches SLiM script with the correct variables

* *gen_load.slim*
  * simulation SLiM script

* *ancestry_load.py*
  * calculates ancestry proportions based on TreeSeq files obtained during the simulation