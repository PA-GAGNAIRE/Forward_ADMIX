import subprocess, msprime, pyslim, time
import matplotlib.pyplot as plt
import numpy as np
import os
import sys

#this will run the ancestry loop
#it will run for the different generations sampling 

#will get the run number from command line
time_start = time.time()
for i in sys.argv:
    i = i.strip().split('=')
    if i[0] == 'run':
        run = i[1]
    if i[0] == 'tadmix':
        tadmix = int(i[1])
    if i[0] == 'timestep':
        timestep = int(i[1])
    if i[0] == 'fim1':
        fim1 = int(i[1])
    if i[0] == 'fim2':
        fim2 = int(i[1])
    if i[0] == 'Step1':
        Step1 = int(i[1])
    if i[0] == 'Step2':
        Step2 = int(i[1])

#temporal samples
gen_1 = np.arange(tadmix+Step1, fim1, Step1)
gen_2 = np.arange(timestep, fim2, Step2)
gen = np.append(gen_1, gen_2)

#run for every tree file saved during simulations
for i in gen:
    filename = run + "_" + str(i) + "_dmi.trees"
    ts = pyslim.load(str(filename)).simplify()

    # Load the .trees file and assess true local ancestry
    breaks = np.zeros(ts.num_trees + 1)
    ancestry = np.zeros(ts.num_trees + 1)
    
    for tree in ts.trees(sample_counts=True):
        subpop_sum, subpop_weights = 0, 0
        for root in tree.roots:
            leaves_count = tree.num_samples(root) - 1  # subtract one for the root, which is a sample
            subpop_sum += tree.population(root) * leaves_count
            subpop_weights += leaves_count
        breaks[tree.index] = tree.interval[0]
        ancestry[tree.index] = subpop_sum / subpop_weights
    

    breaks[-1] = ts.sequence_length
    ancestry[-1] = ancestry[-2]
    
    anc = np.matrix(ancestry)
    with open(("ancestry_" + str(i) + ".txt"), "wb") as f:
        for line in anc:
            np.savetxt(f, line, fmt="%.4f", delimiter="\n")
    
    
    mat = np.matrix(breaks)
    with open(("breaks_" + str(i) + ".txt"), "wb") as f:
        for line in mat:
            np.savetxt(f, line, delimiter="\n", fmt="%e")

    
    time_end = time.time() - time_start
    print("Total time for ", str(i), "generations: ", time_end)

time_end = time.time() - time_start
print("Total time: ", time_end)
