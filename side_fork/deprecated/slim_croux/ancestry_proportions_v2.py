#!/usr/bin/python
import subprocess, msprime, pyslim, time
import matplotlib.pyplot as plt
import numpy as np
import sys

treeFile = sys.argv[1]
nSample = int(sys.argv[2])

ts = pyslim.load(treeFile).simplify()

sample = np.random.choice(ts.samples(), size = nSample, replace=False)

ts = ts.simplify(sample)

breaks = np.zeros(ts.num_trees + 1)
ancestry = np.zeros(ts.num_trees + 1)

##############################################################
time_loop_st = time.time()
###-------------------------------------------
for tree in ts.trees(sample_counts=True):
    subpop_sum, subpop_weights = 0, 0
    for root in tree.roots:
        leaves_count = tree.num_samples(root) - 1  # subtract one for the root, which is a sample
        subpop_sum += tree.population(root) * leaves_count
        subpop_weights += leaves_count
    breaks[tree.index] = tree.interval[0]
    ancestry[tree.index] = subpop_sum / subpop_weights
###-------------------------------------------
time_loop_end = time.time() - time_loop_st
print("Time to do loop: {0} ".format(time_loop_end))

#############################################################
breaks[-1] = ts.sequence_length
ancestry[-1] = ancestry[-2]

#writing ancestry proportions to file
output = open("ancestry_{0}.txt".format(treeFile.split('.')[0]), "w")
for line in ancestry:
	output.write('{0}\n'.format(line))
output.close()

output = open("breaks_{0}.txt".format(treeFile.split('.')[0]), "w")
for line in breaks:
	output.write('{0}\n'.format(line))
output.close()

