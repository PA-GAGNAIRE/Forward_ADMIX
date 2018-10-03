# This is a Python recipe for obtaining ancestry proportions after admixture
import subprocess, msprime, pyslim, time
import matplotlib.pyplot as plt
import numpy as np

#use pyslim to lead tree file from SLiM
ts = pyslim.load("simplify_deleterious.trees").simplify()

#subsampling trees, hopefully?
sample = np.random.choice(ts.samples(), size=1000, replace=False)
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
print("Time to do loop: ", time_loop_end)
#############################################################
breaks[-1] = ts.sequence_length
ancestry[-1] = ancestry[-2]

#writing ancestry proportions to file
anc = np.matrix(ancestry)
with open("ancestry_out_r2.txt", "wb") as f:
    for line in anc:
        np.savetxt(f, line, fmt="%.4f", delimiter="\n")

#writing ancestry break-points/intervals to file
mat = np.matrix(breaks)
with open("breaks_out_r2.txt", "wb") as f:
    for line in mat:
        np.savetxt(f, line, delimiter="\n", fmt="%e")

##############################################
#If you ever want to plot things on python:
#plt.plot(breaks, ancestry)
#plt.show()
