import subprocess, msprime, pyslim, time
import matplotlib.pyplot as plt
import numpy as np
ts = pyslim.load("recipe_16.5.trees").simplify()
print(ts.tables)