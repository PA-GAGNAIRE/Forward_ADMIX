#out of the oven from Jerome in the google group
#I have no idea why this works, and the other one does not.
#oh well, at least it seems to work now!

import msprime, pyslim
import numpy as np

ts =  pyslim.load("your_file.trees").simplify()

subsample = np.random.choice(ts.samples(), size=10, replace=False)
print("Subsample =   ", subsample)

#number of trees before simplification:
ts.num_trees

#simplify
ts = ts.simplify(subsample)

#num trees after:
ts.num_trees

#This will work for any tree sequence, regardless of what program
#produced it. It's definitely better to use the ``ts.samples()`` method
#than to assume that samples will always be 0, 1, ..., n - 1. The numpy
#random choice will be a bit more efficient than using Python's random
#module.

#Then calling simplify() without any arguments we're just saying that we
#want to simplify with respect to all samples. For a tree sequence
#produced by an msprime simulation, the output will be identical.