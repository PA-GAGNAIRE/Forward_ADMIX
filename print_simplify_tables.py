import msprime, pyslim, random

lista = range(0, 2000) #number of genomes: in slim, I had 1000 diploid individuals
samp = [ lista[i] for i in sorted(random.sample(range(len(lista)), 100)) ] #getting 100 random numbers/genomes
ts = pyslim.load("recipe_16.5.trees").simplify() #normal simplify
tss = ts.simplify(samp) #another simplify method, using some samples
print(tss.tables)