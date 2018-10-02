import msprime, random

tt = msprime.simulate(30, recombination_rate = 2, random_seed=1)
lista = list(range(0, 30))
samp = [ lista[i] for i in sorted(random.sample(range(len(lista)), 10)) ]
ts = tt.simplify(samp)
tt1 = tt.first()
ts1 = ts.first()
print(tt1.draw(format="unicode"))
print(tt.tables)
print("\n")
print("#####--------end_of_normal_tree------####\n")
print("#####-----Simplify_Tree_Output-----#####\n")
print(ts1.draw(format="unicode"))
print(ts.tables)