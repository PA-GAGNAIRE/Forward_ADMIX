# Launch the slim script 'test_v1.slim', compute the ancestry along a chromosome and plot it  
./launch_sim.py nPop1=10000 nPop2=10000 nPop3=20000 Lchro=100000000 outputFile=TEST3.tree slimScript=test_v1.slim nSampleAncestry=1000  
*nPop1* = number of individuals in pop1  
*nPop2* = number of individuals in pop2 
*nPop3* = number of individuals in pop3  
*Lchro* = length of the simulated chrosomose  
*outputFile* = name of the slim's output containing the tree  
*slimScript* = name of the slim's script containing the modele to simulate  
*nSampleAncestry* = number of individuals that ancestry_proportions_v2.py will sample  
  

