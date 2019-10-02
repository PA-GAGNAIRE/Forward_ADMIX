#!/usr/bin/python
import os
import sys

#this script will run SLiM, according to the parameters run in the bash script "run_slim_7.sh"

for i in sys.argv:
    i = i.strip().split('=')
    if i[0] == 'nPop1':
        nPop1 = int(i[1])
    if i[0] == 'nPop2':
        nPop2 = int(i[1])
    if i[0] == 'nPop3':
        nPop3 = int(i[1])
    if i[0] == 'Lchro':
        Lchro = int(i[1])
    if i[0] == 'outputFile':
        outputFile = "'" + i[1] + "'"    
    if i[0] == 'slimScript':
        slimScript = i[1]

# Run slim
print('Run slim')
cmd ='slim -d nPop1={0} -d nPop2={1} -d nPop3={2} -d Lchro={3} -d "outputFile={4}" {5}'.format(nPop1, nPop2, nPop3, Lchro, outputFile, slimScript)
os.system(cmd)
