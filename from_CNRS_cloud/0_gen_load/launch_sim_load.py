#!/usr/bin/python
import os
import sys

#this script will run SLiM script, according to the parameters run in the bash script "roda_load.sh"
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
	if i[0] == 'prop1':
		prop1 = float(i[1])
	if i[0] == 'prop2':
		prop2 = float(i[1])
	if i[0] == 'slimScript':
		slimScript = i[1]   

# Run slim
print('Run slim')
cmd = 'slim -d nPop1={0} -d nPop2={1} -d nPop3={2} -d Lchro={3} -d "outputFile={4}" -d prop1={5} -d prop2={6} {7}'.format(nPop1, nPop2, nPop3, Lchro, outputFile, prop1, prop2, slimScript)
os.system(cmd)