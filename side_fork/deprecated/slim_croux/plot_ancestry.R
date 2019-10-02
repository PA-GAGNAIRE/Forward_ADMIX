#!/usr/bin/Rscript
# get the arguments
for(i in commandArgs()){
	tmp = strsplit(i, '=')
	if(tmp[[1]][1] == 'file'){
		file = tmp[[1]][2]
	}
}

# read the ancestry.py's output
anc = paste('ancestry_', file, '.txt', sep='')
pos = paste('breaks_', file, '.txt', sep='')

anc = read.table(anc, h=F)
pos = read.table(pos, h=F)

# get the limits (positions) of the windows of same ancestry
res = c()
cnt = 0
for(i in 1:nrow(anc)){
	if( i==1 ){
		cnt = cnt + 1
		res = rbind(res, c(pos[i,1], pos[i,1], anc[i,1]))
	}else{
		if( anc[i,1]==anc[i-1,1] ){
			res[cnt, 2] = pos[i,1]
		}else{
			cnt = cnt + 1
			res = rbind(res, c(res[cnt-1,2], pos[i,1], anc[i,1]))
		}
	}
}

# get the size of the different windows
colnames(res) = c('pos_start', 'pos_end', 'ancestry')
dist_pop1 = res[which(res[,3]==1),2]-res[which(res[,3]==1),1]
dist_pop2 = res[which(res[,3]==2),2]-res[which(res[,3]==2),1]


# plot the figures
figure_output = paste(file, '.pdf', sep='')
pdf(figure_output, bg='white', width=11.5, height=4.5)
plot(c(0, max(res[,2])), c(0, 1), col="white", xlab="Position", ylab="", yaxt='n')
for(i in 1:nrow(res)){
	if(res[i,3] == 1){
		coul = '#FFB85F' # orange = pop 1
	}else{
		if(res[i,3] == 2){
			coul = '#00AAA0' # blue = pop 2
		}else{
			coul = '#FCF4D9' # pearl = other
		}
	}
	rect(res[i,1], 0, res[i,2], 1, col=coul, border=F)
}
dev.off()

summary_dist = data.frame(rbind(c(quantile(dist_pop1, c(0.025, 0.25, 0.5, 0.75, 0.975)), length(which(res[,3]==1))), c(quantile(dist_pop2, c(0.025, 0.25, 0.5, 0.75, 0.975)), length(which(res[,3]==2)))))

colnames(summary_dist) = c('0.025', '0.25', '0.5', '0.75', '0.975', 'nWindows')
rownames(summary_dist) = c('pop1', 'pop2')


table_output = paste(file, '_table.txt', sep='')
write.table(summary_dist, table_output, col.names=T, row.names=T, sep='\t', quote=F)

