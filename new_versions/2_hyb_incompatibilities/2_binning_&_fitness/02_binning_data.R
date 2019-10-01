runs = c("run_1", "run_2", "run_3", "run_4", "run_5", "run_6", "run_7", "run_8", "run_9", "run_10", "run_11","run_12", "run_13", "run_14", "run_15", "run_16", "run_17", "run_18", "run_19", "run_20", "run_21", "run_22", "run_23", "run_24", "run_25", "run_26", "run_27", "run_28", "run_29", "run_30", "run_31", "run_32", "run_33", "run_34", "run_35", "run_36", "run_37", "run_38", "run_39", "run_40", "run_41", "run_42", "run_43", "run_44", "run_45", "run_46", "run_47", "run_48", "run_49", "run_50")

###############
# Change HERE #
###############
setwd("/home/pagagnaire/Documents/Flavia/dmi_avdqnf/v_4")
#setwd("~/dmi_long_divergence/dmi_avdqnf/")

###############
# Change HERE #
###############
#Change the list of divergence time if needed be
batch = c(5000, 8000, 10000, 12000, 14000, 16000, 18000, 20000)


for (b in batch){
  assign(paste0("gen_1_", b), seq(b+2, b+50, 2))
  assign(paste0("gen_2_", b), seq(b+50, b+300, 10))
  assign(paste0("gener_", b), append(get(paste0("gen_1_", b)), get(paste0("gen_2_", b))))
} 

###############
# Change HERE #
###############
generations = list(gener_5000, gener_8000, gener_10000, gener_12000, gener_14000, gener_16000, gener_18000, gener_20000)

for (i in 1:length(generations)){
  for (u in 1:length(generations[[i]])){
    generations[[i]][u]= toString(generations[[i]][u])
  }
}


#este loop faz uma tabela por geracao com a media de cada replica
#This loop makes a table per generation with the measurement of each replicate
for (b in 1:length(batch)){#cada batch
  all_b = c()
  for (g in 1:length(generations[[b]])){#cada gen in each batch
    all_g=c()
    for (r in runs){
      anc = paste(batch[b], "_dmi/", r, "/", 'ancestry_', generations[[b]][g], '.txt', sep='')
      pos = paste(batch[b], "_dmi/", r, "/", 'breaks_', generations[[b]][g], '.txt', sep='')
      anc = read.table(anc, h=F)
      pos = read.table(pos, h=F)
      
      data = cbind(pos, anc)
      data[,3] = r
      data[,4] = generations[[b]][g]
      data[,5] = as.integer(generations[[b]][g])-batch[b]
      names(data) = c("position", "ancestry", "replicate", "raw_gen", "generation")
      #data = all[all$replicate == r,]
      bin_table = c()
      END = 500
      d=1
      i=1
      while(END < data[nrow(data),1]){
        while(data$position[i] < END){i=i+1}
        TAB <- data[d:(i-1),]
        res = c((END-250), mean(TAB$ancestry), as.character(r), 
                as.integer(generations[[b]][g]), 
                as.integer(generations[[b]][g])-batch[b])
        bin_table = rbind(bin_table, res)
        END= END+500
        d=i
      }
      TAB <- data[d:(i-1),]
      res = c((END-250), mean(TAB$ancestry), as.character(r), 
              as.integer(generations[[b]][g]), 
              as.integer(generations[[b]][g])-batch[b])
      bin_table = rbind(bin_table, res)
      bin_table = data.frame(bin_table)
      bin_table$X1 = as.numeric(levels(bin_table$X1))[bin_table$X1]
      bin_table$X2 = as.numeric(levels(bin_table$X2))[bin_table$X2]
      bin_table$X4 = as.numeric(levels(bin_table$X4))[bin_table$X4]
      #bin_table$X5 = as.numeric(levels(bin_table$X5))[bin_table$X5]
      names(bin_table) = c("position", "ancestry", "replicate", "raw_gen","generation")
      #writing these tables is a mistake, there are hundreds of them, 623 to be exact
      #write.table(bin_table, paste("binned/bin_g", g, "_", r, ".txt", sep = ""), col.names = T, row.names = F)
      all_g = rbind(all_g, bin_table)
    }
    names(all_g) = c("position", "ancestry", "replicate", "raw_gen", "generation")
    all_b = rbind(all_b, all_g)
    write.table(all_g, paste("binned/bin_g", generations[[b]][g], "_all_rep.txt", sep = ""), col.names = T, row.names = F)
  }
  names(all_b) = c("position", "ancestry", "replicate", "raw_gen", "generation")
  write.table(all_b, paste("binned/bin_b_", batch[b], "_all_gen_all_batchs.txt", sep = ""), col.names = T, row.names = F)
}
