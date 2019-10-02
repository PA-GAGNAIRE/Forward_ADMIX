##RECOMB#####################
#       Recombination Level #
#############################

#now that I have all the data separated in bins
#1/ i have to put another column for the value of the recombination
#2/ I want the mean ancestry of each run (per generation) 

runs = c("run_1", "run_2", "run_3", "run_4", "run_5", "run_6", "run_7", "run_8", "run_9", "run_10", "run_11","run_12", "run_13", "run_14", "run_15", "run_16", "run_17", "run_18", "run_19", "run_20", "run_21", "run_22", "run_23", "run_24", "run_25", "run_26", "run_27", "run_28", "run_29", "run_30", "run_31", "run_32", "run_33", "run_34", "run_35", "run_36", "run_37", "run_38", "run_39", "run_40", "run_41", "run_42", "run_43", "run_44", "run_45", "run_46", "run_47", "run_48", "run_49", "run_50")
#runs = c("run_1", "run_2", "run_3", "run_4", "run_5", "run_6", "run_7", "run_8", "run_9", "run_10", "run_11","run_12", "run_13", "run_14", "run_15", "run_16", "run_17", "run_18", "run_19", "run_20", "run_21", "run_22", "run_23", "run_24", "run_25", "run_26", "run_27", "run_28", "run_29")


###############
# Change HERE #
###############
setwd("/home/pagagnaire/Documents/Flavia/dmi_avdqnf/v_4")

###############
# Change HERE #
###############
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



for (b in 1:length(batch)){ 
  for (g in 1:length(generations[[b]])){ 
    data = read.table(paste("binned/bin_g", generations[[b]][g], "_all_rep.txt", sep = ""), header = T, stringsAsFactors=FALSE)
    #data$position = as.integer(data$position)
    #data$ancestry = as.numeric(data$ancestry)
    #data$raw_gen = as.integer(data$raw_gen)
    #data$generation = as.integer(data$generation)
#data$position = as.integer(levels(data$position))[data$position]
    for (i in 1:nrow(data)){
      if(data$position[i]<= 50000){data[i,6]="high"}
      if(data$position[i] > 50000){data[i,6]="low"}}
    colnames(data)[which(names(data) == "V6")] = "recomb"
    write.table(data, paste("binned/recomb/bin_b_", generations[[b]][g], "_all.txt", sep = ""), col.names = T, row.names = F)}}
