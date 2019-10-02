##########Ancestry for all runs######

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(directlabels)
library(ggpubr)

setwd("~/dmi_long_divergence/dmi_avdqnf/v_5")

# time interval you want to explore
gtest = seq(20002, 20050, 2)


#selected replicates to plot animation
runs = c("run_13", "run_17", "run_14")


for (r in runs){
  ma = read.table(paste0("20000_dmi_sum_mut/", r, "_mutation_freqs.txt"), header = F)
  names(ma) = c("pos", "gen", "freq", "gen_or", "group", "sum_gen")
  ma =  ma[ma$gen == 20001,] #mutation frequencies right at the start of admixture
  
  for (i in 1:nrow(ma)){ # "weird" frequency proportions because in thisgen, SLiM consider that the 3 populations co-exist
    if ((ma$group[i] == "B") & ((ma$freq[i] >= 0.30) & (ma$freq[i] <= 0.45))){ma$state[i] = "fixed_B"} else if ((ma$group[i] == "B") & (ma$freq[i] < 0.30)) {ma$state[i] = "poly_B"} else if ((ma$group[i] == "A") & ((ma$freq[i] >= 0.6) & (ma$freq[i] <= 0.70))){ma$state[i] = "fixed_a"} else if ((ma$group[i] == "A") & (ma$freq[i] < 0.6)) {ma$state[i] = "poly_a"}
  }
  
  
  mutB = ma #original mutations present in parental pops
  mutA = ma #actually same as above, but will be needed with a different name
  
  # mutB = ma[ma$state != "mutA",] #original mutations present in parental pops
  # mutA = ma[ma$state != "mutA",] #actually same as above, but will be needed with a different name

  muts = read.table(paste0("20000_dmi_sum_mut/", r, "_mutation_freqs.txt"), header = F) #file with all mut trajectories
  names(muts) = c("pos", "gen", "freq", "gen_or", "group", "sum_gen")
  
  for (g in 1:length(gtest)){
    cmuts =  muts[muts$gen == gtest[g],] #mutations only in generation gtest[g]
    cB = cmuts[cmuts$group == "B",] #mutations from P2
    cA = cmuts[cmuts$group == "A",] #mutations from P1
    remainB = merge(mutB, cB, by= c("pos", "group", "gen_or")) #only P2 mutations that remained in generation gtest[g]
    remainA = merge(mutA, cA, by= c("pos", "group", "gen_or")) #only P1 mutations that remained in generation gtest[g]

    all = read.table(paste0("binned/bin_b_", gtest[g], "_all.txt"), header = T) #this is all ancestry data for said generation (all reps)

    assign(paste0(r, "_", gtest[g]), all[all$replicate == r,]) #subset to current run & assign to variable (object)
    curmut = get(paste0(r, "_",gtest[g])) #getting that table into a dummy variable to plot easily in the loop
    
    #vectors with frequencies of each type of mutation
    fix_B = remainB[remainB$state == "fixed_B",]
    poly_B = remainB[remainB$state == "poly_B",]
    fix_a = remainA[remainA$state == "fixed_a",]
    poly_a = remainA[remainA$state == "poly_a",]
    
    #freq of A mutations in relation to the P2 perspective (it is the frequency of the ANCESTRAL alleles, instead of the derived)
    anc_a = remainA
    for (i in 1:nrow(anc_a)){
      anc_a$anc_freq[i] = (1 - anc_a$freq.y[i])
    }
       
    #necessary for when mutations from P2 disappear
    if (nrow(remainB) == 0){
      remainB[1,] = 0
    }
    
    
    pp = ggplot(data = get(paste0(r, "_",gtest[g])), aes(x= position, y=ancestry))+
      geom_line()+ # plots the ancestry along the chromosome
      ylim(c(0, 1))+ # because it is frequency and proportion 
      #xlim(45000, 55000)+
      #xlim(c(0, 1e+5))+
      ggtitle(paste0("P2 Ancestry in generation ", gtest[g], " - P1/P2 = 80/20"))+
      geom_vline(data = fix_B, xintercept = fix_B$pos, linetype = "solid", color = "red", alpha= 0.5 )+ 
      #geom_vline(data = poly_B, xintercept = poly_B$pos, linetype = "dashed", color = "red", alpha= 0.5) + 
      geom_vline(data = fix_a, xintercept = fix_a$pos, linetype = "solid", color = "blue", alpha= 0.5)+
      #geom_vline(data = poly_a, xintercept = poly_a$pos, linetype = "dashed", color = "blue", alpha= 0.5)+
      geom_point(data = remainB, aes(x= pos, y=freq.y, shape= state, col= "red"))+ 
      geom_point(data = anc_a, aes(x= pos, y=anc_freq, shape= state, col= "blue"))+ 
      xlab("Position")+
      ylab("P2 Ancestry")+
      scale_shape_manual(values = c(15, 16, 0, 1), name = "Alleles")+
      scale_color_manual(values = c("blue", "red"), name = "Alleles")+
      guides(fill= "none", color = "none", shape = "none")+
      theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
            panel.grid.major.x = element_blank(),
            panel.spacing = unit(0.15, "lines"),
            legend.title = element_text(colour="black", size=14, face = "bold"),
            legend.text = element_text(colour="black", size=12, face = "bold"),
            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
            panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
            axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
            axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
            strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
            strip.text.y = element_text(size = 14, color = "black", face = "bold.italic"),
            axis.text.y = element_text(angle=0, colour = "black", size = 12),
            axis.text.x = element_text(angle=0, colour = "black", size = 8))
    
    # finally, saves the plot into your work environment, with a distinct name
    assign(paste0("plot_", r, "_", gtest[g]), pp)
  }
}  

#place to save the images separatedly
setwd("~/dmi_long_divergence/dmi_avdqnf/animation")

#will save each graph to file
for (r in runs){
  for (g in 1:length(gtest)){
    get(paste0("plot_", r, "_", gtest[g]))
    ggsave(file= paste0("2_anc_", r, "_", gtest[g],".png"), plot = get(paste0  ("plot_", r, "_", gtest[g])), width = 35, height = 14, units = "cm", dpi = 300)
  }
}

#now, you just have to make a 'stop motion' movie with the output images 