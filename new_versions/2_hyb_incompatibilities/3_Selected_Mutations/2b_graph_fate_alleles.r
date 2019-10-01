##############
# MUDAR AQUI #
#~~~~~~~~~~~~~~~~~
batch = c(18000) #
bb = 18000       #
#~~~~~~~~~~~~~~~~~
##############
# MUDAR AQUI #
#~~~~~~~~~~~~~~~~~
#typ = "LOSS"    #
#tp = "loss"     #
#path = "loss"   #
typ = "POLY"     #
tp = "poly"      #
path = "persist" #
#~~~~~~~~~~~~~~~~~
##############
# MUDAR AQUI #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
computador="/media/pagagnaire/18E23B38E23B1A08/temp_FLAVIA/dmi_avdqnf/v_5/" #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd(paste0(computador, bb, "_", path))
setwd(paste0(computador, bb, "_", path))

getwd()

dia = format(Sys.Date(), "%d_%m_%Y")
dia

###variables########################
#              important variables #
####################################

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(directlabels)
library(plyr)

for (b in batch) {
  assign(paste0("gen_1_", b), seq(b + 2, b + 50, 2))
  assign(paste0("gen_2_", b), seq(b + 60, b + 300, 10))
  assign(paste0("gener_", b), append(get(paste0("gen_1_", b)), get(paste0("gen_2_", b))))
}

#depending on which you use, you have to ~refresh the generation values
generations = list(get(paste0("gener_", bb)))

for (i in 1:length(generations)) {
  for (u in 1:length(generations[[i]])) {
    generations[[i]][u] = toString(generations[[i]][u])
  }
}

gen_wind = seq((bb + 1), (bb + 300), 1)
gtest = list(gen_wind)

runs = c(
  "run_1",
  "run_2",
  "run_3",
  "run_4",
  "run_5",
  "run_6",
  "run_7",
  "run_8",
  "run_9",
  "run_10",
  "run_11",
  "run_12",
  "run_13",
  "run_14",
  "run_15",
  "run_16",
  "run_17",
  "run_18",
  "run_19",
  "run_20",
  "run_21",
  "run_22",
  "run_23",
  "run_24",
  "run_25",
  "run_26",
  "run_27",
  "run_28",
  "run_29",
  "run_30",
  "run_31",
  "run_32",
  "run_33",
  "run_34",
  "run_35",
  "run_36",
  "run_37",
  "run_38",
  "run_39",
  "run_40",
  "run_41",
  "run_42",
  "run_43",
  "run_44",
  "run_45",
  "run_46",
  "run_47",
  "run_48",
  "run_49",
  "run_50"
)

#preciso dos arquivos t5, t4 e t2...
t2 = read.table(paste0("all_freq_anc_B_mutations_RECOMB.txt"), h = T)
t4 = read.table(paste0(bb, "alleles_by_outcome.txt"), h = T)

t2 =t2[t2$gen.y != (bb+1),]
t4 = t4[t4$gen.y != (bb+1),]


t4$time = as.factor(t4$gen.y)
t2$time = as.factor(t2$gen.y)
#has result variable
t5 = t4[t4$gen.y <= (bb+30), ]
tokeep = c("fix_a", "fixed_B")

t6 = t4[t4$state %in% tokeep,]
t7 = t6[t6$gen.y <= (bb+30),]

t8 = t4[t4$gen.y %in% generations[[1]],]
t9 = t8[t8$gen.y <= (bb+30),]


#I need to get from gen 200002 onwards... for t2, t4, t5, and t6, t7...
#ok maybe start at the beginning...


png(
  "4_mean_freq_recomb_state_rep_outcome.png",
  width = 42,
  height = 21,
  units = "cm",
  res = 300
)
ggplot(t7, aes(x = time, y = freq.y, fill = result, color = result, alpha = 0.3)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(recomb ~ state) +
  geom_segment(aes(
    y = 0.20,
    x = 0,
    yend = 0.20,
    xend = 30
  ), linetype = 2) +
  ggtitle("MEAN allele frequencies - ALL replicates") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      size = 0.5,
      linetype = 'solid',
      colour = "lightgray"
    ),
    panel.grid.minor.y = element_line(
      size = 0.25,
      linetype = 'solid',
      colour = "lightgray"
    ),
    axis.line = element_line(
      size = 0.5,
      linetype = "solid",
      colour = "black"
    ),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90)
  )
dev.off()


png(
  "5_cacete_de_agulha.png",
  width = 42,
  height = 21,
  units = "cm",
  res = 300
)
ggplot(t5, aes(x = time, y = freq.y, color = result, alpha = 0.05)) +
  geom_jitter() +
  facet_grid(recomb ~ state) +
  geom_segment(aes(
    y = 0.2,
    x = 0,
    yend = 0.20,
    xend = 30
  ), linetype = 2) +
  ggtitle("MEAN allele frequencies - ALL replicates") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      size = 0.5,
      linetype = 'solid',
      colour = "lightgray"
    ),
    panel.grid.minor.y = element_line(
      size = 0.25,
      linetype = 'solid',
      colour = "lightgray"
    ),
    axis.line = element_line(
      size = 0.5,
      linetype = "solid",
      colour = "black"
    ),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90)
  )

dev.off()


png(
  "6_mean_freq_recomb_state_rep_outcome.png",
  width = 42,
  height = 21,
  units = "cm",
  res = 300
)
ggplot(t8, aes(x = time, y = freq.y, fill = result, color = result, alpha = 0.3)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(recomb ~ state) +
  guides(alpha = FALSE)+
  geom_segment(aes(
    y = 0.20,
    x = 0,
    yend = 0.20,
    xend = 50
  ), linetype = 2) +
  ggtitle("MEAN allele frequencies - ALL replicates") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      size = 0.5,
      linetype = 'solid',
      colour = "lightgray"
    ),
    panel.grid.minor.y = element_line(
      size = 0.25,
      linetype = 'solid',
      colour = "lightgray"
    ),
    axis.line = element_line(
      size = 0.5,
      linetype = "solid",
      colour = "black"
    ),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90)
  )
dev.off()

png(
  "7_mean_freq_recomb_state_rep_outcome.png",
  width = 42,
  height = 21,
  units = "cm",
  res = 300
)
ggplot(t9, aes(x = time, y = freq.y, fill = result, color = result, alpha = 0.3)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(recomb ~ state) +
  guides(alpha = FALSE)+
  geom_segment(aes(
    y = 0.20,
    x = 0,
    yend = 0.20,
    xend = 15
  ), linetype = 2) +
  ggtitle("MEAN allele frequencies - ALL replicates") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      size = 0.5,
      linetype = 'solid',
      colour = "lightgray"
    ),
    panel.grid.minor.y = element_line(
      size = 0.25,
      linetype = 'solid',
      colour = "lightgray"
    ),
    axis.line = element_line(
      size = 0.5,
      linetype = "solid",
      colour = "black"
    ),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90)
  )
dev.off()


