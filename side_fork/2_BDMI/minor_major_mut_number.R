
setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/")
d5 = read.table("v_5/20000_persist/muts_minor_major.txt", h=T)
d7 = read.table("v_7/20000_persist/muts_minor_major.txt", h=T)

data = rbind(d5, d7)

#verde        #rosa,     #azul,    vermelho,    amarelo,    roxo
#"#009200", "#ff00ae", "#4cbef0", "#f0382f",  "#c27e1d",  "#856fea"

#verde com      rosa/roxo/azul
#vermelho com   verde/roxo/azul
#azul com       vermelho/rosa/verde
#amarelo com    roxo/rosa/azul
#rosa com       verde/azul/amarelo
#roxo com       verde/vermelho/amarelo

for (i in 1:nrow(data)){
  if(data$outcome[i] == "persist"){data$result[i] = "Polymorphism"} else {data$result[i] = "Loss"}
}

write.table(data, "minor_major_tudo_junto.txt", col.names = T, row.names = F)


data = read.table("minor_major_tudo_junto.txt", h=T)


library(car)

leveneTest(num ~ parent*result, data = data)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  0.3127 0.8162
# 196  


a4 = aov(data$num ~ data$parent*data$result)
a5 = aov(data$num ~ data$parent*data$outcome)
summary(a4)

# summary(a4)
#                           Df Sum Sq Mean Sq  F value   Pr(>F)    
# data$parent               1     37    37.0    1.746    0.188    
# data$result               1   1270   1269.7   59.947   5.08e-13 ***
# data$parent:data$result   1    556   555.8    26.244  7.18e-07 ***
# Residuals               196   4151    21.2                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


require("dplyr")
atab = group_by(data, result, parent) %>%
  summarise(
    count = n(),
    mean = mean(num, na.rm = TRUE),
    sd = sd(num, na.rm = TRUE)
  )

atab
# A tibble: 4 x 5
# # Groups:   result [?]
# result         parent  count  mean   sd
#     <fct>        <fct>  <int> <dbl> <dbl>
# 1 Loss         major     64  38.8  4.26
# 2 Loss         minor     64  37.1  4.82
# 3 Polymorphism major     36  30.0  4.64
# 4 Polymorphism minor     36  35.3  4.75

library(agricolae)
pqp = HSD.test(a4, c("data$parent", "data$result"))

# aov(data$num ~ data$parent*data$result)
# $statistics
# MSerror  Df  Mean       CV
# 21.17963 196 36.04 12.76951
# 
# $parameters
# test                  name.t ntr StudentizedRange alpha
# Tukey data$parent:data$result   4         3.664521  0.05
# 
# $means
#                    data$num      std  r  Min Max Q25  Q50   Q75
# major:Loss         38.75000 4.261306 64  31  49 35.75  39 41.25
# major:Polymorphism 30.02778 4.638264 36  21  39 26.75  31 33.00
# minor:Loss         37.10938 4.820977 64  25  47 34.00  36 41.00
# minor:Polymorphism 35.33333 4.750940 36  27  45 31.75  36 39.00
# 
# $comparison
# NULL
# 
# $groups
#                 data$num groups
# major:Loss         38.75000      a
# minor:Loss         37.10938     ab
# minor:Poly         35.33333      b
# major:Poly         30.02778      c
# 
# attr(,"class")
# [1] "group"

TukeyHSD(a4)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = data$num ~ data$parent * data$result)
# 
# $`data$parent`
#              diff    lwr      upr     p adj
# minor-major 0.86  -0.4235481 2.143548 0.1879186
# 
# $`data$result`
#                      diff       lwr       upr     p adj
# Polymorphism-Loss -5.249132 -6.586161 -3.912103     0
# 
# $`data$parent:data$result`
#                                         diff        lwr        upr     p adj
# minor:Loss-major:Loss                 -1.640625  -3.748701  0.4674514 0.1853697
# major:Polymorphism-major:Loss         -8.722222 -11.206614 -6.2378304 0.0000000 *
# minor:Polymorphism-major:Loss         -3.416667  -5.901058 -0.9322748 0.0025722 *
# major:Polymorphism-minor:Loss         -7.081597  -9.565989 -4.5972054 0.0000000 *
# minor:Polymorphism-minor:Loss         -1.776042  -4.260433  0.7083502 0.2522049
# minor:Polymorphism-major:Polymorphism  5.305556   2.494787  8.1163240 0.0000124 *


# major:Poly-major:Loss         -8.722222 -11.206614 -6.2378304 0.0000000 * #major menor em poly
# minor:Poly-major:Loss         -3.416667  -5.901058 -0.9322748 0.0025722 * #minor poly menor q major loss
# major:Poly-minor:Loss         -7.081597  -9.565989 -4.5972054 0.0000000 * #major poly menor que minor loss
# minor:Poly-major:Poly          5.305556   2.494787  8.1163240 0.0000124 * #minor is larger than major poly



png(paste0("3_outcome_fixed_muts_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(data, aes(y = num, fill= parent))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~result, ncol = 1, dir = "h", strip.position = "left")+
  coord_flip()+
  scale_fill_manual(values = c("#4cbef0", "#c27e1d"))+
  #xlab("Simulation Result")+
  xlab("Number of Derived Alleles")+
  ggtitle("Fixed mutations and the result of the simulations - V5 + v7")+
  labs(fill = "Parent \nContribution")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        #axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        #axis.title.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        strip.background = element_rect(color="gray", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 18, color = "black", face = "bold.italic"),
        axis.text.x = element_text(angle=0, colour = "black", size = 14),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())#,
        #axis.text.x = element_text(angle=0, colour = "black", size = 14))
dev.off()


png(paste0("4_outcome_fixed_muts_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(data, aes(y = num, fill= parent))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~result, ncol = 2, strip.position = "top")+
  #coord_flip()+
  scale_fill_manual(values = c("#4cbef0", "#c27e1d"))+
  #xlab("Simulation Result")+
  ylab("Number of Derived Alleles")+
  ggtitle("Fixed mutations and the result of the simulations - V5 + v7")+
  labs(fill = "Parent \nContribution")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        legend.title = element_text(colour="black", size=16, face = "bold"),
        legend.text = element_text(colour="black", size=14, face = "bold"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        #axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())#,
#axis.text.x = element_text(angle=0, colour = "black", size = 14))
dev.off()


png(paste0("5_outcome_fixed_muts_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(data, aes(y = num, fill= parent))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~result, ncol = 2, strip.position = "top")+
  #coord_flip()+
  scale_fill_manual(values = c("#4cbef0", "#c27e1d"))+
  #xlab("Simulation Result")+
  ylab("Number of Derived Alleles")+
  ggtitle("Fixed mutations and the result of the simulations - V5 + v7")+
  labs(fill = "Parent \nContribution")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        legend.title = element_text(colour="black", size=16, face = "bold"),
        legend.text = element_text(colour="black", size=14, face = "bold"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        #axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())#,
#axis.text.x = element_text(angle=0, colour = "black", size = 14))
dev.off()

tHSD <- TukeyHSD(a4, ordered = FALSE, conf.level = 0.95)
plot(tHSD)

tHSD <- TukeyHSD(a5, ordered = FALSE, conf.level = 0.95)
plot(tHSD , las=1, col="brown")



png(paste0("6_outcome_fixed_muts_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(data, aes(y = num, fill= result, color = result, alpha = 0.5))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~parent, ncol = 2, strip.position = "top")+
  #coord_flip()+
  guides(color = F, alpha = F)+
  scale_fill_manual(values = c("#4cbef0", "#c27e1d"))+
  scale_color_manual(values = c("#4cbef0", "#c27e1d"))+
  #xlab("Simulation Result")+
  ylab("Number of Derived Alleles")+
  ggtitle("Fixed mutations and the result of the simulations - V5 + v7")+
  labs(fill = "Simulation\nResult")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        legend.title = element_text(colour="black", size=16, face = "bold"),
        legend.text = element_text(colour="black", size=14, face = "bold"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        #axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())#,
#axis.text.x = element_text(angle=0, colour = "black", size = 14))
dev.off()
