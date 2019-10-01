


ggplot(uu, aes(fill=mfilter, x = outcome, alpha = 0.5))+
  geom_density()+
  facet_wrap(~mfilter)


ggplot(uu, aes(x=mfilter, fill= outcome, alpha = 0.5))+
  geom_density()+
  facet_wrap(~mfilter)


ggplot(uu, aes(x=outcome, color=mfilter, alpha = 0.01))+
  geom_density()+
  facet_wrap(~outcome)


ggplot(uu, aes(x=mfilter, fill= outcome, alpha = 0.5))+
  geom_density()

ggplot(uu, aes(x=mfilter, y=num, fill= outcome, alpha = 0.5))+
  geom_boxplot()#+
  #facet_wrap(~outcome, ncol=5)

library(car)


leveneTest(num ~ mfilter*outcome, data =uu)

minor = c("f[minor +2 to +5]", "g[minor +1]")
major = c("i[major +1]", "j[major +2 to +5]")

dt = uu[uu$mfilter %in% minor,]
dtm = uu[uu$mfilter %in% major,]
leveneTest(num ~ mfilter*outcome, data =dt)
leveneTest(num ~ mfilter*outcome, data =dtm)

lala = aov(dt$num ~ dt$outcome*mf)


#+
 # facet_wrap(~mfilter)
  
ggplot(uu, aes(x=mfilter,y=num, fill= outcome, alpha = 0.5))+
  geom_bar(stat="identity",  position = "fill")

