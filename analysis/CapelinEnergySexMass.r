library(cowplot)
library(viridis)

endens <- data.frame(sex = c('male', 'female.ovid', 'female.spent'), density = rep(c(3.8, 4.6, 3.9), times = length(10:18)), mass = rep(10:18, each = 3))#, umass = rep(18,3))


endens$energy <- with(endens, density * mass)

cols <- viridis::viridis(3)#[-4]
ggplot(data = endens, aes(x = mass, y = energy, color = sex)) +
  geom_line() + 
  theme_cowplot() +
  scale_color_manual(name = '', values= cols) + 
  theme(legend.position = c(0.05, 0.9)) +
  xlab('Mass (g)') +
  ylab('Energy (kJ)') +
  cowplot::ggsave("analysis/output/CapelinEnergySexMass.png", height = 6, width = 6)  
