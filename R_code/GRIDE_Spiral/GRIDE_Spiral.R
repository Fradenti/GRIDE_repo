library(tidyverse)
library(here)
library(latex2exp)
here()
# Plot the results for the spiral data
Sp_gride <- read.table(here::here("GRIDE_Spiral/data_1d_spiral_gride.csv"), quote="\"", comment.char="")
Sp_cride <- read.table(here::here("GRIDE_Spiral/data_1d_spiral_cride.csv"), quote="\"", comment.char="")
Sp_twonn <- read.table(here::here("GRIDE_Spiral/data_1d_spiral_decimation-twonn.csv"), quote="\"", comment.char="")

Data <- rbind(
data.frame(est = Sp_gride[,3],dist = Sp_gride[,2],Model = "Gride"),
data.frame(est = Sp_twonn[,3],dist = Sp_twonn[,2],Model = "TWO-NN"),
data.frame(est = Sp_cride[,3],dist = Sp_cride[,2],Model = "MG"))
Data <- as_tibble(Data)

Data <- Data %>% mutate(Model = factor(Model, levels = c("Gride","MG","TWO-NN")))


spiral = ggplot(Data)+ theme_bw()+
  ggtitle("Spiral dataset")+
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = .1,lty=3)+
  geom_path(aes(x=(dist),y=est, group = Model, col=Model),lwd=1) +
  geom_point(aes(x=(dist),y=est,group = Model, col=Model),size=4) + 
  scale_x_continuous(trans='log10') +
  xlab(TeX("Average $n_2$ distance - log10 scale")) +
  ylab("ID estimate") + 
  theme(text = element_text(size=18), #legend.position = "bottom",
        legend.position = c(.78,.95), legend.direction = "horizontal",
        legend.background = element_rect(size=0.2, linetype="solid", 
                                         colour =1))
spiral
ggsave("GRIDE_Spiral/spiral_r1.png",height = 8,width =  12)
ggsave("GRIDE_Spiral/spiral_r1.pdf",height = 8,width =  12)
ggsave("GRIDE_Spiral/spiral_r1.tiff",height = 8,width = 12)



Data = Data %>% mutate(title = "Spiral dataset")

spiral2 = ggplot(Data)+ theme_bw()+
  facet_wrap(~title)+
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = .1,lty=3)+
  geom_path(aes(x=(dist),y=est, group = Model, col=Model),lwd=1) +
  geom_point(aes(x=(dist),y=est,group = Model, col=Model),size=4) + 
  scale_x_continuous(trans='log10') +
  xlab(TeX("Average $n_2$ distance - log10 scale")) +
  ylab("ID estimate") + 
  theme(text = element_text(size=18), #legend.position = "bottom",
        legend.position = c(.8,.9), legend.direction = "horizontal",
        legend.background = element_rect(size=0.2, linetype="solid", 
                                         colour =1))
spiral2
ggsave("GRIDE_Spiral/spiral_r1_facet.png",height =  8,width = 12)