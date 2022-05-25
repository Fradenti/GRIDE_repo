library(tidyverse)
library(here)

# noise from Gaussians ----------------------------------------------------
here()
file <- list.files(here("GRIDE_2Dnoise"))
inds = grepl("ids",file)

f1 <- file[inds]

a1  <- (read_table(here("GRIDE_2Dnoise/datasets_with_results",f1[1]),col_names = FALSE))
a2  <- (read_table(here("GRIDE_2Dnoise/datasets_with_results",f1[2]),col_names = FALSE))
a3  <- (read_table(here("GRIDE_2Dnoise/datasets_with_results",f1[3]),col_names = FALSE))
a4  <- (read_table(here("GRIDE_2Dnoise/datasets_with_results",f1[4]),col_names = FALSE))


a1 <- a1[,c(3,1,2)]
a2 <- a2[,c(1,2,3)]
a3 <- a3[,c(3,1,2)]
a4 <- a4[,c(1,2,3)]

colnames(a1) <- colnames(a2)
colnames(a3) <- colnames(a2)

D <- rbind(
  data.frame(a2, mod="TWO-NN",ind="1D orthogonal noise"),
  data.frame(a1, mod="Gride n_{2,1} = 2",ind="1D orthogonal noise"),
  data.frame(a4, mod="TWO-NN",ind="20D orthogonal noise"),
  data.frame(a3, mod="Gride n_{2,1} = 2",ind="20D orthogonal noise"))
D <- as_tibble(D)


my_palette <- RColorBrewer::brewer.pal(name="Blues",n=9)[c(4,8)]


n20 = ggplot()+
  geom_hline(yintercept=2)  +
  geom_vline(xintercept=2*sqrt(1e-4),lty=2)  +
  geom_ribbon(data=D,aes(x=X1,y=X2,xmin=X1,xmax=X1,
                         ymin=X2-2*X3,ymax=X2+2*X3, 
                         fill=factor(mod)),alpha=.2)+
  geom_path(data=D,aes(x=X1,y=X2, group=factor(mod),col=factor(mod)))+
  geom_point(data=D,aes(x=X1,y=X2,group=factor(mod),col=factor(mod)))+
  theme_bw()+facet_wrap(~ind,scales = "free")+
  scale_color_manual("Model",values = my_palette,labels=c(TeX("Gride $n_{1,2}=2$"),"TWO-NN"))+
  scale_fill_manual(guide=NULL,values = my_palette)+
  xlab("Average neighbor distance")+
  ylab("ID estimate")+
  theme(text=element_text(size=18),legend.position = c(.28,.9),legend.direction = "horizontal",
        legend.background = element_rect(size=0.2, linetype="solid", 
                                         colour =1))
n20


ggsave("n20.png",width = 12,height = 10)


# use patchwork to combine this plot with the spiral results as in the main paper