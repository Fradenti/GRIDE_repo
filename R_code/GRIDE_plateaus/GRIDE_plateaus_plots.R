library(tidyverse)
library(patchwork)
source("R_code/Experiments_plateau_no_noise/GRIDE_plateaus_functions.R")
here::here()

id = seq(2,10,by = 2)
sn = 0

# RES_500  = readRDS("Plateaus_500.RDS")
# RES_1000 = readRDS("Plateaus_1000.RDS")
# RES_2500 = readRDS("Plateaus_2500.RDS")
# RES_5000 = readRDS("Plateaus_5000.RDS")
# RES_10000= readRDS("Plateaus_10000.RDS")


RES_500  = readRDS("Plateaus_500_new.RDS")
RES_1000 = readRDS("Plateaus_1000.RDS")
RES_2500 = readRDS("Plateaus_2500.RDS")
RES_5000 = readRDS("Plateaus_5000_new.RDS")
RES_10000= readRDS("Plateaus_10000_new.RDS")


give_n2 = function(n){
n1    = seq(0,n/2-10,by=10)
n1[1] = 1
n2    = n1 * 2
return(n2)
}

D_500  = postprocess_res(RES = RES_500$res,id = id,  sn = sn,n2 = give_n2(500)  ,n = (500)  )
D_1000 = postprocess_res(RES = RES_1000$res,id = id, sn = sn,n2 = give_n2(1000) ,n = (1000) )
D_2500 = postprocess_res(RES = RES_2500$res,id = id, sn = sn,n2 = give_n2(2500) ,n = (2500) )
D_5000 = postprocess_res(RES = RES_5000$res,id = id, sn = sn,n2 = give_n2(5000) ,n = (5000) )
D_10000= postprocess_res(RES = RES_10000$res,id = id,sn = sn,n2 = give_n2(10000),n = (10000))

q1  = plot_postproc(D_500,n=500)
q1
q2  = plot_postproc(D_1000,n=1000)
q2
q3  = plot_postproc(D_2500,n=2500)
q3
q4  = plot_postproc(D_5000,n=5000)
q4
q5  = plot_postproc(D_10000,n=10000)
q5

q1+q5




D = rbind(cbind(D_500,tit = "n = 500"),
          cbind(D_5000,tit = "n = 5,000"),
          cbind(D_10000,tit = "n = 10,000"))
D= as_tibble(D)

D= D %>% mutate(Var2 = case_when(Var2 == 1 ~ "2",
                                   Var2 == 2 ~ "4",
                                   Var2 == 3 ~ "6",
                                   Var2 == 4 ~ "8",
                                   Var2 == 5 ~ "10"),
                  Var2 = factor(Var2, levels = c("2",
                                                 "4",
                                                 "6",
                                                 "8",
                                                 "10") ),
                  ind = case_when(ind == 1 ~  paste0(sn[1]),
                                  ind == 2 ~ paste0(sn[2]),
                                  ind == 3 ~ paste0(sn[3]),
                                  ind == 4 ~ paste0(sn[4])))
colnames(D)[2] = "id" 

D$tit = factor(D$tit, levels = c("n = 500","n = 5,000","n = 10,000"))


ggplot(data=D)+
  geom_hline(yintercept = id, lty=2)+
  scale_y_continuous(breaks  =(seq(2,10,by=2)))+
  geom_ribbon(aes(x= 2*x,
                  y=value,
                  ymin =low, ymax = upp, fill  =id, 
                  group = id),alpha = .25)+
  
  
  geom_line(aes(x= 2*x,y=value,
                col  = id, 
                group = id))+
  geom_point(aes(x= 2*x,y=value,
                 col  =id, 
                 group = id))+
  scale_fill_manual( "Hypercube dimension",values = c("red","blue","green","orange","darkgray"))+
  scale_color_manual("Hypercube dimension",values = c("red","blue","green","orange","darkgray"))+
  scale_x_log10() + 
  theme_bw() +
  theme(text = element_text(size=18),
        legend.position = "top") +
  ylab("ID estimate") + 
  #xlab(TeX("$N/n_1$ - log10 scale")) + 
  xlab(TeX("$n/n_1$ - log10 scale")) + 
  facet_wrap(~tit,scales = "free_x")
            # labeller = label_bquote(sigma == .(ind)))

ggsave("Figures/Exp_noNoise_500_5k10k_small_n_new.png",width = 12,height = 5)



ggplot(data=D %>% filter(tit =="n = 10,000" ))+
  geom_hline(yintercept = id, lty=2)+
  geom_ribbon(aes(x= 2*x,
                  y=value,
                  ymin =low, ymax = upp, fill  =id, 
                  group = id),alpha = .25)+
  geom_line(aes(x= 2*x,y=value,
                col  = id, 
                group = id))+
  geom_point(aes(x= 2*x,y=value,
                 col  =id, 
                 group = id))+
  geom_hline(yintercept = 10.1,col=NA)+
  geom_hline(yintercept = 1,col=1,lty=3)+
  scale_fill_manual( "Hypercube dimension",values = c("red","blue","green","orange","darkgray"))+
  scale_color_manual("Hypercube dimension",values = c("red","blue","green","orange","darkgray"))+
  scale_x_log10() + 
  theme_bw() +
    scale_y_continuous(breaks = (c(1,seq(2,12,by=2))))+
  theme(text = element_text(size=18),
        legend.position = "top") +
  ylab("ID estimate") + 
  xlab(TeX("$n/n_1$ - log10 scale - $n_2 = 2n_1$")) + 
  facet_wrap(~tit,scales = "free_x")

ggsave("Figures/Exp_noNoise_only10k_small_n_new.png",width =  10,height = 5)