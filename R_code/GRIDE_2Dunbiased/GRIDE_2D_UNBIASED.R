library(readr)
library(tidyverse)
here::here()
# Dati Uniform density ----------------------------------------------------
file <- list.files(here::here("R_code/GRIDE_2Dunbiased/Data_with_results/"))
inds = grepl(pattern = "ids",file)

file <- file[inds]
f1 <- file[c(1,3,2,4)]
f2 <- file[c(5,7,6,8)]

# focus on MLE, f2

a1 <- (read_table(here::here("R_code/GRIDE_2Dunbiased/Data_with_results/",f2[1]),col_names = FALSE))
a2 <- (read_table(here::here("R_code/GRIDE_2Dunbiased/Data_with_results/",f2[2]),col_names = FALSE))
a3 <- (read_table(here::here("R_code/GRIDE_2Dunbiased/Data_with_results/",f2[3]),col_names = FALSE))
a4 <- (read_table(here::here("R_code/GRIDE_2Dunbiased/Data_with_results/",f2[4]),col_names = FALSE))

a1 <- cbind(2^(0:8),t(apply(a1,2,function(x) c(mean(x),sd(x))  )),128 )
a2 <- cbind(2^(0:8),t(apply(a2,2,function(x) c(mean(x),sd(x))  )),512 )
a3 <- cbind(2^(0:8),t(apply(a3,2,function(x) c(mean(x),sd(x))  )),2048)
a4 <- cbind(2^(0:8),t(apply(a4,2,function(x) c(mean(x),sd(x))  )),8192)


A <- (rbind(a1,a2,a3,a4))
A <- as_tibble(A) %>% mutate(dsdl = V2-2*V3/sqrt(1000),
                             dsdu = V2+2*V3/sqrt(1000))
A <- A %>% mutate(V4 = paste("j =",V4))
A <-  A %>% mutate(V4 = case_when(V4 == "j = 2048" ~ "j = 2,408",
                                V4 == "j = 8192" ~ "j = 8,192",
                                TRUE ~ V4
))

A = A %>% mutate(V4 = factor(V4,levels = c("j = 128","j = 512", "j = 2,408","j = 8,192")))

ggplot(A,aes(x=factor(V1),y=V2,group=V4))+
  geom_hline(aes(yintercept=2))+
  geom_ribbon(aes(ymin=dsdl,ymax=dsdu,group=V4),alpha=.3,col="lightblue",fill="lightblue")+
  geom_path(col="darkblue")+geom_point(col="darkblue")+
  theme_bw()+
  facet_wrap(~V4,scales = "free")+theme(strip.text = element_text(size=14),axis.title = element_text(size=14))+
  ylab("Average MLE")+
  xlab(TeX("$n_1$"))

surr = ggplot(A,aes(x=factor(V1),y=V2,group=V4))+
  geom_hline(aes(yintercept=2))+
  geom_ribbon(aes(ymin=dsdl,ymax=dsdu,group=V4),alpha=.3,col="lightblue",fill="lightblue")+
  geom_path(col="darkblue")+geom_point(col="darkblue")+
  theme_bw()+
  facet_wrap(~V4,nrow = 1)+theme(strip.text = element_text(size=18),axis.title = element_text(size=14))+
  ylab("Aveage MLE")+
  xlab(TeX("$n_1$"))
surr
ggsave("Surr_mean_long.png",width = 15,height = 8)  

surr_fixed = ggplot(A,aes(x=factor(V1),y=V2,group=V4))+
  geom_hline(aes(yintercept=2))+
  geom_ribbon(aes(ymin=dsdl,ymax=dsdu,group=V4),alpha=.3,col="lightblue",fill="lightblue")+
  geom_path(col="darkblue")+geom_point(col="darkblue")+
  theme_bw()+
  facet_wrap(~V4,nrow = 1,scales = "free_y")+theme(strip.text = element_text(size=18),axis.title = element_text(size=14))+
  ylab("Aveage MLE")+
  xlab(TeX("$n_1$"))
surr_fixed
ggsave("Surr_mean_long_zoom.png",width = 15,height = 8)  