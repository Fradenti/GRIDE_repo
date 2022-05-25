library(tidyverse)
library(patchwork)
here::here()

setwd("datasets_with_results")

list_files = list.files()

# Cifar N
ind_mnist = which(grepl( "cifarN", list_files, fixed = TRUE))
lapply(ind_mnist, function(x) read.table(list_files[x],header = T) )

CFR_N = list()
CFR_N[[1]] = cbind(read.table("DanCo_cifarN.txt",header = T),type = "DANCo")
CFR_N[[2]] = cbind(read.table("2nn_cifarN.txt",header = T),type = "TWO-NN")
CFR_N[[3]] = cbind(read.csv("ESS_cifarN.csv",header = T)[,2:4],type = "ESS")
colnames(CFR_N[[3]]) = colnames(CFR_N[[2]])

CFR_N[[4]] = cbind(read.table("geomle_cifarN_k10_40_nrep1_nboots20.txt",header = T),type = "GeoMLE")
CFR_N[[4]] = data.frame(N = as.numeric(rownames(CFR_N[[4]])), CFR_N[[4]][,c(1,3,4)])
colnames(CFR_N[[4]]) = colnames(CFR_N[[2]])
#colonna di zeri?
CFR_N[[5]] = cbind(read.table("Gride_cifarN.txt",header = T),type = "Gride")


D_CFR_N = do.call(rbind,CFR_N) %>% rename(Estimator = type) %>% mutate(tit = "CIFAR-10 (training) - D = 3072")


cn_id = ggplot(D_CFR_N)+
  theme_bw()+
  geom_line(aes(x=N,y=id,group=Estimator,col=Estimator),lwd=1)+
  geom_point(aes(x=N,y=id,group=Estimator,col=Estimator),size=4)+
  scale_x_log10(n.breaks =5)+
  scale_y_log10(n.breaks =5)+
  ylab("ID estimate - log10 scale")+
  xlab("n - log10 scale")+
  theme(legend.position = "bottom",text = element_text(size=15))+
  ggtitle("CIFAR-10 - D = 3072")


cn_time = ggplot(D_CFR_N)+
  theme_bw()+
  geom_line(aes(x=N,y=time,group=Estimator,col=Estimator),lwd=1)+
  geom_point(aes(x=N,y=time,group=Estimator,col=Estimator),size=4)+
  scale_x_log10(n.breaks =5)+
  scale_y_log10(n.breaks =5)+
  ylab("seconds - log10 scale")+
  xlab("n - log10 scale")+
  theme(legend.position = "bottom",text = element_text(size=15))+
  ggtitle("CIFAR-10 - D = 3072")





# Cifar P
ind_mnist = which(grepl( "cifarP", list_files, fixed = TRUE))
lapply(ind_mnist, function(x) read.table(list_files[x],header = T) )

CFR_P = list()
CFR_P[[1]] = cbind(read.table("DanCo_cifarP.txt",header = T),type = "DANCo")
CFR_P[[2]] = cbind(read.table("2nn_cifarP.txt",header = T),type = "TWO-NN")
CFR_P[[3]] = cbind(read.csv("ESS_cifarP.csv",header = T)[,2:4],type = "ESS")
CFR_P[[3]][,1] = CFR_P[[3]][,1] ^2 *3
colnames(CFR_P[[3]]) = colnames(CFR_P[[2]])

CFR_P[[4]] = cbind(read.table("geomle_cifarP_k10_40_nrep1_nboots20.txt",header = T),type = "GeoMLE")
CFR_P[[4]] = data.frame(P = as.numeric(rownames(CFR_P[[4]])), CFR_P[[4]][,c(1,3,4)])
colnames(CFR_P[[4]]) = colnames(CFR_P[[2]])
#colonna di zeri?
CFR_P[[5]] = cbind(read.table("Gride_cifarP.txt",header = T),type = "Gride")


D_CFR_P = do.call(rbind,CFR_P) %>% rename(Estimator = type) %>% mutate(tit = "CIFAR-10 (cats) - n = 5000")



# Plotting ----------------------------------------------------------------

cp_id = ggplot(D_CFR_P)+
  theme_bw()+
  geom_line(aes(x=P,y=id,group=Estimator,col=Estimator),lwd=1)+
  geom_point(aes(x=P,y=id,group=Estimator,col=Estimator),size=4)+
  scale_x_log10(n.breaks =5)+
  scale_y_log10(n.breaks =5)+
  ylab("ID estimate - log10 scale")+
  xlab("D - log10 scale")+
  theme(legend.position = "bottom",text = element_text(size=15))+
  ggtitle("CIFAR-10 (cats) - N = 5000")


cp_time = ggplot(D_CFR_P)+
  theme_bw()+
  geom_line(aes(x=P,y=time,group=Estimator,col=Estimator),lwd=1)+
  geom_point(aes(x=P,y=time,group=Estimator,col=Estimator),size=4)+
  scale_x_log10(n.breaks =5)+
  scale_y_log10(n.breaks =5)+
  ylab("seconds - log10 scale")+
  xlab("D - log10 scale")+
  theme(legend.position = "bottom",text = element_text(size=15))+
  ggtitle("CIFAR-10 (cats) - N = 5000")



cn_id+cn_time+cp_id+cp_time+
plot_layout(ncol = 4,guides = 'collect')&
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

cn_id = ggplot(D_CFR_N)+
  theme_bw()+
  geom_line(aes(x=N,y=id,group=Estimator,col=Estimator),lwd=1)+
  geom_point(aes(x=N,y=id,group=Estimator,col=Estimator),size=4)+
  scale_color_manual(values = c("steelblue","orange","black","red","forestgreen"))+
  facet_wrap(~tit)+
  scale_x_log10(n.breaks =5)+
  scale_y_log10(n.breaks =5)+
  ylab("ID estimate - log10 scale")+
  xlab("n - log10 scale")+
  theme(legend.position = "bottom",text = element_text(size=18))


cn_time = ggplot(D_CFR_N)+
  theme_bw()+
  geom_line(aes(x=N,y=time,group=Estimator,col=Estimator),lwd=1)+
  geom_point(aes(x=N,y=time,group=Estimator,col=Estimator),size=4)+
  scale_color_manual(values = c("steelblue","orange","black","red","forestgreen"))+
  scale_x_log10(n.breaks =5)+
  scale_y_log10(n.breaks =5)+
  ylab("time in seconds - log10 scale")+
  xlab("n - log10 scale")+
  theme(legend.position = "bottom",text = element_text(size=18))+
  facet_wrap(~tit)
  
cp_id = ggplot(D_CFR_P)+
  theme_bw()+
  geom_line(aes(x=P,y=id,group=Estimator,col=Estimator),lwd=1)+
  geom_point(aes(x=P,y=id,group=Estimator,col=Estimator),size=4)+
  scale_color_manual(values = c("steelblue","orange","black","red","forestgreen"))+
  scale_x_log10(n.breaks =5)+
  scale_y_log10(n.breaks =5)+
  ylab("ID estimate - log10 scale")+
  xlab("D - log10 scale")+
  theme(legend.position = "bottom",text = element_text(size=18))+
  facet_wrap(~tit)


cp_time = ggplot(D_CFR_P)+
  theme_bw()+
  geom_line(aes(x=P,y=time,group=Estimator,col=Estimator),lwd=1)+
  geom_point(aes(x=P,y=time,group=Estimator,col=Estimator),size=4)+
  scale_color_manual(values = c("steelblue","orange","black","red","forestgreen"))+
  scale_x_log10(n.breaks =5)+
  scale_y_log10(n.breaks =5)+
  ylab("time in seconds - log10 scale")+
  xlab("D - log10 scale")+
  theme(legend.position = "bottom",text = element_text(size=18))+
  facet_wrap(~tit)




P = (cn_id+cp_id+cn_time+cp_time)+
  plot_layout(ncol = 2,guides = 'collect')&
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')
P[[2]] = P[[2]] +ylab("")
P[[4]] = P[[4]] +ylab("")
P[[1]] = P[[1]] +xlab("")
P[[2]] = P[[2]] +xlab("")
P
ggsave("Figures/cifar.png",height =  10,width =  13)


# relative speed
minmax_time = cp_time$data %>% group_by(Estimator) %>% summarise(mi = min(time),ma = max(time))
minmax_time[,3]/pull(minmax_time[4,3])


minmax_time = cn_time$data %>% group_by(Estimator) %>% summarise(mi = min(time),ma = max(time)) %>% mutate(
ratio = minmax_time[,3]/pull(minmax_time[4,3]))
minmax_time

minmax_time = cn_time$data %>% group_by(Estimator) %>% summarise(mi = min(time),ma = max(time)) %>% mutate(
  ratio = pull(minmax_time[4,3])/minmax_time[,3])
minmax_time



N = (cn_id+cn_time)+
  plot_layout(ncol = 2,guides = 'collect')&
  theme(legend.position = 'top',
        legend.direction = 'horizontal')
N[[2]] = N[[2]] +ylab("")

N
ggsave("Figures/cifar_N.png",height =  6,width =  12)


P = (cp_id+cp_time)+
  plot_layout(ncol = 2,guides = 'collect')&
  theme(legend.position = 'top',
        legend.direction = 'horizontal')
P[[2]] = P[[2]] +ylab("")
P

ggsave("Figures/cifar_P.png",height =  6,width =  12)
