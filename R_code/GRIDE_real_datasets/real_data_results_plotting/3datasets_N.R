library(reticulate) 
library(tidyverse)
library(here)
np <- import("numpy")
# data reading
getwd() 
twonn_isolet <- t(np$load("datasets_with_results/2nn_isolet_7797.npy"))
twonn_isolet = as_tibble(twonn_isolet) %>% arrange(V3) %>% 
  mutate(id = V1, N = (round(7797/2^(0:(nrow(twonn_isolet)-1)))))
twonn_isolet = twonn_isolet %>% select(N,id)

twonn_isomap <- t(np$load("datasets_with_results/2nn_isomap_698.npy"))
twonn_isomap <- as_tibble(twonn_isomap) %>% arrange(V3)%>% 
  mutate(id = V1, N = (round(698/2^(0:(nrow(twonn_isomap)-1)))))
twonn_isomap = twonn_isomap %>% select(N,id)


twonn_mnist  <- t(np$load("datasets_with_results/2nn_mnist_6742.npy"))
twonn_mnist  <- as_tibble(twonn_mnist) %>% arrange(V3)%>% 
  mutate(id = V1,N = (round(6742/2^(0:(nrow(twonn_mnist)-1)))))
twonn_mnist = twonn_mnist %>% select(N,id)
         
# gride
gride_isolet <- t(np$load("datasets_with_results/gride_isolet_7797.npy"))
gride_isolet = as_tibble(gride_isolet) %>% arrange(V3) %>% 
  mutate(id = V1, N = (round(7797/2^(0:(nrow(gride_isolet)-1)))))
gride_isolet = gride_isolet %>% select(N,id)

gride_isomap <- t(np$load("datasets_with_results/gride_isomap_698.npy"))
gride_isomap <- as_tibble(gride_isomap) %>% arrange(V3)%>% 
  mutate(id = V1, N = (round(698/2^(0:(nrow(gride_isomap)-1)))))
gride_isomap = gride_isomap %>% select(N,id)

gride_mnist  <- t(np$load("datasets_with_results/gride_mnist_6742.npy"))
gride_mnist  <- as_tibble(gride_mnist) %>% arrange(V3)%>% 
  mutate(id = V1,N = (round(6742/2^(0:(nrow(gride_mnist)-1)))))
gride_mnist = gride_mnist %>% select(N,id)



TWONN = cbind(rbind(cbind(twonn_isolet,data = "Isolet"),
                   cbind( twonn_isomap,data = "Isomap faces"),
                   cbind( twonn_mnist,data = "MNIST")),Method = "TWO-NN")
GRID = cbind(rbind(cbind(gride_isolet,data = "Isolet"),
                   cbind(gride_isomap,data = "Isomap faces"),
                   cbind(gride_mnist,data = "MNIST")),Method = "Gride")


# danco
danco_isolet <- read_table("datasets_with_results/DANCo_isoletN.txt")
danco_isolet <- danco_isolet %>% group_by(N) %>% summarise(id = mean(id))
danco_isomap <- read_table("datasets_with_results/DANCo_isomapN.txt")
danco_isomap <- danco_isomap %>% group_by(N) %>% summarise(id = mean(id))
danco_mnist  <- read_table("datasets_with_results/DANCo_mnistN.txt")
danco_mnist  <- danco_mnist %>% group_by(N) %>% summarise(id = mean(id))

DNCO = cbind(rbind(cbind(danco_isolet,data = "Isolet"),
                   cbind(danco_isomap,data = "Isomap faces"),
                   cbind(danco_mnist,data = "MNIST")),Method = "DANCo")


# geomle
geomle_isolet <- read_table("datasets_with_results/geomle_isolet_k5_15_nrep10_nboots20.txt")
geomle_isolet <- geomle_isolet %>% group_by(N) %>% summarise(id = mean(id))
geomle_isomap <- read_table("datasets_with_results/geomle_isomap_k5_15_nrep10_nboots20.txt")
geomle_isomap <- geomle_isomap %>% group_by(N) %>% summarise(id = mean(id))
geomle_mnist  <- read_table("datasets_with_results/geomle_mnist_k5_15_nrep10_nboots20.txt")
geomle_mnist  <- geomle_mnist %>% group_by(N) %>% summarise(id = mean(id))


GMLE = cbind(rbind(cbind(geomle_isolet,data = "Isolet"),
      cbind(geomle_isomap,data = "Isomap faces"),
      cbind(geomle_mnist,data = "MNIST")),Method = "GeoMLE")

# ESS
ess_isolet <- read_csv("datasets_with_results/ESS_isolet_results.csv")
ess_isolet <- ess_isolet %>% rename(id = ESSa, N= sample_size) %>%  
  group_by(N) %>% summarise(id = mean(id))
ess_isomap <- read_csv("datasets_with_results/ESS_faces_results.csv")
ess_isomap <- ess_isomap %>% rename(id = ESSa, N= sample_size) %>%  
  group_by(N) %>% summarise(id = mean(id))
ess_mnist  <- read_csv("datasets_with_results/ESS_mnist_results.csv")
ess_mnist  <- ess_mnist %>% rename(id = ESSa, N= sample_size) %>%  
  group_by(N) %>% summarise(id = mean(id))


ESS = cbind(rbind(cbind(ess_isolet,data = "Isolet"),
                   cbind(ess_isomap,data = "Isomap faces"),
                   cbind(ess_mnist,data = "MNIST")),Method = "ESSa")

ESS %>% group_by(data) %>% summarise(m = min(N))
GRID  = GRID %>% filter(!(data == "Isolet" & N<244)) %>% 
  filter(!(data == "MNIST" & N<211)) %>% 
  filter(!(data == "Isomap faces" & N<22))
TWONN = TWONN %>% filter(!(data == "Isolet" & N<244)) %>% 
  filter(!(data == "MNIST" & N<211)) %>% 
  filter(!(data == "Isomap faces" & N<22))


Data = rbind(
  GRID,TWONN,
  DNCO,GMLE,ESS)
Data = Data %>% mutate(Method = ifelse(Method == "ESSa", "ESS", Method)) %>% rename(n = N)

ggplot(Data)+
  geom_path( aes(x=n,y=id,col= Method),lwd=1)+
  geom_point(aes(x=n,y=id,col=Method),size=4)+
  #scale_x_log10()+
  scale_color_manual(values = c("steelblue","orange","black","red","forestgreen"))+
  facet_wrap(~data,scales = "free")+
  theme_bw()+
  xlab(TeX("n"))+
  ylab("ID estimate")+
  theme(text=element_text(size=18),legend.position = "top")
        #legend.background = element_rect(size=0.2, linetype="solid", 
         #                                colour =1))
ggsave("Figures/3datasets.png",width = 12,height =  6)

Data = Data %>% filter( (data == "Isomap faces" & n >45 )|
                 (data == "Isolet" & n >950 )|
                 (data == "MNIST" & n > 500 ))
ggplot(Data)+
  geom_path( aes(x=n,y=id,col= Method),lwd=1)+
  geom_point(aes(x=n,y=id,col=Method),size=4)+
  #scale_x_log10()+
  scale_color_manual(values = c("steelblue","orange","black","red","forestgreen"))+
  facet_wrap(~data,scales = "free")+
  theme_bw()+
  xlab("n")+
  ylab("ID estimate")+
  theme(text=element_text(size=18),legend.position = "top")
ggsave("Figures/3datasets_lesspoints.png",width = 12,height =  6)

