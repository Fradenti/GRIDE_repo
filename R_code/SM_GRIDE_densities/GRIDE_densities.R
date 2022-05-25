library(intRinsic)
library(latex2exp)

x <- seq(1.001,3,by = 0.001)
z <- matrix(NA,length(x),16)

N1 <- c(1,2,32,256)
N2 <- N1*2

z[,1] <-  intRinsic::dgera(x,n1 =N1[1],n2 =N2[1],  d = 1)
z[,2] <-  intRinsic::dgera(x,n1 =N1[2],n2 =N2[2],  d = 1)
z[,3] <-  intRinsic::dgera(x,n1 =N1[3],n2 =N2[3],d = 1)
z[,4] <-  intRinsic::dgera(x,n1 =N1[4],n2 =N2[4], d = 1)
z[,5] <-  intRinsic::dgera(x,n1 =N1[1],n2 =N2[1],   d = 2)
z[,6] <-  intRinsic::dgera(x,n1 =N1[2],n2 =N2[2],   d = 2)
z[,7] <-  intRinsic::dgera(x,n1 =N1[3],n2 =N2[3], d = 2)
z[,8] <-  intRinsic::dgera(x,n1 =N1[4],n2 =N2[4], d = 2)
z[,9] <-  intRinsic::dgera(x,n1 =N1[1],n2 =N2[1],   d = 3)
z[,10] <- intRinsic::dgera(x,n1 =N1[2],n2 =N2[2],   d = 3)
z[,11] <- intRinsic::dgera(x,n1 =N1[3],n2 =N2[3], d = 3)
z[,12] <- intRinsic::dgera(x,n1 =N1[4],n2 =N2[4], d = 3)
z[,13] <- intRinsic::dgera(x,n1 =N1[1],n2 =N2[1],   d = 4)
z[,14] <- intRinsic::dgera(x,n1 =N1[2],n2 =N2[2],   d = 4)
z[,15] <- intRinsic::dgera(x,n1 =N1[3],n2 =N2[3],  d = 4)
z[,16] <- intRinsic::dgera(x,n1 =N1[4],n2 =N2[4], d = 4)


library(tidyverse)
library(reshape2)
length(x)*16
ind_d   <- rep(1:4,rep(length(x)*4,4))
ind_rat <- rep(rep(1:4,rep(length(x),4)),4)
xx <- rep(x,16)
Z <- melt(z)
Z <- cbind(Z,ID = ind_d,Ratio = ind_rat,x=xx)

Z$Var2==Z$Ratio

Z <- as_tibble(Z) %>% mutate(RatioS = (paste("$n_1$ =",N1[Ratio],", $n_2$ =",N2[Ratio])))


a <- factor(Z$RatioS)
levels(a) <- levels(a)[c(1,2,4,3)]
Z <- as_tibble(Z) %>% mutate(RatioZ = factor(RatioS,levels = levels(a)))

levels(Z$RatioZ) = 
  c(A = TeX("$n_1 = 1 , n_2 = 2$"),
    B = TeX("$n_1 = 2 , n_2 = 4$"),
    C = TeX("$n_1 = 32 , n_2 = 64$"),
    D = TeX("$n_1 = 256 , n_2 = 512$"))

Z$RatioZ


library(RColorBrewer)

my_palette <- brewer.pal(name="Blues",n=9)[c(4,5,7,9)]

ggplot(data=Z)+
  geom_path(aes(x=x,y=value,group=ID,col=factor(ID)),lwd=1)+
  facet_wrap(~RatioZ,
             scales = "free",labeller = label_parsed,
             ncol = 4)+
  theme_bw()+scale_color_manual("id",values = my_palette)+
  theme_bw()+ylab("Densities") + xlab(bquote(dot(mu)))+
  theme(strip.text.x = element_text(size = 12),legend.position = "bottom")

ggsave("Figures/mudots/Mudots_suppmat.png", height = 5,width = 12)
ggsave("Figures/mudots/Mudots_suppmat.pdf", height = 5,width = 12)
ggsave("Figures/mudots/Mudots_suppmat.tiff",height = 5,width = 12)


# 
# dmudot <- function (d, mu.dot, n1, n2,  log = F) 
# {
#   if (n2 < n1) {
#     stop("n2 is lower than n1")
#   }
#   d_n12 <- (n2 - n1)
#   if (d_n12 == 1) {
#     ch <- n1
#     lognum <- log(d)
#     logden <- ((n2 - 1) * d + 1) * log(mu.dot)
#     log_dens <- log(ch) + lognum - logden + log(mu.dot > 
#                                                   1)
#   }
#   else {
#     logch <- sum(log(1:n2)) - sum(log(1:n1)) - sum(log(1:(n2 - 
#                                                             n1)))
#     lognum <- log(d) + (d_n12 - 1) * log(mu.dot^d - 1)
#     logden <- ((n2 - 1) * d + 1) * log(mu.dot)
#     log_dens <- log(d_n12) + logch + lognum - logden + log(mu.dot > 
#                                                              1)
#   }
#   if (log) {
#     log_dens
#   }
#   else {
#     exp(log_dens)
#   }
# }





# to study the variance of the estimator, parametric bootstrap
X <- replicate(5,rnorm(10000))
f1 <- intRinsic::bootstrap_gride(X,1,2,nsim = 50000)
plot(density(f1))
f2 <- intRinsic::bootstrap_gride(X,4,8,nsim = 50000)
lines(density(f2))
