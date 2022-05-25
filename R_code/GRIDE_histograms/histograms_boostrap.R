library(tidyverse)
library(intRinsic)
library(patchwork)
library(latex2exp)

set.seed(12345)
X <- replicate(2,rnorm(10000))

# FREQUENTIST -------------------------------------------------------------

# TO SAVE TIME, READ THE boot_samples.RDS files - see below.
# Otherwise, uncomment the following lines

 # s1 <- gride(X = X,n1 = 1,n2 = 2 ,nsim = 5000)
 # s2 <- gride(X = X,n1 = 2,n2 = 4 ,nsim = 5000)
 # s3 <- gride(X = X,n1 = 4,n2 = 8 ,nsim = 5000)
 # s4 <- gride(X = X,n1 = 8,n2 = 16,nsim = 5000)
 #  
 # hist(s1$boot_sample)
 # hist(s2$boot_sample)
 # hist(s3$boot_sample)
 # hist(s4$boot_sample)
 # 
 # 
 # DD <- rbind(
 # data.frame(x=s1$boot_sample,ind="n1 = 1, n2 = 2"),
 # data.frame(x=s2$boot_sample,ind="n1 = 2, n2 = 4"),
 # data.frame(x=s3$boot_sample,ind="n1 = 4, n2 = 8"),
 # data.frame(x=s4$boot_sample,ind="n1 = 8, n2 = 16"))
 # 
 # DD <- as_tibble(DD)
 # var(s1$boot_sample)
 # var(s2$boot_sample)
 # var(s3$boot_sample)
 # var(s4$boot_sample)

# saveRDS(DD,"boot_samples.RDS")
DD <- readRDS("boot_samples.RDS")


# PLOTS

DD = DD %>% mutate(ind = factor(ind))
levels(DD$ind) = c(A = TeX("$n_1 = 1, n_2 = 2$"), B = TeX("$n_1 = 2, n_2 = 4$"),
                   C = TeX("$n_1 = 4, n_2 = 8$"), D = TeX("$n_1 = 8, n_2 = 16$"))


g1 <- ggplot(DD)+
  geom_vline(xintercept = 2,lty=2)+
  geom_histogram(aes(x=x,y=..density..),col="steelblue",fill="lightgray")+
  facet_wrap(~ind,nrow=1,labeller = label_parsed)+theme_bw()+
  xlab("Gride bootstrap sample")+
  ylab("Density")+
  scale_x_continuous(breaks = c(1.95,2,2.05))+
  theme(text = element_text(size=18),
        axis.text = element_text(size=10))
g1
ggsave("Figures/histograms_boostrap.png",width = 15,height = 8)


g2 <- ggplot(DD)+
  geom_vline(xintercept = 2,lty=2)+
  geom_histogram(aes(x=x,y=..density..),col="steelblue",fill="lightgray")+
  facet_wrap(~ind,nrow=1,scales = "free_y",labeller = label_parsed)+theme_bw()+
  xlab("Gride bootstrap sample")+
  ylab("Density")+
  scale_x_continuous(breaks = c(1.95,2,2.05))+
  theme(text = element_text(size=18),
        axis.text = element_text(size=10))
g2
ggsave("Figures/histograms_boostrap_free_y.png",width = 15,height = 8)


# Alternative: density plot
g_dens <- ggplot(DD)+
  geom_vline(xintercept = 2,lty=2)+
  geom_density(aes(x=x,y=..density..,col=ind,group=ind,fill=ind),alpha=.25,lwd=.6)+
  #facet_wrap(~ind,nrow=1)+
  theme_bw()+
  xlab("Gride bootstrap sample")+
  ylab("Density")+
  theme(text = element_text(size=18))
g_dens




# BAYES -------------------------------------------------------------------

set.seed(12345)

m1 = intRinsic::compute_mus(X,n1 = 1,n2 = 2 )
m2 = intRinsic::compute_mus(X,n1 = 2,n2 = 4 )
m3 = intRinsic::compute_mus(X,n1 = 4,n2 = 8 )
m4 = intRinsic::compute_mus(X,n1 = 8,n2 = 16)


b1 <- intRinsic:::gride_bayes(mus_n1_n2 = m1,n1 = 1,n2 = 2 ,sigma = .01,nsim = 10000, a_d = 1,b_d = 1)
b2 <- intRinsic:::gride_bayes(mus_n1_n2 = m2,n1 = 2,n2 = 4 ,sigma = .01,nsim = 10000, a_d = 1,b_d = 1)
b3 <- intRinsic:::gride_bayes(mus_n1_n2 = m3,n1 = 4,n2 = 8 ,sigma = .01,nsim = 10000, a_d = 1,b_d = 1)
b4 <- intRinsic:::gride_bayes(mus_n1_n2 = m4,n1 = 8,n2 = 16,sigma = .01,nsim = 10000, a_d = 1,b_d = 1)

DD <- rbind(
  data.frame(x=as.numeric(b1$post_sample),ind="n1 = 1, n2 = 2"),
  data.frame(x=as.numeric(b2$post_sample),ind="n1 = 2, n2 = 4"),
  data.frame(x=as.numeric(b3$post_sample),ind="n1 = 4, n2 = 8"),
  data.frame(x=as.numeric(b4$post_sample),ind="n1 = 8, n2 = 16")
)

plot(b1$post_sample,type="l")
plot(b2$post_sample,type="l")
plot(b3$post_sample,type="l")
plot(b4$post_sample,type="l")

autoplot(b1)
autoplot(b2)
autoplot(b3)
autoplot(b4)

DD <- as_tibble(DD)

var(b1$post_sample)
var(b2$post_sample)
var(b3$post_sample)
var(b4$post_sample)

DD = DD %>% mutate(ind = factor(ind))
levels(DD$ind) = c(A = TeX("$n_1 = 1, n_2 = 2$"), B = TeX("$n_1 = 2, n_2 = 4$"),
                   C = TeX("$n_1 = 4, n_2 = 8$"), D = TeX("$n_1 = 8, n_2 = 16$"))


g2 <- ggplot(DD)+
  geom_vline(xintercept = 2,lty=2)+
  geom_histogram(aes(x=x,y=..density..),col="steelblue",fill="lightgray",bins=20)+
  facet_wrap(~ind,nrow=1,labeller = label_parsed, scales = "free_y")+theme_bw()+
  xlab("Gride posterior sample")+
  ylab("Density")+
  theme(text = element_text(size=18))
g2
ggsave("Figures/histograms_bayes_free_y.png",width = 15,height = 8)
