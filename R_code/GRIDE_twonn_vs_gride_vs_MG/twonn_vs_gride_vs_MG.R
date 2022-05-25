library(tidyverse)
library(patchwork)


set.seed(12345)
X <- as_tibble(cbind(runif(500),runif(500),1:500))
X %>% filter( V1< .3 & V2< .3 )
X %>% filter( V1> .7 & V2> .7 )
plot(X[,1:2],type="n")
text(as.matrix(X[,1:2]),label=pull(X[,3]))
ind1 <- 66
ind2 <- 3
ind3 <- 143
ind4 <- 30

nn <- FNN::get.knn(X[,1:2],40)
nn$nn.dist[,1]
in1 <- nn$nn.index[ind1,]
in2 <- nn$nn.index[ind2,]
in3 <- nn$nn.index[ind3,]
in4 <- nn$nn.index[ind4,]

points(X[in1,1:2],col=2)
points(X[ind1,1:2],col=4)

Y <- cbind(X[ind1,1],X[ind1,2],X[in1,1],X[in1,2],
      X[ind2,1],X[ind2,2],X[in2,1],X[in2,2],
      X[ind3,1],X[ind3,2],X[in3,1],X[in3,2],
      X[ind4,1],X[ind4,2],X[in4,1],X[in4,2])
colnames(Y) <- paste0("V",1:16)
X <- X %>% mutate(title = factor("MG - L = 40"))
levels(X$title) = c(A = TeX("$MG - L = 40$"))

p2 <- ggplot()+
  geom_point(data = X,mapping = aes(x=V1,y=V2),alpha=.2)+
  theme_bw()+
  geom_segment(data=Y,mapping = aes(x=V1,xend=V3,y=V2,yend=V4),lty=3)+
  geom_segment(data=Y,mapping = aes(x=V5,xend=V7,y=V6,yend=V8),lty=3)+
  geom_segment(data=Y,mapping = aes(x=V9,xend=V11,y=V10,yend=V12),lty=3)+
  geom_segment(data=Y,mapping = aes(x=V13,xend=V15,y=V14,yend=V16),lty=3)+
  geom_point(data = X %>% filter(V3%in%in1),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3%in%in2),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3==ind1),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  geom_point(data = X %>% filter(V3==ind2),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  geom_point(data = X %>% filter(V3%in%in3),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3==ind3),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  geom_point(data = X %>% filter(V3%in%in4),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3==ind4),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  xlab("x")+ylab("")+
  facet_wrap(~title,labeller = label_parsed)+
theme(axis.title = element_text(size=18), strip.text = element_text(size=18))

p2


nn <- FNN::get.knn(X[,1:2],2)
nn$nn.dist[,1]
in1 <- nn$nn.index[ind1,]
in2 <- nn$nn.index[ind2,]
in3 <- nn$nn.index[ind3,]
in4 <- nn$nn.index[ind4,]

points(X[in1,1:2],col=2)
points(X[ind1,1:2],col=4)

Y <- cbind(X[ind1,1],X[ind1,2],X[in1,1],X[in1,2],
           X[ind2,1],X[ind2,2],X[in2,1],X[in2,2],
           X[ind3,1],X[ind3,2],X[in3,1],X[in3,2],
           X[ind4,1],X[ind4,2],X[in4,1],X[in4,2])
colnames(Y) <- paste0("V",1:16)
X <- X %>% mutate(title = factor("."))

levels(X$title) = c(A = TeX("$TWO-NN$"))



p1 <- ggplot()+
  geom_point(data = X,mapping = aes(x=V1,y=V2),alpha=.2)+
  theme_bw()+
  geom_segment(data=Y,mapping = aes(x=V1,xend=V3,y=V2,yend=V4),lty=3)+
  geom_segment(data=Y,mapping = aes(x=V5,xend=V7,y=V6,yend=V8),lty=3)+
  geom_segment(data=Y,mapping = aes(x=V9,xend=V11,y=V10,yend=V12),lty=3)+
  geom_segment(data=Y,mapping = aes(x=V13,xend=V15,y=V14,yend=V16),lty=3)+
  geom_point(data = X %>% filter(V3%in%in1),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3%in%in2),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3==ind1),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  geom_point(data = X %>% filter(V3==ind2),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  geom_point(data = X %>% filter(V3%in%in3),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3==ind3),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  geom_point(data = X %>% filter(V3%in%in4),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3==ind4),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  xlab("")+ylab("y")+
  facet_wrap(~title,labeller = label_parsed)+
  theme(axis.title = element_text(size=18), strip.text = element_text(size=18))

p1

nn <- FNN::get.knn(X[,1:2],40)
nn$nn.dist[,1]
in1 <- nn$nn.index[ind1,c(20,40)]
in2 <- nn$nn.index[ind2,c(20,40)]
in3 <- nn$nn.index[ind3,c(20,40)]
in4 <- nn$nn.index[ind4,c(20,40)]

points(X[in1,1:2],col=2)
points(X[ind1,1:2],col=4)

Y <- cbind(X[ind1,1],X[ind1,2],X[in1,1],X[in1,2],
           X[ind2,1],X[ind2,2],X[in2,1],X[in2,2],
           X[ind3,1],X[ind3,2],X[in3,1],X[in3,2],
           X[ind4,1],X[ind4,2],X[in4,1],X[in4,2])
colnames(Y) <- paste0("V",1:16)
X <- X %>% mutate(title = factor("A"))


levels(X$title) = c(A = TeX("$Gride - n_1 = 20, n_2 = 40$"))

p3 <- ggplot()+
  geom_point(data = X,mapping = aes(x=V1,y=V2),alpha=.2)+
  theme_bw()+
  geom_segment(data=Y,mapping = aes(x=V1,xend=V3,y=V2,yend=V4),lty=3)+
  geom_segment(data=Y,mapping = aes(x=V5,xend=V7,y=V6,yend=V8),lty=3)+
  geom_segment(data=Y,mapping = aes(x=V9,xend=V11,y=V10,yend=V12),lty=3)+
  geom_segment(data=Y,mapping = aes(x=V13,xend=V15,y=V14,yend=V16),lty=3)+
  geom_point(data = X %>% filter(V3%in%in1),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3%in%in2),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3==ind1),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  geom_point(data = X %>% filter(V3==ind2),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  geom_point(data = X %>% filter(V3%in%in3),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3==ind3),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  geom_point(data = X %>% filter(V3%in%in4),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=2)+
  geom_point(data = X %>% filter(V3==ind4),
             mapping = aes(x=V1,y=V2),col=1,pch=21,fill=4,size=4)+
  xlab("")+ylab("")+
  facet_wrap(~title,labeller = label_parsed)+
  theme(axis.title = element_text(size=18), strip.text = element_text(size=18))


p3


P = p1+p2+p3
P
ggsave("gride_twonn_cride.png",height = 5,width = 12)


