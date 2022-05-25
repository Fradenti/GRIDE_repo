
# Source this fle to create the functions needed to run the analyses
library(reshape2)
library(intRinsic)
library(tidyverse)
library(latex2exp)
library(patchwork)

cride_new_path_2 = function(n,n1,n2,id, sigma_noise, seed= 1234, lev = 50){
  
  A1 = list()
  
  set.seed(seed)
  
  for(l in 1:lev){
    
    e1 = cbind( replicate( id, runif(n) ), 0, 0, 0 )  
    A1[[l]] =  e1 + rnorm(prod(dim(e1)),0,sigma_noise)    
    
  }
  
  B1 = lapply(A1,function(x) {
    t1 = Sys.time()
    d  = gride_evolution(X = x,vec_n1 = n1,vec_n2 = n2)
    t2 = Sys.time()
    list(path = d$path, time = t2-t1)
  })
  
  M1 = matrix(NA, lev, length(B1[[1]]$path))
  G1 = list()
  for(l in 1:lev){
    M1[l,]  = B1[[l]]$path
    G1[[l]] = B1[[l]]$time
  }
  
  return(list(M1,G1))
  
}

simulations = function(n, n1, n2, id, sn, seed= 1234){
  
  
  RES = list()
  TIM = list()
  for(l in 1:length(sn)){
    
    L = list()
    Q = list()
    
    for(j in 1:length(id)){
      
      XX = cride_new_path_2(n, 
                            n1 = n1, 
                            n2 = n2, 
                            id = id[j], 
                            sigma_noise = sn[l],
                            seed = seed,
                            lev = 50)
      L[[j]] = XX[[1]]
      Q[[j]] = XX[[2]]
    } 
    
    RES[[l]] = L
    TIM[[l]] = Q
    cat(l)
    
  }
  
  return(list(res = RES,time = TIM))
}

postprocess_res = function(RES,id,sn,n2,n){
  
  D = list()
  
  for(j in 1:length(sn)){
    
    # no noise
    xmeans = do.call(cbind,lapply(RES[[j]], function(x) colMeans(x)))
    serrs = do.call(cbind,lapply(RES[[j]], function(x) apply(x,2,sd)))
    
    D[[j]] = cbind(
      data.frame(melt(xmeans), x = rep(n/n2,length(id))),
      low = data.frame(melt(xmeans+2*serrs), x = rep(n/n2,length(id)))$value,
      upp = data.frame(melt(xmeans-2*serrs), x = rep(n/n2,length(id)))$value,
      ind = j)
    
  }
  
  D = do.call(rbind,lapply(D, function(x) x))
  return(D)
  
  
}

postproc_time = function(RES,id,sn){
  M =   matrix(NA,20,52)
  q = 0
  for(i in 1:4){
    for(h in 1:5){
      q = q+1
      M[q,1:50] = unlist(RES$time[[i]][[h]])
      M[q,51:52] = c(id[h],sn[i])   
    }
  }
  colnames(M) = c(paste0("V",1:50),c("id","sn"))
  return(M)
}



plot_postproc_no_noise = function(D2, n){
  D2= D2 %>% mutate(Var2 = case_when(Var2 == 1 ~ "2",
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
  colnames(D2)[2] = "id" 
  
  ggplot(D2 %>% filter(ind == paste0(sn[1])))+
    geom_hline(yintercept = id, lty=2)+
    geom_ribbon(aes(x= 2*x,y=value,ymin =low, ymax = upp, fill  =id, 
                    group = id),alpha = .25)+
    geom_line(aes(x= 2*x,y=value,
                  col  = id, 
                  group = id))+
    geom_point(aes(x= 2*x,y=value,
                   col  =id, 
                   group = id))+
    scale_fill_manual( paste0("n = ",n," - id"),values = c("red","blue","green","orange","darkgray"))+
    scale_color_manual(paste0("n = ",n," - id"),values = c("red","blue","green","orange","darkgray"))+
    scale_x_log10() + 
    theme_bw() +
    scale_y_discrete(limits=(seq(2,10,by=2)))+
    coord_cartesian(ylim = c(1,10.2))+
    theme(text = element_text(size=15),
          legend.position = "top") +
    ylab("id estimate") + xlab(TeX("$N/n_1$ - log10 scale"))# + 
}

plot_postproc = function(D2, n){
  D2= D2 %>% mutate(Var2 = case_when(Var2 == 1 ~ "2",
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
  colnames(D2)[2] = "id" 
  ggplot(D2)+
    scale_y_discrete(limits=(seq(2,10,by=2)))+
    geom_hline(yintercept = id, lty=2)+
    geom_ribbon(aes(x= 2*x,y=value,ymin =low, ymax = upp, fill  =id, 
                    group = id),alpha = .25)+
    geom_line(aes(x= 2*x,y=value,
                  col  = id, 
                  group = id))+
    geom_point(aes(x= 2*x,y=value,
                   col  =id, 
                   group = id))+
    scale_fill_manual( paste0("n = ",n," - id"),values = c("red","blue","green","orange","darkgray"))+
    scale_color_manual(paste0("n = ",n," - id"),values = c("red","blue","green","orange","darkgray"))+
    scale_x_log10() + 
    theme_bw() +
    theme(text = element_text(size=15),
          legend.position = "top") +
    ylab("id estimate") + xlab(TeX("$N/n_1$ - log10 scale")) + 
    facet_wrap(~ind,
               labeller = label_bquote(sigma == .(ind)))
}



plot_postproc_time = function(data_time){
  D_time = reshape2::melt(data_time,measure.vars=  1:50,id.vars = 51:52)
  ggplot(data = D_time )+geom_boxplot(aes(y=value,x=factor(id),col=factor(id),group=factor(id)))+
    facet_wrap(~sn)+scale_y_log10()+
    scale_fill_manual( paste0("n = ",n," - id"),values = c("red","blue","green","orange","darkgray"))+
    scale_color_manual(paste0("n = ",n," - id"),values = c("red","blue","green","orange","darkgray"))+
    theme_bw()+ylab("elapsed time in seconds - log10 scale") +
    xlab("id")
  
  
}
