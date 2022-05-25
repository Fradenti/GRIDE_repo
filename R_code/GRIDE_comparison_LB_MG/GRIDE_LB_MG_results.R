library(tidyverse)
setwd(here::here())

set.seed(2134)
Id2_n1000 <- Comparison(id = 2,sample.size = 1000)
# saveRDS(Id2_n1000,"GRIDE_comparison_LB_MG/results_2_1000_new.RDS")
set.seed(2134)
Id2_n5000 <- Comparison(id = 2,sample.size = 5000)
# saveRDS(Id2_n5000,"GRIDE_comparison_LB_MG/results_2_5000_new.RDS")
set.seed(2134)
Id5_n1000 <- Comparison(5,1000)
# saveRDS(Id5_n1000,"GRIDE_comparison_LB_MG/results_5_1000_new.RDS")
set.seed(2134)
Id5_n5000 <- Comparison(5,5000)
# saveRDS(Id5_n5000,"GRIDE_comparison_LB_MG/results_5_5000_new.RDS")
RESULTS <- list(Id2_n1000,Id2_n5000,Id5_n1000,Id5_n5000)
# saveRDS(RESULTS,"GRIDE_comparison_LB_MG/results_2_5_10005000_new.RDS")

# faster, results already saved:
# RESULTS <- readRDS("GRIDE_comparison_LB_MG/results_2_5_10005000_new.RDS")

Id2_n1000 <- RESULTS[[1]] # list with 30 replicates, 2 NN ratios, 4 level of noise
Id2_n5000 <- RESULTS[[2]]
Id5_n1000 <- RESULTS[[3]]
Id5_n5000 <- RESULTS[[4]]



extract_results <- function(Lista){
  runs <- list()
#  for(i in 1:2){ OLD VERSION
  i = 1  
  dat <- array(NA, c(50,5,2,4) )
    for(j in 1:4){
      L   <- 
        array(unlist(lapply(Lista, function(x)   x[[i]][[j]]  )),c(50,5,30))  
      dat[,,1,j] <- apply(L,c(1,2),function(x) c(mean(x)))
      dat[,,2,j] <- apply(L,c(1,2),function(x) c(sd(x)))
    }
  runs[[i]] <- dat
  #  }
return(runs)
  }


d2_1000 <- extract_results(Id2_n1000)
d5_1000 <- extract_results(Id5_n1000)
d2_5000 <- extract_results(Id2_n5000)
d5_5000 <- extract_results(Id5_n5000)


plotter <- function(list,order,dd, enne){
  
  
  require(tidyverse);require(latex2exp)
  D1 <- tibble(reshape2::melt(list[[order]][,,1,]))
  D2 <- tibble(reshape2::melt(list[[order]][,,2,]))
  D1 <- D1 %>% mutate(low = value - 2* D2$value,
                      upp = value + 2* D2$value,
                      Var3 = factor(Var3),
                      Var2 = case_when(Var2 == 1 ~"Gride",
                                       Var2 == 2 ~"LB",
                                       Var2 == 3 ~"MG"))
  levels(D1$Var3) = c(A = TeX("$\\sigma = 0.00$"), B = TeX("$\\sigma = 0.10$"),
                      C = TeX("$\\sigma = 0.25$"), D = TeX("$\\sigma = 0.50$"))
  
  ggplot(D1 %>% filter(Var1 >1 & Var2 != "4" & Var2 != "5") %>% mutate(Var2=factor(Var2)))+theme_bw()+
    geom_ribbon(aes(x=enne/Var1,ymin=low,ymax=upp,fill=Var2),alpha=.25)+
    geom_line(aes(  x=enne/Var1,y=value,group=Var2,col=Var2))+
    geom_point(aes( x=enne/Var1,y=value,group=Var2,col=Var2))+
    scale_fill_manual("Method", values = c("red","blue","orange"))+
    scale_color_manual("Method",values = c("red","blue","orange"))+
    facet_wrap(~Var3,scales = "free_x",labeller = label_parsed,nrow = 1)+
    geom_hline(yintercept = dd)+
    theme_bw()+
    scale_y_continuous(breaks = 1:18)+
    scale_x_log10()+
    theme(text=element_text(size=14),legend.position = "bottom")+
    ylab("ID estimate") + xlab(TeX("$n/n_1$ - log10 scale")) + 
    ggtitle(TeX(paste0("$n_2 = 2n_1$,   ID = ",dd)))
  
}

a1 <- plotter(list = d2_1000,order = 1,dd = 2,enne = 1000)
a1

ggsave("GRIDE_comparison_LB_MG/LB_MG_Gride_2_1000_new.png",height = 5,width = 12)

a2 <- plotter(d2_5000,order = 1,dd = 2,enne = 5000)
a2
ggsave("GRIDE_comparison_LB_MG/LB_MG_Gride_2_5000_new.png",height =  5,width = 12)

a3 <- plotter(d5_1000,order = 1,dd = 5,enne = 1000)
a3
ggsave("GRIDE_comparison_LB_MG/LB_MG_Gride_5_1000_new.png",height =  5,width = 12)

a4 <- plotter(d5_5000,order = 1,dd = 5,enne = 5000)
a4
ggsave("GRIDE_comparison_LB_MG/LB_MG_Gride_5_5000_new.png",height = 5,width = 12)


library(patchwork)
(a1+xlab(""))/a3+plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave("GRIDE_comparison_LB_MG/LB_MG_Gride_25_1000_new.png",height = 10,width = 12)

(a2+xlab(""))/a4+plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("GRIDE_comparison_LB_MG/LB_MG_Gride_25_5000_new.png",height = 10,width = 12)