library(tidyverse)
source("R_code/Experiments_plateau_no_noise/GRIDE_plateaus_functions.R")


# 500 ---------------------------------------------------------------------
n     = 500
n1    = seq(0,n/2-10,by=10)
n1[1] = 1
n2    = n1 * 2
id    = c(2,4,6,8,10)
sn    = c(0,.01,.05,.1)

# RES_500 = simulations(n = n,n1 = n1,n2 = n2,id = id,sn = sn,seed = 123452)
# saveRDS(RES_500,"Plateaus_500.RDS")

# 1000 --------------------------------------------------------------------
n     = 1000
n1    = seq(0,n/2-10,by=10)
n1[1] = 1
n2    = n1 * 2
id    = c(2,4,6,8,10)
sn    = c(0,.01,.05,.1)

# RES_1000 = simulations(n = n,n1 = n1,n2 = n2,id = id,sn = sn,seed = 54321)
# saveRDS(RES_1000,"Plateaus_1000.RDS")


n     = 2500
n1    = seq(0,n/2-10,by=10)
n1[1] = 1
n2    = n1 * 2
id    = c(2,4,6,8,10)
sn    = c(0,.01,.05,.1)

# RES_2500 = simulations(n = n,n1 = n1,n2 = n2,id = id,sn = sn,seed = 54321)
# saveRDS(RES_2500,"Plateaus_2500.RDS")



n     = 5000
n1    = seq(0,n/2-10,by=10)
n1[1] = 1
n2    = n1 * 2
id    = c(2,4,6,8,10)
sn    = c(0)
RES_5k = simulations(n = n,n1 = n1,n2 = n2,id = id,sn = sn,seed = 123452)
# saveRDS(RES_5k,"Plateaus_5000_new.RDS")
# RES5k = readRDS("Plateaus_5000_new.RDS")


n     = 10000
n1    = seq(0,n/2-10,by=10)
n1[1] = 1
n2    = n1 * 2
id    = c(2,4,6,8,10)
sn    = c(0)
RES_10k = simulations(n = n,n1 = n1,n2 = n2,id = id,sn = sn,seed = 123452)
# saveRDS(RES_10k,"Plateaus_10000_new.RDS")
