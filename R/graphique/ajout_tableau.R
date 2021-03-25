library(tidyverse)
#Génération de données ressemblante pour être potentiellement partageable
###############################################################################
sim_dat <- function(proportions ,n_ind,time) {
  
 res <- numeric(time)
 for (i in 1:time){
    if (i == 1){
      sim <- rbinom(n_ind,1,proportions)
      remaining_ind <- n_ind - sum(sim)
      res[i] <- remaining_ind/n_ind
    } else {
      sim <- rbinom(remaining_ind,1,proportions)
      remaining_ind <- remaining_ind - sum(sim)
      res[i] <- remaining_ind/n_ind
      
    }
  }
  return(res)
}
n_ind = 2000
n = 1095
Data <- data.frame(jour = rep(seq(1,n),2),methode = as.factor(rep(c(0,1),each = n)), Surv = c(sim_dat(proportions = 0.0001,n_ind = n_ind,time =n ),sim_dat(proportions = 0.0002,n_ind = n_ind,time =n ) ))


mytable <- data.frame(jour = rep(c(0,183,366,549,732,915,1098),3),Gpe = rep(c("All","Pers","NoPers"),each = 7), N_at_risk = 10000 -  rep(c(0,183,366,549,732,915,1098),3)*rep(seq(1,3),each = 7)) 
new_table <- mytable %>% mutate(Gpe = str_replace(Gpe,"^All","all patient machin machin"),
                                Gpe = str_replace(Gpe,"^Pers","Persone machin machin"),
                                Gpe = str_replace(Gpe,"^NoPers","NoPers bidule truc "),
)


# Plot
###############################################################################
library(ggplot2)
library(gridExtra)
# juste une version ressemblante à ton plot
p <- ggplot(data = Data ,aes(x = jour,y = Surv, group = methode,color = methode)) + 
  geom_line() +  
  scale_x_continuous(name = "Follow-up (years)", breaks = c(0,366,732,1098), labels = as.character(c(0,366,732,1098)/366 )) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(Data$Surv)-(0.1*min(Data$Surv)), NA)) + theme(legend.position=c(0.1,.75))

theme(plot.margin=unit(c(.2,1,.1,1),"cm"))
###############################################################################
# Ce qui t'intérresse 
dat_table <- 
  ggplot(new_table, aes(x =jour,y=  Gpe,label = format(N_at_risk, nsmall = 1)))+
  geom_text(size = 3.5) + theme_bw()+
  theme(axis.line =  element_blank(),
        axis.ticks =  element_blank(),
        axis.text.y = element_text( face="bold", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank())  +
  xlab(NULL) + ylab(NULL)






library(cowplot)
plot_grid(p,dat_table, align = "hv", ncol = 1, rel_heights = c(4/5, 1/5))
