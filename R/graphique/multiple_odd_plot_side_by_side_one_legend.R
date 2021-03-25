
library(ggplot2)
# Multiple figure on same plot with ggplot 
df <- rbind(data.frame(exemple_side_byside = rep("A",100) ,
                       lockdown_status=rep("pre_lock", 100),
                       class = rep("week1", 100),
                       shift_sleep_duration=rnorm(100,0,1)),
            data.frame(exemple_side_byside = rep("A",100) ,
                       lockdown_status=rep("pre_lock", 100),
                       class = rep("week-end1", 100),
                       shift_sleep_duration=rnorm(100,0,2)),
            data.frame(exemple_side_byside = rep("B",100) ,
                       lockdown_status=rep("post_lock", 100),
                       class = rep("week", 100),
                       shift_sleep_duration=rweibull(100,2,1)),
            data.frame(exemple_side_byside = rep("B",100) ,
                       lockdown_status=rep("post_lock", 100),
                       class = rep("week-end", 100),
                       shift_sleep_duration=rweibull(100,2,2))
            )

ggplot(df, aes(x = class, y=shift_sleep_duration, color=lockdown_status, shape=class)) +
  geom_boxplot() +
  facet_wrap(~exemple_side_byside )  +
  scale_color_brewer(palette="Dark2") +
  theme(legend.position="bottom")

# Exemple avec nombre impaire de catégorie ce qui peut poser problème

df2 <- rbind(df ,
            data.frame(exemple_side_byside = rep("C",100) ,
                       lockdown_status=rep("pre_lock", 100),
                       class = rep("week1", 100),
                       shift_sleep_duration=rnorm(100,4,2)),
            data.frame(exemple_side_byside = rep("C",100) ,
                       lockdown_status=rep("pre_lock", 100),
                       class = rep("week-end1", 100),
                       shift_sleep_duration=rnorm(100,4,4)),
            data.frame(exemple_side_byside = rep("D",100) ,
                       lockdown_status=rep("post_lock", 100),
                       class = rep("week", 100),
                       shift_sleep_duration=rweibull(100,3,10)),
            data.frame(exemple_side_byside = rep("D",100) ,
                       lockdown_status=rep("post_lock", 100),
                       class = rep("week-end", 100),
                       shift_sleep_duration=rweibull(100,3,20)),
            data.frame(exemple_side_byside = rep("E",100) ,
                       lockdown_status=rep("post_lock", 100),
                       class = rep("week", 100),
                       shift_sleep_duration=rweibull(100,3,10)),
            data.frame(exemple_side_byside = rep("E",100) ,
                       lockdown_status=rep("post_lock", 100),
                       class = rep("week-end", 100),
                       shift_sleep_duration=rweibull(100,3,1))
            
)

ggplot(df2, aes(x = class, y=shift_sleep_duration, color=lockdown_status, shape=class)) +
  geom_boxplot() +
  facet_wrap(~exemple_side_byside)+
  scale_color_brewer(palette="Dark2") +
  theme(legend.position="bottom")

# Solution en utilisant grid extra et avec une legende par tout les graphiques  


# utilisation de lapply pour une liste de plot a partir d'une colonnes 
exemple <- lapply(unique(df2$exemple_side_byside) ,function(x) {
  if (x == unique(df2$exemple_side_byside)[1]) { # si nous sommes sur la premier itération de la boucle ajout de la légende
    ggplot(df2[df2$exemple_side_byside == x,], aes(x = class, y=shift_sleep_duration, color=lockdown_status, shape=class)) +
      geom_boxplot() +
      scale_color_brewer(palette="Dark2") +
      theme(legend.position="bottom") # ajout de la légende ici
    } else { # sinon pas de legnde
        ggplot(df2[df2$exemple_side_byside == x,], aes(x = class, y=shift_sleep_duration, color=lockdown_status, shape=class)) +
          geom_boxplot() +
          scale_color_brewer(palette="Dark2") + 
          theme(legend.position="none") # suppression de la légende ici 
      }
}
)



#extract legend from ggplot object 
# source : 
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

exemple[["mylegend"]] <- g_legend(exemple[[1]]) # ajout de l'objet légende a ma liste

exemple[[1]] <- exemple[[1]] +   theme(legend.position="none")

#layout of plots

# création d'un tableau indiquant ou doivent etres placer chacun de mes 6 élémebts de ma liste et la tailles qu'il occupes
# les Na permette de centrer le 5 eme éléments 

lay <- matrix(c(1, 2, NA, 
                1, 2, 5, 
                3, 4, 5, 
                3, 4, NA,
                6, 6, 6), 
              nrow = 5, byrow=TRUE)

gridExtra::marrangeGrob(exemple, layout_matrix = lay )


