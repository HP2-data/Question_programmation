df <- data.frame(x=rep(letters[1:5], 9), val=sample(1:100, 45)) # données 


plot(df$val, type = "l", 
     yaxt="none")  # retire les valeurs de l'axe des Y

points_break <- seq(0,100,20) # redéfinition de l'axe des Y 
axis(2, at=points_break, labels=paste0(points_break," suffix")) 
# utilisations de axis(2) pour redéfinir un axes des Y et de la fonction paste pour définir un suffix