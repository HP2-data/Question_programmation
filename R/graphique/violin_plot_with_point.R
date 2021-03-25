 # Non finalisé 

df <- rbind(data.frame(lockdown_status=rep("pre_lock", 100),class = rep("week1", 100),shift_sleep_duration=rnorm(100,0,1)),
data.frame(lockdown_status=rep("pre_lock", 100),class = rep("week-end1", 100),shift_sleep_duration=rnorm(100,0,2)),
data.frame(lockdown_status=rep("post_lock", 100),class = rep("week", 100),shift_sleep_duration=rweibull(100,2,1)),
data.frame(lockdown_status=rep("post_lock", 100),class = rep("week-end", 100),shift_sleep_duration=rweibull(100,2,2)))




ggplot(df, aes(x=class, y=shift_sleep_duration, color=lockdown_status, shape=class)) +
  geom_boxplot() +
  geom_violin(aes(fill = class),trim=FALSE,alpha = 0.1) +
  scale_color_brewer(palette="Dark2") +
  theme_minimal()

