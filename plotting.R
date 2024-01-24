#ggplot2 

plt <- ggplot(
data = surveys_complete,
mapping = aes(x=weight, y= hindfoot_length)
)
plt
str(plt)

plt +
  geom_point()

plt + 
  geom_point() +
  ggtitle("My first plot!") 

# a ggplot object contains data.frame + aestetics for x, y, axis, colors etc + geometry layers such as geom_poin/line/violin etc

ggplot(data = surveys_complete,
       mapping=aes (x=weight, y=(hindfoot_length)))+
  geom_point()

plt <- ggplot(data = surveys_complete,
       mapping=aes (x=weight, y=(hindfoot_length)))+
  geom_point()

plt + 
  ggtitle("Weight vs hindfoot length")

#hexbin to represent density of dots
install.packages("hexbin")
library(hexbin)

ggplot(data = surveys_complete,
  mapping=aes (x=weight, y=(hindfoot_length)))+
  geom_hex()

#manipulate transparency to see density again
ggplot(data = surveys_complete,
              mapping=aes (x=weight, y=(hindfoot_length)))+
  geom_point(alpha=0.2)

#manipulate color, size
ggplot(data = surveys_complete,
       mapping=aes (x=weight, y=(hindfoot_length)))+
  geom_point(alpha=0.2, color= "blue", size=3)

#manipulate color in order to be informative on 
ggplot(data = surveys_complete,
       mapping=aes (x=weight, y=hindfoot_length))+
  geom_point(alpha=0.2, aes(color=species_id))

# write better
ggplot(data = surveys_complete,
       mapping=aes (x=weight, y=hindfoot_length, color=species_id))+
  geom_point(alpha=0.5)

# challenge: scatter plo weight vs species ID, colo by plot type
ggplot(data = surveys_complete,
       mapping=aes (x=species_id, y=weight, color=plot_type))+
  geom_point(alpha=0.5)

#boxplot are more informative

ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight, color=species_id)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.1, color = "black") +
  geom_boxplot(outlier.shape = NA, fill=NA)

# violin plot
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight)) + 
  geom_violin()
 
# add scale layer
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight)) + 
  geom_violin() +
  scale_y_log10() +
  ylab("Weight (log10)")

# challenge: create new plot with boxplot + jittered scatter blot of hindfoot_l and species_id. boxplot in front of dots and fille with white
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=hindfoot_length)) + 
  geom_jitter(alpha = 0.1, color = "black") +
  geom_boxplot(outlier.shape = NA, fill="white")

## the trick is that the order of jitter and box plot does count
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=hindfoot_length)) + 
  geom_boxplot(outlier.shape = NA, fill="white") +
  geom_jitter(alpha = 0.1, color = "black")

# color by plot id
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=hindfoot_length, color=plot_id)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, fill="white")

# convert plot_id into factor _NON FUNZIONA_

ggplot(data = surveys_complete, mapping = aes(x=species_id, y=hindfoot_length)) + 
  geom_jitter (aes (color = factor (plot_id)))


yearly_count <- surveys_complete %>% 
  count(year, genus)
ggplot(data=yearly_count, 
       mapping = aes(x=year, y=n, group=genus, color=genus)) + 
  geom_line()

# pipe can be use to merge data generation and graph, graphs can be store in variable


yearly_count %>% 
  ggplot(mapping=aes(x=year, y=n, color=genus)) + 
  geom_line()

yearly_count_graph <- surveys_complete %>% 
count (year, genus) %>% 
  ggplot(mapping = aes(x=year, y=n, color= genus)) + 
  geom_line()

yearly_count_graph

# use facet to make single graph

ggplot (data=yearly_count, mapping = aes(x=year, y=n)) + 
  geom_line()+
  facet_wrap(facets = vars(genus))

# use facet to make single graph, add another variable
surveys_complete %>% 
  count (year, genus, sex) %>% 
  ggplot (mapping = aes(x=year, y=n, color=sex)) + 
  geom_line()+
  facet_wrap(facets = vars(genus))

# facet_grid uses two variable and separate
surveys_complete %>% 
  count (year, genus, sex) %>% 
  ggplot (mapping = aes(x=year, y=n, color=sex)) + 
  geom_line()+
  facet_grid (rows=vars(sex), col=vars(genus))

# use facet to make single graph, add another variable


plt <- surveys_complete %>% 
  count (year, genus, sex) %>% 
  ggplot (mapping = aes(x=year, y=n, color=sex)) + 
  geom_line()+
  facet_wrap(facets = vars(genus)) +
  scale_color_manual(values = c ("green", "orange"),
                     labels = c ("female", "male"),
                     name = "Sex") +
  xlab("Years of observation") +
  ylab("Numbers of individual") +
  ggtitle("Observed genera over time") +
  theme_bw(base_size = 10) + 
  theme(legend.position = "bottom", aspect.ratio = 1, axis.text.x = element_text (angle=45, hjust = 1), panel.grid = element_blank())

plt
ggsave(filename = "data/plot.pdf",
       plot = plt,
       width = 8,
       height = 8)
       
