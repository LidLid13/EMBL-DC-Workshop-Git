# create four objects called xxx 

# load tidyverse
library(tidyverse)

raw_cts <- read_csv("data_RNAseq/counts_raw.csv")
trans_cts <- read_csv("data_RNAseq/counts_transformed.csv")
sample_info <- read_csv("data_RNAseq/sample_info.csv")
test_result <- read_csv("data_RNAseq/test_result.csv")

# clicking on the files in the environment, have a look at the data

# use ggplot to see distribution of the counts across samples
# ggplot2 prefers long format

trans_cts_long <- trans_cts %>% 
pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)
view(trans_cts_long)

# to make a readable plot, use sample_info to create a new table, using join paying attentio to L and inner and full

trans_cts_long <- full_join(trans_cts_long,sample_info, by="sample")

#plot counts of cts, distribution
trans_cts_long %>% 
  ggplot(aes (x=cts)) +
  geom_freqpoly()

#plot counts of cts, colored by replicate, binwidth allow to define how wide the bin is
trans_cts_long %>% 
  ggplot(aes (x=cts, color=replicate)) +
  geom_freqpoly(binwidth=1)

#plot counts of cts, colored by replicate, separate by time and strain
trans_cts_long %>% 
  ggplot(aes (x=cts, color=replicate)) +
  geom_freqpoly(binwidth=1) +
  facet_grid (rows=vars(strain), col=vars(minute))

# challenge convert raw_cts in long and plot as before
raw_cts_long <- raw_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)
view(raw_cts_long)

raw_cts_long <- full_join(raw_cts_long, sample_info, by="sample")
view(raw_cts_long)

raw_cts_long %>% 
  ggplot(aes (x=cts)) +
  geom_freqpoly()

raw_cts_long %>% 
  ggplot(aes (x= cts, color=replicate)) +
  geom_freqpoly() +
  facet_grid (rows=vars(strain), col=vars(minute))

#not informative, try to scale x to log
raw_cts_long %>% 
  ggplot(aes (x= cts, color=replicate)) +
  geom_freqpoly() +
  facet_grid (rows=vars(strain), col=vars(minute)) +
  scale_x_log10()

# you can also insert the log in the ggplot, also axes legends are better
raw_cts_long %>% 
  ggplot(aes (x= log10 (cts), color=replicate)) +
  geom_freqpoly() +
  facet_grid (rows=vars(strain), col=vars(minute))

# what about the warning message Removed 645 rows containing non-finite values (`stat_bin()`). 
# this is likely due to the 0 which log10 is infinite, we change every 0 in 1 which log10 is 0  
raw_cts_long %>% 
  ggplot(aes (x= log10 (cts+1), color=replicate)) +
  geom_freqpoly(binwidth=1) +
  facet_grid (rows=vars(strain), col=vars(minute))

# instead of frequency poligon, make a boxblot
raw_cts_long %>% 
  ggplot(aes (x= factor (minute), y=log10 (cts+1), fill = strain)) +
  geom_boxplot() +
  facet_grid(cols = vars (replicate))

# check correlation of wt at time 0 and 30_r1, in the trans_cts data
trans_cts %>% 
  ggplot (aes (x=wt_0_r1, y=wt_30_r1)) + 
  geom_point()

# add correlation line
trans_cts %>% 
  ggplot (aes (x=wt_0_r1, y=wt_30_r1)) + 
  geom_point() +
  geom_abline(colour="green")

# correlate the replicates of wt_0 r1 and 2
trans_cts %>% 
  ggplot (aes (x=wt_0_r1, y=wt_0_r2)) + 
  geom_point() +
  geom_abline(colour="green")

# build something similar for all conditions and replicates, removing the gene column bcs is not numeric
# the result of cor is a matrix!
trans_cts_corr <- trans_cts %>% 
  select (-gene) %>% 
  cor (method = "spearman")

view (trans_cts_corr)

# let's make an heatmap
library(corrr)
rplot (trans_cts_corr)

# modify theme and legend
rplot (trans_cts_corr) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# compare raw and trans cts
summary(raw_cts_long$cts)
summary(trans_cts_long$cts)

# scatter plot of raw counts wt 0 r1 and r2
raw_cts %>% 
  ggplot (aes(x=wt_0_r1, y=wt_0_r2)) +
  geom_point

# use log scale
raw_cts %>% 
  ggplot (aes(x=wt_0_r1 + 1, y=wt_0_r2 + 1)) +
  geom_point () +
  scale_x_continuous (trans = "log2") +
  scale_y_continuous(trans = "log2")

# mean count x, variance on y on raw data
raw_cts_long %>% 
group_by(gene) %>% 
  summarize(mean_cts=mean(cts), var_cts= var(cts)) %>% 
  ggplot(aes(x=mean_cts, y=var_cts)) +
  geom_point() +
  geom_abline(colour="green") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

# mean count x, variance on y on trans data, no need of log2
## trans data are raw data passed trough DEseq2
trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts=mean(cts), var_cts= var(cts)) %>% 
  ggplot(aes(x=mean_cts, y=var_cts)) +
  geom_point()


