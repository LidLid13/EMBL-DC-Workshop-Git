# variance is how big difference are between your genes, how spread values are 
# PCA try to group/intersec values to catch their variance, the n of PCA you can calculate is the n of variable you measure/their comparison
# scale is squared st.dv, tells similarity

library(tidyverse)

trans_cts <- read_csv("data_RNAseq/counts_transformed.csv")
sample_info <- read_csv("data_RNAseq/sample_info.csv")

# converts our dataframe to a matrix
# t is to transpose matrix
pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>% 
  as.matrix() %>% 
  t()

sample_pca <- prcomp(pca_matrix)
# sample_pca is a complex object, we cannot tibble it all, but every time we extract a specific item 

#explore results (other than environment-file)
class(sample_pca)
str(sample_pca)
summary(sample_pca)

# see some value, rownames allows to maintain sample names
pca_matrix [1:10,1:5]
as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames="sample")

# calculate eigenvalue
pc_eigenvalues <- sample_pca$sdev^2
pc_eigenvalues <- tibble(PC= factor(1:length(pc_eigenvalues)),
                         variance=pc_eigenvalues) %>% 
  mutate (pct= variance/sum(variance)*100) %>% 
  mutate(pct_cum=cumsum(pct))

# pareto plot, combination of comulative line plot and bar plot
pc_eigenvalues %>% 
  ggplot(aes(x=PC)) + 
  geom_col (aes(y=pct)) +
  geom_line(aes (y=pct_cum), group=1) +
  geom_point(aes(y=pct_cum)) +
  geom_hline (yintercept=90) +
  labs(x="Principal component", y="Fraction variance explained")

# PCA1 is the most variant, line represent cumulation of fractions, you may want to consider those that constitute up to 90% of total variance (added a line to see), so the first 14 PCA

# visualize principal component scores
pc_scores <- sample_pca$x %>% 
  as_tibble (rownames= "sample") 

pc_scores %>% 
  ggplot (aes (x=PC1, y=PC2)) +
  geom_point()

# add some info, color should be a factor otherwise it uses a shade of color (time ranges from 0 to 180min)
pc_scores %>% 
  full_join (sample_info, by="sample") %>% 
  ggplot (aes (x=PC1, y=PC2, 
               color= factor(minute), 
               shape= strain)) +
  geom_point()

# comment to graph: something is happening at time 15 and 30, no big effects of genetic state

# add some info, color should be a factor otherwise it uses a shade of color (time ranges from 0 to 180min)

# plot PC3 and PC4
pc_scores %>% 
  full_join (sample_info, by="sample") %>% 
  ggplot (aes (x=PC3, y=PC4, 
               color= factor(minute), 
               shape= strain)) +
  geom_point()
  
# PCA 
pc_loading <- sample_pca$rotation %>% 
  as_tibble(rownames="gene")
  
# top genes obtain 10

top_genes <- pc_loading %>% 
  select (gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()

# 
top_loadings <-  pc_loading %>% 
  filter(gene %in% top_genes)

ggplot(data = top_loadings) +
  geom_segment(aes (x=0, y=0, xend=PC1, yend=PC2),
               arrow = arrow (length=unit(0.1, "in")),
               color = "green") +
  geom_text (aes (x=PC1, y=PC2, label=gene), nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

