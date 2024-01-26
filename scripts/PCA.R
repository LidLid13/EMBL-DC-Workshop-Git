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
pca_plot <- pc_scores %>% 
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

# plot PC1 and PC2 (x, y) to visualize the 10 genes
top_loadings <-  pc_loading %>% 
  filter(gene %in% top_genes)

loadings_plot <- ggplot(data = top_loadings) +
  geom_segment(aes (x=0, y=0, xend=PC1, yend=PC2),
               arrow = arrow (length=unit(0.1, "in")),
               color = "red") +
  geom_text (aes (x=PC1, y=PC2, label=gene), nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

plot(loadings_plot)
plot(pca_plot)

# remeber to save plot in a variable, then you use plot to print it 
# so you can also combine plots using patchwork, side by side with | or up and down with /
# also inserting A B C


library(patchwork)
(pca_plot | loadings_plot)
(pca_plot / loadings_plot)

(pca_plot|pca_plot|pca_plot)/loadings_plot +
  plot_annotation(tag_levels = "A")

# auto filling
library(ggfortify)

autoplot(sample_pca)
autoplot(sample_pca, data = sample_info, colour = "minute", shape= "strain")

# broom allows to use tidy to do the eigenvalues transformation
library(broom)

tidy(sample_pca, matrix = "eigenvalues")
tidy(sample_pca, matrix = "loadings") 

# modify -NON FUNZIONA-
autoplot(sample_pca, 
         data = sample_info, %>% 
         mutate (minute=asfactor(minute)),
         colour = "minute", 
         shape= "strain")

# for DEG results
test_results <- read_csv ("data_RNAseq/test_result.csv")
view (test_results)

# have a look at the column we have in the file that is the results of deseq on raw count (model fitting normalized data + this table)
# log2FC here is between WT at 0 time point and 15 min 
# stat_column is log2FC/lfgSE compared to normal standard distribution
# pvalue is associated with FC, how likely that FC is happening by chance
# padj is pvalue corrected fro multiple hypothesis testing
# comparison is telling the timepoint we are working on

# quick way to plot that is MA plot, to plot basemean on x axes and log2FC on the y, organize panels by comparison (time point) hint: consider log - transform base mean

test_results %>% 
ggplot (aes(x= log10 (baseMean), y=log2FoldChange)) +
  geom_point(alpha=0.1) +
facet_wrap(facets = vars (comparison))

# we usually use pvalue < 0.05 to identify relevant genes, we can plot it
# if else checks wether padj is lower than 0.01, if it is it copies log2FC, if no leaves empty
ma_plot <- test_results %>% 
  mutate(sig= ifelse (padj<0.01, log2FoldChange, NA)) %>% 
  ggplot (aes(x= log10 (baseMean), y=log2FoldChange)) +
  geom_point(alpha=0.1) +
  geom_point(aes(y=sig), color="tomato", size=1) +
  geom_hline(yintercept = 0, color= "blue") +
  facet_wrap(facets = vars (comparison))
# comments: genes with higher base mean tend to change less, hline adds a horizontal line at 0 

(ma_plot | pca_plot)

# visualizing expression trends: 1. identify significative genes: padj<0.01
# pull test_results [,gene] aka test_results$gene, it becomes a vector
candidate_genes <- test_results %>% 
  filter(padj<0.01) %>% 
  pull (gene) %>%
  unique ()

#2. filter trans_cts_long for candidate genes and compute mean expression value for each gene in each timepoint and genotype

trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene, strain, minute) %>% 
  summarise(mean_cts = mean(cts), nrep=n()) %>% 
  ungroup ()

#3 make spaghetti plot of trends
trans_cts_mean %>% 
  ggplot(aes (x=minute, y= mean_cts)) +
  geom_line (aes(group=gene), alpha=0.3) +
  facet_grid(rows=vars(strain))

# Scaling data to improve visualization, the one below is called transformation
trans_cts_mean <- trans_cts_long  %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene) %>% 
  mutate(cts_scaled = ( cts - mean(cts))/sd (cts)) %>% 
  summarize(mean_cts_scaled = mean(cts_scaled), nrep=n()) %>% 
  ungroup ()

#plot it - NON FUNZIONA-
trans_cts_mean %>% 
  ggplot (aes (x= minute, y= mean_cts_scaled)) +
  geom_line (aes (group=gene), alpha=0.3) +
  geom_hline (yintercept = 0, color = "red", linetype= "dashed") +
  facet_grid (rows = vars (strain)) 

#################
# CLUSTERING calculate distance between genes, 

trans_cts <- read_csv("data_RNAseq/counts_transformed.csv")

#1 create a matrix of counts

hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix()

row.names(hclust_matrix) <- trans_cts$gene 

hclust_matrix <- hclust_matrix [candidate_genes,] 

hclust_matrix <- hclust_matrix %>% 
  t () %>% 
  scale() %>% 
  t ()

# to do cluster we need to understand pairwise correlation, calculate distance

gene_dist <- dist(hclust_matrix)

# hierarchical clustering, put a cut at 10 and ask to build cluster from there

gene_hclust <- hclust (gene_dist, method = "complete")
plot (gene_hclust, labels=F)
  abline(h=10, col="red", lwd=2)
  
# there is also a command that allows to say how many cluster you want and tell it
  
cutree(gene_hclust, k=5)

# generate a table with these version

gene_cluster <- cutree(gene_hclust, k=5) %>% 
  enframe() %>% 
rename (gene= name, cluster=value)  

trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")

# -NON FUNZIONA-
trans_cts_cluster %>% 
  ggplot (aes (x=minute, y= mean_cts_scaled)) +
  geom_line(aes(group=gene)) +
  facet_grid(cols=vars (cluster), rows= vars (strain))

# heatmap

library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = F)
