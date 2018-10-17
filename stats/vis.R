df <- algmtc[2:22]
df <- algmirex[2:22]

df[is.na(df)] <-0
# df[df > 100] <- 0
df <- df[,apply(df ,2, var,na.rm=TRUE) != 0]


dfs <- scale(df)
dfs <- data.frame(dfs)

dfs <- dfs[1:3606,2:12]
dfs <- dfs[1:433,2:15]


library(pheatmap)
pheatmap(as.matrix(dfs),color = colorRampPalette(c("navy", "white", "firebrick3"))(50),cluster_rows = FALSE,cluster_cols = FALSE, scale = "column")


library(reshape2)
d <- melt(dfs)
library(ggplot2)
ggplot(d, aes(x=value,colour=variable)) + 
  # geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
  #                binwidth=.5,
  #                colour="black", fill="white") +
  geom_density(alpha=.2) +  # Overlay with transparent density plot 
  xlim(-1, 10)

