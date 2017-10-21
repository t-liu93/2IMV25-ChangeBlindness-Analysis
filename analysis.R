# install.packages('plyr', dep = TRUE)
# install.packages('ggplot2', dep = TRUE)
# install.packages('ggpubr')
# install.packages('car')
library(plyr)
library(ggplot2)
library(ggpubr)
library(car)

### Define functions
box_group <- function(df, group, lables){
          ggboxplot(df, x = group, y = "time", 
          color = group, palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = lables,
          ylab = "search time(ms)", xlab = group) +
          stat_compare_means(method = "anova")
}

### Read csv file and define detected as time<2000
input = "~/Documents/y2_q1/VR/CollectedData.csv"
data <- read.table(input, header = TRUE, sep = ",", 
                   col.names = c("id", "scale", "category", "trajectory", "time"))
df <- data.frame(data[complete.cases(data),])
df_dect <- subset(df, time!=2000)
df$detected[df$time >= 2000] <- 'False'
df$detected[df$time <  2000] <- 'True'

### Count detected rate in different groups
list.group <- c('category', 'scale', 'trajectory')

for (group in list.group){
  print(group)
  tmp <- ddply(df, c(group), transform,
        sum.n    = length(time)
  )
  group_rate <- ddply(tmp, c(group, "detected"), summarise,
        n    = length(time),
        detected_rate = (n / sum.n[1])*100,
        mean = mean(time[time != 2000]),
        sd   = sd(time[time != 2000]),
        se   = sd / sqrt(n)
  )


### Plot the detected rate in each group
  print(ggplot(group_rate, aes(fill=detected, y=detected_rate, x=eval(as.symbol(group)))) + 
    {if (group == 'scale') geom_bar(position="dodge", stat="identity", width = 0.2)
     else geom_bar(position="dodge", stat="identity", width = 0.6)} +
    {if (group == 'scale') scale_x_continuous(name = group, breaks=seq(1.25, 1.5, 0.25), limits=c(1,1.75))} +
    scale_y_continuous(limits=c(0, 100)) +
    ggtitle(paste("Detected rate in different", group)) +
    xlab(group) +
    ylab("detected rate(%)") +
    theme(plot.title = element_text(size= 14, vjust = 1, hjust = 0.5),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12)))
}
### Box-plot of each group with Anova p-value
lables = c("big", "medium", "small")
box_category <- box_group(df_dect, "category", lables)
lables = c("1.25", "1.5")
box_scale <- box_group(df_dect, "scale", lables)
lables = c("linear", "nonLinear")
box_trajectory <- box_group(df_dect, "trajectory", lables)

ggarrange(box_category, box_scale, box_trajectory + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

### Compute the analysis of variance(Anova) in each group
res.aov <- aov(time ~ category, data = df_dect)

# Summary of the analysis
summary(res.aov)

# Tukey multiple pairwise-comparisons
TukeyHSD(res.aov)

# Check homogeneity of variances
leveneTest(time ~ category, data = df_dect)

# Check the normality assumption
qqplot <- plot(res.aov, 2)
# Shapiro test 
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )

# Non-parametric alternative to one-way ANOVA test
kruskal.test(time ~ category, data = df_dect)
#rm(list=ls(all=TRUE)) 
