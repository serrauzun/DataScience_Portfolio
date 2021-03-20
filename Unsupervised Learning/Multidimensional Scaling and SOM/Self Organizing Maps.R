install.packages("tidyverse")
install.packages("scales")
install.packages("magrittr")
install.packages("dplyr") 
install.packages("corrplot")
library(corrplot)
library(magrittr)
library(dplyr) 
library(plyr)
library(psych)
library(tidyverse)
require(tidyverse)
require(kohonen)
require(ggplot2)
require(ggridges)

find_node_by_coordinates <- function(x, y, grid_width) {
  return(((y * grid_width) + x) - grid_width)
}

# return the number of observations represented by each node
get_node_counts <- function(x) {
  df <- data.frame(node = x)
  counts <- df %>%
    group_by(node) %>%
    summarize(observations = n())
}

# guideline for grid size = 5 * sqrt(N)
# where N is the number of observations in the data set
find_grid_size <- function(N) {
  return(floor(sqrt(sqrt(N) * 5)))
}

# Shane Lynn 14-01-2014 used to define the palette
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
###############################

college.df <- file.choose()
college.df <- read.csv(college.df,header=TRUE)

multi.hist(college.df)
summary(college.df)
dim(college.df)
unique(college.df$gre)
table(college.df$admit)
sapply(college_acceptance, function(x) sum(is.na(x)))
coll_cor <- cor(college.df)
corrplot(coll_cor, method = "color", outline = T, addrect = 4, rect.col = "black", rect.lwd = 3,cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex = 1, addCoef.col = "black", number.digits = 1, number.cex = 1, col = colorRampPalette(c("purple","white","dark green"))(100))

apply(college.df, 2, function(x) length(unique(x)))
levels(college.df$admit) <- c('Not Admitted', 'Admitted')

gre <- college.df$gre
admit <- college.df$admit
rank <- college.df$rank
gpa <- college.df$gpa

gre
admit
rank
gpa

admitted <- college.df[college.df$admit == 1,]
not_admitted <- college.df[college.df$admit == 0,]

str(college.df)
# review the distributions of normal and fraudulent applications
admitted %>%
  gather(variable, value, -admit) %>%
  ggplot(aes(y = as.factor(variable),
             fill = admit,
             x = percent_rank(value))) +
  geom_density_ridges() +
  ggtitle('Distributions of Admitted') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Value') +
  ylab('Variable')

not_admitted %>%
  gather(variable, value, -admit) %>%
  ggplot(aes(y = as.factor(variable),
             fill = admit,
             x = percent_rank(value))) +
  geom_density_ridges() +
  ggtitle('Distributions of Not_Admitted') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Value') +
  ylab('Variable')


find_grid_size <- function(N) {
  return(floor(sqrt(sqrt(N) * 3)))
}

get_node_counts <- function(x) {
  df <- data.frame(node = x)
  counts <- df %>%
    group_by(node) %>%
    summarize(observations = n())
}

########normalize
college.df$admit <- as.numeric(college.df$admit)
college.df$gre <- as.numeric(college.df$gre)
college.df$gpa <- as.numeric(college.df$gpa)
college.df$rank <- as.numeric(college.df$rank)

college.df_norm <- normalize(college.df)
gre_norm <- college.df_norm$gre
admit_norm <- college.df_norm$admit
rank_norm <- college.df_norm$rank
gpa_norm <- college.df_norm$gpa

str(college.df_norm)

######

epochs = 2000
set.seed(123)
map_dimension = find_grid_size(dim(college.df_norm)[1])
som_grid = somgrid(xdim = map_dimension
                   ,ydim = map_dimension
                   ,topo = "rectangular")

college.sc = scale(college.df)
college.som <- supersom(college.sc, grid=som_grid, rlen=epochs, alpha=c(0.05,0.01), keep.data = TRUE)
summary(college.som)

college.som$unit.classif
observations_by_node <- get_node_counts(college.som$unit.classif)

college.som$unit.classif

par(mar = c(5,5,3,1)) #no of plots to combine
plot(college.som, type="changes", palette.name = coolBlueHotRed)
plot(college.som, type="count", palette.name = coolBlueHotRed)
plot(college.som, type="codes", palette.name = coolBlueHotRed)
plot(college.som, type = "dist.neighbours", palette.name = coolBlueHotRed)
