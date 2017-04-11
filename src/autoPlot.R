

library(dplyr)
library(ggplot2)
lambda = .4
kappa = 1
maxGen = 10
seedx=12062013
set.seed(seedx); tree <- familyTree(lambda=lambda, kappa = kappa, maxGen = maxGen)

tree_df <- data.frame()
for (i in 1:length(tree)){
  # convert gen i to data frame
  tmp <- as.data.frame(tree[i])
  # lable gen i
  tmp$Gen <- i
  tree_df <- rbind(tree_df, tmp)
}

# Add y cord
#tree_df$y_cord <- 1:nrow(tree_df)*.5
tree_df$y_cord <- c(0.0, -1.0, .5, -.5, -1.5, 1.5, -2.0, 1.0, 2.0)

# Add parent generation reference
tree_df$Parent_Gen <- tree_df$Gen-1

# Self join the table to identify the births on the parent
marks <- inner_join(select(tree_df, kidID, Gen, y_cord), 
                    select(tree_df, parentID, Parent_Gen, births), 
                    by=c("kidID" = "parentID", "Gen"="Parent_Gen"))

# Connect birth to life line
conn_lines <- inner_join(select(tree_df, births, y_cord),
                         select(marks, births, y_cord), by="births")

# Generation separation locations
gen_lines <- tree_df %>% 
  group_by(Gen) %>% 
  summarize(Gen_Break = max(y_cord)+.25) %>%
  mutate(Gen_Label = paste("Gen", Gen))

g <- ggplot(tree_df) + 
  geom_segment(aes(y = -births, x = y_cord, yend = -completes, xend = y_cord), color="#7B7F7B", size=1.25) +
  geom_segment(aes(y = -births, x = y_cord.x, yend = -births, xend = y_cord.y), data = conn_lines, color="#7B7F7B", size=1.25) +
  geom_point(aes(y = -births, x = y_cord), data = marks, shape=19, size=3, color="#007F00") +
  #geom_hline(aes(yintercept=Gen_Break), data = gen_lines, linetype = 2) +
  #scale_y_continuous(labels=gen_lines$Gen_Label, breaks=gen_lines$Gen_Break) + 
  theme_bw() + 
  labs(y = "Time") +
  theme(axis.title.y=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


tree_df$hasChildren <- TRUE
tree_df[!(tree_df$y_cord %in% conn_lines$y_cord.y),]$hasChildren <- FALSE

# .5*tree_df$Gen + .5 * childNums/2

kidCount <- group_by(tree_df, Parent_Gen, parentID) %>% summarize(numKids = n()) %>% na.omit()

kids_df <- full_join(tree_df, kidCount, by=c("kidID" = "parentID", "Gen"="Parent_Gen"))
kids_df[is.na(kids_df$numKids),]$numKids <- 0
kids_df

kids_df$predict_cord <- .5*kids_df$Parent_Gen + .5 * (kids_df$numKids%%1)
kids_df

# if orgin == TRUE then 0
# if first born of orign go left
# if next born

spots <- seq(-((nrow(kids_df)-1)/4),((nrow(kids_df)-1)/4),.5)
spots
