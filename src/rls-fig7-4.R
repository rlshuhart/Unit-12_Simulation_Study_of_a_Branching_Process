# Only Simulation Code
genKidsV = function(bTimes, cTimes, parentID, lambda = 0.5, kappa = 0.3) {
  # Determine how many children each job has
  parentAge = cTimes - bTimes
  numKids = rpois(n = length(parentAge), lambda = lambda * parentAge)
  
  if (sum(numKids) == 0) return(NULL)
  
  # Determine the birth times of the children  
  kidStats = 
    mapply(function(n, min, max) {
      births = sort(runif(n, min, max))
      runtimes = rexp(n, rate = kappa)
      completes = rep(max, n) + runtimes
      data.frame(births, completes)
    },
    n = numKids , min = bTimes, max = cTimes, 
    SIMPLIFY = FALSE)
  
  
  return(data.frame(parentID = rep(parentID, numKids),
                    kidID = 1:sum(numKids), 
                    births = unlist(lapply(kidStats, "[[", "births")), 
                    completes = unlist(lapply(kidStats,"[[", "completes"))
  ))
}

familyTree = function(lambda = 0.5, kappa = 0.3, 
                      maxGen = 10, maxOffspring = 1000) {
  
  # Return value - a list with 1 data frame per generation.
  allGens = vector(mode = "list", length = maxGen)
  
  # Generate root of the tree
  allGens[[1]] = data.frame(parentID = NA, kidID = 1, 
                            births = 0, 
                            completes = rexp(1, rate = kappa))
  
  currentNumOffspring = 0
  
  # Generate future generations, one at a time.
  for (i in 2:maxGen) {
    nextGen = genKidsV(bTimes = allGens[[ (i - 1) ]]$births,
                       cTimes = allGens[[ (i - 1) ]]$completes,
                       parentID = allGens[[ (i - 1) ]]$kidID,
                       lambda = lambda, kappa = kappa)
    if (is.null(nextGen)) return(allGens[ 1:(i - 1) ]) 
    allGens[[ i ]] = nextGen
    currentNumOffspring = currentNumOffspring + nrow(nextGen)
    if (currentNumOffspring > maxOffspring) 
      return(allGens[1:i])
  }  
  allGens
}

exptOne = function(l, k, mG, mO){
  # Helper function to call familyTree
  # Returns - summary statistics for analysis,
  
  aTree = familyTree(lambda = l, kappa = k, maxGen = mG,
                     maxOffspring = mO)
  numGen = length(aTree)
  numJobs = sum(sapply(aTree, nrow))
  c(numGen, numJobs)
}


draw_tree <- function(lambda = .4, kappa = 1, maxGen = 10, seedx=12062013){
  library(dplyr)
  library(ggplot2)
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
  tree_df$y_cord <- 1:nrow(tree_df)*.5
  #tree_df$y_cord2 <- 0:(nrow(tree_df)-1)*.5
  tree_df$Parent_Gen <- tree_df$Gen-1
  
  marks <- inner_join(select(tree_df, kidID, Gen, y_cord), 
                      select(tree_df, parentID, Parent_Gen, births), 
                      by=c("kidID" = "parentID", "Gen"="Parent_Gen"))
  
  conn_lines <- inner_join(select(tree_df, births, y_cord),
                          select(marks, births, y_cord), by="births")
  
  gen_lines <- tree_df %>% 
    group_by(Gen) %>% 
    summarize(Gen_Break = max(y_cord)+.25) %>%
    mutate(Gen_Label = paste("Gen", Gen))
  
  g <- ggplot(tree_df) + 
    geom_segment(aes(x = births, y = y_cord, xend = completes, yend = y_cord)) +
    geom_segment(aes(x = births, y = y_cord.x, xend = births, yend = y_cord.y), data = conn_lines, linetype = 3) +
    geom_point(aes(x = births, y = y_cord), data = marks, shape=4, size=3, color="red") +
    geom_hline(aes(yintercept=Gen_Break), data = gen_lines, linetype = 2) +
    scale_y_continuous(labels=gen_lines$Gen_Label, breaks=gen_lines$Gen_Break) + 
    theme_bw() + 
    labs(x = "Time") +
    theme(axis.title.y=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  return(g)
}

draw_tree()
