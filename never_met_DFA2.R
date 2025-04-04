# DFA for bats that never met
# groups: zoo, chilibre, gamboa, las pavas, lake bayano/la chorrera 
# what to do with tole bats?? my instinct is to ignore OR add the 2016 tole bats to las.pavas pool

# goal: pull one bat from each group at random, run a DFA
# how many times?

# load packages
library(MASS)
library(tidyverse)

# set working directory
setwd("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape")

# get call measures
raw <- read.csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/LFS/vampire_call_measures_filtered_transformed.csv") %>% 
  dplyr::select(sound.files:indicator) %>% 
  separate(sound.files, into=c('ds','date', 'bat', 'file', 'sel'), sep="_", remove = FALSE) %>% 
  dplyr::select(!c(ds,file,sel)) %>%
  group_by(bat) %>% 
  mutate(sample.size= n()) %>%
  ungroup()

# make sample size filter
samples <- raw %>% 
  dplyr::select(bat, sample.size) %>% 
  distinct() %>% 
  filter(sample.size > 100)

# get bat data; remove bats with less than 100 calls
bats <- read.csv("metadata.csv") %>% 
  dplyr::select(!X) %>% 
  filter(bat_ID %in% samples$bat)


# separate into groups -----
zoo <- bats %>% 
  filter(capture.site == "zoo")

chili <- bats %>% 
  filter(capture.site == "chilibre")

gamboa <- bats %>% 
  filter(capture.site == "gamboa")

tole.pavas <- bats %>% 
  filter(capture.site == "las.pavas" | capture.site == "tole") %>%
  separate(date, into = c("year","month","day"), remove = FALSE) %>% 
  filter(year < 2018)

lake.chorr <- bats %>% 
  filter(capture.site == "chorrera" | capture.site == "lake.bayano")


# get bat names for each group -----
zoo_bats <- unique(zoo$bat_ID)

chili_bats <- unique(chili$bat_ID)

gamboa_bats <- unique(gamboa$bat_ID)

tolpav_bats <- unique(tole.pavas$bat_ID)

lch_bats <- unique(lake.chorr$bat_ID)


# get unique combinations of bats -----
perms <- crossing(chilibre = chili_bats, lakech = lch_bats, zoo = zoo_bats, tolpav = tolpav_bats)

# set number of DFAs to run
reps <- 20

# pull the correct number of combos from perms, making sure to get an even sampling of sam bats
per_sam <- reps/4
a <- nrow(perms)/4
b <- a + 1
c <- 2*a
d <- c + 1
e <- 3*a
f <- e + 1
g <- 4*a

rownums <- c(sample(1:a, size = per_sam, replace = FALSE),
             sample(b:c, size = per_sam, replace = FALSE),
             sample(d:e, size = per_sam, replace = FALSE),
             sample(f:g, size = per_sam, replace = FALSE))

to_run <- perms[rownums,]


# DFA to repeat X times -----
loadings <- list()

for (i in 1:nrow(to_run)) {
  
  usebats <- as.character(c(to_run[i,],gamboa_bats))
  
  d <- raw %>% 
    filter(bat %in% usebats)
  
  # classify calls to bat using a single dfa without cross validation
  dfa <- lda(bat ~ 
               duration+    
               meanfreq+    
               sd+          
               freq.median+ 
               freq.Q25+ 
               freq.Q75+    
               freq.IQR+    
               time.median+
               time.Q25+   
               time.Q75+    
               time.IQR+    
               skew+        
               kurt+
               sp.ent+      
               time.ent+  
               entropy+     
               sfm+         
               meandom+     
               mindom+     
               maxdom+      
               dfrange+    
               modindx+     
               startdom+    
               enddom+      
               dfslope+   
               meanpeakf+   
               peakf+
               maxslope+
               minslope+
               abs_minslope+
               pos_slopes+
               neg_slopes+
               turns+
               meanslope+
               segments,
             CV=F, 
             data=d)
  
  # save all DFA loadings sorted by absolute value of DF1
  loadings[[i]] <- 
    dfa$scaling %>% 
    as.data.frame() %>% 
    mutate(!!paste("LD1",i,sep = "_") := abs(LD1)) %>% 
    dplyr::select(!!paste("LD1",i,sep = "_"))
  
}

load_df <- data.frame(Reduce(cbind, loadings))

write.csv(load_df, "abs_never-met-dfa-loadings.csv")
