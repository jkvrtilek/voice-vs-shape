# DFA for each group of familiar bats: zoo, 2016, 2019
# use ONLY post-introduction calls!

# OBC bats are from a bunch of different zoos, ancestry chart on website will tell me what's up (bat1-7 are from Chicago zoo, some bats from OBC are also from Chicago zoo, but half of them died in Maryland??)
# don't include bats 1-7, everyone else knows each other

# load packages
library(MASS)
library(tidyverse)

# set working directory
setwd("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape")

# get bat data
bats <- read.csv("metadata.csv") %>% 
  dplyr::select(!X)

# separate into groups
bat_zoo <- bats %>% 
  filter(capture.site == "zoo") %>% 
  filter(!str_detect(bat_ID, "^bat"))

bat16 <- bats %>% 
  filter(str_detect(date, "^2016") | str_detect(date, "^2017")) %>% 
  filter(capture.site != "chilibre") %>% 
  filter(capture.site != "gamboa")

bat19 <- bats %>% 
  filter(str_detect(date, "^2019"))

# get call measures
raw <- read.csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/LFS/vampire_call_measures_filtered_transformed.csv") %>% 
  dplyr::select(sound.files:indicator) %>% 
  separate(sound.files, into=c('ds','date', 'bat', 'file', 'sel'), sep="_", remove = FALSE) %>% 
  dplyr::select(!c(ds,file,sel))

# separate call measures by bat group
dzoo <- raw %>% 
  filter(bat %in% bat_zoo$bat_ID)

d16 <- raw %>% 
  filter(bat %in% bat16$bat_ID) %>% 
  filter(!str_detect(date, "^2019"))

d19 <- raw %>% 
  filter(bat %in% bat19$bat_ID) %>% 
  filter(str_detect(date, "^2019"))

# ZOO DFA ------

# get sample sizes
dzoo2 <- dzoo %>%
  group_by(bat) %>% 
  mutate(sample.size= n()) %>%
  ungroup()

# look at sample sizes
sort(unique(dzoo2$sample.size))

# filter out less than 100 calls - loses 9 out of 31 bats
dzoo3 <- dzoo2 %>% 
  filter(sample.size > 100)

# classify calls to bat using a single dfa without cross validation
dfa_zoo <- lda(bat ~ 
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
            data=dzoo3)

# save all DFA loadings sorted by absolute value of DF1
loadings_zoo <- 
  dfa_zoo$scaling %>% 
  as.data.frame() %>% 
  arrange(desc(abs(LD1)))

write.csv(loadings_zoo, "familiar-dfa-loadings-zoo.csv")

# get classification rates
predictions_zoo <- predict(dfa_zoo)
dzoo3$prediction <- predictions_zoo$class
cm_zoo <- table(dzoo3$bat, dzoo3$prediction)

# get overall correct classification rate
correct.cases <- sum(diag(cm_zoo))
all.cases <- sum(cm_zoo)
accuracy_zoo <- correct.cases/all.cases

# 2016 DFA ------

# get sample sizes
d16_2 <- d16 %>%
  group_by(bat) %>% 
  mutate(sample.size= n()) %>%
  ungroup()

# look at sample sizes
sort(unique(d16_2$sample.size))

# filter out less than 100 calls - loses 6 out of 45 bats
d16_3 <- d16_2 %>% 
  filter(sample.size > 100)

# classify calls to bat using a single dfa without cross validation
dfa16 <- lda(bat ~ 
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
               data=d16_3)

# save all DFA loadings sorted by absolute value of DF1
loadings16 <- 
  dfa16$scaling %>% 
  as.data.frame() %>% 
  arrange(desc(abs(LD1)))

write.csv(loadings16, "familiar-dfa-loadings-2016.csv")

# get classification rates
predictions16 <- predict(dfa16)
d16_3$prediction <- predictions16$class
cm16 <- table(d16_3$bat, d16_3$prediction)

# get overall correct classification rate
correct.cases <- sum(diag(cm16))
all.cases <- sum(cm16)
accuracy16 <- correct.cases/all.cases

# 2019 DFA ------

# get sample sizes
d19_2 <- d19 %>%
  group_by(bat) %>% 
  mutate(sample.size= n()) %>%
  ungroup()

# look at sample sizes
sort(unique(d19_2$sample.size))

# filter out less than 100 calls - loses 1 out of 23 bats
d19_3 <- d19_2 %>% 
  filter(sample.size > 100)

# classify calls to bat using a single dfa without cross validation
dfa19 <- lda(bat ~ 
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
               data=d19_3)

# save all DFA loadings sorted by absolute value of DF1
loadings19 <- 
  dfa19$scaling %>% 
  as.data.frame() %>% 
  arrange(desc(abs(LD1)))

write.csv(loadings19, "familiar-dfa-loadings-2019.csv")

# get classification rates
predictions19 <- predict(dfa19)
d19_3$prediction <- predictions19$class
cm19 <- table(d19_3$bat, d19_3$prediction)

# get overall correct classification rate
correct.cases <- sum(diag(cm19))
all.cases <- sum(cm19)
accuracy19 <- correct.cases/all.cases