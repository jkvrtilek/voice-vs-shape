# make table of DFA loading percentages
# Julia Vrtilek
# 22 Sept 2024, modified 24 Nov 2024

library(tidyverse)
library(plyr)
library(ggrepel)
library(geomtextpath)
library(ggh4x)

# get LD1s from familiar groups----
# zoo
fam_zoo <- read.csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/familiar-dfa-loadings-zoo.csv") %>% 
  mutate(zoo = abs(LD1)) %>% 
  select(X, zoo)

# 2016
fam16 <- read.csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/familiar-dfa-loadings-2016.csv") %>% 
  mutate(b16 = abs(LD1)) %>% 
  select(X, b16)

# 2019
fam19 <- read.csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/familiar-dfa-loadings-2019.csv") %>% 
  mutate(b19 = abs(LD1)) %>% 
  select(X, b19)

# get average loading percentage for each variable
fam <- join_all(list(fam_zoo, fam16, fam19), by='X', type='left')

familiar <- fam %>% 
  gather(origin, value, -X) %>% spread(X, value)


# get LD1s from never-met groups----
raw <- read.csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/never-met-dfa-loadings_1000.csv")

never_met <- raw %>% 
  gather(rep, value, -X) %>% spread(X, value)


# make plot for each variable----
for (i in 2:36) {
  q <- quantile(never_met[,i], probs = c(0.025,0.975))
  ci <- data.frame(quantile=names(q), values = unname(q))
  
  print(
    ggplot() + 
    geom_histogram(data = never_met, aes(x = never_met[,i])) + 
    geom_point(data = familiar, aes(x = familiar[,i], y = 0), color = "red", size = 5) +
    geom_vline(data = ci, aes(xintercept = values, color = "red"), show.legend = F) +
    xlab(paste(colnames(never_met)[i], colnames(familiar)[i], sep = "-"))
  )
}

#b16 turns is the only one outside lines
