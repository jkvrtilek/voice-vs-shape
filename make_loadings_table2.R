# make table of DFA loading percentages
# Julia Vrtilek
# 22 Sept 2024

library(tidyverse)
library(plyr)
library(ggrepel)
library(geomtextpath)
library(ggh4x)


# get LD1 from pre-intro DFA
pre_met <- read.csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/pre_dfa-loadings.csv") %>% 
  mutate(pre = abs(LD1)) %>% 
  select(X, pre)

# get LD1 from post-intro DFA
post_met <- read.csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/post_dfa-loadings.csv") %>% 
  mutate(post = abs(LD1)) %>% 
  select(X, post)


# get LD1s from familiar groups
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

fn <- colnames(fam)[-1]

fam_mean <- fam %>% 
  mutate(fam_avg = rowMeans(.[,fn])) %>% 
  select(X, fam_avg) %>% 
  arrange(desc(fam_avg))


# get LD1s from never-met groups
never_met <- read.csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/abs_never-met-dfa-loadings.csv")

cn <- colnames(never_met)[-1]

never_met_mean <- never_met %>% 
  mutate(nm_avg = rowMeans(.[, cn])) %>% 
  select(X, nm_avg) %>% 
  arrange(desc(nm_avg))


# summary table
all_LD1s <- join_all(list(never_met_mean, fam_mean, pre_met, post_met), by='X', type='left') %>% 
  arrange(desc(abs(post)))

write.csv(all_LD1s, "/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/all_LD1s.csv")


# make plot

green <- c("sp.ent","skew","kurt","sfm")
red <- c("segments","turns","maxslope","minslope","meanslope","neg_slopes","pos_slopes","abs_minslope")
to_use <- c(green,red)

all_LD1s <- read_csv("/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/all_LD1s.csv")

percent_LD1s <- all_LD1s %>% 
  mutate(measure = X) %>% 
  mutate(never_met = nm_avg/sum(nm_avg)) %>% 
  mutate(familiar = fam_avg/sum(fam_avg)) %>% 
  mutate(pre_convergence = pre/sum(pre)) %>% 
  mutate(post_convergence = post/sum(post)) %>% 
  select(measure, never_met:post_convergence)

write.csv(percent_LD1s, "/Users/jkvrtilek/Desktop/OSU/PhD/GitHub/voice-vs-shape/percent_LD1s.csv")

plot_data <- percent_LD1s %>% 
  filter(measure %in% to_use) %>% 
  pivot_longer(cols = never_met:post_convergence,
               names_to = "group",
               values_to = "loadings") %>% 
  mutate(facet = case_when(group == "never_met" ~ "overall",
                           group == "familiar" ~ "overall",
                           group == "pre_convergence" ~ "met_in_2016",
                           group == "post_convergence" ~ "met_in_2016")) %>% 
  mutate(color = case_when(measure %in% green ~ "green",
                           measure %in% red ~ "red"))

plot_data$group <- factor(plot_data$group, levels = c("never_met","pre_convergence","familiar","post_convergence"))

cpal <- c("#D55E00", "#009E73")

# zoomed out
plot_data %>% 
  ggplot(aes(x = group, y = loadings, group = measure, col = color))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  facet_wrap(~facet, scales = "free") +
  scale_colour_manual(values=cpal) +
  facetted_pos_scales(x = list(
    facet == "met_in_2016" ~ scale_x_discrete(labels = c("pre-\nconvergence", "post-\nconvergence")),
    facet == "overall" ~ scale_x_discrete(labels = c("never met", "familiar")))) +
  geom_label_repel(data = subset(plot_data, group != "never_met" & group != "pre_convergence"),
                   aes(label = measure),
                   max.overlaps = 20,
                   direction = "y",
                   nudge_x = 0.2) +
  coord_cartesian(ylim = c(0,0.33)) +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        strip.text = element_text(size= 24)) +
  theme_bw() +
  theme(legend.position = "none")

# zoomed in
plot_data %>% 
  ggplot(aes(x = group, y = loadings, group = measure, col = color))+
  geom_point(size=3)+
  geom_line(size=1)+
  facet_wrap(~facet, scales = "free") +
  scale_colour_manual(values=cpal) +
  scale_y_continuous(labels = function(x) format(x, digits = 1)) + 
  facetted_pos_scales(x = list(
    facet == "met_in_2016" ~ scale_x_discrete(labels = c("pre-\nconvergence", "post-\nconvergence")),
    facet == "overall" ~ scale_x_discrete(labels = c("never met", "familiar")))) +
  geom_label_repel(data = subset(plot_data, group != "never_met" & group != "pre_convergence"),
                   aes(label = measure),
                   max.overlaps = 20,
                   direction = "y",
                   nudge_x = 0.2) +
  coord_cartesian(ylim = c(0, 0.01)) +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        strip.text = element_text(size = 22)) +
  theme_bw() +
  theme(legend.position = "none")

plot_data %>% 
  ggplot(aes(x = group, y = loadings, group = measure, col = color))+
  geom_point()+
  facet_wrap(~facet, scales = "free") +
  scale_colour_manual(values=cpal) +
  geom_textline(aes(label = measure)) +
  theme(legend.position = "none")

plot_data %>% 
  ggplot(aes(x = group, y = loadings, group = measure, col = color))+
  geom_point()+
  facet_wrap(~facet, scales = "free") +
  scale_colour_manual(values=cpal) +
  geom_textline(aes(label = measure)) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 0.01))
