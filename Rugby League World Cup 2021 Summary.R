library(package = "scales")
library(package = "ggthemes")
library(package = "tidyjson")
library(package = "tidyverse")

library(forcats)
#source("v_theme.R")

filename = 'England.json'
England <- paste(readLines(filename), collapse="")

Tries <- England %>% as.tbl_json %>% gather_array %>%
  spread_values(
    score = jstring("score"),
    team = jstring('team'),
    minute = jstring('minute'),
    avg_rating = jnumber('avg_rating'),
    watches = jnumber("watches"),
    likes = jnumber("likes"),
    time = jnumber("time")
  )
Tries %>% head(n = 5) %>% select(score, team, minute)

Players <- England  %>% as.tbl_json  %>% gather_array %>%
  spread_values(
    score = jstring("score"),
    team = jstring('team'),
    minute = jstring('minute'),
    avg_rating = jnumber('avg_rating'),
    watches = jnumber("watches"),
    likes = jnumber("likes"),
    time = jnumber("time")
  ) %>% enter_object("players") %>% gather_array() %>%
  spread_values(
    name = jstring("name")
  )
Players %>% head(n = 8) %>% select(score, minute, name)

nrow(Tries)

Tries %>% ggplot(aes(x = minute)) +
  geom_bar() +
  labs(score = 'Summary Points by time') + 
  theme_fivethirtyeight()
#ggsave("imgs/films_by_minute.png", width = 8, height = 5)

by_team <- Tries %>% group_by(team) %>% summarise(n = n()) %>% arrange(-n)
by_team %>% filter(n > 0) %>%
  ggplot(aes(x = fct_reorder(team, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(score = 'Summary points scorers (number of times scored)') +
  theme_fivethirtyeight()
#ggsave("imgs/team_count.png", width = 8, height = 6)

# pull out just the top 5 teams
top_teams <- by_team %>% head(n = 6)
# filter films to those directed by these titans of Kung fu
tries_top_team <- Tries %>% filter(team %in% top_teams$team)
#plot bar chart
tries_top_team %>%
  ggplot(aes(x = minute)) +
  geom_bar(aes(fill = team)) +
  labs(score = 'Summary Point Scorers by Minute (Percentage version)') + 
  theme_fivethirtyeight()

# Try Fill Position
tries_top_team %>%
  ggplot(aes(x = minute)) +
  geom_bar(aes(fill = team), position = "fill") +
  labs(score = 'Summary Point Scorers by Minute') + 
  theme_fivethirtyeight()

tries_top_team_all <- Tries %>% mutate(team_label = ifelse(team %in% top_teams$team, team, 'Other'))
tries_top_team_all %>%
  ggplot(aes(x = minute)) +
  geom_bar(aes(fill = team_label)) +
  labs(score = 'Summary Point Scorers by Minute', fill = '') + 
  theme_fivethirtyeight()
#ggsave("imgs/top_teams_by_minute.png", width = 8, height = 5)

tries_top_team_all %>%
  ggplot(aes(x = minute)) +
  geom_bar(aes(fill = team_label), position = 'fill') +
  labs(score = 'Summary Point Scorers by Minute (Percentage)', fill = '') + 
  theme_fivethirtyeight() +
  scale_y_continuous(labels = percent)
#ggsave("imgs/top_teams_by_minute_fill.png", width = 8, height = 5)

summary(Tries)

by_player <- Players %>% group_by(name) %>% summarise(n = n()) %>% arrange(-n)
by_player %>% filter(n >0) %>%
  ggplot(aes(x = fct_reorder(name, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(score = 'Summary players on the field at more than 1 scoring moment') +
  theme_fivethirtyeight()

top_player <- by_player %>% head(n = 16)
tries_top_player <- Players %>% filter(name %in% top_player$name)
tries_top_player %>% 
  ggplot(aes(x = minute)) +
  geom_bar() +
  labs(score = 'Top Players by time on pitch') + 
  facet_wrap( ~ fct_relevel(name, top_player$name)) +
  # only label half of the minutes to make things a bit look cleaner
  scale_x_discrete(labels = function(x) { return(ifelse(as.numeric(x) %% 2, x, '')) }) +
  theme_fivethirtyeight() + 
  # angle label text
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

minute_count <- Tries %>% select(minute) %>% unique() %>% nrow()
player_active_minutes <- tries_top_player %>% group_by(name) %>% 
  summarise(active_minutes = length(unique(minute)), 
            percent_active = active_minutes / minute_count, 
            start = min(as.numeric(minute)), 
            end = max(as.numeric(minute))) %>% 
  arrange(percent_active)
player_active_minutes %>% 
  ggplot(aes(x = fct_inorder(name), y = percent_active)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  scale_y_continuous(labels = percent) + 
  labs(score = "Summary Percentage of points scoring present for") +
  theme_fivethirtyeight()
#ggsave("imgs/actor_percent_active.png", width = 8, height = 5)

player_active_minutes %>% gather(caps, cap_minute, start, end) %>%
  ggplot(aes(x = cap_minute, y = fct_inorder(name))) +
  geom_point(size = 3) + 
  geom_path(aes(group = name)) +
  labs(score = "Summary - When Players Were On The Pitch For Points") +
  theme_fivethirtyeight()
#ggsave("imgs/actor_career_span.png", width = 8, height = 5)   

summary(by_player$n)

library(package = "reshape2")

# filter players not present for many points
min_tries_players <- by_player %>% filter(n > 0)
popular_players <- Players %>% filter(name %in% min_tries_players$name) 
players_tries_matrix <- popular_players %>%
  acast(name ~ score,  fun.aggregate = length)

dim(players_tries_matrix)

players_tries_df_filtered <- players_tries_matrix %>% colSums(.)

norm <- players_tries_matrix / rowSums(players_tries_matrix)
hc_norm_players <- hclust(dist(norm, method = "manhattan"))

library(ggdendro)
ggdendrogram(hc_norm_players, rotate = TRUE)

ordering <-hc_norm_players$labels[hc_norm_players$order]
ordering

# http://stackoverflow.com/questions/13281303/creating-co-occurrence-matrix
cooccur <- players_tries_matrix %*% t(players_tries_matrix)
diag(cooccur) <- 0
heatmap(cooccur, symm = TRUE )

summary(rowSums(cooccur))

summary(colSums(cooccur))

summary(colSums(cooccur != 0))

collab_counts <- as.data.frame(colSums(cooccur != 0))

library(igraph)

cooccur <- players_tries_matrix %*% t(players_tries_matrix)
#cooccur <- ifelse(cooccur < 4, 0, cooccur)
g <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
summary(E(g)$weight)

summary(degree(g))

summary(strength(g))

library(igraph)
cooccur <- players_tries_matrix %*% t(players_tries_matrix)
#cooccur <- ifelse(cooccur < 4, 0, cooccur)
g <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
low_degree_v <- V(g)[degree(g) < 10] #identify those vertices part of less than three edges
g <- delete_vertices(g, low_degree_v) #exclude them from the graph
low_weight_e <- E(g)[E(g)$weight < 3]
g <- delete_edges(g, low_weight_e)
low_strength_v <- V(g)[strength(g) < 90]
g <- delete_vertices(g, low_strength_v) #exclude them from the graph
V(g)$betweenness <- strength(g)
plot(g, edge.width = E(g)$weight, 
     #layout=layout.fruchterman.reingold,
     layout=layout_with_fr,
     vertex.label.dist=0.5,
     #vertex.size = V(g)$betweenness,
     vertex.size = 3,
     vertex.color='steelblue',
     vertex.frame.color='white',        #the color of the border of the dots 
     vertex.label.color='black',        #the color of the name labels
     vertex.label.font=2,           #the font of the name labels
     vertex.label.cex=1,            #specifies the size of the font of the labels. can also be made to vary
     edge.color = hsv(0,0.2,0.5,alpha=0.2)
)
