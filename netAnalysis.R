library(sfnetworks)
library(tidygraph)
library(sf)
library(tmap)

setwd("C:\\Users\\jakob\\projects\\nyc-covid-ridership-decrease")


# get a list of GTFS.zip files
my_gtfs_feeds <- list.files(path = "input\\", pattern =".zip", full.names = T)

# load function
source("gtfs_to_igraph.R")

# run function
g <- gtfs_to_igraph(list_gtfs = my_gtfs_feeds,  dist_threshold =30 , save_muxviz =FALSE)

g2 <- as_tbl_graph(g, directed = TRUE) 

net <- as_sfnetwork(g2)


#Degree centrality assigns an importance score based simply on the number of links held by each node.
#Betweenness centrality measures the number of times a node lies on the shortest path between other nodes.
#Closeness centrality scores each node based on their ‘closeness’ to all other nodes in the network.
#EigenCentrality measures a node’s influence based on the number of links it has to other nodes in the network. EigenCentrality then goes a step further by also taking into account how well connected a node is, and how many links their connections have, and so on through the network.
net2 <- net %>% convert(to_spatial_explicit) %>% 
  activate("nodes") %>% 
  mutate(cent_btwn = centrality_betweenness(),
         cent_degree = centrality_degree(),
         #cent_close = centrality_closeness(),
         cent_eigen = centrality_eigen()
         ) %>% 
  activate("edges") %>% 
  mutate(centrality = centrality_edge_betweenness())
  


save(net2, file = "net2.Rda")
edges_sf <- net2 %>% st_as_sf("edges")
nodes_sf <- net2 %>% st_as_sf("nodes")



tmap_mode("view")
tm_shape(edges_sf) + 
  tm_lines(col = "gray50") +#"centrality") +
  tm_shape(nodes_sf) +
  tm_dots(col = "cent_btwn", id = "stop_name")

