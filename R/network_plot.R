#' Plots a network object
#' 
#' Plots the network created using the netfacs.network() function; for networks with clear clusterin of elements, clusters can get different colours
#' 
#' @param netfacs.graph igraph network object resulting from netfacs.network() function
#' @param title string of the graph's main title
#' @param clusters if TRUE, the igraph::cluster_fast_greedy function is used to establish possible clusters in the dataset
#'
#' @return Function returns a ggnet plot of the network, where the size of nodes indicates how often they occur on their own, and edges indicate significant co-occurrance between them
#' 
#' @export
#'
#' @examples
#' data(emotions_set)
#' angry.face = netfacs(data = emotions_set[[1]],
#'  condition = emotions_set[[2]]$emotion,
#'  test.condition = 'anger',
#'  null.condition = NULL,
#'  duration = NULL, 
#'  ran.trials = 100,
#'  control = NULL,
#'  random.level = NULL,
#'  combination.size = 2)
#'  
#'  anger.net = netfacs.network(netfacs.data = angry.face, package = 'igraph', link = 'unweighted', significance = 0.01, min.count = 1, min.prob = 0, min.specificity = 0, ignore.element = NULL)
#'  anger.plot = network.plot(anger.net, title = 'Angry Faces', clusters = F)
#'  anger.plot + ggtitle('This is a ggplot object and can be manipulated')
#'  
#'  all.face = netfacs(data = emotions_set[[1]],
#'  ran.trials = 100,
#'  combination.size = 2)
#'  all.net = netfacs.network(netfacs.data = all.face, package = 'igraph', link = 'unweighted', significance = 0.01, min.count = 1, min.prob = 0, min.specificity = 0, ignore.element = NULL)
#'  all.plot = network.plot(anger.net, title = 'All Emotions', clusters = F)



network.plot <- function(netfacs.graph, title = 'network', clusters = T){
  
  # set digits printed to 3
  options(digits = 3)
  
  net.graph = netfacs.graph
  
  # prepare node and edge information
  node.label = vertex.attributes(net.graph)$name #nodes are named as they were in the original network object
  node.size = vertex.attributes(net.graph)$element.probability #size of nodes is determined by their probability to occur
  node.size[is.na(node.size)] = min(node.size, na.rm = T)
  
  edge.weight = edge.attributes(net.graph)$weight # weight of edges is determined by their weight attribute
  edge.size = cut(edge.weight, 3) # the line width of the edges is assinged to either weak, medium, strong
  edge.size.char = as.character(edge.size)
  edge.size.char[edge.size==levels(edge.size)[1]] = 1
  edge.size.char[edge.size==levels(edge.size)[2]] = 3
  edge.size.char[edge.size==levels(edge.size)[3]] = 5
  edge.size = as.numeric(edge.size.char)
  if(length(unique(edge.size))==1){edge.size = edge.size/edge.size}
  
  # if 'cluster' is not selected, the graph is plotted in black and white
  if(clusters == F){
    p = ggraph(graph = net.graph, 
               layout = 'igraph', 
               algorithm = 'kk') + # algorithm could be changed, e.g. to 'graphopt'
      geom_edge_link(mapping = aes (label = round(observed.probability,2)), # this creates and changes the edges
                     arrow = NULL,
                     colour="grey",
                     end_cap = circle(2, 'mm'), , 
                     start_cap = circle(2, 'mm'), 
                     label_dodge  = unit(3, "mm"),
                     angle_calc = "along",
                     show.legend = F) +
      geom_node_text(mapping = aes(label = name, # this creates and changes the nodes
                                   size = node.size,
                                   fontface = 'bold'),
                     show.legend = F) + 
      scale_size(range = c(2,7)) +
      ggtitle(title) +  
      theme_graph(base_family = "sans")
  }
  
  # if 'clusters' == T, then the fast and greedy algorithm is used to detect clusters and color the nodes accordingly
  if(clusters == T){
    net.un = net.graph
    net.community = igraph::cluster_fast_greedy(net.un) # other clustering algorithms exist, eg walktrap
    modular = round(igraph::modularity(net.community), 2) # modularity measure. Above 0.3 is good modularity
    net.com = data.frame(element = net.community$names, community = net.community$membership)
    color = rainbow(length(unique(net.com$community)))
    
    p = ggraph(graph = net.graph, # see above
               layout = 'igraph', 
               algorithm = 'kk') +
      geom_edge_link(mapping = aes (label = round(observed.probability,2)),
                     arrow = NULL,
                     colour="grey",
                     end_cap = circle(2, 'mm'), , 
                     start_cap = circle(2, 'mm'), 
                     label_dodge  = unit(3, "mm"),
                     angle_calc = "along",
                     show.legend = F) +
      geom_node_text(mapping = aes(label = name, 
                                   color = color[net.com$community], 
                                   size = node.size,
                                   fontface = 'bold'),
                     show.legend = F) + 
      scale_size(range = c(2,7)) +
      ggtitle(title) +
      theme_graph(base_family = "sans")
  }
  return(p)
}