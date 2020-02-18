#' Plots the overlap of multiple conditions as bipartite network
#' 
#' The function takes multiple netfacs objects and plots how different elements connect the conditions
#' 
#' @param netfacs.list list of objects resulting from netfacs() function or multiple.netfacs() function
#'
#' @return Function returns a ggnet plot where each condition is connected to those elements that occur significantly in this condition, and each element is connected to each condition under which it occurs significantly more than expected
#' 
#' @export
#'
#' @examples
#' data(emotions_set)
#' emo.faces = multiple.netfacs(data = emotions_set[[1]],
#'  condition = emotions_set[[2]]$emotion,
#'  duration = NULL, 
#'  ran.trials = 100,
#'  control = NULL,
#'  random.level = NULL,
#'  combination.size = 5)
#'  
#'  overlap.network(emo.faces)


overlap.network <- function(netfacs.list, min.prob = 0, min.count = 5, significance = 0.01, specificity = 0.1, ignore.element = NULL){
  require(igraph)
  require(ggplot2)
  require(ggnet)
  require(scales)
  if(is.null(names(netfacs.list))){names(netfacs.list)=1:length(netfacs.list)}
  net.data = lapply(netfacs.list, function(x){x$result[x$result$combination.size == 1 & 
                                                         x$result$pvalue <= significance &
                                                         x$result$z > 0 &
                                                         x$result$count >= min.count &
                                                         !(x$result$combination %in% ignore.element) &
                                                         x$result$observed.probability >= min.prob
                                                       ,]})
  multi.net = lapply(1:length(net.data), function(x){
    xx = data.frame(condition = names(net.data)[x], element = net.data[[x]]$combination, observed.probability = net.data[[x]]$observed.probability, specificity = net.data[[x]]$specificity)
    return(xx)
  })
  
  multi.net = do.call(rbind, multi.net)
  
  condition.element = multi.net
  element.condition = multi.net
  condition.element$observed.probability = NULL
  element.condition$specificity = NULL

  colnames(condition.element) = c('A', 'B', 'probability')
  colnames(element.condition) = c('B', 'A', 'probability')
  condition.element$type = 'Context Specificity (P[Context|Element])'
  element.condition$type = 'Occurrence Probability (P[Element|Context])'

  net.graph = graph_from_data_frame(rbind(condition.element, element.condition), directed = T, vertices = NULL)
  V(net.graph)$type <- bipartite_mapping(net.graph)$type
  node.color = rep(2, length(vertex.attributes(net.graph)$name))
  node.color[vertex.attributes(net.graph)$name%in%multi.net[,1]] = 3
  node.label = vertex.attributes(net.graph)$name
  node.size = rep(4, length(vertex.attributes(net.graph)$name))
  node.size[vertex.attributes(net.graph)$name%in%multi.net[,1]] = 8
  edge.size = edge.attributes(net.graph)$probability * 5
  V(net.graph)$size = node.size*8
  V(net.graph)$color <- ifelse(V(net.graph)$type, "lightblue", "salmon")
  V(net.graph)$shape <- ifelse(V(net.graph)$type, "circle", "square")
  
  p <- ggraph(net.graph, layout = 'igraph', algorithm = 'kk') + 
    geom_edge_fan(aes(colour = type), show.legend = F) +
    geom_node_text(mapping = aes(color = color, label = name, size = 50, fontface = 'bold'), show.legend = F) +
    scale_edge_alpha(guide = 'none') +
    facet_edges(~type) + 
    theme_graph() +
    geom_edge_link(mapping = aes(
      label = round(probability, 2)), label_size = 3,
      arrow = arrow(type = "closed", angle = 15, length = unit(2,'mm')), 
      end_cap = circle(2, 'mm'), 
      start_cap = circle(2, 'mm'), 
      colour="grey",
      label_dodge  = unit(3, "mm"),
      angle_calc = "along", show.legend = F)
  
  
  multi.net.short = multi.net[multi.net$observed.probability>0.5 & multi.net$specificity>specificity,]
  net.graph.short = graph_from_data_frame(multi.net.short, directed = F, vertices = NULL)
  V(net.graph.short)$type <- bipartite_mapping(net.graph.short)$type
  node.color = rep(2, length(vertex.attributes(net.graph.short)$name))
  node.color[vertex.attributes(net.graph.short)$name%in%multi.net.short[,1]] = 3
  node.label = vertex.attributes(net.graph.short)$name
  node.size = rep(4, length(vertex.attributes(net.graph.short)$name))
  node.size[vertex.attributes(net.graph.short)$name%in%multi.net.short[,1]] = 8
  edge.size = edge.attributes(net.graph.short)$probability * 5
  V(net.graph.short)$size = node.size*8
  V(net.graph.short)$color <- ifelse(V(net.graph.short)$type, "lightblue", "salmon")
  V(net.graph.short)$shape <- ifelse(V(net.graph.short)$type, "circle", "square")
  
  q <- ggraph(net.graph.short, layout = 'igraph', algorithm = 'kk') + 
    geom_node_text(mapping = aes(color = color, label = name, size = 50, fontface = 'bold'), show.legend = F) +
    scale_edge_alpha(guide = 'none') +
    theme_graph() +
    geom_edge_link(
      arrow = NULL, 
      end_cap = circle(2, 'mm'), 
      start_cap = circle(2, 'mm'), 
      colour="grey",
      label_dodge  = unit(3, "mm"),
      angle_calc = "along", show.legend = F)
  
  
  return(list(graph = p, data = multi.net, reduced.graph = q))
}
