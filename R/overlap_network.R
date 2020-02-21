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


overlap.network <- function(netfacs.list, min.prob = 0, min.count = 5, significance = 0.01, specificity = 0.1, ignore.element = NULL, clusters = F){
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
  element.condition = element.condition[,c('A', 'B', 'probability')]
  condition.element$type = 'Context Specificity (P[Context|Element])'
  element.condition$type = 'Occurrence Probability (P[Element|Context])'
  
  if(clusters == T){
    net.graph = graph_from_data_frame(element.condition, directed = F, vertices = NULL)
    V(net.graph)$type <- bipartite_mapping(net.graph)$type
    memb.colour = data.frame(com = cluster_fast_greedy(as.undirected(net.graph))$membership, node = V(net.graph)$name)
  }
  
  
  net.graph = graph_from_data_frame(element.condition, directed = T, vertices = NULL)
  V(net.graph)$type <- bipartite_mapping(net.graph)$type
  node.color = rep(2, length(vertex.attributes(net.graph)$name))
  node.color[vertex.attributes(net.graph)$name%in%multi.net[,1]] = 3
  node.label = vertex.attributes(net.graph)$name
  node.size = rep(4, length(vertex.attributes(net.graph)$name))
  node.size[vertex.attributes(net.graph)$name%in%multi.net[,1]] = 8
  edge.size = edge.attributes(net.graph)$probability * 5
  V(net.graph)$size = node.size*8
  V(net.graph)$color <- ifelse(V(net.graph)$type, "salmon", "lightblue")
  V(net.graph)$shape <- ifelse(V(net.graph)$type, "circle", "square")
  if(clusters == T){V(net.graph)$color = memb.colour$com[match(memb.colour$node, V(net.graph)$name)]}
  all.layout <- create_layout(net.graph, layout = 'igraph', algorithm = 'kk')
  
  p.occurrence <- ggraph(all.layout) + 
    geom_node_text(mapping = aes(color = color, label = name, size = 50, fontface = 'bold'), show.legend = F) +
    scale_edge_alpha(guide = 'none') +
    theme_graph(base_family = "sans") +
    ggtitle('Occurrence Probability P(Element|Context)') +
    geom_edge_fan(mapping = aes(
      label = round(probability, 2), colour = type), label_size = 3,
      arrow = arrow(type = "closed", angle = 15, length = unit(2,'mm')), 
      end_cap = circle(2, 'mm'), 
      start_cap = circle(2, 'mm'), 
      colour="grey",
      label_dodge  = unit(3, "mm"),
      angle_calc = "along", show.legend = F)

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
  if(clusters == T){V(net.graph)$color = memb.colour$com[match(V(net.graph)$name, memb.colour$node)]}
  both.layout <- create_layout(net.graph, layout = 'igraph', algorithm = 'kk')
  both.layout$x = all.layout$x[match(both.layout$name, all.layout$name)]
  both.layout$y = all.layout$y[match(both.layout$name, all.layout$name)]
  
  
  p.both <- ggraph(both.layout) + 
    geom_node_text(mapping = aes(color = color, label = name, size = 50, fontface = 'bold'), show.legend = F) +
    scale_edge_alpha(guide = 'none') +
    facet_edges(~type) + 
    theme_graph(base_family = "sans") +
    geom_edge_fan(mapping = aes(
      label = round(probability, 2), colour = type), label_size = 3,
      arrow = arrow(type = "closed", angle = 15, length = unit(2,'mm')), 
      end_cap = circle(2, 'mm'), 
      start_cap = circle(2, 'mm'), 
      colour="grey",
      label_dodge  = unit(3, "mm"),
      angle_calc = "along", show.legend = F)
    net.graph.both = net.graph

    
    net.graph = graph_from_data_frame(condition.element, directed = T, vertices = NULL)
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
    if(clusters == T){V(net.graph)$color = memb.colour$com[match(V(net.graph)$name, memb.colour$node)]}
    
    spec.layout <- create_layout(net.graph, layout = 'igraph', algorithm = 'kk')
    spec.layout$x = all.layout$x[match(spec.layout$name, all.layout$name)]
    spec.layout$y = all.layout$y[match(spec.layout$name, all.layout$name)]
    
    p.specificity <- ggraph(spec.layout) + 
      geom_node_text(mapping = aes(color = color, label = name, size = 50, fontface = 'bold'), show.legend = F) +
      scale_edge_alpha(guide = 'none') +
      theme_graph(base_family = "sans") +
      ggtitle('Context Specificity P(Context|Element)') +
      geom_edge_fan(mapping = aes(
        label = round(probability, 2), colour = type), label_size = 3,
        arrow = arrow(type = "closed", angle = 15, length = unit(2,'mm')), 
        end_cap = circle(2, 'mm'), 
        start_cap = circle(2, 'mm'), 
        colour="grey",
        label_dodge  = unit(3, "mm"),
        angle_calc = "along", show.legend = F)
  
    
    multi.net.short = multi.net[multi.net$specificity>specificity,]
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
    V(net.graph.short)$shape <- ifelse(V(net.graph.short)$type, "bold", "italic")
    if(clusters == T){V(net.graph.short)$color = memb.colour$com[match(V(net.graph.short)$name, memb.colour$node)]}
    
    red.layout <- create_layout(net.graph.short, layout = 'igraph', algorithm = 'kk')
    red.layout$x = all.layout$x[match(red.layout$name, all.layout$name)]
    red.layout$y = all.layout$y[match(red.layout$name, all.layout$name)]
    
    p.reduced <- ggraph(red.layout) + 
      geom_node_text(mapping = aes(color = color, label = name, size = 50, fontface = shape), show.legend = F) +
      scale_edge_alpha(guide = 'none') +
      theme_graph(base_family = "sans") +
      ggtitle('Edges with high specificity and occurrence') +
      geom_edge_fan(
        arrow = NULL, 
        end_cap = circle(2, 'mm'), 
        start_cap = circle(2, 'mm'), 
        colour="grey",
        label_dodge  = unit(3, "mm"),
        angle_calc = "along", show.legend = F)
  
  return(list(specificity = p.specificity, occurrence = p.occurrence, both = p.both, reduced = p.reduced, data = multi.net, network = net.graph.both, modularity = modularity(cluster_fast_greedy(as.undirected(net.graph.both), weights = NULL))))
}
