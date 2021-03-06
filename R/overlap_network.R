#' Plots the overlap of multiple conditions as bipartite network
#' 
#' The function takes multiple netfacs objects and plots how different elements connect the conditions, based on the conditional probabilities that the element occurs in the condition and that the condition is seen when the element is present
#' 
#' @param netfacs.list list of objects resulting from netfacs() function or multiple.netfacs() function
#' @param min.prob minimum conditional probability that should be shown in the graph
#' @param min.count minimum number of times that a combination should occur before being included in the graph
#' @param significance sets the level of significance that combinations have to pass before added to the network
#' @param clusters boolean; if TRUE, the cluster_fast_greedy algorithm is used to detect underlying community structure, based on the occurrence probability network
#' @param ignore.element string vector, can be used to exclude certain elements when creating the plots
#' @param specificity for the 'reduced' graph, select only elements that surpass this context specificity value 
#'
#' @return Function returns a ggraph plot where each condition is connected to those elements that occur significantly in this condition, and each element is connected to each condition under which it occurs significantly more than expected. Creates four graphs: context specificity, occurrence, a combined graph, and a 'reduced' graph where edges are only included if they pass the 'specificity' value set by the user
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
#'  overlap.network(emo.faces, min.prob = 0.01, min.count = 3, significance = 0.01, specificity = 0.5, ignore.element = '25', clusters = T)


overlap.network <- function(netfacs.list, min.prob = 0, min.count = 5, significance = 0.01, specificity = 0.1, ignore.element = NULL, clusters = F){

  # set digits printed to 3
  options(digits = 3)
  
  # if the netfacs.list object doesn't have names for the conditions, they are set to numbers
  if(is.null(names(netfacs.list))){names(netfacs.list)=1:length(netfacs.list)}
  
  # from the different netfacs objects in the list, reduce them all to single elements that meet the criteria specified by the user
  net.data = lapply(netfacs.list, function(x){x$result[x$result$combination.size == 1 & 
                                                         x$result$pvalue <= significance & # select significance level
                                                         x$result$z > 0 & # have to be MORE likely than expected
                                                         x$result$count >= min.count & # have to occur at least this many times
                                                         !(x$result$combination %in% ignore.element) & # remove the 'ignore.element' elements
                                                         x$result$observed.probability >= min.prob # minimum probability of occurrance
                                                       ,]})
  
  # create a dataframe that connects the condition with the elements
  multi.net = lapply(1:length(net.data), function(x){
    xx = data.frame(condition = names(net.data)[x], element = net.data[[x]]$combination, observed.probability = net.data[[x]]$observed.probability, specificity = net.data[[x]]$specificity)
    return(xx)
  })
  multi.net = do.call(rbind, multi.net)
  
  # create two conditional probability objects: one for the probability that the condition is present given the element, and one the opposite
  condition.element = multi.net # creates the context specificity: the probability that the context is found in any event with the element present
  condition.element$observed.probability = NULL
  colnames(condition.element) = c('A', 'B', 'probability')
  condition.element$type = 'Context Specificity (P[Condition|Element])'
  
  element.condition = multi.net # creates the occurrence probability: the probability that the element is found in any event within this condition
  element.condition$specificity = NULL
  colnames(element.condition) = c('B', 'A', 'probability')
  element.condition = element.condition[,c('A', 'B', 'probability')]
  element.condition$type = 'Occurrence Probability (P[Element|Condition])'
  
  # if clusters should be detected, assign the color to each community
  modularity.net = NA # has to be set to NA if 'clusters' == FALSE
  if(clusters == T){
    net.graph = graph_from_data_frame(element.condition, directed = F, vertices = NULL) # create undirected unweighted network based on the occurrence rate
    V(net.graph)$type <- bipartite_mapping(net.graph)$type # assign types to bipartite network
    memb.colour = data.frame(com = cluster_fast_greedy(net.graph, weights = E(net.graph)$probability)$membership, node = V(net.graph)$name) # create dataframe with the element and its community membership
    modularity.net = modularity(cluster_fast_greedy(net.graph, weights = E(net.graph)$probability)) # determine modularity
  }
  
  
  ########### create the four graphs: occurrence probability alone, specificity alone, both combined, and the reduced graph
  
  ## occurrence probability
  
  net.graph = graph_from_data_frame(element.condition, directed = T, vertices = NULL) # create graph
  V(net.graph)$type <- bipartite_mapping(net.graph)$type # assign bipartite type as either condition or element
  V(net.graph)$color <- ifelse(V(net.graph)$type, "salmon", "lightblue") # color set if there are no clusters
  V(net.graph)$shape <- ifelse(V(net.graph)$type, "bold", "italic")
  if(clusters == T){V(net.graph)$color = memb.colour$com[match(memb.colour$node, V(net.graph)$name)]} # colors set if there are clusters
  all.layout <- create_layout(net.graph, layout = 'igraph', algorithm = 'kk') # create basic layout that all the graphs will share, so they are symmetrical
  
  # create graph for occurrence
  p.occurrence <- ggraph(all.layout) + 
    geom_node_text(mapping = aes(color = color, label = name, size = 50, fontface = shape), show.legend = F) +
    scale_edge_alpha(guide = 'none') +
    theme_graph(base_family = "sans") + # if this is removed, there is bizarrely a constant message telling us that the font does not exist
    ggtitle('Occurrence Probability P(Element|Condition)') +
    geom_edge_fan(mapping = aes( # make edges, labels, and arrows
      label = round(probability, 2), colour = type), label_size = 3,
      arrow = arrow(type = "closed", angle = 15, length = unit(2,'mm')), 
      end_cap = circle(2, 'mm'), 
      start_cap = circle(2, 'mm'), 
      colour="grey",
      label_dodge  = unit(3, "mm"),
      angle_calc = "along", show.legend = F)
  
  
  ## context specificity and occurrence probability together
  
  net.graph = graph_from_data_frame(rbind(condition.element, element.condition), directed = T, vertices = NULL)
  V(net.graph)$type <- bipartite_mapping(net.graph)$type
  V(net.graph)$color <- ifelse(V(net.graph)$type, "lightblue", "salmon")
  V(net.graph)$shape <- ifelse(V(net.graph)$type, "bold", "italic")
  if(clusters == T){V(net.graph)$color = memb.colour$com[match(V(net.graph)$name, memb.colour$node)]}
  
  # take on same layout as first graph
  both.layout <- create_layout(net.graph, layout = 'igraph', algorithm = 'kk')
  both.layout$x = all.layout$x[match(both.layout$name, all.layout$name)]
  both.layout$y = all.layout$y[match(both.layout$name, all.layout$name)]
  
  # create graph
  p.both <- ggraph(both.layout) + 
    geom_node_text(mapping = aes(color = color, label = name, size = 50, fontface = shape), show.legend = F) +
    scale_edge_alpha(guide = 'none') +
    facet_edges(~type) + # this is the commmand that splits the plot into two, one for each direction of the arrow
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

    
    ## context specificity
    
    net.graph = graph_from_data_frame(condition.element, directed = T, vertices = NULL)
    V(net.graph)$type <- bipartite_mapping(net.graph)$type
    V(net.graph)$color <- ifelse(V(net.graph)$type, "lightblue", "salmon")
    V(net.graph)$shape <- ifelse(V(net.graph)$type, "bold", "italic")
    if(clusters == T){V(net.graph)$color = memb.colour$com[match(V(net.graph)$name, memb.colour$node)]}
    
    # take on same layout as others
    spec.layout <- create_layout(net.graph, layout = 'igraph', algorithm = 'kk')
    spec.layout$x = all.layout$x[match(spec.layout$name, all.layout$name)]
    spec.layout$y = all.layout$y[match(spec.layout$name, all.layout$name)]
    
    # make graph
    p.specificity <- ggraph(spec.layout) + 
      geom_node_text(mapping = aes(color = color, label = name, size = 50, fontface = shape), show.legend = F) +
      scale_edge_alpha(guide = 'none') +
      theme_graph(base_family = "sans") +
      ggtitle('Context Specificity P(Condition|Element)') +
      geom_edge_fan(mapping = aes(
        label = round(probability, 2), colour = type), label_size = 3,
        arrow = arrow(type = "closed", angle = 15, length = unit(2,'mm')), 
        end_cap = circle(2, 'mm'), 
        start_cap = circle(2, 'mm'), 
        colour="grey",
        label_dodge  = unit(3, "mm"),
        angle_calc = "along", show.legend = F)
  
    
    ## reduced graph
    
    # for this one, we only include those element above a certain specificity value; helps to show only really important units
    
    multi.net.short = multi.net[multi.net$specificity>specificity,] # reduce dataset
    net.graph.short = graph_from_data_frame(multi.net.short, directed = F, vertices = NULL) # create network
    V(net.graph.short)$type <- bipartite_mapping(net.graph.short)$type # make bipartite
    V(net.graph.short)$color <- ifelse(V(net.graph.short)$type, "lightblue", "salmon")
    V(net.graph.short)$shape <- ifelse(V(net.graph.short)$type, "bold", "italic")
    if(clusters == T){V(net.graph.short)$color = memb.colour$com[match(V(net.graph.short)$name, memb.colour$node)]}
    
    # make same layout as others
    red.layout <- create_layout(net.graph.short, layout = 'igraph', algorithm = 'kk')
    red.layout$x = all.layout$x[match(red.layout$name, all.layout$name)]
    red.layout$y = all.layout$y[match(red.layout$name, all.layout$name)]
    
    #create graph
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
  
  return(list(specificity = p.specificity, occurrence = p.occurrence, both = p.both, reduced = p.reduced, data = multi.net, network = net.graph.both, modularity = modularity.net))
}
