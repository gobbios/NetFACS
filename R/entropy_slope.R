#' Creates the entropy on different levels
#' 
#' Establishes the n-th order entropies for the netfacs object. 0-Level entropy is the overall possible information content if all elements were equally distributed; first-order entropy is the possible information content gives the probabilities of each element; second-order entropy is the information content given the one element of a combination is known; etc. Decreasing entropy indicates higher predictability/fewer degrees of freedom.
#' 
#' 
#' @param netfacs.data object resulting from netfacs() function
#' @param entropy.level numeric value, indicating the maximum order up to which entropy should be established
#' @param random if F, the entropy slope for the data is produced; if T, the mean entropy slope of a randomisation of the dataset is also shown for comparison
#'
#' @return Function returns dataframe with different orders and entropy level on this order. Entropy should decrease with increasing order, as fewer degrees of freedom exist (i.e., if only one element is known, predictability for the next element is low; if three elements are known, the next element is usually quite predictable). Expected random entropy is the average entropy of 10 datasets with the same number of events and keeping the number of elements per event the same as in the observed data. 
#' 
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' data(emotions_set)
#' angry.face = netfacs(data = emotions_set[[1]],
#'  condition = emotions_set[[2]]$emotion,
#'  test.condition = 'anger',
#'  null.condition = NULL,
#'  duration = NULL, 
#'  ran.trials = 100,
#'  control = NULL,
#'  random.level = NULL,
#'  combination.size = 5)
#'  
#'  entropy.slope(angry.face, entropy.level = 5)

entropy.slope <- function(netfacs.data, entropy.level = 5, random = F){
 ################################ this needs to be overhauled, right now it is based on the assumption that for example for an 8th level combination, each element could come before every other, but what it should measure is the information content at each level of combinations compared to random
  # select the used data
  data = as.matrix(netfacs.data$used.data$data)
  
  # if no condition was specified, create condition
  if(is.null(netfacs.data$used.parameters$test.condition)){
    netfacs.data$used.parameters$test.condition = 'all'
    netfacs.data$used.data$condition = rep('all', times = nrow(netfacs.data$used.data$data))
  }
  
  # select only test data
  data = data[netfacs.data$used.data$condition==netfacs.data$used.parameters$test.condition,]
  
  # create data frame with entropy and order
  xx.slope=data.frame(order=0, entropy=0)
  # zero order entropy is the entropy given random distribution of all elements
  xx=data.frame(letter=1:ncol(data), freq=1/ncol(data))
  yy=lapply(xx$freq, function(x){
    y=x*log2(x)
    return(y)
  })
  xx.slope$entropy[1]=-1*sum(unlist(yy))
  # for 0 order entropy, expected is the same as observed
  if(isTRUE(random)){xx.slope$expected.entropy = xx.slope$entropy[1]}
  
  # create function that takes the data and the level and creates entropy for that level
  nth.order.entropy <- function(data, level){
    
    elements = lapply(1:nrow(data), function(x){ # create elements based on arules package
      xx = colnames(data)[data[x,]==1]
    })
    ar = apriori(elements, parameter = list(supp=1/nrow(data), conf=1/nrow(data), maxlen=level+1), control = list(verbose = F)) # use arules to assign probabilities to all possible combinations on this level
    
    # apply entropy measure to each 'level', based on probabilities of all combinations of that level; joint entropy
    xx.s = lapply(1:level, function(f){ # for all levels below the one that is currently interesting
      rs = quiet(data.frame(inspect(ar, ruleSep = '', itemSep = '_', setStart = '', setEnd = ''))) # quietly create a dataframe from the arules object
      rs$lhs = as.character(rs$lhs)
      rs$rhs = as.character(rs$rhs)
      rs$size = unlist(lapply(rs$lhs, function(x){length(unlist(strsplit(x, split = '_'))) + 1})) # determine the combination size for each combination
      rs = rs[rs$size==f | rs$size ==f-1,] # only select those combinations of the current size and that below
      
      # create joint entropy of an antecedent (part of the combination that is 'known') and the part that can vary
      xx.f = data.frame(ante = rs$lhs) # select 'known' part of the combination
      xx.f$ante = as.character(xx.f$ante) # 'ante' is for 'antecedent'
      xx.f$condition = unlist(lapply(1:nrow(rs), function(x){ # select the known plus unknown part of the combination
        ante = unlist(strsplit(rs$lhs[x], split = '_'))
        return(paste(c(ante, rs$rhs[x]), collapse = '_'))
      }))
      xx.f$condition = as.character(xx.f$condition)
      xx.f$condition.count = rs$count # count how often this condition occurs
      xx.f$condition.size = rs$size # add size of the combination
      xx.f$ante.count = xx.f$condition.count[match(xx.f$ante, xx.f$condition)] # as count of the known part of the combination, use the information from the level-1 parts of the dataset
      xx.f$ante.count[is.na(xx.f$ante.count)]= nrow(data)
      
      xx.f = xx.f[xx.f$condition.size == f,] # remove the antecedents
      
      xx.f$freq.tot = xx.f$ante.count/sum(xx.f$ante.count) # total frequency of the specific antecedent combination occurring
      xx.f$freq.cond = xx.f$condition.count/xx.f$ante.count # frequency of the condition occurring when the antecedent is known
      xx.f$yy = xx.f$freq.tot*log2(xx.f$freq.cond) # calculate the entropy for each combination
      
      return(c(f, -1*sum(xx.f$yy, na.rm=T)))
    })
    xx.s = data.frame(do.call(rbind, xx.s))
    colnames(xx.s) = c('order', 'entropy')
    return(xx.s)
  }
  
  # do the calculation for each entropy level
  xx.s = nth.order.entropy(data = data, level = entropy.level)
  
  # if a expected entropy value is desired, here is the formular
  if(isTRUE(random)){
    expected.entropy = lapply(1:10, function(x){
      out <- vegan::permatfull(data, fixedmar = "rows", shuffle = "samp", times = 1, mtype = "prab")
      ran.data=out$perm[[1]] # select outcome of shuffle
      ran.data[ran.data>1]=1 
      xx.ran = nth.order.entropy(data = ran.data, level = entropy.level) 
      return(xx.ran$entropy)
    })
    expected.entropy = do.call(cbind, expected.entropy)
    xx.s$expected.entropy = rowMeans(expected.entropy)
  }
  
  xx.slope = rbind(xx.slope, xx.s)
  
  return(xx.slope)
}