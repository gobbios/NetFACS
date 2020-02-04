#' Compares the observed and expected information content of the dataset
#' 
#' Establishes how'ordered' the data is: values close to 0 indicate that combinations are highly repetitive and predictable, while values close to 1 indicate that combinations are equiprobable and prediction of future combinations is difficult
#' 
#' 
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns the ratio of observed entropy/ expected entropy. Expected entropy is based on randomization and represents the maximum entropy a dataset with the same properties as this one can reach. The further the ratio is from 1, the more ordered the data are.
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
#'  entropy.overall(angry.face)

  
entropy.overall <- function(netfacs.data){
  require(entropy)
  require(vegan)
  
  data = as.matrix(netfacs.data$used.data$data[netfacs.data$used.data$condition==netfacs.data$used.parameters$test.condition,])
  data_mat = unlist(lapply(1:nrow(data), function(x){
    return(paste(data[x,], collapse = ""))
  }))
  # Count the number of occurrences for each unique composition
   t = table(data_mat)
  # Calculate the information content of the observed data
   data.entropy = entropy.empirical(t, unit="log2")/log(ncol(data))
    
   ran.entropy=lapply(1:10, function(b){
     out <- vegan::permatfull(data, fixedmar = "row", shuffle = "samp", times = 1, mtype = "prab")
     ran.data=out$perm[[1]] # select outcome of shuffle
     ran.data[ran.data>1]=1
     ran.data = as.matrix(ran.data)
     ran_mat = unlist(lapply(1:nrow(ran.data), function(x){
       return(paste(ran.data[x,], collapse = ""))
     }))
     t = table(ran_mat)
     return(entropy.empirical(t, unit="log2")/log(ncol(ran.data)))
    })
    ran.entropy=mean(unlist(ran.entropy))
    entropy.list = data.entropy/ran.entropy
    return(entropy.list)
}
