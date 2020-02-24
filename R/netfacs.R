#' Create probability distribution of combinations of elements in the data
#' 
#' Take dataset and report observed and expected likelihood that elements and combinations of elements occur in this dataset, and whether this differs from a null condition
#' Expected values are based on bootstraps of null distribution, so the values represent distribution of element co-occurrence under null condition; or permutations of the observed distribution to test it against 'random'.
#' The resulting object is the basis for most other functions in this pacakge.
#'
#' @param data matrix with one column per element, and one row per event, consisting of 1 (element was active during that event) and 0 (element was not active)
#' @param condition character vector of same length as 'data' that contains information on the condition each event belongs to, so probabilities can be compare across conditions; if NULL, all events will be tested against a random null condition based on permutations
#' @param test.condition level of 'condition' that is supposed to be tested
#' @param null.condition level of 'condition' that is used to create the null distribution of values; if NULL, all levels that are not the test condition will be used
#' @param duration numeric vector that contains information on the duration of each event; if NULL, all rows have the same value
#' @param ran.trials Number of randomisations that will be performed to find the null distribution
#' @param random.level character vector of the level on which the randomization should take place. If NULL, the randomization takes place on the event level (i.e., every row can either be selected or not); if a vector is provided, the randomization takes place on the levels of that vector rather than individual events
#' @param control list of vectors that are used as control variables. During bootstraps, the ratio of events in each level will be adapted. So, for example, if in the test distribution, there are three angry participants for each happy participant, the null distribution will maintain that ratio
#' @param combination.size if not all combinations of elements are of interest (e.g., if the question only concerns single elements or dyads of elements), this variable allows to reduce the results to those combinations, increasing speed
#'
#' @return Function returns a Result dataframe that includes the combination name, how many elements it consisted of, how often it was observed, the probability it was observed under this condition, the expected probability under null condition (based on the permutation or bootstrap), the z value (standard deviations from the mean of the randomisations), p-value (how many randomisations were more extreme), and for direct comparisons of contexts the specificity (probability that the condition is in fact the test condition if that combination is known) and probability increase (the factor by which the probability of the element is higher in the test than null condition) 
#' @return Also contains 'affinity' (Simple Ratio Index for all elements based on arules package)
#' @return 'arules' object that can be used to further explore the rules underlying association of elements and combinations using the arules and arulesViz packages
#' @return 'event.size.information' contains information about the observed and expected size of combination or elements per event based on the randomisations
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
#'  head(angry.face$result, 20)
#'  angry.face$event.size.information



netfacs <- function(data, condition = NULL, test.condition = NULL, null.condition=NULL, duration = NULL, ran.trials = 1000, control = NULL, random.level = NULL, combination.size = NULL){
  # arules package is the basis for most of the probability calculations, considerably faster than most other packages and has some interesting features
  # fix column names of dataset to not include special characters or spaces
  colnames(data) = gsub(colnames(data), pattern = ' ', replacement = '', fixed = T) 
  colnames(data) = gsub(colnames(data), pattern = '+', replacement = '', fixed = T)
  colnames(data) = gsub(colnames(data), pattern = '_', replacement = '', fixed = T)
  colnames(data) = gsub(colnames(data), pattern = '.', replacement = '', fixed = T)
  
  # determine number of cores for parallelization
  cl=detectCores()-2
  if(cl<1){cl=1}
  
  # if combination.size is not determined, the maximum combination size that is considered is set to the maximum observed combination size
  if(is.null(combination.size)){combination.size = max(rowSums(data))}
  
  # data preparation happens for two different cases: either 'condition' is set, in which case the 'test.condition' is tested against all other cases or against one specific 'null.condition'; alternatively, if no condition is set, the probabilities are compared with random probabilities
  if(!is.null(condition)){
    
    # Error messages in case test.condition is wrongly specified
    if(is.null(test.condition)){return(print('Error: specify test condition'))}
    if(!test.condition%in%condition){return(print('Test condition not part of the condition vector'))}
    
    # if random.level is not defined, each event/row is its own case, and all events are compared against each other. If random.level is defined, the randomization will select cases based on which level they belong to
    if(is.null(random.level)){random.level = 1:nrow(data)}
    
    # if the null condition is not determined, all cases that are not part of the test dataset are classed as null condition
    condition.x=as.character(condition)
    if(is.null(null.condition)){
      null.condition = 'all'
      condition.x[condition.x!=test.condition] = 'all'
      }
    
    # the control argument, which is entered as a list (to allow for multiple control variables) is turned into a combination of those variables. E.g., if you control for sex and social group, then each individual or is classed as sex_group
    if(length(control)>1){
      control.obj=do.call(cbind, control)
      control.obj=apply(control.obj, 1, function(k) paste(k, collapse = '_'))
    }
    if(length(control)==1){
      control.obj=unlist(control[1])
    }
    if(is.null(control)){
      control.obj=rep(1, nrow(data))
    }
    
    # if duration is set, all variable vectors have to be multiplied accordingly
    if(is.null(duration)){min.duration = 1}
    if(!is.null(duration)){
      min.duration = min(duration)
      duration = round(duration/min.duration, 0) # duration is determined by the minimum value. So, if the shortest event is 0.05s, then an even of 1sec will be represented 20 times
      data = data.frame(data, check.names = F)
      data = lapply(1:nrow(data), function(x){
        xx = data[x,]
        xx = xx[rep(seq_len(nrow(xx)), duration[x]), ]
        return(xx)
      })
      data = do.call(rbind, data)
      
      condition.x = unlist(sapply(1:length(condition.x), function(x){
        xx = condition.x[x]
        xx = rep(xx, length.out = duration[x])
        return(xx)
      }))
      
      control.obj = unlist(sapply(1:length(control.obj), function(x){
        xx = control.obj[x]
        xx = rep(xx, length.out = duration[x])
        return(xx)
      }))
      
      random.level = unlist(sapply(1:length(random.level), function(x){
        xx = random.level[x]
        xx = rep(xx, length.out = duration[x])
        return(xx)
      }))
    }
    
    ### turn data into test data and null data
    data.test=data[condition.x==test.condition,]
    data.null=data[condition.x==null.condition,]
    
    #turn control variable into test and null control variable
    test.control = control.obj[condition.x==test.condition]
    null.control=control.obj[condition.x==null.condition]
    
    #turn random level variable into test and null control variable
    random.level.null = random.level[condition.x==null.condition]
    random.level.test = random.level[condition.x==test.condition]
    
    #create ratio for randomisation of null dataset: if the test dataset contains 12 males and 15 females (ratio of 1.25), then the selection of the null dataset should reflect this
    rl.test.ratio = table(test.control[!duplicated(random.level.test)])
    rl.test.ratio = rl.test.ratio/sum(rl.test.ratio) # ratio for test data
    
    rl.null.ratio = data.frame(subj = random.level.null[!duplicated(random.level.null)], 
                                  control = null.control[!duplicated(random.level.null)])
    rl.null.ratio$llh = as.numeric(rl.test.ratio[match(rl.null.ratio$control, names(rl.test.ratio))])
    
    
    # create probabilities for null dataset using the arules package
    elements.null = lapply(1:nrow(data.null), function(x){ # create a list for each event/row of the null dataset, containing the names of the elements that were active
      xx = colnames(data.null)[data.null[x,]==1]
    })
    rs.null = create.rule.set(elements = elements.null, maxlen = combination.size) # this function takes the elements.null list, gives it over to arules, which is used to extract the probabilities for each combination
    
    # for single units that are not active during this specific condition, add them anyways with the information that they did not show up
    single.units = do.call(rbind, lapply(colnames(data), function(x){
      yy = rs.null[1,]
      yy$observed.probability = 0
      yy$count = 0
      yy$combination.size = 1
      yy$combination = x
      return(yy)
    }))
    
    if(nrow(single.units[!single.units$combination%in%rs.null$combination,])>0){
      rs.null = rbind(single.units[!single.units$combination%in%rs.null$combination,], rs.null)
    }
    
    # create probabilities for test dataset, using the same method as above
    elements.test = lapply(1:nrow(data.test), function(x){
      xx = colnames(data.test)[data.test[x,]==1]
    })
    rs.test = create.rule.set(elements = elements.test, maxlen = combination.size)
    
    # also add the unused single units
    if(nrow(single.units[!single.units$combination%in%rs.test$combination,])>0){
      rs.test = rbind(single.units[!single.units$combination%in%rs.test$combination,], rs.test)
    }
    
    rownames(rs.test) = rs.test$combination
    
    # create an object with the observed event sizes
    event.sizes = table(sapply(elements.test, FUN = length))
    max.event.size = max(rowSums(data))
    
    # prepare parallel computing by exporting the elements that have to be present on each cluster
    myCluster <- makeCluster(cl, type = "PSOCK") # create cluster for parallelization, using all available cores minus 2
    clusterExport(myCluster, c('ran.trials', 'elements.null', 'max.event.size', 'rs.test', 'condition.x', 'create.rule.set', 'rl.null.ratio', 'combination.size', 'random.level.null'), envir=environment()) # export the relevant information to each core
    registerDoParallel(myCluster)
    
    # the following function creates the probabilities of each element combination for random selections of the null data set
    boot.values = pblapply(1:ran.trials, cl=myCluster, function(x){
      boot.ids = unique(sample(rl.null.ratio$subj, replace = T, prob = rl.null.ratio$llh)) #select which events to take data from
      elements.boot = elements.null[random.level.null%in%boot.ids] # select null dataset
      rs.boot = create.rule.set(elements = elements.boot, maxlen = combination.size) # create probabilities for random dataset
      boot.probability = rs.boot$observed.probability[match(rs.test$combination, rs.boot$combination)] # order to match real data
      boot.probability[is.na(boot.probability)]=0
      boot.count = rs.boot$count[match(rs.test$combination, rs.boot$combination)]
      boot.count[is.na(boot.count)]=0
      event.sizes.boot = table(sapply(elements.boot, FUN = length)) # create information for event sizes as well
      xx = data.frame(combination.size = 0:max.event.size, observed.probability = 0)
      xx$observed.probability[match(names(event.sizes.boot), xx$combination.size)] = event.sizes.boot/sum(event.sizes.boot, na.rm = T)
      
      return.list = list(boot.probability = boot.probability, boot.event.sizes = xx$observed.probability, boot.count = boot.count)
      return(return.list)
    })
    stopCluster(myCluster)
    
    ##### take the calculated probabilities for the randomisations and use them to return pvalues, z values, specificity, and increase
    boot.probability = lapply(boot.values, function(x){x$boot.probability})
    boot.probability = do.call(cbind, boot.probability)
    boot.count = lapply(boot.values, function(x){x$boot.count})
    boot.count = do.call(cbind, boot.count)
    
    rs.test$expected.probability = rowMeans(boot.probability) # average probability under null condition
    
    rs.test$z = as.numeric(lapply(1:nrow(boot.probability),function(z){ # create z value by taking the observed value for each element and comparing it against the mean and standard devation of the same element for the permutations
      m=mean(boot.probability[z,])
      stan = sd(boot.probability[z,])
      xx=(rs.test$observed.probability[z] - m) / stan
      return(xx)
    }))
    rs.test$z[is.infinite(rs.test$z)]=10
    
    rs.test$pvalue = as.numeric(lapply(1:nrow(boot.probability),function(z){ # p-value: two-sided, testing how many of the permutation results for that element are more extreme than the observed value
      xx=min(mean(boot.probability[z,]>=rs.test$observed.probability[z], na.rm=T), mean(boot.probability[z,]<=rs.test$observed.probability[z], na.rm=T)) # take the smaller of the two p-values (number of permutations with larger and smaller values)
      return(xx)
    }))
    
    if(!is.null(duration)){rs.test$count = rs.test$count * min.duration}
    rs.test[sapply(rs.test, is.numeric)] = lapply(rs.test[sapply(rs.test, is.numeric)], round, 3)
    
    ### for specificity, determine how often the combination occurs across test and null condition and then divide observed count in test by the sum
    xx = rs.null$count[match(rs.test$combination, rs.null$combination)]
    xx[is.na(xx)] = 0
    rs.test$specificity = (rs.test$count/(nrow(data.test) + nrow(data.null))) / ((rs.test$count + xx)/(nrow(data.test) + nrow(data.null)))
    
    rs.test$probability.increase = as.numeric(lapply(1:nrow(boot.probability),function(z){ # create probability increase by comparing the observed probability with the mean probability of the randomisation process
      m=mean(boot.probability[z,])
      xx=rs.test$observed.probability[z] / m
      return(xx)
    }))
    rs.test$probability.increase[is.infinite(rs.test$probability.increase)] = 100
    
    rs.test = rs.test[, c('combination', 'combination.size', 'count', 'observed.probability', 'expected.probability', 'z', 'pvalue', 'specificity', 'probability.increase')]
    
    
    ##### combination size information per event
    event.probability = lapply(boot.values, function(x){x$boot.event.sizes})
    event.probability = do.call(cbind, event.probability)
    
    t.event.size = data.frame(combination.size = 0:(nrow(event.probability)-1), observed.probability = 0, expected.probability = 0)
    t.event.size$observed.probability[match(names(event.sizes), t.event.size$combination.size)] = event.sizes/sum(event.sizes)
    t.event.size$expected.probability = rowMeans(event.probability)
    t.event.size$z = as.numeric(lapply(1:nrow(event.probability),function(z){ # create z value by taking the observed value for each element and comparing it against the mean and standard devation of the same element for the permutations
      m=mean(event.probability[z,])
      stan = sd(event.probability[z,])
      xx=(t.event.size$observed.probability[z] - m) / stan
      return(xx)
    }))
    t.event.size$z[is.infinite(t.event.size$z)]=10
    
    t.event.size$pvalue = as.numeric(lapply(1:nrow(event.probability),function(z){ # p-value: two-sided, testing how many of the permutation results for that element are more extreme than the observed value
      xx=min(mean(event.probability[z,]>=t.event.size$observed.probability[z], na.rm=T), mean(event.probability[z,]<=t.event.size$observed.probability[z], na.rm=T)) # take the smaller of the two p-values (number of permutations with larger and smaller values)
      return(xx)
    }))
    t.event.size[sapply(t.event.size, is.numeric)] <- lapply(t.event.size[sapply(t.event.size, is.numeric)], round, 3)
  }
  
  
  ###### the following calculations are done when there is no condition specified, meaning the observed probability across all cases is compared to a null model based on permutations maintaining the event size and element probability
  if(is.null(condition)){
    require(vegan)
    
    # same as above, account for added duration data
    if(is.null(duration)){min.duration = 1}
    if(!is.null(duration)){
      min.duration = min(duration)
      duration = round(duration/min.duration, 0)
      data = data.frame(data, check.names = F)
      data = lapply(1:nrow(data), function(x){
        xx = data[x,]
        xx = xx[rep(seq_len(nrow(xx)), duration[x]), ]
        return(xx)
      })
      data = do.call(rbind, data)
    }
    
    condition.x=rep('all', nrow(data))
    
    ### create probabilities for test dataset
    elements.test = lapply(1:nrow(data), function(x){
      xx = colnames(data)[data[x,]==1]
    })
    rs.test = create.rule.set(elements = elements.test, maxlen = combination.size)
    
    # add single elements that are not represented in the test data
    single.units = do.call(rbind, lapply(colnames(data), function(x){
      yy = rs.test[1,]
      yy$observed.probability = 0
      yy$count = 0
      yy$combination.size = 1
      yy$combination = x
      return(yy)
    }))
    
    if(nrow(single.units[!single.units$combination%in%rs.test$combination,])>0){
      rs.test = rbind(single.units[!single.units$combination%in%rs.test$combination,], rs.test)
    }
    
    # add event size information
    event.sizes = table(sapply(elements.test, FUN = length))
    max.event.size = max(rowSums(data))
    
    # prepare parallelization for randomisation process
    myCluster <- makeCluster(cl, type = "PSOCK") # create cluster for parallelization, using all available cores minus one
    clusterExport(myCluster, c('permatfull','ran.trials', 'elements.test', 'rs.test', 'condition.x', 'create.rule.set', 'combination.size', 'max.event.size', 'data'), envir=environment()) # export the relevant information to each core
    registerDoParallel(myCluster)
    
    # to create null probabilities, the dataset is split into single element randomization and combination randomization.
    boot.values = pblapply(1:ran.trials, cl = myCluster, function(x){
      # To establish the expected probabilities for the elements, the data is compared to permutations that keep the number of elements per row constant but allow for the element probability to vary
      out <- vegan::permatfull(data, fixedmar = "rows", shuffle = "samp", times = 1, mtype = "prab") # shuffles dataset while maintaining number of elements per row
      xx=out$perm[[1]] # select outcome of shuffle
      xx[xx>1]=1 
      elements.boot = lapply(1:nrow(xx), function(x){
        xx = colnames(xx)[xx[x,]==1]
      })
      
      # create probabilities for only the single element combinations
      rs.boot.1 = create.rule.set(elements = elements.boot, maxlen = 1)
      
      # for all other combinations, do permutations that keep the number of cases per row and per column the same
      out <- vegan::permatfull(data, fixedmar = "both", shuffle = "both", times = 1, mtype = "prab")
      xx=out$perm[[1]] # select outcome of shuffle
      xx[xx>1]=1 
      
      elements.boot = lapply(1:nrow(xx), function(x){
        xx = colnames(xx)[xx[x,]==1]
      })
      
      # create overall random dataset
      rs.boot.all = create.rule.set(elements = elements.boot, maxlen = combination.size)
      rs.boot.all = rs.boot.all[rs.boot.all$combination.size >1,]
      
      # combine the datasets
      rs.boot = rbind(rs.boot.1, rs.boot.all)
      
      boot.probability = rs.boot$observed.probability[match(rs.test$combination, rs.boot$combination)]
      boot.probability[is.na(boot.probability)]=0
      
      return.list = list(boot.probability = boot.probability)
      return(return.list)
    })
    stopCluster(myCluster)
    
    ##### calculate difference between expected and observed probability as z value, p value, and probability increase
    boot.probability = lapply(boot.values, function(x){x$boot.probability})
    boot.probability = do.call(cbind, boot.probability)
    rs.test$expected.probability = rowMeans(boot.probability)
    rs.test$z = as.numeric(lapply(1:nrow(boot.probability),function(z){ # create z value by taking the observed value for each element and comparing it against the mean and standard devation of the same element for the permutations
      m=mean(boot.probability[z,])
      stan = sd(boot.probability[z,])
      xx=(rs.test$observed.probability[z] - m) / stan
      return(xx)
    }))
    rs.test$z[is.infinite(rs.test$z)]=10
    
    rs.test$pvalue = as.numeric(lapply(1:nrow(boot.probability),function(z){ # p-value: two-sided, testing how many of the permutation results for that element are more extreme than the observed value
      xx=min(mean(boot.probability[z,]>=rs.test$observed.probability[z], na.rm=T), mean(boot.probability[z,]<=rs.test$observed.probability[z], na.rm=T)) # take the smaller of the two p-values (number of permutations with larger and smaller values)
      return(xx)
    }))
    
    if(!is.null(duration)){rs.test$count = rs.test$count * min(duration)}
    rs.test[sapply(rs.test, is.numeric)] <- lapply(rs.test[sapply(rs.test, is.numeric)], round, 3)
    
    rs.test$probability.increase = as.numeric(lapply(1:nrow(boot.probability),function(z){ # create probability increase by comparing the observed probability with the mean probability of the randomisation process
      m=mean(boot.probability[z,])
      xx=rs.test$observed.probability[z] / m
      return(xx)
    }))
    rs.test$probability.increase[is.infinite(rs.test$probability.increase)] = 100
    
    rs.test = rs.test[, c('combination', 'combination.size', 'count', 'observed.probability', 'expected.probability', 'z', 'pvalue', 'probability.increase')]
    
    ##### combination size information per event; shuffle so that the number of time each element appears is kept constant, but number of elements per row differs
    event.probability = lapply(1:ran.trials, function(x){
      out <- vegan::permatfull(data, fixedmar = "columns", shuffle = "samp", times = 1, mtype = "prab")
      xx=out$perm[[1]] # select outcome of shuffle
      xx[xx>1]=1 
      event.sizes.boot = table(rowSums(xx)[rowSums(xx)<=max.event.size])/nrow(xx)
      event.sizes.boot = event.sizes.boot[!is.na(event.sizes.boot)]
      cs = data.frame(combination.size = 0:max.event.size, observed.probability = 0)
      cs$observed.probability[match(names(event.sizes.boot), cs$combination.size)] = event.sizes.boot
      return(cs$observed.probability)
    })
    event.probability = do.call(cbind, event.probability)
    
    t.event.size = data.frame(combination.size = 0:(nrow(event.probability)-1), observed.probability = 0, expected.probability = 0)
    t.event.size$observed.probability[match(names(event.sizes), t.event.size$combination.size)] = event.sizes/sum(event.sizes)
    t.event.size$expected.probability = rowMeans(event.probability)
    t.event.size$z = as.numeric(lapply(1:nrow(event.probability),function(z){ # create z value by taking the observed value for each element and comparing it against the mean and standard devation of the same element for the permutations
      m=mean(event.probability[z,])
      stan = sd(event.probability[z,])
      xx=(t.event.size$observed.probability[z] - m) / stan
      return(xx)
    }))
    t.event.size$z[is.infinite(t.event.size$z)]=10
    
    t.event.size$pvalue = as.numeric(lapply(1:nrow(event.probability),function(z){ # p-value: two-sided, testing how many of the permutation results for that element are more extreme than the observed value
      xx=min(mean(event.probability[z,]>=t.event.size$observed.probability[z], na.rm=T), mean(event.probability[z,]<=t.event.size$observed.probability[z], na.rm=T)) # take the smaller of the two p-values (number of permutations with larger and smaller values)
      return(xx)
    }))
    t.event.size[sapply(t.event.size, is.numeric)] <- lapply(t.event.size[sapply(t.event.size, is.numeric)], round, 3)
  }
  
  # arules has a number of functions that can come in handy, and these are added to the resulting object simply because they can be used for all kinds of stuff later
  test.affinity = affinity(as(elements.test, 'transactions')) # this essentially calculates the dyadic Simple Ratio Index for all combinations
  # arules works with so-called 'apriori' objects. In contrast to the probabilities used here, it works with conditional probabilities of all combinations. Really useful for lots of further things
  test.rules = apriori(as(elements.test, 'transactions'), parameter = list(supp=1/nrow(data), conf=1/nrow(data), maxlen=max(rowSums(data))), control = list(verbose = F))
  
  # summaries for the object
  used.parameters = list(test.condition = test.condition, null.condition = null.condition)
  used.data = list(data = data, condition = condition, random.level = random.level, control = control, random.probability = boot.probability)
  event.size.information = list(total.event.sizes = t.event.size)
  
  rs.list = list(result = rs.test, affinity = test.affinity, arules = test.rules, used.parameters = used.parameters, used.data = used.data, event.size.information = event.size.information)
  return(rs.list)
}
