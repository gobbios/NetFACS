#' Combine columns into one
#' 
#' Take frame-by-frame data and combine information of two columns, e.g. because they represent the same signal or because they overlap so much that they should be treated as one
#'
#' @param data dataframe, containing one row per frame, one column per communication unit
#' @param output character vector with combinations of columns that should be turned into one column, separated by underscore. Should have the form 'AU1_AU2' 
#'
#' @return Function returns the dataframe enter as 'data', but with specified columns aggregated and the original columns removed
#' @export
#'
#' @examples
#' data(netfacstestdata)
#' head(netfacstestdata)
#' net.set = combine.columns(data = netfacstestdata, output = '25_26')
#' head(net.set)
#'  


combine.columns <- function(data, output){

  # create as loop in case the user tries to combine several columns
  for(i in 1:length(output)){
    data=data.frame(data, check.names = F) #turn data into a data frame for easier manipulation
    xx=unlist(strsplit(output[i], split = '_')) # find the two columns
    data$x=as.vector(data[,xx[1]]) # create a new column that includes the information of both original columns
    data$x[data[,xx[2]]==1]=1
    colnames(data)[ncol(data)]=output[i] # rename the new variable
    data[,xx[1]]=NULL # remove original columns
    data[,xx[2]]=NULL
    data = as.matrix(data)
  }
  return(data)
}
