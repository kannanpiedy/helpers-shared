#'Build literal query
#'
#'Code to build a query for sciDB call from the given vector input.
#'formulate_build_literal_query is a function that can be used to create an input query for the
#'AFL
#'
#'@param vec a vector
#'@param value_name (optional) a name for the value to be given in the query string, by default it is \strong{value_id}
#'@param index_name (optional) a default name for the index parameter in the scidb query , by default it is \strong{idx}
#'@return  a string to use as the input query for scidb
#'@examples
#'formulate_build_literal_query(c(1,2,3,4))
#'[1] "build(<value_id:int64>[idx=0:3], '[1,2,3,4]', true)"
#'
#'formulate_build_literal_query(c(1:5,8:10))
#'[1] "build(<value_id:int64>[idx=0:7], '[1,2,3,4,5,8,9,10]', true)"

formulate_build_literal_query = function(vec, value_name = 'value_id', index_name = 'idx') {
  vec = sort(unique(vec))
  stopifnot(class(vec) == 'integer' | class(vec) == 'numeric')
  if (class(vec) == 'numeric') {
    message("Potentially converting floating points to integers")
    vec = as.integer(vec)
  }
  paste0(
    "build(<", value_name, ":int64>[", index_name, "=0:",
    length(vec)-1,
    "], '[",
    paste0(vec, collapse = ","),
    "]', true)"
  )
}



#'Base selection query
#'
#'Code to build a query from a list of id input.
#'\enumerate{
#'\item formulate_build_literal_query is a function that can be used to create an input query for the
#'AFL.
#'\item It takes idname as input.
#'\item idname is received from get_base_idname under revealgenomics by passing fullarrayname as input
#'and is the name of the attribute as mentioned in the scidb array.
#'}
#'@param idname a string denoting the type of id
#'@param id a contiguous vector of id values from the array
#'@return  a string to use as the input query for scidb to select the given id values
formulate_base_selection_query = function(idname, id){
  # this limit is based on the number of operands that SciDB can handle in an expression
  # https://paradigm4.atlassian.net/browse/SDB-5801
  THRESH_K = 397
  sorted=sort(id)
  breaks=c(0, which(diff(sorted)!=1), length(sorted))
  if (length(breaks) <= (THRESH_K + 2)) # few sets of contiguous tickers; use `cross_between`
  {
    expr_query1 = paste( sapply(seq(length(breaks)-1), function(i) {
      left = sorted[breaks[i]+1]
      right = sorted[breaks[i+1]]
      if (left == right) {
        paste0("(", idname, "=", right, ")")
      } else {
        sprintf("(%s>=%d AND %s<=%d)", idname, left,
                idname, right)
      }
    }), collapse=" OR ")
  } else {
    stop("Try fewer ids at a time")
  }
  return(expr_query1)
}




#' Equijoin query for scidb
#'
#'equi-join query by taking parameters of both queries and field over which to join them.
#'@param left_array_or_query first array or query to be joined
#'@param right_array_or_query second array or query to be joined
#'@param left_fields_to_join the fields from the first array or query on which to join
#'@param right_fields_to_join the fields from the second array or query on which to join
#'@param keep_dimensions whether to maintain the dimensions of the arrays
#'@return returns the query string for performing equijoin in scidb
#'\describe{
#'   The formulate_equi_join_query creates the equi join query to be used in scidb based on these parameters
#'}
#'
formulate_equi_join_query = function(left_array_or_query, right_array_or_query, left_fields_to_join_by, right_fields_to_join_by, keep_dimensions = TRUE) {
  paste0(
    "equi_join(",
    left_array_or_query,
    ", ", right_array_or_query,
    ", 'left_names=",  paste0(left_fields_to_join_by, collapse=","), "'",
    ", 'right_names=", paste0(right_fields_to_join_by, collapse=","), "'",
    ifelse(keep_dimensions, ", 'keep_dimensions=true')", ", 'keep_dimensions=false')")
  )
}


#'Drop Columns with NA
#'
#'\describe{
#'we've used two methods of handling it and the system handles dataframe of type data.table differently compared to others.
#'}
#'\enumerate{
#'\item for the given dataframe we filter out those columns which are not NA and return the dataframe
#'\item we choose whether to do splicing or using the Filter function based on the type of dataframe
#'}
#'@param df dataframe as an input
#'@return returns a dataframe without those columns which have na values


drop_na_columns = function(df){
  if (nrow(df) > 0) {
    if( "data.table" %in% class(df) |
        (nrow(df) == 1 & ncol(df) == 1)) {
      # Use a different method to remove NA columns if a data.table
      # http://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
      base::Filter(function(x)
        !all(is.na(x)),
        df)

    } else {
      df[,colSums(is.na(df))<nrow(df)]
    }
  } else {
    df
  }
}
