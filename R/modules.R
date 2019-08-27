#'Code to build a query for sciDB call from the given vec input.

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

#'takes idname as input , idname received from get_base_idname under revealgenomics by passing fullarrayname as input
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

#equi-join query by taking parameters of both queries and field over which to join them.
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


