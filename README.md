# SCIDBHELPER

This contains 2 functions which can be accessed within the scidbhelper module as given below.
Build Literal : formulate_build_literal_query
Base Selection : formulate_base_selection_query

## Build Literal
```R
#The function can be called using the module name
scidbhelper::formulate_build_literal_query(vector_obj)
#vector_obj corresponds to the vector object which should be converted into the query.
```

## Base Selection
```R
#The function can be called using the module name
scidbhelper::formulate_base_selection_query(idname, id)
#idname corresponds to the attribute element referenced within the object array
```
