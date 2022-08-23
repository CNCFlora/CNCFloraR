

cond_row_spec <- function(){

  conditionally <- function(fun){

    function(

      first_arg, ..., execute

    ){

      if(execute) return(fun(first_arg, ...))
      else return(first_arg)

    }

  }

  cond_row_spec <- conditionally(row_spec)

  return(cond_row_spec)

}
