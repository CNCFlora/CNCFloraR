

cond_addPolygons <- function(){

  conditionally <- function(fun){

    function(

      first_arg, ..., execute

    ){

      if(execute) return(fun(first_arg, ...))
      else return(first_arg)

    }

  }

  cond_addPolygons <- conditionally(addPolygons)

  return(cond_addPolygons)

}
