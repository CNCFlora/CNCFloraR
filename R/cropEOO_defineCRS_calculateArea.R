cropEOO_defineCRS_calculateArea <- function(EOO){

  EOO_18N <<- crop(EOO, extent(-78, -72, 0, 90))
  if(is.null(EOO_18N) == T){}else{

    crs(EOO_18N) <<- "+proj=utm +zone=18 +north +datum=WGS84"
    area_EOO_18N <<- raster::area(EOO_18N)
    area_km2_EOO_18N <<- set_units(area_EOO_18N * 10000, km^2)

  }

  EOO_18S <<- crop(EOO, extent(-78, -72, -90, 0))
  if(is.null(EOO_18S) == T){}else{

    crs(EOO_18S) <<- "+proj=utm +zone=18 +south +datum=WGS84"
    area_EOO_18S <<- raster::area(EOO_18S)
    area_km2_EOO_18S <<- set_units(area_EOO_18S * 10000, km^2)

  }

  EOO_19N <<- crop(EOO, extent(-72, -66, 0, 90))
  if(is.null(EOO_19N) == T){}else{

    crs(EOO_19N) <<- "+proj=utm +zone=19 +north +datum=WGS84"
    area_EOO_19N <<- raster::area(EOO_19N)
    area_km2_EOO_19N <<- set_units(area_EOO_19N * 10000, km^2)

  }

  EOO_19S <<- crop(EOO, extent(-72, -66, -90, 0))
  if(is.null(EOO_19S) == T){}else{

    crs(EOO_19S) <<- "+proj=utm +zone=19 +south +datum=WGS84"
    area_EOO_19S <<- raster::area(EOO_19S)
    area_km2_EOO_19S <<- set_units(area_EOO_19S * 10000, km^2)

  }

  EOO_20N <<- crop(EOO, extent(-66, -60, 0, 90))
  if(is.null(EOO_20N) == T){}else{

    crs(EOO_20N) <<- "+proj=utm +zone=20 +north +datum=WGS84"
    area_EOO_20N <<- raster::area(EOO_20N)
    area_km2_EOO_20N <<- set_units(area_EOO_20N * 10000, km^2)

  }

  EOO_20S <<- crop(EOO, extent(-66, -60, -90, 0))
  if(is.null(EOO_20S) == T){}else{

    crs(EOO_20S) <<- "+proj=utm +zone=20 +south +datum=WGS84"
    area_EOO_20S <<- raster::area(EOO_20S)
    area_km2_EOO_20S <<- set_units(area_EOO_20S * 10000, km^2)

  }

  EOO_21N <<- crop(EOO, extent(-60, -54, 0, 90))
  if(is.null(EOO_21N) == T){}else{

    crs(EOO_21N) <<- "+proj=utm +zone=21 +north +datum=WGS84"
    area_EOO_21N <<- raster::area(EOO_21N)
    area_km2_EOO_21N <<- set_units(area_EOO_21N * 10000, km^2)

  }

  EOO_21S <<- crop(EOO, extent(-60, -54, -90, 0))
  if(is.null(EOO_21S) == T){}else{

    crs(EOO_21S) <<- "+proj=utm +zone=21 +south +datum=WGS84"
    area_EOO_21S <<- raster::area(EOO_21S)
    area_km2_EOO_21S <<- set_units(area_EOO_21S * 10000, km^2)

  }

  EOO_22N <<- crop(EOO, extent(-54, -48, 0, 90))
  if(is.null(EOO_22N) == T){}else{

    crs(EOO_22N) <<- "+proj=utm +zone=22 +north +datum=WGS84"
    area_EOO_22N <<- raster::area(EOO_22N)
    area_km2_EOO_22N <<- set_units(area_EOO_22N * 10000, km^2)

  }

  EOO_22S <<- crop(EOO, extent(-54, -48, -90, 0))
  if(is.null(EOO_22S) == T){}else{

    crs(EOO_22S) <<- "+proj=utm +zone=22 +south +datum=WGS84"
    area_EOO_22S <<- raster::area(EOO_22S)
    area_km2_EOO_22S <<- set_units(area_EOO_22S * 10000, km^2)

  }

  EOO_23N <<- crop(EOO, extent(-48, -42, 0, 90))
  if(is.null(EOO_23N) == T){}else{

    crs(EOO_23N) <<- "+proj=utm +zone=23 +north +datum=WGS84"
    area_EOO_23N <<- raster::area(EOO_23N)
    area_km2_EOO_23N <<- set_units(area_EOO_23N * 10000, km^2)

  }

  EOO_23S <<- crop(EOO, extent(-48, -42, -90, 90))
  if(is.null(EOO_23S) == T){}else{

    crs(EOO_23S) <<- "+proj=utm +zone=23 +south +datum=WGS84"
    area_EOO_23S <<- raster::area(EOO_23S)
    area_km2_EOO_23S <<- set_units(area_EOO_23S * 10000, km^2)

  }

  EOO_24N <<- crop(EOO, extent(-42, -36, 0, 90))
  if(is.null(EOO_24N) == T){}else{

    crs(EOO_24N) <<- "+proj=utm +zone=24 +north +datum=WGS84"
    area_EOO_24N <<- raster::area(EOO_24N)
    area_km2_EOO_24N <<- set_units(area_EOO_24N * 10000, km^2)

  }

  EOO_24S <<- crop(EOO, extent(-42, -36, -90, 0))
  if(is.null(EOO_24S) == T){}else{

    crs(EOO_24S) <<- "+proj=utm +zone=24 +south +datum=WGS84"
    area_EOO_24S <<- raster::area(EOO_24S)
    area_km2_EOO_24S <<- set_units(area_EOO_24S * 10000, km^2)

  }

  EOO_25N <<- crop(EOO, extent(-36, -30, 0, 90))
  if(is.null(EOO_25N) == T){}else{

    crs(EOO_25N) <<- "+proj=utm +zone=25 +north +datum=WGS84"
    area_EOO_25N <<- raster::area(EOO_25N)
    area_km2_EOO_25N <<- set_units(area_EOO_25N * 10000, km^2)

  }

  EOO_25S <<- crop(EOO, extent(-36, -30, -90, 0))
  if(is.null(EOO_25S) == T){}else{

    crs(EOO_25S) <<- "+proj=utm +zone=25 +south +datum=WGS84"
    area_EOO_25S <<- raster::area(EOO_25S)
    area_km2_EOO_25S <<- set_units(area_EOO_25S * 10000, km^2)

  }

  area_EOO_km2 <- sum(

    if(exists("area_km2_EOO_18N")){area_km2_EOO_18N},
    if(exists("area_km2_EOO_18S")){area_km2_EOO_18S},
    if(exists("area_km2_EOO_19N")){area_km2_EOO_19N},
    if(exists("area_km2_EOO_19S")){area_km2_EOO_19S},
    if(exists("area_km2_EOO_20N")){area_km2_EOO_20N},
    if(exists("area_km2_EOO_20S")){area_km2_EOO_20S},
    if(exists("area_km2_EOO_21N")){area_km2_EOO_21N},
    if(exists("area_km2_EOO_21S")){area_km2_EOO_21S},
    if(exists("area_km2_EOO_22N")){area_km2_EOO_22N},
    if(exists("area_km2_EOO_22S")){area_km2_EOO_22S},
    if(exists("area_km2_EOO_23N")){area_km2_EOO_23N},
    if(exists("area_km2_EOO_23S")){area_km2_EOO_23S},
    if(exists("area_km2_EOO_24N")){area_km2_EOO_24N},
    if(exists("area_km2_EOO_24S")){area_km2_EOO_24S},
    if(exists("area_km2_EOO_25N")){area_km2_EOO_25N},
    if(exists("area_km2_EOO_25S")){area_km2_EOO_25S}

  )

  return(area_EOO_km2)

}
