# Script for accessing historic NWM snow data
# All credit to https://mikejohnson51.github.io/

# Load packages
library(AOI); library(nwmTools); library(climateR)

# Set the area of interest
#AOI = AOI::aoi_get(state = "conus")
AOI = AOI::aoi_get(state = "VT")

# Get the file URLs
#fileList = get_aws_urls(date = "2020-01-01", num = 1, output = "LDASOUT_DOMAIN1") 
fileList = get_aws_urls(date = "2019-12-15", num = 120 * 8, output = "LDASOUT_DOMAIN1") 
#fileList = get_aws_urls(date = "2019-03-01", num = 10 * 8, output = "LDASOUT_DOMAIN1") 

# Subset to one file per day
fileList = fileList[seq(1, nrow(fileList), 8), ]

# Get the data
start_time = Sys.time()
data = get_gridded_data(fileList, AOI = AOI, varname = "SNEQV")
end_time = Sys.time()
end_time - start_time

# Reproject the AOI to match data
crs_data = terra::crs(data$SNEQV)
AOI_reproj = sf::st_transform(AOI, crs_data)

# Make an animation
#climateR::animation(data$SNEQV, AOI = AOI, outfile = "~/Downloads/snow.gif")

# Make a better animation

# Add scaling factor for data
mysteryScaleFactor = 10 # maybe

# Add a couple tweaks to the original animation function
animation_raster = function(data, AOI = NULL, outfile, colors = blues9){
  
  y = terra::minmax(data, compute = TRUE)
  
  y = y[!is.na(y)]
  
  brk <- seq(min(y/mysteryScaleFactor), max(y/mysteryScaleFactor), (max(y/mysteryScaleFactor) - min(y/mysteryScaleFactor)) / length(colors))
  n = names(data)
  
  gifski::save_gif({
    for (t in 1:terra::nlyr(data)) {
      try({
        terra::plot(
          data[[t]]/mysteryScaleFactor,
          breaks = brk,
          col = colors ,
          legend = T,
          axes = F,
          box = F,
          main = as.character(fileList[t, "dateTime"]),
          plg = list(title = "SWE (mm)")
        )
        if(!is.null(AOI)){
          plot(spatAOI(AOI), col = NULL, add = TRUE)
        }
        
      }, silent = F)
    }
  }, gif_file = outfile, width = 800, height = 600, delay = .25, loop = TRUE)
  
  return(outfile)
}

# Define this function from utils
spatAOI = function(AOI){
  if(inherits(AOI, c("sf", "sfc", "sfg"))){ 
    terra::vect(AOI)
  } else { 
    AOI
  }
}

# Output the snow data
animation_raster(data$SNEQV, AOI = AOI_reproj, outfile = "~/Downloads/snow_withoutline.gif")
