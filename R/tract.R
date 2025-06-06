get_state_fips_from_name = function(state_name)
{
  # Within choroplethr, states are identified by the region column of the state.regions object.
  # (I.e. lower-case, full name). However, when working with tigris, we use the numeric fips code
  data(state.regions, package="choroplethrMaps", envir=environment())
  stopifnot(state_name %in% state.regions$region)
  
  state.regions[state.regions$region == state_name, "fips.numeric"]
}

#' Get a map of tracts in a state, as a data.frame
#' 
#' The map returned is exactly the same map which tract_choropleth uses. It is downloaded
#' using the "tracts" function in the tigris package, and then it is modified for use with 
#' choroplethr.
#' @param state_name The name of the state. See ?state.regions for proper spelling and capitalization.
#' @export
#' @importFrom tigris tracts
#' @importFrom dplyr inner_join
get_tract_map = function(state_name) 
{
  state_fips = get_state_fips_from_name(state_name)
  
  # tigris returns the map as a Simple Features data frame, 
  tract.map.df = tracts(state = state_fips, cb = TRUE)
  
  # and choroplethr requires a "region" column
  # calling as.numeric is the easiest way to handle leading 0's
  tract.map.df$region = as.numeric(tract.map.df$GEOID)
  
  # choroplethr also wants a list of counties as unique numeric fips codes
  tract.map.df$county.fips.numeric = paste0(tract.map.df$STATEFP, tract.map.df$COUNTYFP)
  tract.map.df$county.fips.numeric = as.numeric(tract.map.df$county.fips.numeric)
  
  tract.map.df
}

#' An R6 object for creating choropleths of Census Tracts.
#' @importFrom tigris tracts
#' @export
TractChoropleth = R6Class("TractChoropleth",
  inherit = choroplethr::Choropleth,
  public = list(
    
    # initialize with the proper shapefile
    initialize = function(state_name, user.df)
    {
      tract.map = get_tract_map(state_name)

      super$initialize(tract.map, user.df)
      
      if (private$has_invalid_regions)
      {
        warning("Your dataframe contains unmappable regions")
      }
    },
    
    # All zooms, at the end of the day, are tract zooms. But often times it is more natural
    # for users to specify the zoom by county 
    # This function name is a bit of a hack - it seems like I cannot override the parent set_zoom directly
    # because this function has a different number of parameters than that function, and the extra parameters
    # seeming just disappear
    set_zoom_tract = function(county_zoom, tract_zoom)
    {
      # user can zoom by at most one of these options
      num_zooms_selected = sum(!is.null(c(county_zoom, tract_zoom)))
      if (num_zooms_selected > 1) {
        stop("You can only zoom in by one of county_zoom or tract_zoom")
      }

      # if the zip_zoom field is selected, just do default behavior
      if (!is.null(tract_zoom)) {
        super$set_zoom(tract_zoom)
        # if county_zoom field is selected, extract zips from counties  
      } else if (!is.null(county_zoom)) {
        stopifnot(all(county_zoom %in% unique(self$map.df$county.fips.numeric)))

        # wow, this line below *literally* does not return a vector of regions.
        # the class of self$map.df is a tuple: " "sf"         "data.frame"". I can only
        # guess that sf somehow doesn't allow you to strip the geometry column out this way
        tracts = self$map.df[self$map.df$county.fips.numeric %in% county_zoom, "region"]
        
        # this line fixes the issue above. We just want a vector of regions here
        tracts = tracts$region
        
        super$set_zoom(tracts)        
      }
    }
    
  )
)

#' Create a choropleth of Census Tracts in a particular state.
#' 
#' @param df A data.frame with a column named "region" and a column named "value".  
#' @param state_name The name of the state. See ?state.regions for proper spelling and capitalization.
#' @param title An optional title for the map.
#' @param legend An optional name for the legend.  
#' @param num_colors The number of colors to use on the map.  A value of 0 uses 
#' a divergent scale (useful for visualizing negative and positive numbers), A 
#' value of 1 uses a continuous scale (useful for visualizing outliers), and a 
#' value in [2, 9] will use that many quantiles. 
#' @param tract_zoom An optional vector of tracts to zoom in on. Elements of this vector must exactly 
#' match the names of tracts as they appear in the "region" column of the object returned from "get_tract_map".
#' @param county_zoom An optional vector of county FIPS codes to zoom in on. Elements of this 
#' vector must exactly match the names of counties as they appear in the "county.fips.numeric" column 
#' of the object returned from "get_tract_map".
#' @param reference_map If true, render the choropleth over a reference map from Google Maps.
#'
#' @seealso \url{https://www.census.gov/data/academy/data-gems/2018/tract.html} for more information on Census Tracts
#' @export
#' @importFrom Hmisc cut2
#' @importFrom stringr str_extract_all
#' @importFrom ggplot2 ggplot aes geom_polygon scale_fill_brewer ggtitle theme theme_grey element_blank geom_text
#' @importFrom ggplot2 scale_fill_continuous scale_colour_brewer  
tract_choropleth = function(df, 
                            state_name,
                            title         = "", 
                            legend        = "", 
                            num_colors    = 7, 
                            tract_zoom    = NULL,
                            county_zoom   = NULL,
                            reference_map = FALSE)
{
  c = TractChoropleth$new(state_name, df)
  c$title  = title
  c$legend = legend
  c$set_num_colors(num_colors)
  c$set_zoom_tract(tract_zoom = tract_zoom, county_zoom = county_zoom)
  if (reference_map) {
    c$render_with_reference_map()
  } else {
    c$render()
  }
}