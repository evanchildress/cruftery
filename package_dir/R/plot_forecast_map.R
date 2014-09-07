## plot forecasts on a map
## Nicholas Reich
## September 2014

#'@param forecast_data forecast file dataframe
#'@param cdata cntry.data object with spatial polygons needed for plotting

plot_forecast_map <- function(forecast_data, cdata) {
        require(ggplot2)
        
        ## retrieve location info
        thai.locs <- fortify(cdata@loc.info)
        loc.data <- cdata@loc.info@data
        
        ## match thai.locs to a FIPS
        ## ASSUMES THAT loc.data HAS "ID_1" AND "FIPS_ADMIN" COLUMNS
        ## required to have "region" as this column name!
        thai.locs$region <- loc.data[match(thai.locs$id, loc.data$ID_1), "FIPS_ADMIN"]
        
        sp_map <- ggplot(subset(forecast_data, biweek=13), 
               aes(map_id=pid)) + 
                geom_map(aes(fill=predicted_count), map=thai.locs) + 
                expand_limits(x = thai.locs$long, y = thai.locs$lat) +
                #scale_fill_gradient2(low = "palegoldenrod", mid="orange", high = "red", 
                #                     limits=c(0, 1), midpoint=.5, name="outbreak probability") +
                theme_bw() +
                theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
                xlab("") + ylab("")        
        print(sp_map)
        
}