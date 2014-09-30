## plot forecasts on a map
## Nicholas Reich
## September 2014

#'@param forecast_data forecast file dataframe
#'@param cdata cntry.data object with spatial polygons needed for plotting
#'@param biweek biweek to plot
#'@param plot_type one of either "incidence" or "outbreak"

plot_forecast_map <- function(forecast_data, cdata, biweek, plot_type=c("incidence", "outbreak")) {
        require(ggplot2)
        
        data(thai_prov_data)
        
        if(!(biweek %in% unique(forecasts$biweek)))
                stop("biweek must be in forecast_data.")
        
        ## merge thai_prov_data with forecasts to get population
        forecast_data$FIPS <- forecast_data$pid
        forecast_data_merged <- left_join(forecast_data, thai_prov_data)
        forecast_data_merged <- mutate(forecast_data_merged,
                                       incidence = predicted_count/Population)
                
        ## retrieve location info
        thai.locs <- fortify(cdata@loc.info)
        loc.data <- cdata@loc.info@data
        
        ## match thai.locs to a FIPS
        ## ASSUMES THAT loc.data HAS "ID_1" AND "FIPS_ADMIN" COLUMNS
        ## required to have "region" as this column name!
        thai.locs$region <- loc.data[match(thai.locs$id, loc.data$ID_1), "FIPS_ADMIN"]
        
        ## plotting choices based on type
        if(plot_type=="incidence") {
                fill_var <- "log10(incidence)"
                plot_lims <- range(forecast_data_merged$incidence)
                plot_lims <- c(floor(log10(plot_lims)[1]),
                               ceiling(log10(plot_lims)[2]))
                plot_breaks <- seq(plot_lims[1], plot_lims[2])
                plot_midpoint <- mean(plot_lims)
                legend_title <- "incidence"
                plot_labels <- paste0("1e", plot_breaks)
                
        } else {
                fill_var <- "outbreak_prob"
                plot_lims <- c(0,1)
                plot_midpoint <- .5
                plot_breaks <- seq(0, 1, by=.2)
                plot_labels <- plot_breaks
                legend_title <- "outbreak probability"
        }
        
        sp_map <- ggplot(subset(forecast_data_merged, biweek=biweek), 
               aes(map_id=pid)) + 
                geom_map(aes_string(fill=fill_var), map=thai.locs) + 
                expand_limits(x = thai.locs$long, y = thai.locs$lat) +
                scale_fill_gradient2(low = "palegoldenrod", mid="orange", high = "red", 
                                     name=legend_title,
                                     limits=plot_lims, 
                                     midpoint=plot_midpoint, 
                                     breaks=plot_breaks,
                                     labels=plot_labels) +
                #scale_fill_gradient2(low = "palegoldenrod", mid="orange", high = "red", 
                #                     limits=c(0, 1), midpoint=.5, name="outbreak probability") +
                theme_bw() +
                theme(axis.ticks = element_blank(), axis.text = element_blank(),
                      panel.background = element_rect(fill = "transparent",colour = NA)) + # or element_blank()
                xlab("") + ylab("")        
        print(sp_map)
        
}