## plot forecasts on a map
## Nicholas Reich
## September 2014

#'@param forecast_data forecast file dataframe
#'@param cdata cntry.data object with spatial polygons needed for plotting
#'@param biweek biweek to plot

plot_forecast_map <- function(forecast_data, cdata, biweek) {
        require(ggplot2)
        
        data(thai_prov_data)
        
        if(!(biweek %in% unique(forecasts$biweek)))
                stop("biweek must be in forecast_data.")
        
        ## merge thai_prov_data with forecasts to get population
        forecast_data$FIPS <- forecast_data$pid
        forecast_data_merged <- left_join(forecast_data, thai_prov_data)
        forecast_data_merged <- mutate(forecast_data_merged,
                                       incidence = predicted_count/Population)
        
        plot_lims <- range(forecast_data_merged$incidence)
        plot_lims_logscale <- c(floor(log10(plot_lims)[1]),
                                ceiling(log10(plot_lims)[2]))
        plot_breaks <- seq(plot_lims_logscale[1], plot_lims_logscale[2])
        
        ## retrieve location info
        thai.locs <- fortify(cdata@loc.info)
        loc.data <- cdata@loc.info@data
        
        ## match thai.locs to a FIPS
        ## ASSUMES THAT loc.data HAS "ID_1" AND "FIPS_ADMIN" COLUMNS
        ## required to have "region" as this column name!
        thai.locs$region <- loc.data[match(thai.locs$id, loc.data$ID_1), "FIPS_ADMIN"]
        
        png("test.png")
        sp_map <- ggplot(subset(forecast_data_merged, biweek=biweek), 
               aes(map_id=pid)) + 
                geom_map(aes(fill=log10(incidence)), map=thai.locs) + 
                expand_limits(x = thai.locs$long, y = thai.locs$lat) +
                scale_fill_gradient2(low = "palegoldenrod", mid="orange", high = "red", name="incidence",
                                     limits=plot_lims_logscale, midpoint=mean(plot_lims_logscale), 
                                     breaks=plot_breaks,
                                     labels=paste0("1e", plot_breaks)) +
                #scale_fill_gradient2(low = "palegoldenrod", mid="orange", high = "red", 
                #                     limits=c(0, 1), midpoint=.5, name="outbreak probability") +
                theme_bw() +
                theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
                xlab("") + ylab("")        
        print(sp_map)
        dev.off()
        
}