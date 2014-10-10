## make province prediction graph
## Nicholas Reich
## September 2014

#'@param forecasts object with the forecasts
get_forecast_prov_data <- function(forecasts) {
        require(dplyr)
        require(lubridate)
        require(cruftery)
        data(thai_prov_data)
        forecasts_prov <- forecasts %>% 
                mutate(time = year + (biweek-1)/26,
                       date_sick = biweek_to_date(biweek, year),
                       FIPS = pid)
        forecasts_prov <- left_join(forecasts_prov, thai_prov_data)
        return(forecasts_prov)
}


#'@param forecasts_prov object with the forecasts by province, output from get_forecast_prov_data
#'@param counts object with count data
#'@param min_plot_date the earliest date desired in the plots
get_count_prov_data <- function(counts, forecasts_prov, min_plot_date=as.Date('2012-04-01')) {
        require(dplyr)
        require(lubridate)
        require(cruftery)
        
        data(thai_prov_data)
        
        ## get forecast times to categorize counts
        forecast_times <- (forecasts_prov$year + (forecasts_prov$biweek-1)/26)
        
        ## set counts
        counts_prov <- counts %>% 
                group_by(date_sick_year, date_sick_biweek, province) %>%
                summarize(prov_count = sum(count)) %>%
                mutate(time = date_sick_year + (date_sick_biweek-1)/26,
                       date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
                       forecast_biweek = time %in% forecast_times) %>%
                filter(date_sick >= min_plot_date)
        
        ## ready the province data
        thai_prov_data <- mutate(thai_prov_data, 
                                 province=ISO,
                                 pid=FIPS,
                                 pname=reorder(Province, Population, FUN=mean))
        counts_prov <- left_join(counts_prov, thai_prov_data)
        
        return(counts_prov)
}

#'@param forecasts_prov object containing processed forecasts
#'@param counts_prov object containing aggregated counts
#'@param region MOPH region to plot
#'@param show_unused_cases if true, shows the counts unused in the forecasts

make_province_prediction_line_graph <- function(forecasts_prov, 
                                                counts_prov, 
                                                region=1,
                                                show_unused_counts=TRUE) {
        require(dplyr)
        require(lubridate)
        require(cruftery)
        require(scales)
        
        ## subset based on region
        forecasts_prov <- filter(forecasts_prov, MOPH_Admin_Code == region)
        counts_prov <- filter(counts_prov, MOPH_Admin_Code == region)
        
        ## make plot
        plot_title <- ifelse(region==0,
                             paste("Observed and predicted DHF case counts for Bangkok"),
                             paste("Observed and predicted DHF case counts for MOPH Region", region))
        
        ## 
        
        p <- ggplot() + theme_bw() + 
                theme(legend.position="bottom", #legend.justification=c(1,1),
                      axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5),
                      panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
                      panel.grid.major =  element_blank(),
                      panel.grid.minor =  element_blank(),
                      plot.background = element_rect(fill = "transparent",colour = NA)) +
                ## add forecasts
                geom_line(data=forecasts_prov, aes(x=date_sick, y=predicted_count)) +
                geom_point(data=forecasts_prov, aes(x=date_sick, y=predicted_count)) +
                geom_ribbon(data=forecasts_prov, aes(x=date_sick, 
                                                     ymin=lb, ymax=ub), 
                            alpha=I(.3)) +
                facet_grid(pname~.) +
                # air-brushing
                scale_x_date(breaks = "3 months",
                             labels = date_format("%d %b %Y"))+
                xlab(NULL) + ylab(NULL) + #ylim(0, 1000) +
                ggtitle(plot_title)
        if(show_unused_counts){
                ## using gray bars for unused cases
                p <- p + geom_bar(data=counts_prov, 
                                 aes(x=date_sick, y=prov_count, fill=forecast_biweek), 
                                 stat="identity") + 
                        scale_fill_manual(values=c("black", "gray"),
                                          name="",
                                          labels=c("used by forecast model", "not used by forecast model"))
        } else {
                ## no unused cases
                p <- p + geom_bar(data=filter(counts_prov, forecast_biweek==FALSE), 
                                 aes(x=date_sick, y=prov_count), 
                                 stat="identity")
        }
        p
                        
}