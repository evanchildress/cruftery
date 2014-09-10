## make country prediction graph
## Nicholas Reich
## September 2014

#'@param forecast_file file containing forecasts
#'@param counts_file file containing counts
#'

make_country_prediction_line_graph <- function(forecasts, counts, ylim_scale=1) {
                require(dplyr)
                require(lubridate)
                forecasts <- tbl_df(forecasts)
                counts <- tbl_df(counts)
                        
                ## aggregate to country-level
                forecasts_cntry <- forecasts %>% group_by(biweek, year) %>% 
                        summarize(predicted_cntry_count = sum(predicted_count),
                                  predicted_ub = sum(ub),
                                  predicted_lb = sum(lb)) %>%
                        mutate(time = year + (biweek-1)/26)
                forecast_times <- (forecasts_cntry$year + (forecasts_cntry$biweek-1)/26)
                
                counts_cntry <- counts %>% 
                        group_by(date_sick_year, date_sick_biweek) %>%
                        summarize(cntry_count = sum(count)) %>%
                        mutate(time = date_sick_year + (date_sick_biweek-1)/26,
                               forecast_biweek = time %in% forecast_times)
                

                ## add column in counts_cntry indicating which biweeks were left out of the fit
                
                ## make better time labels!
                
                ## make plot
                ggplot() + theme_bw() + 
                        theme(legend.position=c(1,1), legend.justification=c(1,1)) +
                        ## plot counts
                        geom_bar(data=counts_cntry, 
                                 aes(x=time, y=cntry_count, fill=forecast_biweek), 
                                 stat="identity") + 
                        scale_fill_manual(values=c("black", "gray"),
                                          name="Forecast data",
                                          labels=c("used", "unused"))+
                        ## add forecasts
                        geom_line(data=forecasts_cntry, aes(x=time, y=predicted_cntry_count)) +
                        geom_point(data=forecasts_cntry, aes(x=time, y=predicted_cntry_count)) +
                        geom_ribbon(data=forecasts_cntry, aes(x=time, 
                                                              ymin=predicted_lb, ymax=predicted_ub), 
                                                              alpha=I(.1)) +
                        # air-brushing
                        xlim(2013, 2015) + xlab(NULL) + ylab(NULL) + ylim(0, max(counts_cntry$cntry_count)*ylim_scale) +
                        ggtitle("Observed and predicted DHF case counts for all of Thailand")
        }