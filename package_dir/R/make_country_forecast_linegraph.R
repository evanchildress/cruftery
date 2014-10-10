## make country prediction graph
## Nicholas Reich
## September 2014

#'@param forecast_file file containing forecasts
#'@param counts_file file containing counts
#'

make_country_prediction_line_graph <- function(forecasts, counts, ylim_scale=1, min_plot_date=as.Date("2012-04-01"), show_unused_counts=TRUE) {
                require(dplyr)
                require(lubridate)
                require(cruftery)
                require(scales)
                forecasts <- tbl_df(forecasts)
                counts <- tbl_df(counts)
                        
                ## aggregate to country-level
                forecasts_cntry <- forecasts %>% group_by(biweek, year) %>% 
                        summarize(predicted_cntry_count = sum(predicted_count),
                                  predicted_ub = sum(ub),
                                  predicted_lb = sum(lb)) %>%
                        mutate(time = year + (biweek-1)/26,
                               date_sick = biweek_to_date(biweek, year))
                forecast_times <- (forecasts_cntry$year + (forecasts_cntry$biweek-1)/26)
                
                counts_cntry <- counts %>% 
                        group_by(date_sick_year, date_sick_biweek) %>%
                        summarize(cntry_count = sum(count)) %>%
                        mutate(time = date_sick_year + (date_sick_biweek-1)/26,
                               date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
                               forecast_biweek = time %in% forecast_times) %>%
                        filter(date_sick >= min_plot_date)

                ## add column in counts_cntry indicating which biweeks were left out of the fit
                
                ## make plot
                p <- ggplot() + theme_bw() + 
                        theme(legend.position="bottom",
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5),
                              panel.background = element_rect(fill = "transparent",colour = NA), # or element_blank()
                              panel.grid.major =  element_blank(),
                              panel.grid.minor =  element_blank(),
                              plot.background = element_rect(fill = "transparent",colour = NA)) +
               
                if(show_unused_cases){
                        ## using gray bars for unused cases
                        p <- p + geom_bar(data=counts_cntry, 
                                          aes(x=date_sick, y=cntry_count, fill=forecast_biweek), 
                                          stat="identity") + 
                                scale_fill_manual(values=c("black", "gray"),
                                                  name="",
                                                  labels=c("used by forecast model", "not used by forecast model"))
                } else {
                        ## no unused cases
                        p <- p + geom_bar(data=filter(counts_cntry, forecast_biweek==FALSE), 
                                          aes(x=date_sick, y=cntry_count), 
                                          stat="identity")
                }
                p <- p +  
                        ## add forecasts
                        geom_line(data=forecasts_cntry, 
                                  aes(x=date_sick, y=predicted_cntry_count)) +
                        geom_point(data=forecasts_cntry, 
                                   aes(x=date_sick, y=predicted_cntry_count)) +
                        geom_ribbon(data=forecasts_cntry, 
                                    aes(x=date_sick, ymin=predicted_lb, ymax=predicted_ub), 
                                    alpha=I(.3)) +
                        # air-brushing
                        scale_x_date(breaks = "3 months",
                                     labels = date_format("%d %b %Y"))+
                        xlab(NULL) + ylab(NULL) + 
                        ylim(0, max(counts_cntry$cntry_count)*ylim_scale) +
                        ggtitle("Observed and predicted DHF case counts for all of Thailand")
                p
                
        }