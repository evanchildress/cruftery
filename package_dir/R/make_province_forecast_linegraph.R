## make province prediction graph
## Nicholas Reich
## September 2014

#'@param forecast_file file containing forecasts
#'@param counts_file file containing counts
#'

make_province_prediction_line_graph <- function(forecasts, counts, region=1) {
                require(dplyr)
                require(lubridate)
                require(cruftery)
                require(scales)
                
                ## load in data
                data(thai_prov_data)
                forecasts <- tbl_df(forecasts)
                counts <- tbl_df(counts)
                        
                ## date plotting limits
                min_plot_date <- as.Date('2012-01-01')
                
                ## aggregate to country-level
                forecasts_prov <- forecasts %>% group_by(biweek, year, pid, pname) %>% 
                        summarize(predicted_prov_count = sum(predicted_count),
                                  predicted_ub = sum(ub),
                                  predicted_lb = sum(lb)) %>%
                        mutate(time = year + (biweek-1)/26,
                               date_sick = biweek_to_date(biweek, year),
                               FIPS = pid)
                forecasts_prov <- left_join(forecasts_prov, thai_prov_data)
                forecasts_prov <- filter(forecasts_prov, MOPH_Admin_Code == region)
                
                forecast_times <- (forecasts_prov$year + (forecasts_prov$biweek-1)/26)
                
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
                                         pname=Province)
                counts_prov <- left_join(counts_prov, thai_prov_data)
                counts_prov <- filter(counts_prov,MOPH_Admin_Code == region)
                        
                                            
                ## add column in counts_prov indicating which biweeks were left out of the fit
                                
                ## make plot
                plot_title <- paste("Observed and predicted DHF case counts for MOPH Region", region)
                ggplot() + theme_bw() + 
                        theme(legend.position="bottom", #legend.justification=c(1,1),
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5)) +
                        ## plot counts
                        geom_bar(data=counts_prov, 
                                 aes(x=date_sick, y=prov_count, fill=forecast_biweek), 
                                 stat="identity") + 
                        scale_fill_manual(values=c("black", "gray"),
                                          name=NULL,
                                          labels=c("used by forecast model", "not used by forecast model"))+
                        ## add forecasts
                        geom_line(data=forecasts_prov, aes(x=date_sick, y=predicted_prov_count)) +
                        geom_point(data=forecasts_prov, aes(x=date_sick, y=predicted_prov_count)) +
                        geom_ribbon(data=forecasts_prov, aes(x=date_sick, 
                                                              ymin=predicted_lb, ymax=predicted_ub), 
                                                              alpha=I(.1)) +
                        facet_grid(pname~., scales="free_y") +
                        # air-brushing
                        scale_x_date(breaks = "3 months",
                                     labels = date_format("%d %b %Y"))+
                        xlab(NULL) + ylab(NULL) + #ylim(0, 1000) +
                        ggtitle(plot_title)
        }