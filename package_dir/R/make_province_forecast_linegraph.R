## make province prediction graph
## Nicholas Reich
## September 2014

#'@param forecast_file file containing forecasts
#'@param counts_file file containing counts
#'

make_province_prediction_line_graph <- function(forecasts, counts) {
                require(dplyr)
                require(lubridate)
                require(cruftery)
                require(scales)
                
                ## load in data
                data(thai_prov_data)
                forecasts <- tbl_df(forecasts)
                counts <- tbl_df(counts)
                        
                ## aggregate to country-level
                forecasts_prov <- forecasts %>% group_by(biweek, year, pid, pname) %>% 
                        summarize(predicted_prov_count = sum(predicted_count),
                                  predicted_ub = sum(ub),
                                  predicted_lb = sum(lb)) %>%
                        mutate(time = year + (biweek-1)/26,
                               date_sick = biweek_to_date(biweek, year))
                forecast_times <- (forecasts_prov$year + (forecasts_prov$biweek-1)/26)
                
                counts_prov <- counts %>% 
                        group_by(date_sick_year, date_sick_biweek, province) %>%
                        summarize(prov_count = sum(count)) %>%
                        mutate(time = date_sick_year + (date_sick_biweek-1)/26,
                               date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
                               forecast_biweek = time %in% forecast_times)
                
                ## ready the province data
                thai_prov_data <- mutate(thai_prov_data, 
                                         province=ISO,
                                         pid=FIPS,
                                         pname=Province)
                counts_prov <- left_join(counts_prov, thai_prov_data)
                                            
                ## add column in counts_prov indicating which biweeks were left out of the fit
                                
                ## make plot
                ggplot() + theme_bw() + 
                        theme(#legend.position=c(1,1), legend.justification=c(1,1),
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5)) +
                        ## plot counts
                        geom_bar(data=filter(counts_prov, date_sick >= as.Date('2013-01-01')), 
                                 aes(x=date_sick, y=prov_count, fill=forecast_biweek), 
                                 stat="identity") + 
                        scale_fill_manual(values=c("black", "gray"),
                                          name="Forecast data",
                                          labels=c("used", "unused"))+
                        ## add forecasts
                        geom_line(data=forecasts_prov, aes(x=date_sick, y=predicted_prov_count)) +
                        geom_point(data=forecasts_prov, aes(x=date_sick, y=predicted_prov_count)) +
                        geom_ribbon(data=forecasts_prov, aes(x=date_sick, 
                                                              ymin=predicted_lb, ymax=predicted_ub), 
                                                              alpha=I(.1)) +
                        facet_wrap(~pid, scales="free_y") +
                        # air-brushing
                        scale_x_date(breaks = "3 months",
                                     labels = date_format("%b '%y"),
                                     limits = as.Date(c('2013-01-01','2015-01-01')))+
                        xlab(NULL) + ylab(NULL) + #ylim(0, 1000) +
                        ggtitle("Observed and predicted DHF case counts for all of Thailand")
        }