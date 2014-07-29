
build_aggregate_stmt <- function(
	source_table,
	group_by,
	from_timepoint,
	to_timepoint,
	delivery_timepoint
) {
	stmt <- paste0(
		"SELECT ", paste0(group_by, collapse=', '), ", count(*) FROM ",
		source_table, " WHERE ",
			"date_sick >  '", as.character(from_timepoint), "' AND ",
			"date_sick <= '", as.character(to_timepoint), "' AND ",
			"(delivery_date < '", as.character(delivery_timepoint), "' ",
			" OR delivery_date IS NULL) ",
		"GROUP BY ", paste0(group_by, collapse=', '), " ",
		"ORDER BY ", paste0(group_by, collapse=', '), ";"
	)
	return(stmt)
}

fetch_cases <- function(link, stmt) {
	case_counts <- dbGetQuery(link[['conn']], stmt)
	return(case_counts)
}

aggregate_to_formula <- function(
	counts, formula,
	label_function = function(x) {
		counts[['date_sick_biweek']] <- date_to_biweek(counts[['date_sick']])
		return(counts)
	}
) {
	counts[['date_sick_year']] <- year(counts[['date_sick']])
	split_counts <- split(x=counts, f=counts[['date_sick_year']], drop=FALSE)
	split_counts <- mclapply( X=split_counts, FUN=label_function, mc.cores=getOption("mc.cores",12))
	counts <- do.call(what=rbind, args=split_counts)
	case_counts <- aggregate(formula = formula, data = counts, FUN=sum)
	return(case_counts)
}

import_case_counts <- function(
	source_table = 'unique_case_data',
	group_by = c('disease','date_sick','province'),
	from_timepoint = now() - years(100), ## FROM (open)
	to_timepoint = now(), ## TO (closed)
	delivery_timepoint = now(),
	aggregate_formula = count ~ disease + province + date_sick_biweek + date_sick_year,
	link = stop("Need a link")
) {
	stmt <- build_aggregate_stmt(source_table, group_by, from_timepoint, to_timepoint, delivery_timepoint)
	case_counts <- fetch_cases(link, stmt)
	agg_case_counts <- aggregate_to_formula(counts=case_counts, formula=aggregate_formula)
	return(agg_case_counts)
}

import_old_case_counts <- function(
	source_table = 'biweekly_dengue_counts_1968_to_2005',
	link = stop("Need a link.")
) {
	stmt <- paste0("SELECT * FROM ", source_table, ";")
	old_counts <- dbGetQuery(link$conn, stmt)
	old_counts[['disease']] <- 26
	old_counts[['count']] <- old_counts[['smooth_counts']]
	old_counts[['smooth_counts']] <- NULL
	old_counts[['smooth_cumulative_counts']] <- NULL
	old_counts[['date_sick_year']] <- old_counts[['year']]
	old_counts[['year']] <- NULL
	old_counts[['times']] <- NULL
	return(old_counts)
}
	
joint_old_new_cases <- function(new_counts, old_counts) {
	## Joint data:
	provinces <- unique(new_counts[['province']])
	
	new_counts <- split(x=new_counts, f=new_counts[['province']])
	old_counts <- split(x=old_counts, f=old_counts[['province']])

	new_provinces <- names(new_counts)[!(names(new_counts) %in% names(old_counts))]
	for ( i in new_provinces ) {
		old_counts[[i]] <- NA
	}
	
	new_counts <- new_counts[sort(names(new_counts))]
	old_counts <- old_counts[sort(names(old_counts))]
	
	spliced_counts <- mcmapply(
		FUN = function(new_counts, old_counts) {
			if (!is.data.frame(old_counts) && is.na(old_counts)) return(new_counts)
			new_times <- new_counts[['date_sick_year']] + new_counts[['date_sick_biweek']] / 26
			old_times <- old_counts[['date_sick_year']] + old_counts[['date_sick_biweek']] / 26
			old_counts <- old_counts[old_times < min(new_times),]
			spliced_counts <- rbind(new_counts, old_counts)
			return(spliced_counts)
		},
		new_counts = new_counts,
		old_counts = old_counts,
		SIMPLIFY=FALSE, mc.cores=getOption("mc.cores",6L)
	)
	
	counts <- do.call(what=rbind, args=spliced_counts)
	counts <- counts[order(
		counts[['province']], 
		counts[['date_sick_year']], 
		counts[['date_sick_biweek']]
	),]
	return(counts)
}
	
## Example: 
#new_counts <- import_case_counts(link=link)
#old_counts <- import_old_case_counts(link=link)
#counts <- joint_old_new_cases(new_counts, old_counts)






	
