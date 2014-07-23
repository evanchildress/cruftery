require(RPostgreSQL)
db_connector <- setRefClass(Class="db_connector",
                            fields = list(
                                    driver = "PostgreSQLDriver",
                                    credentials = "character",
                                    conn = function(conn=NULL) {
                                            if (!is.null(conn)) return("no.")
                                            return(get_connection())
                                    }
                            ),
                            methods = list(
                                    initialize = function(credentials) {
                                            driver <<- dbDriver("PostgreSQL")
                                            credentials <<- credentials
                                            add_connection()
                                            check_crd()
                                            connection_in_use(get_connection())
                                            clean_connections()
                                            clear_connections()
                                    },
                                    check_crd = function(crd=NULL) {
                                            if (is.null(crd)) {
                                                    crd <- readRDS(.self$credentials)
                                            } else {
                                                    crd <- readRDS(crd)
                                            }
                                            required_elements <- c('port','host','user','password','dbname')
                                            missing <- sapply(required_elements, function(x,crd) !(x %in% crd), crd=names(crd) )
                                            if (any(missing)) {
                                                    paste0('Credentials list must include elements for:\n ',
                                                           paste('\t',required_elements[missing],sep='',collapse=',\n'))
                                            }
                                            rm(crd)
                                            return("Credentials ok.\n")
                                    },
                                    add_connection = function() {
                                            crd <- readRDS(file=credentials)
                                            status <- tryCatch(expr = {
                                                    dbConnect(drv=driver, port=crd[['port']], host=crd[['host']],
                                                              user = crd[['user']], password=crd[['password']], dbname=crd[['dbname']])
                                            }, error=function(e) return("FAIL"))
                                            rm(crd)
                                            if (identical(status, "FAIL")) {
                                                    dbListTables(dbListConnections(driver)[[length(dbListConnections(driver))]])
                                                    warning(paste0(
                                                            "Was not able to allocate a new connection, returned\n",
                                                            "connection has lost its previously pending result."))
                                            }
                                            connections <- dbListConnections(driver)
                                            return(connections[[length(connections)]])
                                    },
                                    get_connection=function() {
                                            if (identical(list(),dbListConnections(driver)))
                                                    add_connection()
                                            for ( i in 1:length(dbListConnections(driver)) ) {
                                                    connection <- dbListConnections(driver)[[i]]
                                                    if (!connection_in_use(connection)) {
                                                            return(connection)
                                                    }
                                            }
                                            add_connection()
                                            return(dbListConnections(driver)[[length(dbListConnections(driver))]])
                                    },
                                    connection_in_use = function(connection=NULL) {
                                            if (is.null(connection)) return(NA)
                                            temp_result <- tryCatch(
                                                    expr=dbSendQuery(connection,"SELECT * FROM generate_series(1,2);"),
                                                    error=function(e) return("FAIL")
                                            )
                                            test <- (identical(temp_result,"FAIL"))
                                            if (!test) dbClearResult(temp_result)
                                            return(test)
                                    },
                                    clean_connections = function() {
                                            for (i in 1:length(dbListConnections(driver))) {
                                                    if (!isPostgresqlIdCurrent(dbListConnections(driver)[[i]])) {
                                                            dbDisconnect(dbListConnections(driver)[[i]])
                                                    }
                                            }
                                    },
                                    clear_connections = function() {
                                            lapply(dbListConnections(driver),dbListTables)
                                            return(TRUE)
                                    }
                            )
)

