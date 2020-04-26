# This function allows to extract multiple sheets by using the map function. After each
# iteration, it will stop for 3 seconds to avoir overloading the system.

# Load google sheets library
library(googlesheets)

extract_sheet_and_sleep <-
function(spread_sheet, sheet_name){
    
    if(missing(spread_sheet)) stop("spread_sheet argument is missing")
    if(missing(sheet_name)) stop("sheet_name argument is missing")
    
    temp_sheet = gs_read(ss = spread_sheet, ws = sheet_name) 
    
    Sys.sleep(3)
    
    return(temp_sheet)
    
}


# the function extract_sheets will extract all the sheets contained in a spreadsheet
extract_sheets <-
function(spread_sheet_name, sheet_names_contain){
    
    # @ Params
    # spread_sheet_name: a character vector of the name of the spreadsheet
    # sheet_names_contain: a character vector with the strings that the sheets you 
        # you want to extract should contain
    
    if(missing(spread_sheet_name)) stop("Argument missing. Please provide a character vector for spread_sheet_name argument")
    if(missing(sheet_names_contain)) stop("Argument missing. Please provide a character vector for sheet_names_contain argument")
    if(!is.character(spread_sheet_name)) stop("Wrong argument type. Please provide a character vector for spread_sheet_name argument")
    if(!is.character(sheet_names_contain)) stop("Wrong argument type. Please provide a character vector for sheet_names_contain argument")
    
    spread_sheet = gs_title(spread_sheet_name)
    
    # vector to include in filter
    sheet_names_contain = str_c(sheet_names_contain, collapse = "|")
    
    # extract sheet names
    sheet_names = spread_sheet %>% 
        
        pluck("ws") %>% 
        
        filter(ws_title %>% str_detect(sheet_names_contain)) %>% 
        
        pull(ws_title)
    
    # extract a list with all sheets content
    all_sheets = sheet_names %>% 
        map(~ extract_sheet_and_sleep(spread_sheet = spread_sheet, sheet_name = .)) %>% 
        set_names(sheet_names)
    
    return(all_sheets)
    
}
