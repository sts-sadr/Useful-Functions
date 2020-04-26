this functions gives in a long format the classes of each column of a tibble or dataframe
get_classes_of_tlb <-
function(tibble){
    
    tibble %>% 
        map_df(~ class(.)[1]) %>% 
        gather()
    
}
