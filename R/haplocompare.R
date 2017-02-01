## haplocompare methods

#' is_haplocompare
#' 
#' Check if the class of the provided object matches the expected one.
#' 
#' @param x An object to inspect.
#' @export
is_haplocompare = function(x) {
    inherits(x, "haplocompare")
}

#' print.haplocompare
#' 
#' Print a haplocompare object.
#' 
#' @param x A `haplocompare` object.
#' @param ... Extra arguments.
#' @export
print.haplocompare = function(x, ...) {
    print(sapply(x, class))
}

#' merge_nested.haplocompare
#' 
#' Merge contents of a haplocompare object, using the combination of Group.Id,
#' Sample.Id and Replicate.Id as unique identifiers.
#' 
#' @param obj A `haplocompare` object.
#' @param x First nested object to merge. One of: `names(haplocompare_obj)`.
#' @param y Second nested object to merge. One of: `names(haplocompare_obj)`.
#' @param select Columns to keep after merge.
#' @param ... Extra arguments.
#' @return A merged `data.table`.
#' @export
merge_nested.haplocompare = function(obj, x, y, select, ...) {

    ## validate input
    if (!all(c(x, y) %in% names(obj))) {
        stop('Invalid names for nested objects.')
    }
    
    ## query each nested object individually
    cols_to_keep = c('.id', 'Group.Id', 'Sample.Id', 'Replicate.Id', select)
    x_dt = lapply(obj[[x]], function(dt) {
        dt %>% 
        mutate(.id = paste(Group.Id, Sample.Id, Replicate.Id, sep = '-')) %>% 
        select(match(cols_to_keep[cols_to_keep %in% names(.)], names(.)))
    }) %>% bind_rows()
    
    cols_to_keep = c('.id', select)
    y_dt = lapply(obj[[y]], function(dt) {
        dt %>% 
            mutate(.id = paste(Group.Id, Sample.Id, Replicate.Id, sep = '-')) %>% 
            select(match(cols_to_keep[cols_to_keep %in% names(.)], names(.)))
    }) %>% bind_rows()
    
    ## merge
    m_dt = merge(x_dt, y_dt, by = '.id') %>% 
        data.table::data.table()
    return(m_dt)

}
