## internal funtions

substrRight = function(x, n){
    sapply(x, function(xx)
        substr(xx, (nchar(xx) - n + 1), nchar(xx))
    )
}

expand_lanes = function(replicate_id, lanes_str) {
    machines = sapply(unlist(strsplit(lanes_str, split = '\\+')), 
                      function(y) unlist(strsplit(y, split = '_'))[2])

    .tmp = sapply(unlist(strsplit(lanes_str, split = '\\+')), 
                  function(y) unlist(strsplit(y, split = '_'))[4])
    flowcells = substr(.tmp, 1, 10)
    lanes = factor(as.numeric(substrRight(.tmp, 1)), levels = 1:8)

    data.table::data.table(
        replicate_id = rep(replicate_id, length(.tmp)),
        machine = machines,
        flowcell = flowcells,
        lane = lanes
    ) %>%
        unique()
}
