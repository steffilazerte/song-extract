## Update progress
update.progress <- function(x, type = "time") {
    if(type == "time"){
        x$time.n[x$time.done == FALSE][1] <- x$time.n[x$time.done == FALSE][1] + 1
        if(x$time.n[x$time.done == FALSE][1] >= x$total[x$time.done == FALSE][1]) {
            x$time.done[x$time.done == FALSE][1] <- TRUE
        }
    }
    return(x)
}

## VALIDATE: Check Times to make sure doesn't spit out too many
check_times <- function(times, notes) {
    times <- length(times$xmin)
    if(times < notes[1] | is.null(times) | times == ""){
        paste0("You've selected too few notes. The min is ",notes[1])
    } else if (times > notes[2]) {
        paste0("You've selected too many notes. The max is ",notes[2])
    } else {
        TRUE
    }
}
