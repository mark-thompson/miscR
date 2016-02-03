sf_id15to18 <- function(id) 
{
    if (nchar(id) != 15) {
        warning('Not a 15 character ID')
        return(id)
    }
    splt <- lapply(strsplit(id, NULL)[[1]], `%in%`, LETTERS)
    
    res  <- lapply(
        list(splt[1:5], splt[6:10], splt[11:15]), 
        function(nvec) { 
            sum(2^(.Internal(which(nvec == TRUE))-1)) + 1 
        }
    )
    res <- paste(c(id, c(LETTERS, 0:5)[unlist(res)]), collapse = '')
    return(res)
}