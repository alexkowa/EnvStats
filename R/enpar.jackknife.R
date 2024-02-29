enpar.jackknife <-
function (x) 
{
    N <- length(x)
    jack.vec <- numeric(N)
    for (i in 1:N) {
        jack.vec[i] <- mean(x[-i])
    }
    jack.vec
}
