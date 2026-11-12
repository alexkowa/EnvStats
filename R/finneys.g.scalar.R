finneys.g.scalar <-
function (m, z, n.terms.inc = 10, iter = 100, tol = .Machine$double.eps) 
{
    # Finney's (1941) g_m(z), used for the MVUE of the lognormal mean and
    # variance.  The series is the confluent hypergeometric limit function
    #     g_m(z) = 0F1(; m/2; y),   y = m^2 z / (2 (m + 1)),
    # with terms t_p = t_{p-1} * y / ((m/2 + p - 1) p).
    #
    # For z > 0 every term is positive and the sum is accurate to full double
    # precision.  For z < 0 the terms alternate, and once the largest term
    # exceeds the sum by more than about 1e4 the cancellation destroys the
    # result (EnvStats <= 3.1.0 returned values with the wrong sign for, e.g.,
    # m = 20, z = -50).  There the identity
    #     0F1(; b; -u) = Gamma(b) u^((1 - b)/2) J_{b-1}(2 sqrt(u)),   u = -y,
    # gives g without cancellation.
    #
    # 'n.terms.inc', 'iter' and 'tol' are accepted for backward compatibility
    # and are no longer used.
    if (any(length.list(m, z) > 1)) 
        stop("'m' and 'z' must be scalars")
    if (m < 1 || m != trunc(m)) 
        stop("'m' must be an integer greater than or equal to 1")
    if (z == 0) 
        return(1)
    b <- m/2
    y <- (m^2 * z)/(2 * (m + 1))
    eps <- .Machine$double.eps
    max.terms <- 100000L
    S <- 1
    A <- 1
    t <- 1
    p <- 0L
    repeat {
        p <- p + 1L
        t <- (t * y)/((b + p - 1) * p)
        S <- S + t
        A <- A + abs(t)
        if (!is.finite(A) || p >= max.terms) 
            break
        if (p > abs(y)/b && abs(t) <= eps * A) 
            break
    }
    if (z > 0) {
        if (p >= max.terms) 
            warning(paste("Finney's g did not converge for m =", m, "and z =", z))
        return(S)
    }
    if (is.finite(A) && p < max.terms && abs(S) * 1e4 >= A) 
        return(S)
    u <- -y
    J <- besselJ(2 * sqrt(u), b - 1)
    if (J == 0) 
        return(0)
    sign(J) * exp(lgamma(b) + ((1 - b)/2) * log(u) + log(abs(J)))
}
