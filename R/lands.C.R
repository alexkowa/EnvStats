lands.C <-
function (S, nu, conf.level, ...) 
{
    # Land's C, computed directly from Land (1971) in compiled code
    # (src/land.cpp): the integrand is evaluated relative to its maximum over
    # its numerically non-zero support, and C is found by root-finding.
    # Arguments in '...' are accepted for backward compatibility and ignored.
    if (S < .Machine$double.eps) 
        stop("'S' must be positive")
    if (nu < 2) 
        stop("'nu' must be greater than or equal to 2")
    if (conf.level <= 0 || conf.level >= 1) 
        stop("'conf.level' must be strictly between 0 and 1")
    C <- lands_C_cpp(S, nu, conf.level)
    if (!is.finite(C)) 
        stop(paste0("Land's C could not be computed for S = ", 
            format(S), ", nu = ", format(nu), ", conf.level = ", 
            format(conf.level)))
    C
}
