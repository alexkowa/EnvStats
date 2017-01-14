nyblom.lambda <-
function (beta, p, r, n) 
{
    pi.r <- pbinom(q = r - 1, size = n, prob = p)
    pi.r.plus.1 <- pbinom(q = r, size = n, prob = p)
    (1 + (r * (1 - p) * (pi.r.plus.1 - beta))/((n - r) * p * 
        (beta - pi.r)))^(-1)
}
