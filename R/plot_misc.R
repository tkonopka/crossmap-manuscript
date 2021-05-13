

scatter.noisy = function(x, y, x.noise=0.01, y.noise=0.01, Rcssclass=c(), ...) {
  stop("deprecated")
  RcssCompulsoryClass = RcssGetCompulsoryClass(c("noisy", Rcssclass))
  x.sd = sd(x)
  y.sd = sd(y)
  xlen = length(x)
  parplot(x + rnorm(length(x), 0, x.noise*x.sd),
          y + rnorm(length(x), 0, y.noise*y.sd),
          ...)
}
