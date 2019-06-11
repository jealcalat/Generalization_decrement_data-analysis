
qlife = function (r_times, p = c(0.25, 0.75)) {
  # type 8 is the recommended method, see help('quantile')
  ql = quantile(r_times, probs = p, type = 8, names = F)
  data.frame(q25 = ql[1], q75 = ql[2])
}
