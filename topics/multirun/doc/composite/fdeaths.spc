series{
  title="West ONE-FAMILY 2"
  format = "datevalue"
  period = 12
  file = "doc/composite/fdeaths.dta"
  comptype=add
}

transform{
  function = auto
  print = aictransform
}

seats{
  noadmiss = yes
  save = (s10 s11 s12 s13 s16 s18)
}

outlier{

}

automdl{
  print = bestfivemdl
}

estimate{
  save = (model estimates residuals)
}

spectrum{
  print = qs
}
