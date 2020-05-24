series{
  title="West ONE-FAMILY 1"
  format = "datevalue"
  period = 12
  file = "/var/folders/_0/8ksbm6x56vg67bdqy6rms_4c0000gn/T//Rtmp9d5e09/x13ba3035537f6d/data1.dta"
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
