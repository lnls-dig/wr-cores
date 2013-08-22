def __helper():
  dirs = []
  if syn_device[:1] == "5":    dirs.extend(["wr_arria5_phy", "arria5_pll"])
  if syn_device[:4] == "ep2a": dirs.extend(["wr_arria2_phy", "arria2_pll"])
  return dirs
  
files = [ "altera_pkg.vhd" ]
modules = {"local": __helper() }
