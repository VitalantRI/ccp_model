import pyarrow.feather as feather

def read_covidactnow(fname):
  return feather.read_feather(fname)
