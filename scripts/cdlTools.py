import rasterio
from rasterio import plot
import xarray
import matplotlib.pyplot as mpl

cdl_mi = rasterio.open("_episodes_rmd/lagos_ag/data/cdl_mi.tif")

plot.show(cdl_mi)

cdl_mi_array = cdl_mi.read(1)



mpl.hist(cdl_mi_array[1:2,:])