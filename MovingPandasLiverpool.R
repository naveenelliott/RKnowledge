import urllib
import os
import pandas as pd
import geopandas as gpd
from geopandas import GeoDataFrame, read_file
from shapely.geometry import Point, LineString, Polygon
from fiona.crs import from_epsg
from datetime import datetime, timedelta
from matplotlib import pyplot as plt
import holoviews as hv

import sys
sys.path.append("..")
import movingpandas as mpd
mpd.show_versions()

import warnings
warnings.simplefilter("ignore")