# spatial.utils version 0.1.05

* Adds default for argument `sf_data` in `zoom_to_data()` to improve pipe 
  compatibility
  
# spatial.utils version 0.1.04

* Refactor transect-generating functions to accommodate all options provided in 
  the current PronghornLT shiny app (breaking on hunt areas, different buffers 
  for internal breaks versus outer boundary) and improve consistency of 
  coordinate reference system handling
* `lfdraw_to_sf()`: process the json returned by the leaflet draw plugin to 
  return an `sf` object

# spatial.utils version 0.1.03

* `wgfg_agol_layers()`: list layers available on WGFD OpenGIS portal (requested 
  by @tmcd82070)
* `get_transect_angle()` and `get_transect_spacing()`: approximate the 
  parameters used to generate transects when the original values used are lost

# spatial.utils version 0.1.02

* Minor documentation updates
* `zoom_to_data()`: zoom a leaflet object to the bounds of a particular `sf`
  object
* `dms_to_dd()`: convert strings in degree-minute-second format to decimal 
  degrees

# spatial.utils version 0.1.01

* `spatialFileInput()` and `spatialFileInputHandler()`: allow users to upload 
  spatial data into `shiny` apps, explicitly handling the special case of ESRI 
  shapefiles that require more than one file to represent a single dataset
* `zoom_wy()`: zoom a `leaflet` object to the state of Wyoming
* `rotate_geometry()`: apply affine transformation to rotate the geometry 
  column of an `sf` object about the shared centroid, individual centroids, or 
  a user-specified point
* `generate_transects()` and `demo_gen_transects()`: generate lines to sample 
  at a specified orientation within a specified area whose lengths sum as 
  closely as possible to a specified target overall length

# spatial.utils version 0.1.0

* Initial package skeleton
* `wgfd_agol_data()`: read data from WGFD OpenGIS portal into R
* `labeled_kml()`: add centroid waypoints with styling to KML files for 
  displaying labels on polygons in OnX/Gaia
