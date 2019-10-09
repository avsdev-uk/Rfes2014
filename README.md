# Rfes2014

Pre-requisites
---------------
- [R v3.4.0+](https://www.r-project.org/)
- [netCDF4](https://github.com/Unidata/netcdf-c) (see known issues)
- [FES2014](https://bitbucket.org/cnes_aviso/fes) (fetched by this package, see known issues)

How to install
---------------
When installing on a system with netCDF installed in a normal location (ie /usr) then all that is required is:
```
# From the directory above Rfes2014
R CMD INSTALL Rfes2014
```

To build & install when netCDF installed elsewhere (eg `/opt/netcdf4`):
```
export NETCDF_PATH=/opt/netcdf4 LD_LIBRARY_PATH=${NETCDF_PATH}/lib R CMD INSTALL Rfes2014
```
To use the package when netCDF is installed elsewhere, remember to use `LD_LIBRARY_PATH` when calling R:
```
NETCDF_PATH=/opt/netcdf4 LD_LIBRARY_PATH=${NETCDF_PATH}/lib R
```
If you're using Windows: fend for yourself!

Known issues
---------------
#### netCDF-4
There is a known issue in netCDF4 which is currently prevalent in the stable branch. Therefore it is require to download the development branch and compile the latest tag in the stable tree (v4.6.3 at the time of writing).
The issue can be found on the netcdf-c github as [issue 1273](https://github.com/Unidata/netcdf-c/issues/1273)
#### fes2014
- Tight error loop after out-of-bounds result

There is an issue with the fes2014 library in that one of the optimisation additions causes a tight failure loop when an out-of-bounds error occurs. The nature of this error is such that when it occurs, it is impossible to get out of it without closing down fes and starting it up again (loading all required files etc in the process).

To mitigate that issue there is a small fix to use when compiling fes2014:
```
diff --git a/src/grid.c b/src/grid.c
index 18dd16c..1ed1601 100644
--- a/src/grid.c
+++ b/src/grid.c
@@ -465,12 +465,8 @@ int interp(fes_handler* fes, const double lat, const double lon) {
   size_t n;
   fes_double_complex c;
 
-  if (!CONTAINS(
-      fes->west_lon, lon,
-      fes->east_lon) || !CONTAINS (fes->south_lat, lat, fes->north_lat)) {
-    if (_get_nearest_points(fes, lat, lon))
-      return 1;
-  }
+  if (_get_nearest_points(fes, lat, lon))
+    return 1;
 
   /* The zone required is not in the grid */
   if (!fes->in_grid)

```

 - File handle leak in "memory" mode

When  using memory mode there is a file handle leak. The following patch rectifies this:
```
diff --git a/src/grid.c b/src/grid.c
index 18dd16c..d1cc212 100644
--- a/src/grid.c
+++ b/src/grid.c
@@ -617,7 +624,13 @@ int load_grid(const char* const path, const unsigned int n,
 
     free(amp);
     free(pha);
+
+    /* Don't need the netcdf file anymore, close it */
+    nc_close(nc->id);
   } else {
+    /* Store the netcdf file handle.
+     * It will be used for access later and is closed in fes.c::fes_delete()
+     */
     fes->grid.file[n] = *nc;
   }
 
```
