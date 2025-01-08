NOTE: This work is incomplete is to be used as a start-point for further work to make this less janky and more interactive. 

# Pheasant_Dispersal_Simulation

The first port of call is the "Coordinate_picker.bat" file. This will run the Python scripts (in "Py_scripts/") to open a window where the user can pick coordinates from a map and save them as "coord_files/release_coords.csv". These corrdinates can either be a release site or feeder points. However, there is not currently a way to set a release site, set feeders <b><i>to be exclusively used with that release</i></b> and then do the same for a new release site; all feeders will be used by all releases. </br>

The user is then asked if they want to change the landscape around the release site chosen. Again, this is a proof-of-concept and is janky as all hell, but it works(/worked) and saved the altered landscape raster at "Data/UKCEH-2018-25m_AllUK/output.tif" (this directory may not be in the GitHub-hosted version because the UKCEH land-use raster is too large, but it can be downloaded here: https://www.ceh.ac.uk/data/ukceh-land-cover-maps). </br>

The user is then asked how pheasants they want to simulate (I think), then the simulation is run by calling the R scripts in "R_scripts/". The output of this is then saved to "Outputs/Simulations/". </br>

Again, just to emphasise this was a proof-of-concept excercise and by no means a user-friendly, finished version. 
