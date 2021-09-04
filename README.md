# reelease
Study on the effect of externally attached pop-off data storage tags in a natural setting


### Scripts

* `/src:`

1. `upload_data.R`: Upload detection data from raw folder
2. `smooth_eel_tracks.R`: Smooths duplicates and calculates residencies per eel per station. Therefore, it calls the following two functions:
	+ 2a. `get_nearest_stations.R`: general function to extract the smoothed track for one eel (via its `transmitter ID`)
	+ 2b. `get_timeline.R`: function to get the stations which are near a given station (where near means that the distance is smaller than a certain given limit, e.g. detection range).
		- --> Generate residency datasets per project and store them in `/interim/residencies`
3. `calculate_speed.R`: Calculate movement speeds between consecutive detection stations. Also calculates swim distance, swim time, cumulative swim distance and station distance from source station.
	+ 3a. `calculate_speed_function.R`: function to calculate speed between consecutive displacements; based on a function in Hugo Flavio's `actel` package
	+ 3b. `calculate_sourcedistance_function.R`: function to calculate the station distance from a 'source' station; based on a function in Hugo Flavio's `actel` package
4. `merge_eel_characteristics.R`: Add eel meta data to the speed dataset
5. `speed_analysis.R`: Analyse migration speed in relation to external tagging with PDSTs
6. `release_analysis.R`: Analyse time at release site and swim direction