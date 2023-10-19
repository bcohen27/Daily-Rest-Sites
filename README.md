# Daily Rest/Foraging Sites

MoveApps

Github repository: *github.com/movestore/Daily-Rest-Sites*

## Description
This App filters the data set to night or day positions (or of all 24h per day positions) with resting behaviour (in a certain radius for a certain min time with low speed). The resting positions are given as output and a csv table with properties of all resting sites of each individual is saved as artefact. This can be used for roost detection or foraging site extraction. Data with resolutions between 1/min and 1/hour give the best results.

## Documentation
This App extracts all locations of animals in a defined resting site. It is has been developed to generalise nightly roost site extraction of waterfowl. Here daily resting/foraging sites can be extracted, nightly roosts or general resting sites during the 24h of the day.

Before the analysis starts, the input data set is tinned to a resolution of max. 5 minutes to speed up the run time. It is sensible to do this, because high resolution data does not add anything to resting site extraction, for which longer time ranges are required.

Then, the actual analysis is done in three steps. First, all positions with speed above the provided maximum speed are remove. This is sensible, as resing behaviour must show little movement. Second, all night or day positions are selected. For this, the sunriset() function from the maptools() package is used. If there are any locations in Arctic/Antarctic regions in times where there is no sunrise and sunset, all locations are retained or deleted, depending on day/night selection. If it was selected to calculated resting sites of the whole 24h days data set, all location are retained. A csv artefact of this intermediate data set is returned. Third, all positions that define a resting site with minimum duration and minimum radius are selected (note that per resting site a minimum of two locations is required). For each individual and night/day all resting site are extracted, starting from the back, namely sunrise for night roosts, sunset for day resting sites and midnight for 24h resting selection.

Properties of detected resting sites are provided in a table that is given out as csv artefact. There the following properties are listes for each resting site: animal name, year, night number, timestamp of first resting location, timestamp of last resting location, resting site mean loation (longitute/latitude), number of locations in the resting site, duration the animal has stayed in the resting site, realised radius of the resting site.

For rough evaluation, the App is also producing a pdf map showing the locations of all resting sites on top of the tracks. Note that the visualisation requires you to enter an API key from stadia, as it uses their background maps. This is only a workaround for a few months until MoveApps provides an own OSM mirror. Register for a stadia API here, it is free: https://stadiamaps.com/stamen/onboarding/create-account.

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
`data_rest_selectedTime.csv`: csv-file with Table of all positions that are of low speed (below given threshold) and during night (day) as selected (see Documentation above)

`rest_overview.csv`: csv-file with Table of all rest site properites (see details in Documentatin above)

`rest_sites_onTracks.pdf`: pdf-file of Map showing all tracks (blue) with the locations of the extracted resting sites on top (red). The dots have uniform size, i.e. do not indicate the size of the resting site. That way more sites are visible.

### Settings
**Day or night selection (`window`):** Radiobuttons selection to indicate if only night locations ('sundownup'), day locations ('sunupdown') or all locations of a 24h day shall be selected.

**Sunrise adaption time (`upX`):** Sunrise adaption time that indicated how many minutes after (or if negative value before) sunrise your animals usually start the required resting behaviour.

**Sunset adaption time (`downX`):** Sunset adaption time that indicated how many minutes after (or if negative value before) sunset your animals usually start the required resting behaviour.

**Speed variable name (`speedvar`):** Name of the variable in your data set that indicates speed measured at each location to use for filtering for ground locations. By default interlocation speed ('speed') will be used.

**Maximum resting speed (`maxspeed`):** Maximum speed (of the selected variable) an animal is allowed during resting behaviour. Locations with speeds above this value will be removed.

**Minimum resting duration (`duration`):** Defined duration the animal minimally stays in a given radius for it to be considered resting. Unit: `hours`.

**Maximum resting radius (`radius`):** Defined radius the animal has to stay in for a given duration of time for it to be considered resting site. Unit: `metres`.

**Stadia API key (`stamen_key`):** For visualisation of the rest sites on map background you need to enter an API key from stadia here. Note that this is only a workaround for a few months until MoveApps provides an own OSM mirror. Register for a stadia API key here, it is free: https://stadiamaps.com/stamen/onboarding/create-account.

### Null or error handling:
**Setting `window`:** The default value (NULL) inicates that all values of a 24h day will be retained and resting sites thereof extracted (starting from midnight).

**Setting `upX`:** The default value here is '0', indicating that no adaption time is necessary. If the value gets too large, the calculations might become difficult to understand.

**Setting `downX`:** The default value here is '0', indicating that no adaption time is necessary. If the value gets too large, the calculations might become difficult to understand.

**Setting `speedvar`:** The default here is 'speed', which is the interlocation speed calculated with the function speed() in the move package. If a non-existant variable name is entered here the algorithm will fall back on interlocation 'speed'.

**Setting `maxspeed`:** If no maximum ground speed is provided (NULL), all locations are used for the night and roost analysis. This techniqually allows fast movement to be classified as roosting behaviour.

**Setting `duration`:** If no duration AND no radius are given, the input data set is returned with a warning. If no duraiton is given (NULL), but a radius is defined then a default duration of 1 hour is set. 

**Setting `radius`:** If no radius AND no duration are given, the input data set is returned with a warning. If no radius is given (NULL), but a duration is defined then a default radius of 1000m = 1km is set. 

**stamen_key**: Without providing an API key from stadia for using stamen maps, there will be no map pdf artifact.

**Data:** If there are no resting locations retained after all analyses, NULL is returned, likely leading to an error.
