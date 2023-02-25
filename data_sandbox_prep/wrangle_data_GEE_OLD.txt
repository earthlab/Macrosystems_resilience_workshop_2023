////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////// SETUP //////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

//Derive Environmental Variables
//Tyler McIntosh, CU Boulder Earth Lab, 10/26/2022

///////Explanation of analysis
// For a domain of interest, calculate information on underlying environmental variables including:
// Aspect, slope, elevation, MTBS severity

///////Naming conventions wherever possible:
//lowerCamelCase for variables
//underscore_separated for files & functions

///////Data inputs & outputs
// INPUTS:
// an aoi chosen from epa_lvl3 (EPA Ecoregions at the level 3 category)
// OUTPUTS:
// .tif files of standard data exported for further analysis

////////////////////////////////// USER-SET PARAMETERS /////////////////////////////////////

//Google Drive folder for outputs
var folderName = 'GEE_Exports';

//Set output projection
var projection = 'EPSG:32613';

//Select timeframe of interest
var startDate = '1999-01-01';
var endDate = '2020-12-31';
var startYear = 1999;
var endYear = 2020;

//Select EPA region of interest
//print('EPA Level 3 Ecoregions:', epa_lvl3);
var aoiRegion = '6.2.14'; //CHOOSE EPA NA_L3CODE OF INTEREST
var regionReadable = 'SouthernRockies'; //Human-readable region name for output files

//Select drought variables of interest
var drought_vars = ee.List(['pdsi', 'spei30d', 'spei1y', 'spei5y']);


////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////// Analysis /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

//////////// DATA TO PULL ////////////////

//AOI
var aoi = epa_lvl3.filter(ee.Filter.eq('NA_L3CODE', aoiRegion));
print('AOI:', aoi);

//Years
var years = ee.List.sequence(startYear, endYear);

//Calculate slope & aspect
var slope = ee.Terrain.slope(dem);
var aspect = ee.Terrain.aspect(dem);
var hillshade = ee.Terrain.hillshade(dem);
var demDats = dem.addBands(slope).addBands(aspect).addBands(hillshade); //merge images into one by adding bands, now has elevation, slope, aspect, and hillshade
print("DEM data:", demDats);

//Get NLCD
var landcover = nlcd.select('landcover').toBands();
print("Landcover data:", landcover);

//Get ModisVCF
print("ModisVCF:", modisVCF);
var modisPercTreeCov = modisVCF.select('Percent_Tree_Cover').toBands();
var modisPercNonTreeVeg = modisVCF.select('Percent_NonTree_Vegetation').toBands();
var modisPercNoVeg = modisVCF.select('Percent_NonVegetated').toBands();
var modisTreeSD = modisVCF.select('Percent_Tree_Cover_SD').toBands();
var modisNoVegSD = modisVCF.select('Percent_NonVegetated_SD').toBands();
var modisQual = modisVCF.select('Quality').toBands();
var modisCloud = modisVCF.select('Cloud').toBands();
print("modisPercTreeCov:", modisPercTreeCov);


/////////////// Get Drought/Climate //////////////////
print("Drought data:", drought);
drought = drought.filterDate(startDate, endDate).select(drought_vars);

//Function to pull data from one year, average, and reset image properties/band names
//Needs to be mapped over a list of years
//Mapped function needs input of 1) the IC of interest, and 2) string that is the variable name of interest
var annualize = function(ic, variable){
  var wrap = function(yr){
    var yrNm = ee.Number(yr).format('%04d');
    var means = ic.select(variable).filter(ee.Filter.calendarRange(yr,yr,'year')).mean();
    var name = ee.String(variable).cat("_").cat(yrNm).cat('_annual_mean');
    return means.rename(name);
  };
  return wrap;
};
var pdsiAnnual = ee.ImageCollection.fromImages(years.map(annualize(drought, 'pdsi'))).toBands();
var spei30dAnnual = ee.ImageCollection.fromImages(years.map(annualize(drought, 'spei30d'))).toBands();
var spei1yAnnual = ee.ImageCollection.fromImages(years.map(annualize(drought, 'spei1y'))).toBands();
var spei5yAnnual = ee.ImageCollection.fromImages(years.map(annualize(drought, 'spei5y'))).toBands();
print('SPEI 1yr Annual', spei1yAnnual);


//print("Climate data:", gridmet);

///////////////////ADD HUMAN READABLE TO FILE NAMES ////////////////////

//Export the data to google drive
Export.image.toDrive({
  image: dem,
  description: 'USGS_SRTM_30m',
  crs: projection,
  scale: 30,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: slope,
  description: 'USGS_SRTM_30m_slope',
  crs: projection,
  scale: 30,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: aspect,
  description: 'USGS_SRTM_30m_aspect',
  crs: projection,
  scale: 30,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: hillshade,
  description: 'USGS_SRTM_30m_hillshade',
  crs: projection,
  scale: 30,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: landcover,
  description: 'NLCD_landcover',
  crs: projection,
  scale: 30,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: modisPercTreeCov,
  description: 'Modis_Percent_Tree_Cover',
  crs: projection,
  scale: 250,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: modisPercNonTreeVeg,
  description: 'Modis_Percent_NonTree_Vegetation',
  crs: projection,
  scale: 250,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: modisPercNoVeg,
  description: 'Modis_Percent_NonVegetated',
  crs: projection,
  scale: 250,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: modisTreeSD,
  description: 'Modis_Percent_Tree_Cover_SD',
  crs: projection,
  scale: 250,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: modisNoVegSD,
  description: 'Modis_Percent_NonVegetated_SD',
  crs: projection,
  scale: 250,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: modisQual,
  description: 'Modis_Quality',
  crs: projection,
  scale: 250,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: modisCloud,
  description: 'Modis_Cloud',
  crs: projection,
  scale: 250,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: pdsiAnnual,
  description: 'pdsi_Annual',
  crs: projection,
  scale: 4638.3,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: spei30dAnnual,
  description: 'spei30d_Annual',
  crs: projection,
  scale: 4638.3,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: spei1yAnnual,
  description: 'spei1y_Annual',
  crs: projection,
  scale: 4638.3,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: spei5yAnnual,
  description: 'spei5y_Annual',
  crs: projection,
  scale: 4638.3,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});


