# Climate data sources 

## West Coast Ocean Data Portal
http://portal.westcoastoceans.org/
- biological, physical, and human use data

## Hybrid Coordinate Ocean Model (HYCOM) Global Forecast (1/12 deg)
http://thredds.axiomdatascience.com/thredds/dodsC/HYCOM.nc.html
- Naval Research Laboratory
- HYCOM provides comparable resolution to satellite based SST and salinity values
(1/12 degree resolution, or ~7 km spacing on average)

## Connecting surface and bottom temperatures 
- Maynard et al. 2016 Phil. Trans. R. Soc. <http://rstb.royalsocietypublishing.org/content/371/1689/20150208>, Used:
- NOAA PATHFINDER - the US NOAA official climate data record for SST (4-km)
used to that in combination with the World Oceans Atlas (WOA), which has
temperatures at various depths (0.25° resolution)
- Built a linear regression predicting bottom temperature from sea surface temperature

## World Oceans Atlas
https://www.nodc.noaa.gov/OC5/indprod.html
- Temperature, salinity, dissolved oxygen, percent oxygen saturation, …
- Temperature and salinity goes down to 0.25 degrees (for  5564, 6574, 7584, 8594,
95A4, A5B2, and ‘decav’ time spans) (i.e. all years up to 2012)
- Documentation: http://data.nodc.noaa.gov/woa/WOA13/DOC/woa13documentation.pdf
- Available for lots of depths: in 102 levels for annual data; 0-5500m
- Same for the seasonal; only down to 1500m and 57 levels for monthly data (i.e.
we should be able to get monthly data for depth relevant to rockfish)

## NOAA Pathfinder
http://www.nodc.noaa.gov/SatelliteData/pathfinder4km/
1981-2012
- Official Climate Data Record for SST for NOAA
- 4km resolution ?
i- Example statement in a paper:
In this study, we used AVHRR Pathfinder Version 5.2 (PFV5.2) data, obtained
from the US National Oceanographic Data Center and GHRSST
(http://pathfinder.nodc.noaa.gov). The PFV5.2 data are an updated version of
the Pathfinder Version 5.0 and 5.1 collection described in Casey et al. (2010).
If you use Pathfinder 4km data, please acknowledge the use of these data with
the following statement: "These data were provided by GHRSST and the US
National Oceanographic Data Center. This project was supported in part by
a grant from the NOAA Climate Data Record (CDR) Program for satellites".
and cite the following publication:
Casey, K.S., T.B. Brandon, P. Cornillon, and R. Evans (2010). "The Past, Present and Future of the AVHRR Pathfinder SST Program", in Oceanography from Space: Revisited, eds. V. Barale, J.F.R. Gower, and L. Alberotanza, Springer. DOI: 10.1007/978-90-481-8681-5_16.

## Group for High Resolution SST (GHRSST)
https://www.ghrsst.org/
Some example data sets:
Operational Sea Surface Temperature and Sea Ice Analysis (OSTIA): SST 0.05
degree (approximately 5km) daily (current only?)

## NEX (NASA Earth Exchange) Global Daily Downscaled Climate Projections
https://cds.nccs.nasa.gov/nex-gddp/
https://nex.nasa.gov/nex/projects/1356/
- Daily
- Each of the climate projections includes daily maximum temperature, minimum
temperature, and precipitation for the periods from 1950 through 2100. The
spatial resolution of the dataset is 0.25 degrees (~25 km x 25 km).
- derived from the General Circulation Model (GCM) runs conducted under the
Coupled Model Intercomparison Project Phase 5 (CMIP5) and across two of the
four greenhouse gas emissions scenarios known as Representative Concentration
Pathways (RCPs).
- total of 12TB / individual files 750MB
- I believe this is air temperature at the surface

## CMIP5
- http://journals.ametsoc.org/doi/abs/10.1175/BAMS-D-11-00094.1
- 100km scale?
- http://cmip-pcmdi.llnl.gov/cmip5/availability.html
- http://cmip-pcmdi.llnl.gov/index.html
- https://cran.r-project.org/web/packages/RCMIP5/index.html



## Other

http://podaac.jpl.nasa.gov/dataset/JPL_OUROCEAN-L4UHfnd-GLOB-G1SST
global sea surface temperature at 1km 2010-present

https://www.climate.gov/maps-data/datasets

World Oceans Analysis dataset

Regional Ocean Modeling System (ROMS)
http://www.myroms.org/

http://www.narccap.ucar.edu/about/aogcms.html

## Modeling steps:
1) Compare World Oceans Atlas to bottom trawl survey.   
2) Validate regression bottom temperature and surface temperature across datasets (World Oceans Atlas v Pathfinder or NEX)

