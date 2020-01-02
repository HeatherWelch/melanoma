# melanoma

# environmentsl datasets

1. Mean annual cloud cover  
Source: https://www.earthenv.org/cloud  
resolution: 0.008333333, 0.008333333  
type: climatology, 2000-2014
relevant urls: https://www.earthenv.org/metadata/Cloud_DataDescription.pdf
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/MODCF_meanannual.tif
varname: MODCF_meanannual
other:  Valid values range from 0-10,000 and need to be multiplied by 0.01 to result in % cloudy days. Values greater than 10,000 are used for fill.

2. Within year seasonality of cloud cover
Source: https://www.earthenv.org/cloud
resolution: 0.008333333, 0.008333333
type: climatology, 2000-2014
relevant urls: https://www.earthenv.org/metadata/Cloud_DataDescription.pdf
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/MODCF_intraannualSD.tif
varname: MODCF_intraannualSD
other:  Valid values range from 0-10,000 and need to be multiplied by 0.01 to result in % cloudy days. Values greater than 10,000 are used for fill.

3. Mean temperature
Source: http://chelsa-climate.org/downloads/
resolution: 30 arc second
type: montly climatology, 1979-2013
relevant urls: http://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification.pdf
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/CHELSA_temp10_01_1979-2013_V1.2_land.tif (jan only, n=12)
varname: 
other:  By downloading the data you agree to cite the following peer reviewed article: (see http://chelsa-climate.org/downloads/)
other 2: units are degrees C/10

3. Temperature seasonality
Source: http://chelsa-climate.org/downloads/
resolution: 30 arc second
type: climatology, 1979-2013
relevant urls: http://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification.pdf
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/CHELSA_bio10_04.tif 
varname: 
other:  By downloading the data you agree to cite the following peer reviewed article: (see http://chelsa-climate.org/downloads/)
other 2: standard deviation of the monthly mean temperatures

4. Temperature annual range
Source: http://chelsa-climate.org/downloads/
resolution: 30 arc second
type: climatology, 1979-2013
relevant urls: http://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification.pdf
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/CHELSA_bio10_07.tif
varname: 
other:  By downloading the data you agree to cite the following peer reviewed article: (see http://chelsa-climate.org/downloads/)
other 2: The difference between the Maximum Temperature of Warmest month and the Minimum Temperature of Coldest month; units are degrees C/10

5. Latitude - NEEDS TO BE RECALCULATED AS COUNTY MEAN

6. Elevation - https://www.ngdc.noaa.gov/mgg/global/

7. County level UV Exposure data
Source: https://gis.cancer.gov/tools/uv-exposure/
resolution: county
type: climatology,1961-1990 
relevant urls: https://www.researchgate.net/profile/Zaria_Tatalovich/publication/228942287_A_comparison_of_thiessen-polygon_kriging_and_spline_models_of_UV_exposure/links/00463535078e0ab5e9000000.pdf
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/uv-county.xlsx
varname: 
other:  
other 2: 

8. other options: https://power.larc.nasa.gov/#resources
NASA PRediction of Worldwide Energy Resources, lots of solar stuff

9. Climatological erythemal UV dose of Whole year
Source: http://www.temis.nl/uvradiation/UVarchive/v1_GOME/uvclim.php?fb=uvdei
resolution: .5x.5 (i believe)
type: climatology, Aug. 1995 -- May 2003
relevant urls: http://www.temis.nl/uvradiation/UVarchive/v1_GOME/productinfo.html
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/uvdeiclimyear.hdf
varname: 
other:  The daily UV dose, usually given in kJ/m2, is the "amount" of UV radiation that reached the earth, taking cloud cover into account, which means that it can only be computed afterwards.
other 2: The erythemal action spectrum. This is a model for the susceptibility of the caucasian skin to sunburn (erythema). It is proposed by McKinlay & Diffey (1987) and adopted as a standard by the Commission Internationale de l'Éclairage (International Commission on Illumination, CIE).

10. Climatological DNA-damage UV dose of Whole year
Source: http://www.temis.nl/uvradiation/UVarchive/v1_GOME/uvclim.php?fb=uvddi
resolution: .5x.5 (i believe)
type: climatology, Aug. 1995 -- May 2003
relevant urls: http://www.temis.nl/uvradiation/UVarchive/v1_GOME/productinfo.html
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/uvddiclimyear.hdf
varname: 
other:  The daily UV dose, usually given in kJ/m2, is the "amount" of UV radiation that reached the earth, taking cloud cover into account, which means that it can only be computed afterwards.
other 2: DNA-damage. The action spectrum which describes the effect of UV radiation on DNA in the human skin, proposed by R. B. Setlow (1974).

11. Climatological erythemal UV index of Whole year
Source: http://www.temis.nl/uvradiation/UVarchive/v1_GOME/uvclim.php?fb=uviec
resolution: .5x.5 (i believe)
type: climatology, Aug. 1995 -- May 2003
relevant urls: http://www.temis.nl/uvradiation/UVarchive/v1_GOME/productinfo.html
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/uviecclimyear.hdf
varname: 
other:  The clear-sky UV index is the effective UV irradiance (1 unit equals 25 mW/m2) reaching the Earth's surface under clear-sky conditions given for local solar noon, when the Sun is highest in the sky. The UV index is computed from the assimilated global ozone field at local solar noon. With the use of forecast meteorological fields of the ECMWF and data assimilation, KNMI provides a forecast of the ozone fields. Hence, UV index forecasts for today and four days ahead can be made.
other 2: The erythemal action spectrum. This is a model for the susceptibility of the caucasian skin to sunburn (erythema). It is proposed by McKinlay & Diffey (1987) and adopted as a standard by the Commission Internationale de l'Éclairage (International Commission on Illumination, CIE).

12. Climatological DNA-damage UV index of Whole year
Source: http://www.temis.nl/uvradiation/UVarchive/v1_GOME/uvclim.php?fb=uvidc
resolution: .5x.5 (i believe)
type: climatology, Aug. 1995 -- May 2003
relevant urls: http://www.temis.nl/uvradiation/UVarchive/v1_GOME/productinfo.html
savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/uvidcclimyear.hdf
varname: 
other:  The clear-sky UV index is the effective UV irradiance (1 unit equals 25 mW/m2) reaching the Earth's surface under clear-sky conditions given for local solar noon, when the Sun is highest in the sky. The UV index is computed from the assimilated global ozone field at local solar noon. With the use of forecast meteorological fields of the ECMWF and data assimilation, KNMI provides a forecast of the ozone fields. Hence, UV index forecasts for today and four days ahead can be made.
other 2: DNA-damage. The action spectrum which describes the effect of UV radiation on DNA in the human skin, proposed by R. B. Setlow (1974).
