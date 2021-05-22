## Changelog 
This file contain notable changes of all the individual csv or xlsx files, `the raw data files`, made for the project. The steps used are common to all the files.

Version 1.0.0 (10-5-2021)
## New
- Added column to determine *day of week*.
- Added column to track *ride length (minutes)* and *ride length (hh:mm:ss)*.


## Changes
- Changed the result of the calculation of *ride length (minutes)* by multiplying to (24*60) to bring value unit into minutes.
- Changed the result format of *ride length (hh:mm:ss)* to HH:MM:SS.

## Fixes
- Fixed cases where *ride length* came less than zero, mean negative,by removing that observation.
- Fixed cases where *end station* name was **HQ QR** which means that ride ended at quality check station.
    
