# SondePolishR

## SondePolishR 0.0.9002

(2026-07-20)

### Bug Fixes

-   When toggling the period view, the period count wasn't getting reset resulting in issues switching between the periods.
-   When data was re-uploaded, some of the point selections (shift correction) weren't clearing resulting in points still selected from the start.
-   Precipitation data timezone were inconsistent and not matching the data timezone correctly.
-   There was a bug causing the app to crash when using the clear uploads button.
-   Made plots shorter to be easier to view on a less wide screen.

### Updates

-   Added a new method to obtain precipitation data (NLDAS) which is at a finer resolution and appears to be more accurate. However this data requires a token and NASA Earthdata account to download. See documentation for [`get_precip()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/get_precip.html) for more details.
-   Now if site code, latitude and longitude are saved within a project they will show up in the UI when project is loaded.
-   Now when marking data quality there is the option to either mark as "bad" or "questionable".
-   Changed the sidebars to use accordions so it's easier to view the options you want to view.
-   Added a feature in "Explore Data" allowing you to revert your changes and return the data to a previous version.

## SondePolishR 0.0.9001

(2026-07-13)

### Bug Fixes

-   Updated the way flags were saved to not overwrite flags of the same type.

### Updates

-   Reworked the fDOM module to prevent users from correcting fDOM multiple times. Now you must correct temperature before being able to apply turbidity corrections and corrections will only be applied to data that hasn't had the correction (temperature or turbidity) applied previously.
-   Now when a `sondeproj` is loaded, the site metadata will be populated in the app so you can see/update the values.

## SondePolishR 0.0.900

(2026-07-09)

Initial publicly available version.
