# SondePolishR

## SondePolishR 0.0.9004

(2026-07-29)

### Bug Fixes

-   Added a warning when the "remove OOW periods" button is clicked but no field form data is provided so you know why it's not doing anything.

-   Doesn't allow export without a file path to prevent app from crashing.

-   Now exported datetimes don't drop the time for midnight (this causes issues when trying to read back into R).

-   Remaining bug in using quality flags to select outlier points left over from switching flagging methods.

-   Fixed bugs related to version control system preventing raw data from being plotted after data merges and fixing an issue where the diffs in changelog and actual diffs were one number off.

-   The second y-variable no longer resets whenever the data in the project changes.

-   Gaps are correctly identified now, previously was relying on just the DateTime which didn't produce any results after performing interpolation.

-   In shift selection, when selecting a new selection of points that contained points already selected, the old points were not selected.

-   Data versioning had a bug in applying diffs when they had data merges and you were trying to add/remove data from the merges, not just ignore them.

-   Interpolated points had a file name of "interpolated" this caused issues when coloring points by filename and doing drift corrections since those weren't selected with the rest of the file.

-   Random forest interpolation method failed because it was trying to include the flag columns.

### **Updates**

-   Unsummarized exported data will now condense duplicates and have the same formatting as other exports. Also added the site name to the exported data.
-   Flagging module now has more descriptive notes and button names so it's clearer what the "commit"/edit is doing.
-   Now "automated" point selection methods for outliers and interpolation will only select points within the plotted range. This is to prevent accidentally removing or filling in points you haven't reviewed.
-   Added a waiting indicator for loading precipitation data and switched the progress bar to a loading indicator for the interpolation step.

## SondePolishR 0.0.9003

(2026-07-28)

### Bug Fixes

-   Error when combing flags when removing out of water periods due to flagging all parameters.

-   Quality flags were erroring when trying to use to select points for outlier selection due to changes to the `get_qual_flags` function.

-   Fixed how the precipitation data was clipped to not remove a data point at the start or end of the dataset.

-   Default directory wasn't using the user's working directory, it was using the package directory, now will capture user's working directory on loading the app.

### **Updates**

-   Changed `read_sonde` function to also include depth (m) if available in the variables.
-   Major changes to how flags are stored within the `sondeproj` object. These changes won't affect app user experience at all, but now flags are stored within the dataset itself instead of a separate list so they can also be version controlled so when changes are redone/reverted the flags carry over.

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
