# SondePolishR 0.0.9010

(2026-09-12)

## Bug Fixes

-   (#103) Fixed the username still being hard-coded to the users computer username when loading data for the first time. The app now requires you manually enter a value before loading data.
-   (#100) Fixed a bug when loading data with an older version of dplyr causing by not importing a function into the package.
-   (#76) Figured out why the NLDAS precipitation wasn't working on other computers. Was due to a permissions that I had forgotten needed to be changed. Updated documentation to reflect this.
-   (#98) Added protections to restoring versions as clicking the button while on the last row would cause a fatal crash
-   (#101) Discovered that WebGL didn't appear to work when running the app from Firefox. Updated the code to detect if it can be used or not to display the plots regardless. Also added documentation recommending running from Chrome.
-   (#99) Fixed the file encoding so that raw data files will read in correct on a Mac.

## Updates

-   (#110) A popup alert will now show when precipitation data has been successfully loaded to improve clarity of if data has been uploaded.
-   (#106) The filepaths for exporting data in the **Download Data** module will now remain even with changes to the `sondeproj`. Additionally, the save directory will attempt to be maintained, this is particularly useful for exporting data as now the filename will change, but if you are happy with the default you don't have to reselect the save location.
-   (#109) Reworked the way that changes were saved when the period view is on.
    -   If you're in period view you will be prompted to either return to the full view or only commit the changes within the plot.
    -   If you choose to return to the full view you will have to re-click the save edits button.
-   (#104) Updated all the `numericInput` buttons to use the `blur` option which means that they will wait till you click off the button before updating the app. This makes plotting less laggy if you're clicking through numbers via the arrows.
-   (#105) Updated several UI boxes to fit better in screens that are less wide.
-   (#108) Changed the wording of the selection tools in **Remove Outliers** tab to be more clear that it only applies to the manual selection.
-   (#111) Reworked the way that out of water periods are handled. This allows the user to exclude periods that are supposed be out of water but don't appear problematic or add removal/return time after looking at the data to prevent removing large sections of data.
    -   Now the calibration check and field form tables within the **Visualize** tab are editable.
    -   You can also now export these tables in the **Download Data** tab.
    -   The default plot also will show the out of water periods by default.
-   Added a workflow diagram to the **Correction Workflow** article.

# SondePolishR 0.0.9009

(2026-09-10)

## Bug Fixes

-   In the interpolation tab, the period navigation buttons were shifted downward.
-   Median and mean functions were not ignoring `NA` values, causing weird placing of points when filling.
-   The y-min limit wasn't updating in the interpolation tab with y-max limit changes.
-   The auto-guess for the drift corrections wouldn't automatically update if you switched to drift, switched away, and switched back.
-   Fixed a warning when trying to plot when there was no data within the range.
-   Fixed a bug with the shift correction when selecting an area including the first point in the data causing a fatal crash.

## Updates

-   The **hampel** filter method in identifying outliers was adjusted to ignore regions where the median deviation is 0.
-   Adjusted how the **k** parameter was used in outlier detection. Previously due to the way the function worked, the percent change and hampel methods were using twice the window than the high variability method. Also adjusted the numeric input so that if an even value is provided it returns an odd value.
-   The corrections tab was placed before the interpolation tab since this step should typically occur first.
-   Updated the interpolation code to not have to run the `apply_interp` step whenever the period changes.
-   Removed the Savitzky–Golay filter smoothing method as it was not working well for the data.
-   Now when data or a project is loaded the app will fill in any missing `datetimes` within the data so the rows/indices are consistent and don't change during the interpolation step.
-   You can now customize the analyst name used for the changlog. It previously defaulted to you computer username.
-   In the **Check Data** tab you can now select which parameter you want to view for the duplicate plots.
-   Added an article (available on the [website](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/articles/correction-steps.html)) with a preliminary workflow.
-   Updated the app manual to reflect the current workflow.
-   Updated the token space in the **Load Data** tab to pull automatically if the token is stored an a variable under the R environment.

# SondePolishR 0.0.9008

(2026-08-27)

## Bug Fixes

-   When the period length changes, the period being viewed resets to 0 to prevent being in a date range that's outside the data.
-   Actually fixed the issue where selecting a point twice wouldn't trigger the expected action.
-   The summary table in the **Visualize** tab now shows data summaries from the data within the plotting view. However, the summary table will **not** respond to changes in the zoom state.
-   Fixed a bug that allowed you to apply edits to data that were not within the set date range. This was problematic as you could accidentally remove points that you hadn't reviewed.
-   Updated several documentation pages to use markdown styling.

## Updates

-   The **quality flags** tab was removed and the functionality was combined with the **identify outliers** tab. This was done to encourage starting with the automated methods of identifying outliers to make point removal more standardized and make the process faster.
-   A new method was added to identify outliers, "high variability", see `identify_outliers` for details on this method.
-   The default threshold for identifying outliers was changed from 5 to 7 to prevent false positives.

# SondePolishR 0.0.9007

(2026-08-17)

## Bug Fixes

-   Fixed the News page headers so they actually show up on the website.

## Updates

-   Add several smoothing methods via `apply_smoothing()` which can be used to smooth periods of messy data.

# SondePolishR 0.0.9006

(2026-08-10)

## Bug Fixes

-   Fixed issues with when zoom was held versus reset.

# SondePolishR 0.0.9005

(2026-08-05)

## Bug Fixes

-   Can no longer select rows in the other table options within check data module. This allowed you to unintentionally view past data versions.

-   Bug in interpolation code to fill in flags was causing the first flag of the data to apply to the entire dataset.

-   Fixed a number of bugs with how colors were displaying in the plot, particularly when using file colors along with other color schemes.

-   Selected points now clears in the quality flags module when switching the method.

-   App no longer crashes if precipitation data can't be fetched.

-   Issue with flags added to existing flags was fixed.

## Updates

-   Now in the quality flags module, visualizing those flags starts selected so you don't have to remember to select it to view the flags you applied after flagging.

-   File names for saving files now clear when data is cleared.

-   Metadata files can now recognize missing values even if they're not quite how R expects them.

-   Site coordinates are now stored if specified before the project is loaded.

-   Exported the `get_nldas` function.

-   When saving files, a confirmation message will display if the file already exists.

-   When switching between quality flag methods, default selection method switches back to "add".

-   All main plots now have inputs to control the limits of the y-axis. They default to range of the overall dataset and refresh when the project changes or the variable being plotted changes. This is useful to not zoom in on small changes during periods with low variability.

-   You can now select multiple summary method options when exporting the data. Column names are appended with the summary function and the selected methods all show up on the plot. Summary methods are also extended to the "interval" selection so you can choose how duplicates are dealt with and view differences between methods.

-   Zoom will now hold even when the plot changes. It will reset if new data is loaded, you change the y-variable, or you manually change the y-axis limits. Selection modules also no longer default to the selection tool, they will all start using the zoom tool.

# SondePolishR 0.0.9004

(2026-07-29)

## Bug Fixes

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

## **Updates**

-   Unsummarized exported data will now condense duplicates and have the same formatting as other exports. Also added the site name to the exported data.
-   Flagging module now has more descriptive notes and button names so it's clearer what the "commit"/edit is doing.
-   Now "automated" point selection methods for outliers and interpolation will only select points within the plotted range. This is to prevent accidentally removing or filling in points you haven't reviewed.
-   Added a waiting indicator for loading precipitation data and switched the progress bar to a loading indicator for the interpolation step.

# SondePolishR 0.0.9003

(2026-07-28)

## Bug Fixes

-   Error when combing flags when removing out of water periods due to flagging all parameters.

-   Quality flags were erroring when trying to use to select points for outlier selection due to changes to the `get_qual_flags` function.

-   Fixed how the precipitation data was clipped to not remove a data point at the start or end of the dataset.

-   Default directory wasn't using the user's working directory, it was using the package directory, now will capture user's working directory on loading the app.

## **Updates**

-   Changed `read_sonde` function to also include depth (m) if available in the variables.
-   Major changes to how flags are stored within the `sondeproj` object. These changes won't affect app user experience at all, but now flags are stored within the dataset itself instead of a separate list so they can also be version controlled so when changes are redone/reverted the flags carry over.

# SondePolishR 0.0.9002

(2026-07-20)

## Bug Fixes

-   When toggling the period view, the period count wasn't getting reset resulting in issues switching between the periods.
-   When data was re-uploaded, some of the point selections (shift correction) weren't clearing resulting in points still selected from the start.
-   Precipitation data timezone were inconsistent and not matching the data timezone correctly.
-   There was a bug causing the app to crash when using the clear uploads button.
-   Made plots shorter to be easier to view on a less wide screen.

## Updates

-   Added a new method to obtain precipitation data (NLDAS) which is at a finer resolution and appears to be more accurate. However this data requires a token and NASA Earthdata account to download. See documentation for [`get_precip()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/get_precip.html) for more details.
-   Now if site code, latitude and longitude are saved within a project they will show up in the UI when project is loaded.
-   Now when marking data quality there is the option to either mark as "bad" or "questionable".
-   Changed the sidebars to use accordions so it's easier to view the options you want to view.
-   Added a feature in "Explore Data" allowing you to revert your changes and return the data to a previous version.

# SondePolishR 0.0.9001

(2026-07-13)

## Bug Fixes

-   Updated the way flags were saved to not overwrite flags of the same type.

## Updates

-   Reworked the fDOM module to prevent users from correcting fDOM multiple times. Now you must correct temperature before being able to apply turbidity corrections and corrections will only be applied to data that hasn't had the correction (temperature or turbidity) applied previously.
-   Now when a `sondeproj` is loaded, the site metadata will be populated in the app so you can see/update the values.

# SondePolishR 0.0.900

(2026-07-09)

Initial publicly available version.
