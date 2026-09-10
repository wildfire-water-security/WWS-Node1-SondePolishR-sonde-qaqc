# Using Precipitation Data

Precipitation data is particularly helpful when correcting turbidity and
fDOM data to determine if peaks are real or not. **To add precipitation
data within the app there are three options:**

1.  Provide a site latitude and longitude and download hourly
    precipitation data from
    [Merra-2](https://gmao.gsfc.nasa.gov/gmao-products/merra-2/) (uses
    the `get_precip` function). This dataset is available across a
    global scale at a resolution of 0.5 x 0.625 degrees available from
    1981 to near real time.

2.  Provide a site latitude and longitude and download hourly
    precipitation data from [NLDAS](https://ldas.gsfc.nasa.gov/nldas)
    (uses the `get_precip` function). This dataset is available across
    CONUS at a resolution of 0.125 × 0.125 degrees available from 1981
    to near real time. This data requires a token to access the data.
    See
    [here](https://urs.earthdata.nasa.gov/documentation/for_users/user_token)
    for directions on creating a token. Note that this token should be
    kept secret.

3.  Provide your own precipitation data as a `.csv` file.

## Obtaining a Token for Downloading NLDAS Data

To download the NLDAS hourly precipitation data you need an [EarthData
token](https://urs.earthdata.nasa.gov/documentation/for_users/user_token).

1.  Go to: <https://urs.earthdata.nasa.gov/>.

2.  Register for a profile if you don’t already have one.

3.  Log in, and select **Generate Token**.

4.  Click the green **Generate Token** button at the bottom of the page.

5.  Securely save the token by running the following code:

    `{r} usethis::edit_r_environ(scope = "user")}`

6.  Paste the text: `EARTHDATA_TOKEN = "TOKEN HERE"`

7.  Save and close the file. Restart R. Now the token should pull into
    the app automatically when you run it, but will remain privately on
    your computer.

## Using User Precipitation Data

Gridded precipitation products are only so good. If you have
observational data at or near your sonde site, this will provide a more
accurate representation of conditions. To input your own data it needs
to be formatted similar to `example_precip`, as a .csv file with two
columns:

- **DateTime**: The date and time of the observation.
- **Precip_mm_hr**: The precipitation value for that time period in mm.
  It should work to use a non-hourly as long as the data matches the
  timestamps, however especially at a courser resolution the plots may
  not look as expected.
