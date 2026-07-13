# SondePolishR

## SondePolishR 0.0.901

(2026-07-13)

### Bug Fixes

-   Updated the way flags were saved to not overwrite flags of the same type.

### Updates

-   Reworked the fDOM module to prevent users from correcting fDOM multiple times. Now you must correct temperature before being able to apply turbidity corrections and corrections will only be applied to data that hasn't had the correction (temperature or turbidity) applied previously.
-   Now when a `sondeproj` is loaded, the site metadata will be populated in the app so you can see/update the values.

## SondePolishR 0.0.900

(2026-07-09)

Initial publicly available version.
