# QAQC Flags

When data is exported, each analyte will have an associated flag column
which is used to determine what changes were made to the data.

**The flag meanings are as follows:**

| App Module | Flag Code | Flag Meaning |
|:---|:---|:---|
| Interpolation | AD01 | Missing data interpolated. |
| Shift Corrections | CHG01 | Linear shift applied to selected points. |
| Shift Corrections | CHG02 | Linear drift correction applied to file. |
| fDOM Corrections | CHG03 | fDOM corrected for temperature. |
| fDOM Corrections | CHG04 | fDOM corrected for turbidity. |
| Quality Flags | QUAL01 | Data flagged as questionable. |
| Physical Limits | RM02 | Data removed based on absolute limits. |
| Outlier Removal | RM03 | Data removed based on outlier selection methods and user input. |
| Visualize | RM04 | OOW periods removed based on information from the field form. |
| Data Checks | DUP01 | Duplicated values averaged. |
| Data Checks | DUP02 | Duplicated values removed. |
