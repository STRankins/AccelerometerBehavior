# AccelerometerBehavior
A package to classify moose, bighorn sheep, mule deer, and similar ungulate behavior from accelerometer data collected by GPS collars.

**To install from GitHub:**

(1) If you do not already have "devtools" installed, use 'install.packages("devtools")' to install.

(2) Load the "devtools" package with: 'library(devtools)'.

(3) Install the "AccelerometerBehavior" package from GitHub with: 'install_github("STRankins/AccelerometerBehavior/AccelerometerBehavior")'.

(4) Load package using 'library(AccelerometerBehavior)'.

_______________________________________________________________________
If you have:

**Activity Data (5 min. increment) from Vectronics GPS collars**
* Use 'AccelerometerBehavior()'
  * Species available: "moose", "bighorn sheep", "mule deer", or "ungulate"
 
**Activity Count Data (5 min. increment) from Telenoics GPS collars (mule deer only)**
* Use 'AccBehav()'
