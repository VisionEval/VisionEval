# Source this script to get going if you start R and
# navigate to the folder, instead of starting R from
# an .RData in this folder or by double-clicking
# "VisionEval-config.Rdata".  If you do a double
# click or set up a suitable shortcut, this will
# all happen auto-magically.

load("./RunVisionEval.RData")
if ( ! "package:visioneval" %in% search() ) .First()
