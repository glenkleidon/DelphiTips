# DelphiTips
Useful Delphi Units which provide full functionality in a single PAS file.

## Mini Test Framework
The [Mini Test framework](https://github.com/glenkleidon/DelphiTips/wiki/Mini-Test-Framework) is a light-weight unit testing framework which simply requires that you
include the single MiniTestFrameWork.pas in a console app, and start testing your code.
Check out the [Blog here](https://glenkleidon.blogspot.com.au/2017/07/new-mini-test-framework-for-delphi.html)

### Updated Support
Now Supports All versions of Delphi back to Delphi 7 (and probably 4).  This has been tested on:

|Version  |Version     |Version    |Version    |Version     |       |
|---------|-----------:|:---------:|:---------:|:----------:|:------|
|4 &#9744;|2005 &#9744;|XE  &#9745;|XE6 &#9744;|10.0 Seattle|&#9744;|
|5 &#9744;|2006 &#9744;|XE2 &#9745;|XE7 &#9744;|10.1 Berlin |&#9745;|
|6 &#9744;|2007 &#9745;|XE3 &#9744;|XE8 &#9744;|10.2 Tokyo  |&#9744;|
|7 &#9745;|2009 &#9745;|XE4 &#9744;|           |            |       |
|8 &#9744;|2010 &#9745;|XE5 &#9745;|           |            |       |

If using versions of Delphi 2005 or 2006, You may have trouble with the 
2007 DPROJ file in the Test Unit and the Project Template folders.  IF you 
do have trouble, simply remove the DPROJ file and get Delphi to rebild from the
DPR file.

### Added Features: 
  + Test Run support
  + Set level Skipping
  + Set level Expected Exceptions
  
See the updated wiki!

