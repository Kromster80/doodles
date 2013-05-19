REM Adjust these paths to yours
SET PATH="C:\Program Files (x86)\Android\android-sdk\tools";"C:\Program Files (x86)\Android\android-sdk\platform-tools\";"C:\Program Files\Java\jdk1.6.0_45\bin"

adb uninstall com.pascal.lcltest
adb install bin\androidlcltest.apk

pause
