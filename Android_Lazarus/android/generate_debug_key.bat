REM Adjust these paths to yours
SET PATH=C:\Program Files (x86)\Android\android-sdk\tools;C:\Program Files (x86)\Android\android-sdk\platform-tools\;C:\Program Files\Java\jdk1.6.0_45\bin
SET APP_NAME=androidlcltest
SET ANDROID_HOME="C:\Program Files (x86)\Android\android-sdk"
SET APK_SDK_PLATFORM="C:\Program Files (x86)\Android\android-sdk\platforms\android-8"
SET APK_PROJECT_PATH="C:\Users\Krom\Desktop\Delphi\Doodles\Android_Test\android"

mkdir bin

keytool --help

keytool -genkey -v -keystore bin\LCLDebugKey.keystore -alias LCLDebugKey -keyalg RSA -validity 10000

REM call and pause together allow us to see the results in the end
pause
