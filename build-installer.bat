@echo off
setlocal

echo === Building Humbug ===

echo.
echo [1/4] Extracting version...
for /f "tokens=2 delims==" %%a in ('findstr "CURRENT_VERSION" src\desktop\version.py') do set VERSION=%%a
set VERSION=%VERSION: =%
if "%VERSION%"=="" (
    echo ERROR: Could not extract version from src\desktop\version.py
    exit /b 1
)
echo Version: %VERSION%

echo.
echo [2/4] Building C extension...
python setup.py build_ext --inplace
if %ERRORLEVEL% neq 0 (
    echo ERROR: C extension build failed.
    exit /b 1
)

echo.
echo [3/4] Running PyInstaller...
pyinstaller humbug.spec --clean
if %ERRORLEVEL% neq 0 (
    echo ERROR: PyInstaller failed.
    exit /b 1
)

echo.
echo [4/4] Running Inno Setup compiler...

set ISCC=""
if exist "C:\Program Files (x86)\Inno Setup 6\ISCC.exe" set ISCC="C:\Program Files (x86)\Inno Setup 6\ISCC.exe"
if exist "C:\Program Files\Inno Setup 6\ISCC.exe" set ISCC="C:\Program Files\Inno Setup 6\ISCC.exe"

if %ISCC%=="" (
    echo ERROR: Inno Setup not found. Please install it from https://jrsoftware.org/isinfo.php
    exit /b 1
)

%ISCC% /dMyAppVersion=%VERSION% humbug-installer.iss
if %ERRORLEVEL% neq 0 (
    echo ERROR: Inno Setup compilation failed.
    exit /b 1
)

echo.
echo === Done! Installer saved to dist\Humbug-v%VERSION%-windows-x86_64.exe ===
endlocal
