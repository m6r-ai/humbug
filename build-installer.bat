@echo off
setlocal

echo === Building Humbug ===

echo.
echo [1/3] Building C extension...
python setup.py build_ext --inplace
if %ERRORLEVEL% neq 0 (
    echo ERROR: C extension build failed.
    exit /b 1
)

echo.
echo [2/3] Running PyInstaller...
pyinstaller humbug.spec --clean
if %ERRORLEVEL% neq 0 (
    echo ERROR: PyInstaller failed.
    exit /b 1
)

echo.
echo [3/3] Running Inno Setup compiler...

set ISCC=""
if exist "C:\Program Files (x86)\Inno Setup 6\ISCC.exe" set ISCC="C:\Program Files (x86)\Inno Setup 6\ISCC.exe"
if exist "C:\Program Files\Inno Setup 6\ISCC.exe" set ISCC="C:\Program Files\Inno Setup 6\ISCC.exe"

if %ISCC%=="" (
    echo ERROR: Inno Setup not found. Please install it from https://jrsoftware.org/isinfo.php
    exit /b 1
)

%ISCC% humbug-installer.iss
if %ERRORLEVEL% neq 0 (
    echo ERROR: Inno Setup compilation failed.
    exit /b 1
)

echo.
echo === Done! Installer saved to dist\Humbug-Setup.exe ===
endlocal
