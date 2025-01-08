# humbug.spec
#
# PyInstaller config file
#
block_cipher = None

a = Analysis(
    ['src/humbug/__main__.py'],  # Adjust this to your actual entry point
    pathex=[],
    binaries=[],
    datas=[],
    hiddenimports=[
        'PySide6.QtCore',
        'PySide6.QtGui',
        'PySide6.QtWidgets',
        'aiohttp',
        'qasync'
    ],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=['build', 'dist', '*.egg-info'],
    win_no_prefer_redirects=False,
    win_private_assemblies=False,
    cipher=block_cipher,
    noarchive=False,
)

pyz = PYZ(a.pure, a.zipped_data, cipher=block_cipher)

exe = EXE(
    pyz,
    a.scripts,
    [],
    exclude_binaries=True,
    name='humbug',
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    console=False,  # Set to False since this is a GUI application
    icon=''  # Add your icon if you have one
)

app = BUNDLE(
    exe,
    name='Humbug.app',
    icon=None,  # Add path to your .icns file if you have one
    bundle_identifier='com.yourdomain.humbug',  # Change this to your bundle identifier
    info_plist={
        'NSHighResolutionCapable': 'True'
    }
)

coll = COLLECT(
    exe,
    a.binaries,
    a.zipfiles,
    a.datas,
    strip=False,
    upx=True,
    upx_exclude=[],
    name='humbug'
)
