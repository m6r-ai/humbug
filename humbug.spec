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
    hiddenimports=[],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=['build', 'dist', '*.egg-info'],
    win_no_prefer_redirects=False,
    win_private_assemblies=False,
    cipher=block_cipher,
    noarchive=False
)

pyz = PYZ(a.pure, a.zipped_data, cipher=block_cipher)

exe = EXE(
    pyz,
    a.scripts,
    [],
    exclude_binaries=True,
    name='Humbug',
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    console=False,  # Set to False since this is a GUI application
    target_arch=None,
    codesign_identity=None,
    entitlements_file=None
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

app = BUNDLE(
    coll,
    name='Humbug.app',
    icon=None,  # Add path to your .icns file if you have one
    bundle_identifier='ai.m6r.humbug',
    info_plist={
        'NSPrincipalClass': 'NSApplication',
        'NSAppleScriptEnabled': False
    }
)
