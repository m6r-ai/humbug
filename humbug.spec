# humbug.spec
#
# PyInstaller config file
#
import os
from PyInstaller.utils.hooks import collect_data_files
import certifi

codesign_id = os.environ['CODESIGN_IDENTITY'] if 'CODESIGN_IDENTITY' in os.environ else None

block_cipher = None

# Collect certifi's CA bundle
certifi_data = collect_data_files("certifi")

a = Analysis(
    ['src/humbug/__main__.py'],
    pathex=['src'],
    binaries=[],
    datas=certifi_data,
    hiddenimports=[],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=[
        'build',
        'dist',
        '*.egg-info'
    ],
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
    console=False,
    icon='icons\\Humbug.ico',
    target_arch=None,
    codesign_identity=codesign_id,
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
    name='Humbug'
)

app = BUNDLE(
    coll,
    name='Humbug.app',
    icon='icons/Humbug.icns',
    bundle_identifier='ai.m6r.humbug',
    info_plist={
        'CFBundleDisplayName': 'Humbug',
        'CFBundleShortVersionString': '0.25.0',
        'CFBundleVersion': '0.25.0',
        'NSPrincipalClass': 'NSApplication',
        'NSAppleScriptEnabled': False
    }
)
