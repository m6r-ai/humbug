# humbug.spec
#
# PyInstaller config file
#
import os
import glob
import sys
from PyInstaller.utils.hooks import collect_data_files
import certifi

codesign_id = os.environ['CODESIGN_IDENTITY'] if 'CODESIGN_IDENTITY' in os.environ else None

block_cipher = None

# On Windows, explicitly collect the in-place compiled Menai C extension (.pyd).
# PyInstaller does not reliably auto-discover extensions built inplace outside
# the main package tree, so we glob for it and pass it via binaries.
if sys.platform == 'win32':
    _pyd_files = glob.glob('src/menai/menai_vm_c*.pyd')
    _extra_binaries = [(_pyd, 'menai') for _pyd in _pyd_files]
else:
    _extra_binaries = []

# Collect certifi's CA bundle
certifi_data = collect_data_files("certifi")

a = Analysis(
    ['src/humbug/__main__.py'],
    pathex=['src'],
    binaries=_extra_binaries,
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

if sys.platform == 'win32':
    # Single-file exe for Windows — no _internal folder needed
    exe = EXE(
        pyz,
        a.scripts,
        a.binaries,
        a.zipfiles,
        a.datas,
        exclude_binaries=False,
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
else:
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
        icon='icons/Humbug.icns',
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

if sys.platform != 'win32':
    app = BUNDLE(
        coll,
        name='Humbug.app',
        icon='icons/Humbug.icns',
        bundle_identifier='ai.m6r.humbug',
        info_plist={
            'CFBundleDisplayName': 'Humbug',
            'CFBundleShortVersionString': '44',
            'CFBundleVersion': '44',
            'NSPrincipalClass': 'NSApplication',
            'NSAppleScriptEnabled': False
        }
    )
