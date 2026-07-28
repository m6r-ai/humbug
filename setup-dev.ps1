# Bootstrap a Humbug developer environment on Windows (PowerShell): creates
# the venv, installs Humbug + dev dependencies, clones/installs the sibling
# Menai repo, and fetches the prebuilt Menai C VM binary.
#
# Usage: .\setup-dev.ps1
# Safe to re-run: existing venv/menai checkout are reused, not recreated.

$ErrorActionPreference = "Stop"

# $ErrorActionPreference only catches PowerShell-native errors, not a
# non-zero exit code from an external command (pip, git, python) - check
# explicitly after each critical step so failures stop the script here
# rather than silently cascading, matching bash's `set -e`.
function Assert-Success {
    param([string]$Message)
    if ($LASTEXITCODE -ne 0) {
        Write-Error $Message
        exit $LASTEXITCODE
    }
}

Set-Location -Path $PSScriptRoot

$VenvDir = "venv"
$MenaiDir = "..\menai"

$PythonCmd = Get-Command python -ErrorAction SilentlyContinue
if (-not $PythonCmd) {
    $PythonCmd = Get-Command py -ErrorAction SilentlyContinue
}
if (-not $PythonCmd) {
    Write-Error "No 'python' or 'py' command found on PATH. Install Python 3.10+ first."
    exit 1
}
$Python = $PythonCmd.Name

Write-Host "==> Python virtual environment"
if (Test-Path $VenvDir) {
    Write-Host "    $VenvDir already exists, reusing it"
} else {
    & $Python -m venv $VenvDir
    Assert-Success "Failed to create virtual environment."
}

$ActivateScript = Join-Path $VenvDir "Scripts\Activate.ps1"
& $ActivateScript

pip install --upgrade pip
Assert-Success "Failed to upgrade pip."

Write-Host "==> Menai language engine"
# Cloned and installed in the same pip invocation as Humbug below: humbug's
# pyproject.toml requires "menai", which isn't on PyPI, so both editable
# installs must be resolved together in one command.
if (Test-Path $MenaiDir) {
    Write-Host "    $MenaiDir already exists, reusing it"
} else {
    git clone https://github.com/m6r-ai/menai.git $MenaiDir
    Assert-Success "Failed to clone the Menai repository."
}

Write-Host "==> Installing Menai + Humbug (runtime + dev dependencies)"
pip install -e $MenaiDir -e ".[dev]"
Assert-Success "Failed to install Menai/Humbug dependencies."

Write-Host "==> Fetching prebuilt Menai C VM binary"
python fetch-menai-vm.py
if ($LASTEXITCODE -ne 0) {
    Write-Host "    No prebuilt binary available for this platform."
    Write-Host "    Falling back to building from source (requires a C compiler):"
    Push-Location $MenaiDir
    try {
        python setup.py build_ext --inplace
    } finally {
        Pop-Location
    }
}

Write-Host ""
Write-Host "Setup complete. Activate the environment with:"
Write-Host "    $ActivateScript"
Write-Host "Then launch Humbug with:"
Write-Host "    python -m desktop"
