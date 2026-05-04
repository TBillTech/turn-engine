param(
    [string]$OutputDir = "dist\\release"
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$absoluteOutputDir = Join-Path $repoRoot $OutputDir
$stageDir = Join-Path $absoluteOutputDir "package"
$docsSourceDir = Join-Path $repoRoot "docs"
$jsonExamplesDir = Join-Path $docsSourceDir "jsonexamples"
$checksumFileName = "jsonexamples.sha256"
$checksumDigestFileName = "jsonexamples.sha256.sha256"

function Invoke-Step {
    param(
        [string]$Name,
        [scriptblock]$Action
    )

    Write-Host "==> $Name"
    & $Action
}

function Invoke-CabalCommand {
    param(
        [string[]]$Arguments
    )

    & cabal @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "Command failed: cabal $($Arguments -join ' ')"
    }
}

function Invoke-ExampleValidation {
    param(
        [string]$RequestFile,
        [string]$ResponseFile
    )

    Invoke-CabalCommand @(
        "run",
        "turn-engine",
        "--",
        "--test-example",
        $RequestFile,
        $ResponseFile
    )
}

function Write-JsonExamplesChecksumFile {
    param(
        [string]$SourceDir,
        [string]$DestinationFile
    )

    $hashLines = Get-ChildItem -Path $SourceDir -File -Recurse |
        Sort-Object FullName |
        ForEach-Object {
            $relativePath = $_.FullName.Substring($SourceDir.Length + 1).Replace('\', '/')
            $hash = (Get-FileHash -Path $_.FullName -Algorithm SHA256).Hash.ToLowerInvariant()
            "$hash  $relativePath"
        }

    Set-Content -Path $DestinationFile -Value $hashLines
}

function Write-ChecksumDigestFile {
    param(
        [string]$SourceFile,
        [string]$DestinationFile
    )

    $sourceFileName = Split-Path -Leaf $SourceFile
    $hash = (Get-FileHash -Path $SourceFile -Algorithm SHA256).Hash.ToLowerInvariant()
    Set-Content -Path $DestinationFile -Value "$hash  $sourceFileName"
}

function Get-ExecutableVersion {
    param(
        [string]$ExecutablePath
    )

    $version = (& $ExecutablePath --version).Trim()
    if ($LASTEXITCODE -ne 0) {
        throw "Command failed: $ExecutablePath --version"
    }

    if ([string]::IsNullOrWhiteSpace($version)) {
        throw "Executable version output was empty."
    }

    return $version
}

Push-Location $repoRoot
try {
    Invoke-Step "Build" {
        Invoke-CabalCommand @("build")
    }

    Invoke-Step "Test" {
        Invoke-CabalCommand @("test")
    }

    Invoke-Step "Validate selected example pairs" {
        Invoke-ExampleValidation "docs/jsonexamples/CursedTreasure/get-game-setup-players.request.json" "docs/jsonexamples/CursedTreasure/get-game-setup-players.response.json"
        Invoke-ExampleValidation "docs/jsonexamples/CursedTreasure/create-new-game.request.json" "docs/jsonexamples/CursedTreasure/create-new-game.response.json"
        Invoke-ExampleValidation "docs/jsonexamples/CursedTreasure/make-move-clue-play.request.json" "docs/jsonexamples/CursedTreasure/make-move-clue-play.response.json"
        Invoke-ExampleValidation "docs/jsonexamples/CursedTreasure/make-move-raise-treasure.request.json" "docs/jsonexamples/CursedTreasure/make-move-raise-treasure.response.json"
    }

    $exePath = (cabal list-bin exe:turn-engine).Trim()
    if (-not (Test-Path $exePath)) {
        throw "Could not find built executable at '$exePath'."
    }

    $version = Get-ExecutableVersion -ExecutablePath $exePath
    $zipFileName = "turn-engine-release-$version.zip"
    $zipPath = Join-Path $absoluteOutputDir $zipFileName

    if (Test-Path $stageDir) {
        Remove-Item $stageDir -Recurse -Force
    }

    if (-not (Test-Path $absoluteOutputDir)) {
        New-Item -ItemType Directory -Path $absoluteOutputDir -Force | Out-Null
    }

    if (Test-Path $zipPath) {
        Remove-Item $zipPath -Force
    }

    New-Item -ItemType Directory -Path $stageDir -Force | Out-Null
    Copy-Item $exePath -Destination (Join-Path $stageDir "turn-engine.exe")
    Copy-Item $docsSourceDir -Destination (Join-Path $stageDir "docs") -Recurse

    Invoke-Step "Write jsonexamples checksum manifest" {
        $checksumFilePath = Join-Path $stageDir $checksumFileName
        $checksumDigestFilePath = Join-Path $stageDir $checksumDigestFileName

        Write-JsonExamplesChecksumFile -SourceDir $jsonExamplesDir -DestinationFile $checksumFilePath
        Write-ChecksumDigestFile -SourceFile $checksumFilePath -DestinationFile $checksumDigestFilePath
    }

    Compress-Archive -Path (Join-Path $stageDir "*") -DestinationPath $zipPath -Force
    Write-Host "Created $zipPath"
}
finally {
    Pop-Location
}