$url = "https://download.visualstudio.microsoft.com/download/pr/02aebac1-9464-4473-9af5-710a97b8f023/92c237448ec5563948a83f2f9e01d3050755a15eb473c9e7ffefd735bf7474f1/vs_BuildTools.exe"

$scriptPath = split-path -parent $MyInvocation.MyCommand.Definition

function getTmpDir {
    $parent = [System.IO.Path]::GetTempPath()
    [string] $name = [System.Guid]::NewGuid()
    return New-Item -ItemType Directory -Path (Join-Path $parent $name)
}

function download {
    $outDir = getTmpDir
    $output = "$outDir\vs_BuildTools.exe"
    Invoke-WebRequest -Uri $url -OutFile $output
    return $output
}

function runInstaller($installer) {
    Start-Process -Wait -FilePath $installer -ArgumentList "--norestart --wait --includeRecommended --add Microsoft.VisualStudio.Workload.VCTools"
    #--quiet --installPath="C:\raku_vs_buildtools"
}

function getInput($prompt) {
    Write-Host -NoNewline $prompt
    $Host.UI.ReadLine()
}

Write-Host @"
================================================================================
Welcome to Rakudos Visual Studio Build Tools 2019 installation assistant.
The installation
- needs approximately 4.5 GB of disk space
- downloads ~1.2 GB of data
- might take 30 minutes or longer with a 10 MBit/s connection
================================================================================

"@

$response = getInput "Shall we start? [Y] / N ? "
if (($response -ne '') -and ($response -ne 'y') -and ($response -ne 'Y')) {
    Write-Host "Aborting ... "
    exit
}

Write-Host -NoNewline "Downloading the installer stub (1.3 MB) ... "
$installer = download

Write-Host @"
done

================================================================================
We are about to start the graphical installer. After some dialogs, downloading
the actuall installer and more dialogs you will be presented with a component
selection interface. The required components will already be selected. Just
click the "Install" button on the bottom right. You are free to select
additional components or change the installation location.
================================================================================

"@

Write-Host -NoNewline "Press Return to start. "
Read-Host

Write-Output "Starting installer ..."
runInstaller $installer

Write-Host @"

================================================================================
Installation finished.
To make use of the Build Tools in Rakudo using CMD:
- start a CMD window using
    Start Menu -> Visual Studio 2019 ->
    x86 / x64 Native Tools Command Prompt for VS 2019
- Execute $scriptPath\set-env.bat

To make use of the Build Tools in Rakudo using PowerShell:
- start a PowerShell window using
    Start Menu -> Visual Studio 2019 -> Developer PowerShell for VS 2019
- Execute $scriptPath\set-env.ps1
================================================================================

"@

Write-Host -NoNewline "Press Return to close this installation assistant. "
Read-Host