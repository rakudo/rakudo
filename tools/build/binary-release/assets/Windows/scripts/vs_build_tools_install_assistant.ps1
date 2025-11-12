$url = "https://aka.ms/vs/stable/vs_BuildTools.exe"

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
    Start-Process -Wait -FilePath $installer -ArgumentList "--norestart --passive --wait --includeRecommended --add Microsoft.VisualStudio.Workload.VCTools"
}

function getInput($prompt) {
    Write-Host -NoNewline $prompt
    $Host.UI.ReadLine()
}

Write-Host @"
================================================================================
Welcome to Rakudos Visual Studio Build Tools installation assistant.
The installation
- needs approximately 9 GB of disk space
- downloads ~2.2 GB of data
- might take 10 minutes or longer with a 50 MBit/s connection
================================================================================

"@

$response = getInput "Shall we start? [Y] / N ? "
if (($response -ne '') -and ($response -ne 'y') -and ($response -ne 'Y')) {
    Write-Host "Aborting ... "
    exit
}

Write-Host -NoNewline "Downloading the installer stub (4.2 MB) ... "
$installer = download

Write-Host @"
done

================================================================================
We are about to start the graphical installer. After some dialogs, downloading
the real installer and more dialogs the actual download and installation will
start. Just be patient.
================================================================================

"@

Write-Host -NoNewline "Press Return to start. "
Read-Host

Write-Output "Starting installer ..."
runInstaller $installer

Write-Host @"

================================================================================
Installation finished.

To make use of the Build Tools in Rakudo execute $scriptPath\set-env.bat / ps1
in a CMD / Powershell window or if you used the MSI installer use the start
menu entries `Rakudo -> Rakudo CMD` / `Rakudo -> Rakudo Powershell`.
================================================================================

"@

Write-Host -NoNewline "Press Return to close this installation assistant. "
Read-Host