$OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encoding
$script_path = split-path -parent $MyInvocation.MyCommand.Definition
$bin_path = (Resolve-Path "$script_path\..\bin").Path
&"$bin_path\raku.exe"