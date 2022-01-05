param ([string]$rakudo_version, [string]$rakudo_release_dir, [string]$out_file)

$msi_dir = split-path -parent $MyInvocation.MyCommand.Definition

$work_dir = "$msi_dir\work"
$asset_dir = "$msi_dir\assets"
$installer_asset_dir = "$msi_dir\installer-assets"
$wix_dir = [Environment]::GetEnvironmentVariable('WIX', 'Machine') + "bin"
$work_rakudo_release_dir = "$work_dir\rakudo_release_files"

Remove-Item -ErrorAction Ignore -Recurse $work_rakudo_release_dir
Copy-Item -Path $rakudo_release_dir -Destination $work_rakudo_release_dir –Recurse
Remove-Item "$work_rakudo_release_dir\README.txt"

&"$wix_dir\heat.exe" dir $work_rakudo_release_dir -dr RAKUDO_BIN_DIR_REFS -cg FileComponents -gg -g1 -sfrag -srd -suid -ke -sw5150 -var "var.RAKUDO_BIN_FILES_DIR" -out "$work_dir\rakudo_bin_files.wxs"
&"$wix_dir\heat.exe" dir $asset_dir -dr ASSET_DIR_REFS -cg AssetComponents -gg -g1 -sfrag -srd -suid -ke -sw5150 -var "var.ASSET_FILES_DIR" -out "$work_dir\asset_files.wxs"
&"$wix_dir\candle.exe" "$work_dir\rakudo_bin_files.wxs" -arch x64 -dRAKUDO_BIN_FILES_DIR="$work_rakudo_release_dir" -out "$work_dir\rakudo_bin_files.wxsobj"
&"$wix_dir\candle.exe" "$work_dir\asset_files.wxs" -arch x64 -dASSET_FILES_DIR="$asset_dir" -out "$work_dir\asset_files.wxsobj"

&"$wix_dir\candle.exe" "$msi_dir\rakudo.wxs" -ext WixUtilExtension -dVERSION="$rakudo_version" -dASSET_FILE_DIR="$asset_dir" -dINSTALLER_ASSET_FILE_DIR="$installer_asset_dir" -out "$work_dir\rakudo.wxsobj"

&"$wix_dir\light.exe" -ext WixUIExtension -ext WixUtilExtension "$work_dir\rakudo_bin_files.wxsobj" "$work_dir\asset_files.wxsobj" "$work_dir\rakudo.wxsobj" -sw1076 -o $out_file
