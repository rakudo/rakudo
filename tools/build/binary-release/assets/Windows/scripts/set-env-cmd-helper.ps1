$script_path = split-path -parent $MyInvocation.MyCommand.Definition
$cmds = ""

function activate_buildtools() {
    $cmds += "ECHO                   Detecting and activating MS Build tools`n"
    $cmds += "ECHO                  =========================================`n"
    $cmds += "ECHO.`n"

    $cims = Get-CimInstance MSFT_VSInstance -Namespace root/cimv2/vs
    $chosen_cim = $null
    foreach ($cim in $cims) {
        $install_location = $cim.InstallLocation
        if (-Not(Get-ChildItem -Path $install_location -Filter CL.exe -Recurse -ErrorAction SilentlyContinue)) {
            $cmds += "ECHO No CL found in $install_location. Skipping.`n"
            continue
        }
        if ($chosen_cim -eq $null) {
            $chosen_cim = $cim
        }
        else {
            if ($cim.Version > $chosen_cim.Version) {
                $chosen_cim = $cim
            }
        }
    }
    if ($chosen_cim -eq $null) {
        $cmds += "ECHO No suitable installation found.`n"
        return
    }

    $install_location = $chosen_cim.InstallLocation

    $setup = Get-ChildItem -Path $install_location -Filter VsDevCmd.bat -Recurse -ErrorAction SilentlyContinue | Select-Object -First 1
    if (-Not $setup) {
        $cmds += "ECHO The CMD setup module wasn't found.`n"
        return
    }
    $setup = $setup.FullName

    $cmds += "ECHO Found suitable installation in $install_location.`n"

    $arch = ""
    if ((Get-CimInstance win32_operatingsystem | select osarchitecture).osarchitecture -eq "64-bit") {
        $cmds += "`"$setup`" -arch=amd64`n"
    }
    else {
        $cmds += "`"$setup`"`n"
    }
    $cmds += "ECHO.`n"
    return $cmds
}

function add_to_path() {
    $cmds += "ECHO                            Adding Rakudo to PATH`n"
    $cmds += "ECHO                           =======================`n"
    $cmds += "ECHO.`n"

    $done_stuff = $null

    $rakudo_path0 = (Resolve-Path "$script_path\..\bin").Path
    $rakudo_path1 = (Resolve-Path "$script_path\..\share\perl6\site\bin").Path
    $new_path = $Env:PATH

    if ($env:PATH -NotLike "*$rakudo_path1*") {
        $new_path = "$rakudo_path1;$new_path"
        $done_stuff = $true
    }

    if ($env:PATH -NotLike "*$rakudo_path0*") {
        $new_path = "$rakudo_path0;$new_path"
        $done_stuff = $true
    }

    if ($done_stuff -ne $null) {
        $cmds += "SET `"PATH=$new_path`"`n"
        $cmds += "ECHO Paths successfully added.`n"
    }
    else {
        $cmds += "ECHO Paths already set. Nothing to do.`n"
    }
    $cmds += "ECHO.`n"
    return $cmds
}


function enable_utf8() {
    $cmds += "ECHO                              Enabling UTF-8`n"
    $cmds += "ECHO                             ================`n"
    $cmds += "ECHO.`n"
    $cmds += "chcp 65001`n"
    $cmds += "ECHO Done.`n"
    return $cmds
}

$cmds += activate_buildtools
$cmds += add_to_path
$cmds += enable_utf8
$cmds += @'
ECHO.
ECHO ================================================================================
ECHO  =========                                                             __   __
ECHO   ||_|_||                =============================                (  \,/  )
ECHO   || # ||                 Welcome to the Raku Console                  \_ O _/
ECHO   || # ||                =============================                 (_/ \_)
ECHO.
ECHO This console has all the tools available you need to get started using Raku.
ECHO.
ECHO Rakudo provides an interactive command line interpreter (a so called Read Eval
ECHO Print Loop, REPL for short) you can use to quickly try out pieces of Raku code.
ECHO Start it by typing:
ECHO.
ECHO     raku.exe
ECHO.
ECHO If you already have a Raku program in a file, you can run it by typing:
ECHO.
ECHO     raku.exe path\to\my\program.raku
ECHO.
ECHO To install additional modules you can use the Zef module manager:
ECHO.
ECHO     zef install Some::Module
ECHO.
ECHO https://rakudo.org/           - The home of this implementation of Raku.
ECHO https://raku.land/            - Go here to browse for Raku modules.
ECHO https://docs.raku.org/        - The Raku documentation.
ECHO https://web.libera.chat/#raku - The Raku user chat. Talk to us!
ECHO.
ECHO                               Happy hacking!
ECHO.
ECHO ================================================================================
ECHO.
'@

echo $cmds