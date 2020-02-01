$ErrorActionPreference = "Stop"

# Don't display progressbars when doing Invoke-WebRequest and similar.
# That would cause the command to fail, because in the CircleCI environment
# one can't modify the display.
# "Win32 internal error “Access is denied” 0x5 occurred while reading the console output buffer. Contact Microsoft Customer Support Services."
$progressPreference = 'silentlyContinue'

function CheckLastExitCode {
    if ($LastExitCode -ne 0) {
        $msg = @"
Program failed with: $LastExitCode
Callstack: $(Get-PSCallStack | Out-String)
"@
        throw $msg
    }
}


# Install Perl

mkdir download
mkdir strawberry
Invoke-WebRequest http://strawberryperl.com/download/5.30.0.1/strawberry-perl-5.30.0.1-64bit.zip -OutFile download/strawberry-perl-5.30.0.1-64bit.zip
Expand-Archive -Path download/strawberry-perl-5.30.0.1-64bit.zip -DestinationPath strawberry
strawberry\relocation.pl.bat
$Env:PATH = (Join-Path -Path $repoPath -ChildPath "\strawberry\perl\bin") + ";" + (Join-Path -Path $repoPath -ChildPath "\strawberry\perl\site\bin") + ";" + (Join-Path -Path $repoPath -ChildPath "\strawberry\c\bin") + ";$Env:PATH"


# Download release file

Invoke-WebRequest $Env:RELEASE_URL -OutFile download/rakudo.tgz
tar -xzf download/rakudo.tgz .
cd rakudo-*

# Build Rakudo

perl Configure.pl --gen-moar --gen-nqp --backends=moar --moar-option='--toolchain=msvc' --relocatable
CheckLastExitCode
nmake install
CheckLastExitCode


# Test the build

nmake test
CheckLastExitCode


# Build Zef

git clone https://github.com/ugexe/zef.git
CheckLastExitCode
cd zef
..\install\bin\raku.exe -I. bin\zef install .
CheckLastExitCode
cd ..


# Prepare the package

cp -Force -r "tools\build\binary-release\Windows\*" install
cp LICENSE install

mv install rakudo-$Env:VERSION

Compress-Archive -Path rakudo-$Env:VERSION -DestinationPath ..\rakudo-win.zip

