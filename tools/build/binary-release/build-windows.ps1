$ErrorActionPreference = "stop"

# Make sure Powershell 7 doesn't just silently swallows errors.
# https://david.gardiner.net.au/2020/04/azure-pipelines-powershell-7-errors.html
# https://github.com/microsoft/azure-pipelines-tasks/issues/12799
$ErrorView = 'NormalView'

function CheckLastExitCode {
    if ($LastExitCode -ne 0) {
        $msg = @"
Program failed with: $LastExitCode
Callstack: $(Get-PSCallStack | Out-String)
"@
        throw $msg
    }
}

$repoPath = Get-Location

echo "========= Starting build"

echo "========= Downloading Perl"
mkdir download
mkdir strawberry
Invoke-WebRequest http://strawberryperl.com/download/5.30.0.1/strawberry-perl-5.30.0.1-64bit.zip -OutFile download/strawberry-perl-5.30.0.1-64bit.zip

echo "========= Extracting Perl"
Expand-Archive -Path download/strawberry-perl-5.30.0.1-64bit.zip -DestinationPath strawberry
strawberry\relocation.pl.bat
$Env:PATH = (Join-Path -Path $repoPath -ChildPath "\strawberry\perl\bin") + ";" + (Join-Path -Path $repoPath -ChildPath "\strawberry\perl\site\bin") + ";" + (Join-Path -Path $repoPath -ChildPath "\strawberry\c\bin") + ";$Env:PATH"

echo "========= Downloading release"
Invoke-WebRequest $Env:RELEASE_URL -OutFile download/rakudo.tgz

echo "========= Extracting release"
tar -xzf download/rakudo.tgz
cd rakudo-*

echo "========= Configuring Rakudo (includes building MoarVM and NQP)"
perl Configure.pl --gen-moar --gen-nqp --backends=moar --moar-option='--toolchain=msvc' --relocatable
CheckLastExitCode

echo "========= Building Rakudo"
nmake install
CheckLastExitCode

echo "========= Testing Rakudo"
rm -r t\spec
prove -e install\bin\raku -vlr t
CheckLastExitCode

echo "========= Cloning Zef"
git clone https://github.com/ugexe/zef.git
CheckLastExitCode

echo "========= Installing Zef"
cd zef
..\install\bin\raku -I. bin\zef install .
CheckLastExitCode
cd ..

echo "========= Copying auxiliary files"
cp -Force -r "tools\build\binary-release\Windows\*" install
cp LICENSE install

echo "========= Preparing archive"
$FILE_NAME = "rakudo-moar-${Env:VERSION}-${Env:REVISION}-win-x86_64-msvc"
mv install $FILE_NAME
Compress-Archive -Path $FILE_NAME -DestinationPath ..\rakudo-win.zip

