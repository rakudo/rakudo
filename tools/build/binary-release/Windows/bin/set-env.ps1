$scriptPath = split-path -parent $MyInvocation.MyCommand.Definition
$Env:PATH = "$scriptPath\..\bin;$scriptPath\..\share\perl6\site\bin;$Env:PATH"

