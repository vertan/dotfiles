Import-Module -Name posh-git
Start-SshAgent

Import-Module Get-ChildItemColor

function Test-Administrator {
    $user = [Security.Principal.WindowsIdentity]::GetCurrent();
    (New-Object Security.Principal.WindowsPrincipal $user).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
}



$OriginalForegroundColor = $Host.UI.RawUI.ForegroundColor
if ([System.Enum]::IsDefined([System.ConsoleColor], 1) -eq "False") { $OriginalForegroundColor = "Gray" }
$CompressedList = @(".7z", ".gz", ".rar", ".tar", ".zip")
$ExecutableList = @(".exe", ".bat", ".cmd", ".py", ".pl", ".ps1",
                    ".psm1", ".vbs", ".rb", ".reg", ".fsx", ".sh")
$DllPdbList = @(".dll", ".pdb")
$TextList = @(".csv", ".log", ".markdown", ".rst", ".txt")
$ConfigsList = @(".cfg", ".conf", ".config", ".ini", ".json")
$ColorTable = @{}
$ColorTable.Add('Default', $OriginalForegroundColor) 
$ColorTable.Add('Directory', "Green") 

ForEach ($Extension in $CompressedList) {
    $ColorTable.Add($Extension, "Yellow")
}
ForEach ($Extension in $ExecutableList) {
    $ColorTable.Add($Extension, "Blue")
}
ForEach ($Extension in $TextList) {
    $ColorTable.Add($Extension, "Cyan")
}
ForEach ($Extension in $DllPdbList) {
    $ColorTable.Add($Extension, "DarkGreen")
}
ForEach ($Extension in $ConfigsList) {
    $ColorTable.Add($Extension, "DarkYellow")
}

Function Get-Color($Item) {
    $Key = 'Default'

    If ($Item.GetType().Name -eq 'DirectoryInfo') {
        $Key = 'Directory'
    } else {
        If ($Item.PSobject.Properties.Name -contains "Extension") {
            If ($ColorTable.ContainsKey($Item.Extension)) {
                $Key = $Item.Extension
            }
        }
    }

    $Color = $ColorTable[$Key]
    Return $Color
}
Function Get-ChildItemColor {
    Param(
        [string]$Path = ""
    )
    $Expression = "Get-ChildItem -Path `"$Path`" $Args"

    $Items = Invoke-Expression $Expression

    ForEach ($Item in $Items) {
        $Color = Get-Color $Item

        $Host.UI.RawUI.ForegroundColor = $Color
        $Item
        $Host.UI.RawUI.ForegroundColor = $OriginalForegroundColor
    }
}
Function Get-ChildItemColorFormatWide {
    Param(
        [string]$Path = "",
        [switch]$Force
    )

    $nnl = $True

    $Expression = "Get-ChildItem -Path `"$Path`" $Args"

    if ($Force) {$Expression += " -Force"}

    $Items = Invoke-Expression $Expression

    $lnStr = $Items | Select-Object Name | Sort-Object { "$_".Length } -Descending | Select-Object -First 1
    $len = $lnStr.Name.Length
    $width = $Host.UI.RawUI.WindowSize.Width
    $cols = If ($len) {($width + 1) / ($len + 2)} Else {1}
    $cols = [math]::Floor($cols)
    if (!$cols) {$cols=1}

    $i = 0
    $pad = [math]::Ceiling(($width + 2) / $cols) - 3

    ForEach ($Item in $Items) {
        If ($Item.PSobject.Properties.Name -contains "PSParentPath") {
            If ($Item.PSParentPath -match "FileSystem") {
                $ParentType = "Directory"
                $ParentName = $Item.PSParentPath.Replace("Microsoft.PowerShell.Core\FileSystem::", "")
            }
            ElseIf ($Item.PSParentPath -match "Registry") {
                $ParentType = "Hive"
                $ParentName = $Item.PSParentPath.Replace("Microsoft.PowerShell.Core\Registry::", "")
            }
        } else {
            $ParentType = ""
            $ParentName = ""
            $LastParentName = $ParentName
        }

        $Color = Get-Color $Item

        if ($LastParentName -ne $ParentName) {
            if($i -ne 0 -AND $Host.UI.RawUI.CursorPosition.X -ne 0){  # conditionally add an empty line
                Write-Host ""
            }
            Write-Host -Fore $OriginalForegroundColor ("`n   $($ParentType): $ParentName`n")
        }

        $nnl = ++$i % $cols -ne 0

        # truncate the item name
        $toWrite = $Item.Name
        if ($toWrite.length -gt $pad) {
            $toWrite = $toWrite.Substring(0, $pad - 3) + "..."
        }

        Write-Host ("{0,-$pad}" -f $toWrite) -Fore $Color -NoNewLine:$nnl

        if ($nnl) {
            Write-Host "  " -NoNewLine
        }

        $LastParentName = $ParentName
    }

    if ($nnl) {  # conditionally add an empty line
        Write-Host ""
        Write-Host ""
    }
}

# http://serverfault.com/questions/95431
function Test-Administrator {
    $user = [Security.Principal.WindowsIdentity]::GetCurrent();
    (New-Object Security.Principal.WindowsPrincipal $user).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
}

function prompt {
    # https://github.com/dahlbyk/posh-git/wiki/Customizing-Your-PowerShell-Prompt
    $origLastExitCode = $LastExitCode
    

    if (Test-Administrator) {  # if elevated
        Write-Host "(Elevated) " -NoNewline -ForegroundColor White
    }

    Write-Host "$env:USERNAME@" -NoNewline -ForegroundColor DarkYellow
    Write-Host "$env:COMPUTERNAME" -NoNewline -ForegroundColor Magenta
    Write-Host " : " -NoNewline -ForegroundColor DarkGray

    $curPath = $ExecutionContext.SessionState.Path.CurrentLocation.Path
    if ($curPath.ToLower().StartsWith($Home.ToLower()))
    {
        $curPath = "~" + $curPath.SubString($Home.Length)
    }

    Write-Host $curPath -NoNewline -ForegroundColor Blue
    Write-Host " : " -NoNewline -ForegroundColor DarkGray
    Write-Host (Get-Date -Format G) -NoNewline -ForegroundColor DarkMagenta
    Write-Host " : " -NoNewline -ForegroundColor DarkGray

    Write-VcsStatus

    $LastExitCode = $origLastExitCode
    "`n$('>' * ($nestedPromptLevel + 1)) "
}

Import-Module posh-git

$global:GitPromptSettings.BeforeText = '['
$global:GitPromptSettings.AfterText  = '] '

Set-Alias ls Get-ChildItemColor -option AllScope -Force
Set-Alias dir Get-ChildItemColor -option AllScope -Force