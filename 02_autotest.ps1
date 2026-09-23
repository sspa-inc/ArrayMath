param(
  [string]$ExePath = (Join-Path $PSScriptRoot "bin\ArrayMath.exe")
)

$ErrorActionPreference = "Stop"
$script:Failed = 0
$script:Passed = 0
$Culture = [Globalization.CultureInfo]::InvariantCulture

function Write-TestFile {
  param(
    [string]$Path,
    [string[]]$Lines
  )
  Set-Content -Path $Path -Value $Lines -Encoding ASCII
}

function Invoke-ArrayMath {
  param([string[]]$CliArgs)

  $output = & $ExePath @CliArgs 2>&1
  $code = $LASTEXITCODE
  $text = ($output | Out-String).Trim()
  if ($code -ne 0) {
    throw "ArrayMath exited with code $code for args: $($CliArgs -join ' ')`n$text"
  }
  return $text
}

function ConvertTo-NumericMatrix {
  param([string]$Text)

  $rows = New-Object System.Collections.Generic.List[object]
  foreach ($line in ($Text -split "`r?`n")) {
    if ([string]::IsNullOrWhiteSpace($line)) { continue }
    $values = @()
    $isNumericLine = $true
    foreach ($token in ($line.Trim() -split "\s+")) {
      $parsed = 0.0
      if ([double]::TryParse($token, [Globalization.NumberStyles]::Float, $Culture, [ref]$parsed)) {
        $values += $parsed
      } else {
        $isNumericLine = $false
        break
      }
    }
    if (-not $isNumericLine) {
      continue
    }
    $rows.Add($values)
  }
  return ,$rows.ToArray()
}

function Assert-Matrix {
  param(
    [string]$Name,
    [string[]]$CliArgs,
    [object[]]$Expected,
    [double]$Tolerance = 1e-5
  )

  try {
    $actual = ConvertTo-NumericMatrix (Invoke-ArrayMath $CliArgs)
    $expectedRows = $Expected
    if ($actual.Count -eq 1 -and $Expected.Count -gt 0 -and -not ($Expected[0] -is [array])) {
      $expectedRows = ,$Expected
    }
    if ($actual.Count -ne $expectedRows.Count) {
      throw "row count expected $($expectedRows.Count), got $($actual.Count)"
    }
    for ($i = 0; $i -lt $expectedRows.Count; $i++) {
      if ($actual[$i].Count -ne $expectedRows[$i].Count) {
        throw "row $($i + 1) column count expected $($expectedRows[$i].Count), got $($actual[$i].Count)"
      }
      for ($j = 0; $j -lt $expectedRows[$i].Count; $j++) {
        $e = [double]$expectedRows[$i][$j]
        $a = [double]$actual[$i][$j]
        if ([double]::IsNaN($e)) {
          if (-not [double]::IsNaN($a)) {
            throw "[$($i + 1),$($j + 1)] expected NaN, got $a"
          }
        } elseif ([math]::Abs($a - $e) -gt $Tolerance) {
          throw "[$($i + 1),$($j + 1)] expected $e, got $a"
        }
      }
    }
    $script:Passed += 1
    Write-Host "PASS $Name"
  } catch {
    $script:Failed += 1
    Write-Host "FAIL $Name"
    Write-Host "     $($_.Exception.Message)"
  }
}

if (-not (Test-Path $ExePath)) {
  throw "Could not find ArrayMath executable: $ExePath"
}

$tmp = ".autotest_tmp"
New-Item -ItemType Directory -Force -Path $tmp | Out-Null

$base = Join-Path $tmp "base.dat"
Write-TestFile $base @(
  "1 2 3",
  "4 5 6",
  "7 8 9",
  "10 11 12"
)

$multi = Join-Path $tmp "multi.dat"
Write-TestFile $multi @(
  "2 0 1",
  "1 2 0",
  "0 1 2",
  "3 1 1"
)

$named = Join-Path $tmp "named.dat"
Write-TestFile $named @(
  "id a b",
  "1 10 100",
  "2 20 200",
  "3 30 300",
  "4 40 400"
)

$grouped = Join-Path $tmp "grouped.dat"
Write-TestFile $grouped @(
  "1 10 100",
  "1 20 200",
  "2 30 300",
  "2 40 500"
)

$sortCols = Join-Path $tmp "sort_cols.dat"
Write-TestFile $sortCols @(
  "c a b",
  "3 1 2",
  "6 4 5"
)

$series = Join-Path $tmp "series.dat"
Write-TestFile $series @(
  "1",
  "2",
  "3",
  "4"
)

$repeated = Join-Path $tmp "repeated.dat"
Write-TestFile $repeated @(
  "1 2",
  "3 4",
  "1 2",
  "5 6",
  "3 4"
)

$namedRepeated = Join-Path $tmp "named_repeated.dat"
Write-TestFile $namedRepeated @(
  "first 1 2",
  "middle 3 4",
  "last 1 2"
)

$dated = Join-Path $tmp "dated.dat"
Write-TestFile $dated @(
  "TIME D1 D2 D3",
  "09/30/1973_24:00 0.0000 0.0000 0.0000",
  "10/31/1973_24:00 -0.0151 -0.0592 0.0387"
)

$datedComma = Join-Path $tmp "dated_comma.dat"
Write-TestFile $datedComma @(
  "TIME, D1,D2,D3",
  "09/30/1973_24:00, 0.0000,0.0000,0.0000",
  "10/31/1973_24:00,-0.0151, -0.0592,0.0387"
)

$datedTab = Join-Path $tmp "dated_tab.dat"
Write-TestFile $datedTab @(
  "TIME`tD1`tD2`tD3",
  "09/30/1973_24:00`t0.0000`t0.0000`t0.0000",
  "10/31/1973_24:00`t-0.0151`t-0.0592`t0.0387"
)

$datedLong = Join-Path $tmp "dated_long.dat"
$padding = " " * 5000
Write-TestFile $datedLong @(
  "TIME${padding}D1 D2 D3",
  "09/30/1973_24:00${padding}0.0000 0.0000 0.0000",
  "10/31/1973_24:00${padding}-0.0151 -0.0592 0.0387"
)

Assert-Matrix "add and default output" `
  @("-d", "4", "3", "-a", $base, "-", "1.0") `
  @(@(1, 2, 3), @(4, 5, 6), @(7, 8, 9), @(10, 11, 12))

try {
  $piped = @('TIME,D1,D2', '09/30/1973_24:00,1,2') |
    & $ExePath -d 1 2 -rn -cn -a - - 1
  if ($LASTEXITCODE -ne 0 -or ($piped -join "`n") -notmatch 'TIME\s+D1\s+D2\s+09/30/1973_24:00\s+1\.0+\s+2\.0+') {
    throw "unexpected named stdin output: $($piped -join ' | ')"
  }
  $script:Passed += 1
  Write-Host "PASS piped named input"
} catch {
  $script:Failed += 1
  Write-Host "FAIL piped named input"
  Write-Host "     $($_.Exception.Message)"
}

try {
  $piped = '2 3 4' | & $ExePath -d 1 3 -a $base - 1 -m -
  if ($LASTEXITCODE -ne 0 -or ($piped -join ' ') -notmatch '^2\.0+\s+6\.0+\s+12\.0+') {
    throw "unexpected multiplier stdin output: $($piped -join ' | ')"
  }
  $script:Passed += 1
  Write-Host "PASS piped multiplier input"
} catch {
  $script:Failed += 1
  Write-Host "FAIL piped multiplier input"
  Write-Host "     $($_.Exception.Message)"
}

try {
  $lfOutput = Join-Path $tmp "lf_output.dat"
  & $ExePath -d 4 3 -a $base - 1.0 --lf $lfOutput | Out-Null
  if ($LASTEXITCODE -ne 0) { throw "file output exited with code $LASTEXITCODE" }
  $fileBytes = [IO.File]::ReadAllBytes((Resolve-Path $lfOutput).Path)

  $start = New-Object Diagnostics.ProcessStartInfo
  $start.FileName = (Resolve-Path $ExePath).Path
  $start.Arguments = "-d 4 3 -a $base - 1.0 --lf"
  $start.WorkingDirectory = (Get-Location).Path
  $start.UseShellExecute = $false
  $start.CreateNoWindow = $true
  $start.RedirectStandardOutput = $true
  $process = [Diagnostics.Process]::Start($start)
  $outputBytes = New-Object IO.MemoryStream
  $process.StandardOutput.BaseStream.CopyTo($outputBytes)
  $process.WaitForExit()
  if ($process.ExitCode -ne 0) { throw "stdout output exited with code $($process.ExitCode)" }

  foreach ($bytes in @($fileBytes, $outputBytes.ToArray())) {
    if (@($bytes | Where-Object { $_ -eq 10 }).Count -ne 4 -or
        @($bytes | Where-Object { $_ -eq 13 }).Count -ne 0) {
      throw "expected four LF bytes and no CR bytes"
    }
  }
  $script:Passed += 1
  Write-Host "PASS --lf file and stdout output"
} catch {
  $script:Failed += 1
  Write-Host "FAIL --lf file and stdout output"
  Write-Host "     $($_.Exception.Message)"
}

Assert-Matrix "scale, scalar multiply, offset" `
  @("-d", "4", "3", "-a", $base, "-", "2.0", "-m", "*3", "-o", "1") `
  @(@(7, 13, 19), @(25, 31, 37), @(43, 49, 55), @(61, 67, 73))

Assert-Matrix "multiply by array" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "-m", $multi) `
  @(@(2, 0, 3), @(4, 10, 0), @(0, 8, 18), @(30, 11, 12))

Assert-Matrix "power and clip" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "-p", "2", "-c", "10", "80") `
  @(@(10, 10, 10), @(16, 25, 36), @(49, 64, 80), @(80, 80, 80))

Assert-Matrix "column sum function" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "-x", "sum") `
  @(@(22, 26, 30))

Assert-Matrix "diff function" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "-x", "diff") `
  @(@(1, 2, 3), @(3, 3, 3), @(3, 3, 3), @(3, 3, 3))

Assert-Matrix "transpose option" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "-t") `
  @(@(1, 4, 7, 10), @(2, 5, 8, 11), @(3, 6, 9, 12))

Assert-Matrix "row and column subsets" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "-s", "r2:3", "-s", "c2,3") `
  @(@(5, 6), @(8, 9))

Assert-Matrix "head option" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "--head", "2") `
  @(@(1, 2, 3), @(4, 5, 6))

foreach ($datedCase in @(@("space", $dated), @("comma", $datedComma), @("tab", $datedTab), @("long-line", $datedLong))) {
try {
  $datedOutput = Invoke-ArrayMath @("-d", "2", "3", "-rn", "-cn", "--add", $datedCase[1], "-", "1")
  $datedLines = @($datedOutput -split "`r?`n")
  if ($datedLines.Count -ne 3 -or $datedLines[0] -notmatch '^TIME\s+D1\s+D2\s+D3' -or
      $datedLines[1] -notmatch '^09/30/1973_24:00\s+' -or
      $datedLines[2] -notmatch '^10/31/1973_24:00\s+') {
    throw "unexpected dated array output: $datedOutput"
  }
  $datedNumeric = ConvertTo-NumericMatrix (($datedLines[1..2] | ForEach-Object { $_ -replace '^\S+\s+', '' }) -join "`n")
  if ($datedNumeric.Count -ne 2 -or [math]::Abs($datedNumeric[1][0] + 0.0151) -gt 1e-5 -or
      [math]::Abs($datedNumeric[1][1] + 0.0592) -gt 1e-5 -or
      [math]::Abs($datedNumeric[1][2] - 0.0387) -gt 1e-5) {
    throw "unexpected dated numeric values: $datedOutput"
  }
  $script:Passed += 1
  Write-Host "PASS $($datedCase[0])-delimited named dates"
} catch {
  $script:Failed += 1
  Write-Host "FAIL $($datedCase[0])-delimited named dates"
  Write-Host "     $($_.Exception.Message)"
}
}

Assert-Matrix "tail option" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "--tail", "2") `
  @(@(7, 8, 9), @(10, 11, 12))

Assert-Matrix "head and tail follow command order" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "-tl", "3", "-hd", "2") `
  @(@(4, 5, 6), @(7, 8, 9))

Assert-Matrix "unique keeps first numeric rows" `
  @("-d", "5", "2", "-a", $repeated, "-", "1.0", "--unique") `
  @(@(1, 2), @(3, 4), @(5, 6))

Assert-Matrix "reverse preserves row order semantics" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "--reverse", "--head", "2") `
  @(@(10, 11, 12), @(7, 8, 9))

Assert-Matrix "reverse then unique retains first occurrence" `
  @("-d", "5", "2", "-a", $repeated, "-", "1.0", "-rv", "-un") `
  @(@(3, 4), @(5, 6), @(1, 2))

try {
  $namedOutput = Invoke-ArrayMath @("-d", "3", "2", "-rn", "-a", $namedRepeated, "-", "1.0", "--reverse", "--unique")
  $names = @($namedOutput -split "`r?`n" | ForEach-Object { ($_ -split "\s+")[0] })
  if (($names -join ",") -ne "last,middle") { throw "expected last,middle; got $($names -join ',')" }
  $script:Passed += 1
  Write-Host "PASS unique and reverse preserve row names"
} catch {
  $script:Failed += 1
  Write-Host "FAIL unique and reverse preserve row names"
  Write-Host "     $($_.Exception.Message)"
}

Assert-Matrix "stack option" `
  @("-d", "4", "3", "-a", $base, "-", "1.0", "-st") `
  @(@(1), @(2), @(3), @(4), @(5), @(6), @(7), @(8), @(9), @(10), @(11), @(12))

Assert-Matrix "filter by column name" `
  @("-d", "4", "3", "-cn", "-a", $named, "-", "1.0", "-f", "id>=3", "-s", "c2,3") `
  @(@(30, 300), @(40, 400))

Assert-Matrix "groupby mean" `
  @("-d", "4", "3", "-a", $grouped, "-", "1.0", "-g", "1", "mean") `
  @(@(1, 15, 150), @(2, 35, 400))

Assert-Matrix "groupby std" `
  @("-d", "4", "3", "-a", $grouped, "-", "1.0", "-g", "1", "std") `
  @(@(0, 5, 50), @(0, 5, 100))

Assert-Matrix "sort columns by name" `
  @("-d", "2", "3", "-cn", "-a", $sortCols, "-", "1.0", "-x", "sortc") `
  @(@(1, 2, 3), @(4, 5, 6))

Assert-Matrix "moving average option" `
  @("-d", "4", "1", "-a", $series, "-", "1.0", "-ma", "2", "0") `
  @(@([double]::NaN), @(1.5), @(2.5), @(3.5))

Write-Host ""
Write-Host "$script:Passed passed, $script:Failed failed"
if ($script:Failed -gt 0) {
  exit 1
}
