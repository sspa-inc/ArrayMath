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

$tmp = Join-Path $PSScriptRoot ".autotest_tmp"
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

Assert-Matrix "add and default output" `
  @("-d", "4", "3", "-a", $base, "-", "1.0") `
  @(@(1, 2, 3), @(4, 5, 6), @(7, 8, 9), @(10, 11, 12))

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
