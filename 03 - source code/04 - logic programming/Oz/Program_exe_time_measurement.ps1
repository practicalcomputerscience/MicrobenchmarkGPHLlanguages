# Program_exe_time_measurement.ps1
#
# 2025-12-09, test in PowerShell version 5.1.26100.7309 (> $PSVersionTable): OK
#
# PowerShell script to measure the program execution time, mean and standard deviation,
# of Oz program (object file): random_streams_for_perf_stats.ozf + benchmark it with the Python version
#
# run in Windows 11 PowerShell terminal like this:
#   Oz:
#     >.\Program_exe_time_measurement.ps1 -Command "ozengine .\random_streams_for_perf_stats.ozf" -Count 10
#
#   Python 3.13.9 + numpy-2.3.5 in PowerShell, too:
#     >.\Program_exe_time_measurement.ps1 -Command "python .\random_streams_for_perf_stats.py" -Count 10
#
#
# MS AI based (one slight correction):

param (
    [Parameter(Mandatory = $true)]
    [string]$Command,  # Command to execute

    [Parameter(Mandatory = $true)]
    [ValidateRange(1, 100)]
    [int]$Count        # Number of repetitions
)

# Store execution times in milliseconds
$times = @()

Write-Host "Running command $Count times..." -ForegroundColor Cyan

for ($i = 1; $i -le $Count; $i++) {
    try {
        $elapsed = (Measure-Command { Invoke-Expression $Command }).TotalMilliseconds
        $times += $elapsed
        Write-Host ("Run {0}/{1}: {2:N3} ms" -f $i, $Count, $elapsed)
    }
    catch {
        Write-Host "Error executing command on run ${i}: ${_}" -ForegroundColor Red
        break
    }
}

if ($times.Count -eq 0) {
    Write-Host "No successful runs to analyze." -ForegroundColor Yellow
    exit
}

# Calculate statistics
$mean = ($times | Measure-Object -Average).Average
$variance = ($times | ForEach-Object { ($_ - $mean) * ($_ - $mean) } | Measure-Object -Average).Average
$stdDev = [math]::Sqrt($variance)

Write-Host "`n===== Statistics =====" -ForegroundColor Green
Write-Host ("Mean: {0:N3} ms" -f $mean)
Write-Host ("Standard Deviation: {0:N3} ms" -f $stdDev)
Write-Host ("Min: {0:N3} ms" -f ($times | Measure-Object -Minimum).Minimum)
Write-Host ("Max: {0:N3} ms" -f ($times | Measure-Object -Maximum).Maximum)

<#
output2:
> .\Program_exe_time_measurement.ps1 -Command "ozengine .\random_streams_for_perf_stats.ozf" -Count 10
Running command 10 times...
Run 1/10: 540,691 ms
Run 2/10: 545,672 ms
Run 3/10: 507,100 ms
Run 4/10: 513,683 ms
Run 5/10: 494,953 ms
Run 6/10: 502,279 ms
Run 7/10: 499,739 ms
Run 8/10: 503,498 ms
Run 9/10: 502,779 ms
Run 10/10: 521,382 ms

===== Statistics =====
Mean: 513,178 ms
Standard Deviation: 16,585 ms
Min: 494,953 ms
Max: 545,672 ms
>
> .\Program_exe_time_measurement.ps1 -Command "python .\random_streams_for_perf_stats.py" -Count 10
Running command 10 times...
Run 1/10: 227,398 ms
Run 2/10: 206,101 ms
Run 3/10: 211,403 ms
Run 4/10: 204,566 ms
Run 5/10: 206,395 ms
Run 6/10: 208,011 ms
Run 7/10: 203,051 ms
Run 8/10: 196,954 ms
Run 9/10: 211,289 ms
Run 10/10: 201,387 ms

===== Statistics =====
Mean: 207,655 ms
Standard Deviation: 7,783 ms
Min: 196,954 ms
Max: 227,398 ms
>
#>

# end of Program_exe_time_measurement.ps1
