param (
    [Parameter(Mandatory=$true, Position=0)]
    [ValidateSet("build", "run", "test", "clean")]
    [string]$Command,

    [Parameter(ValueFromRemainingArguments=$true)]
    [string[]]$Args
)

$WslDistro = "Ubuntu-22.04"
$CargoTargetDir = "~/.cargo-target/dana-lang"
$WslBaseCmd = "export CARGO_TARGET_DIR=$CargoTargetDir && cargo"

switch ($Command) {
    "build" {
        wsl -d $WslDistro -- bash -l -c "$WslBaseCmd build"
    }
    "run" {
        $CargoArgs = $Args -join " "
        wsl -d $WslDistro -- bash -l -c "$WslBaseCmd run -- $CargoArgs"
    }
    "test" {
        wsl -d $WslDistro -- bash -l -c "$WslBaseCmd test"
    }
    "clean" {
        wsl -d $WslDistro -- bash -l -c "$WslBaseCmd clean"
        if (Test-Path "target") {
            Remove-Item -Recururse -Force "target"
        }
    }
}
