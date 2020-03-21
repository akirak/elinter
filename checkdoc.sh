# Run checkdoc on target files

# This script should be run by the builder in a nix-build task
# defined in default.nix.

# The logic is based on an implementation in makel:
# <https://gitlab.petton.fr/DamienCassou/makel/blob/master/makel.mk>

# Fail if any error occurs inside this script
set -e

# Set PATH from buildInputs
unset PATH
for p in $buildInputs; do
    export PATH=$p/bin${PATH:+:}$PATH
done

if [ -n "$src" ]; then
    cd $src
fi

echo "Running checkdoc..."

# nix-build fails if you don't make the output directory
mkdir -p $out

# Create a log file in the output directory
logFile=$out/checkdoc.log
touch $logFile

for f in $files; do
    # Run checkdoc on each file and append the output to the log file
    emacs --batch --eval "(checkdoc-file \"$f\")" 2>&1 | tee -a $logFile
done

# Tell the location of the log file to the user
echo "Saved the checkdoc status to $logFile"

# Check if the log file size is 0, which means no output from the
# program
if [ $(stat --printf='%s' $logFile) -eq 0 ]; then
    echo "The checkdoc result is clean."
else
    # Return non-zero exit code
    echo "The checkdoc result is NOT clean." >&2
    exit 1
fi
