# Select a category of data from UnicodeData.txt
#
# Usage:
#    select category 

CATEGORY=$1
cat UnicodeData.txt | \
awk '-F;' -v category=$CATEGORY '{ if ($3 == category) print($0); }'
