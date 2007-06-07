# Usage:
#   class category
#
# Prints a table for UnicodeTbl.es for the category.

CATEGORY=$1
./select.sh $CATEGORY | \
gawk '-F;' -v category=$CATEGORY \
'BEGIN { 
  sentinel=-37    # Not 0 or -1
  s = "" 
  lo=hi=sentinel
}
function p() {
  if (lo != sentinel) {
    if (hi == lo)
      s = s "\\x{" sprintf("%x", lo) "}"
    else
      s = s "\\x{" sprintf("%x", lo) "}--\\x{" sprintf("%x", hi) "}"
  }
}
{ n=strtonum("0x" $1)
  if (n == hi+1)
    hi=n
  else {
    p()
    lo=hi=n
  }
}
END { 
  p(); 
  print "var category_" category " = \"" s "\";"
}'
