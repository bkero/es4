cat CaseFolding.txt | \
awk '-F;' \
'BEGIN { lo=0; hi=0 } 
{ n = strtonum("0x" $1); 
  if (n > hi+1) { 
    if (lo != 0) print(lo, hi); 
    lo=hi=n 
  } 
  else hi++ 
}
END { if (lo != 0) print(lo,hi); }

