# Generate all the class tables into one file, 'classes.es'

(echo "/* See UnicodeTbl.es for instructions */"
echo "package Unicode {"
./class.sh Lu
./class.sh Ll
./class.sh Lt
./class.sh Lm
./class.sh Lo
./class.sh Mn
./class.sh Mc
./class.sh Me
./class.sh Nd
./class.sh Nl
./class.sh No
./class.sh Pc
./class.sh Pd
./class.sh Ps
./class.sh Pe
./class.sh Pi
./class.sh Pf
./class.sh Po
./class.sh Sm
./class.sh Sc
./class.sh Sk
./class.sh So
./class.sh Zs
./class.sh Zl
./class.sh Zp
./class.sh Cc
./class.sh Cf
./class.sh Cs
./class.sh Co
./class.sh Cn
echo "}" ) > ../builtins/UnicodeClasses.es

