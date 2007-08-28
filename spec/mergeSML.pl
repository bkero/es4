#====================================================================
# Name: ExportES4Wiki.pl
#
# Purpose: Grabs Wiki HTML and creates MS-Word compatible HTML file
#
# Author: Francis Cheng
# Date: October 2006
#
# Script notes: This script runs on Perl 5.8 (ActiveState installation)
# and requires the following modules (which can be installed using 
# Perl Package Manager):
# -> ppm install WWW::Mechanize
# -> ppm install http://theoryx5.uwinnipeg.ca/ppms/Net_SSLeay.pm.ppd
# -> ppm install http://theoryx5.uwinnipeg.ca/ppms/IO-Socket-SSL.ppd
# -> ppm install http://theoryx5.uwinnipeg.ca/ppms/Crypt-SSLeay.ppd
# -> ppm install spreadsheet-parseexcel
# -> ppm install OLE::Storage_Lite
# -> ppm install IO::Scalar
#
# If you have problems with the DLLs needed by the SSL modules, download
# and install the latest openSSL installer (for Windows):
# http://www.openssl.org/related/binaries.html

use strict;
use warnings;
#use WWW::Mechanize;  # for scraping wiki html
#use HTTP::Cookies;
#use HTML::Form;      # for LoginToWiki()
use Spreadsheet::ParseExcel; # for parsing grammar.xls file


# === CONFIGURATION SECTION ====
my $grammarFile = '../doc/grammar.xls'; # expects script to run in root of spec directory
my $smlSourceDir = '';  # command line arg ($ARGV[0])
my @smlFiles = ("ast.sml", "defn.sml", "verify.sml", "eval.sml");
my @funcList = ();
my $outputFile = 'merged.html';
our ($sourceHTML, %grammar, @grammarList, %smlHash);  # globals

# === Read in command line arguments
if ($ARGV[0] ne "") {
  $smlSourceDir = $ARGV[0];
}
else {
  print "ERROR: You must specify the sml source directory path.\n";
  exit(0);
}

if ($ARGV[1] ne "") {
  $sourceHTML = $ARGV[1];
}
else {
  print "ERROR: You must specify the source HTML directory path.\n";
  exit(0);
}

# === construct $outputFile string based on input file
if ($sourceHTML =~ /\/(\w+)\.html?/) {
  $outputFile = $1 . '_merged.html';
}


# === Call subroutines

ParseGrammar($smlSourceDir, $grammarFile);

PrintGrammarToFile();

CreateSMLHash();

PrintSMLHash();

Merge();

#LoginToWiki();

#GetWikiText();

#ParseWikiText();


# =========== SUBROUTINES ==============


# ===============
# Subroutine ParseGrammar
# Purpose: Parse Grammar.xls and place grammar into data structures that will
# be merged with wiki source
# Data Structures:
# 1. The grammar is stored in a single hash.
# Each hashkey is the "left-hand side" (lhs) of a production rule in the
# surface syntax and points to a list of values:
# $lhsContent => [ numSurfaceSyntaxElements,
#                  [listOfrhsSurfaceSyntaxElements],
#                  [listOfAbstractSyntaxElements] ]
#
# Where [listOfrhsSurfaceSyntaxElements] is a list that includes each element on
# the right-hand side (rhs) of the production, and where
# [listOfAbstractSyntaxElements] is a list that includes all of the absract
# syntax elements. These lists do not retain their individuality in the hash.
# In other words, the arrays are assigned to the hash by value, not by
# reference, so the contents of each array is appended to the list. However,
# you can use the first element of the array as an offset for the beginning
# of the abstract syntax elements.
#
# 2. A separate list tracks the order in which each hashkey appears so
# that the order can be reproduced for reproduction of the entire grammar.
# The list variable is named $grammarList
#
# ================
sub ParseGrammar {
   my ($grammarPath, $grammarFileName) = @_;
   my $grammarXLS = $grammarPath . '/doc/' . $grammarFileName;
   my $myBook = Spreadsheet::ParseExcel::Workbook->Parse($grammarXLS);
   my $ES4Worksheet = $myBook->Worksheet('ES4');
   my $maxRow = $ES4Worksheet->{MaxRow};
   my @lexemes = ();  # array to hold list of lexemes

   my $iCol = 7;  # initial value of $iCol is 7 (column H in the spreadsheet)
   %grammar = (); # hash to hold contents of grammar (must be global)
   my @noAbstractSyntax = ();  # track entries without Abstract Syntax
   my @missingAbstractSyntax = (); # entries with missing Abstract Syntax
   for (my $iRow = 1; $iRow <= $maxRow; $iRow++) {
       my $cell = $ES4Worksheet->{Cells}[$iRow][$iCol];

       # if $cell is defined and a value exists, there is valid lhs content.
       # Otherwise move on to next iteration of the for loop.
       if ( $cell && $cell->Value ) {
         my $lhsContent = AddFormattingTags($cell);

         # check whether this is header content, which occurs only in initial
         # column. If yes, change tags to <h1> and skip to next iteration
         if ($lhsContent =~ /^<strong>[A-Z ]+<\/strong>/) {
           $lhsContent =~ s/strong>/h1>/g;
           push (@grammarList, $lhsContent);
           next;
         }

         # build list of lexemes to use later to differentiate lexemes from
         # terminals (both are in bold format in the .xls file)
         if ($lhsContent =~ /^<strong>(.*)<\/strong>/) {
           push (@lexemes, $1);
         }

         push (@grammarList, $lhsContent);

         # Parse the rhs elements in this production and count them
         my @rhs = ();   # array containing rhs elements
         my $num = 0;
         while ($ES4Worksheet->{Cells}[$iRow+1+$num][$iCol+1] &&
                $ES4Worksheet->{Cells}[$iRow+1+$num][$iCol+1]->Value) {
           my $rhsContent = AddFormattingTags($ES4Worksheet->{Cells}[$iRow+1+$num][$iCol+1]);
           push(@rhs, $rhsContent);
           $num++;
         }
         # Place the Abstract Syntax into a list
         my @abstractSyntax;
         # First line of abstract syntax has two-column offset
         my $abstractHeaderCell = $ES4Worksheet->{Cells}[$iRow][$iCol+2];
         if ($abstractHeaderCell && $abstractHeaderCell->Value) {
           push(@abstractSyntax, $abstractHeaderCell->Value);
         }
         # Remainder of abstract syntax has three-column offset
         my $aNum = 0;
         while ($ES4Worksheet->{Cells}[$iRow+1+$aNum][$iCol+3] &&
                $ES4Worksheet->{Cells}[$iRow+1+$aNum][$iCol+3]->Value) {
           push(@abstractSyntax, $ES4Worksheet->{Cells}[$iRow+1+$aNum][$iCol+3]->Value);
           # For some reason, instances of ... in the .xls file get converted to
           # ampersand so we need to convert lone instances of ampersand to ...
           if ($abstractSyntax[$#abstractSyntax] =~ /^&$/) {
             $abstractSyntax[$#abstractSyntax] = '...';
           }
           $aNum++;
         }

         # Create entries for error log
         if ($num > 0 && $aNum == 0 ) {
           push (@noAbstractSyntax, $cell->Value);
         }
         elsif ($num > 0 && $num != $aNum) {
           push (@missingAbstractSyntax, $cell->Value);
         }
         # Create key-value pair in %grammar hash
         $grammar{ $lhsContent } = [ $num, @rhs, @abstractSyntax ];

        } # end of if ( $cell ... statement
   }

   #ModifyTerminalTags(@lexemes);

   # Error log file
   open (F, ">errorLog.txt");
   print F "The following entries have no Abstract Syntax:\n";
   foreach (@noAbstractSyntax) {
     print F "$_\n";
   }
   print F "\nThe following entries may be missing Abstract Syntax:\n";
   foreach (@missingAbstractSyntax) {
     print F "$_\n";
   }
   print F "\nList of lexemes:\n";
   foreach my $lex(@lexemes) {
     print F "$lex\n";
   }
   close F;
}


# ===============
# Subroutine PrintGrammarToFile
# Purpose: Print Grammar to a text file for testing purposes
# ================
sub PrintGrammarToFile {
  open (GFILE, ">Grammar.txt");

  foreach my $prod ( @grammarList ) {
     # print surface syntax lhs content
     print GFILE $prod . "\n";
     
     # if there are any other contents, print them
     if (defined $grammar{$prod}[0]) {
       my $numSurfaceSyntaxElements = $grammar{$prod}[0];
       for (my $i=1; $i <= $numSurfaceSyntaxElements; $i++) {
         print GFILE "\t" . $grammar{$prod}[$i] . "\n";
       }
       print GFILE "\n";

       # print abstract syntax
       my $length = @ {$grammar{$prod}};
       my $abstractSyntaxHeaderIndex = $numSurfaceSyntaxElements + 1;
       if (defined $grammar{$prod}[$abstractSyntaxHeaderIndex]) {
         print GFILE $grammar{$prod}[$abstractSyntaxHeaderIndex] . "\n";
         for (my $j = $abstractSyntaxHeaderIndex+1; $j < $length; $j++) {
           print GFILE "\t" . $grammar{$prod}[$j] . "\n";
         }
         print GFILE "\n";
       }
       print GFILE "\n";
    }
  }

  close GFILE;
}

# ===============
# Subroutine AddFormattingTags
# Purpose: Add XHTML formatting tags to grammar elements that need it
# Methodology: The madness to this method is due to the tools provided by the
# Spreadsheet::ParseExcel module, which provides rich text information
# about a character only when it differs in some rich text formatting aspect
# from the character that immediately precedes it.
#
# Process the string sequentially and place tags and substrings into
# an array. Having tags and substrings broken down into array elements makes
# it easier to check the integrity of the tags and make adjustments
# before constructing the output string.
# 1. Check for Bold, Superscript, or FontName formatting applied to entire cell
#    and push appropriate tag if found.
# 2. Check whether there was any rich text formatting applied to the string.
#    a. If so, iterate through the rich text array for the formatting deltas.
#       Parse the string, pushing substrings and tags into the array.
#       After iterating, push any remaining text and go to step 3.
#    b. If not, push the input string to the array.
# 3. Push any necessary closing tags.
# 4. Verify that tags are in proper order
# 5. Construct output string using array, then return the result.
#
# Note: Converts a, b, g, w to their HTML entity codes (e.g. &alpha;, etc.)
#       at three points in the code. 1) when adding </sup>; 2) when font
#       face changes from Symbol to Arial; 3) at end of string that has
#       font set to Symbol.
#
# Still To Do:
# 1. Verify that tags are in proper order before constructing output string.
# ================
sub AddFormattingTags {
  my ($cell) = @_;
  my $cellValue = $cell->Value;

  my $formattedString = '';
  my @formatArray = ();
  my $lastPos = 0;

  # set initial values of Bold, Font, and Super
  my $bold = ($cell->{Format}->{Font}->{Bold});
  my $font = ($cell->{Format}->{Font}->{Name});
  my $super = ($cell->{Format}->{Font}->{Super});
  my $em = 0;

  # 1. Check for Bold, Superscript, or FontName formatting
  if ($bold) {
    push (@formatArray, '<strong>');
  }
  else {
    push (@formatArray, '<em>');
    $em = 1;
  }

  if ($super == 1) {
    push (@formatArray, '<sup>');
  }
  elsif ($super == 2) {
    push (@formatArray, '<sub>');
  }

  # 2. check for rich text formatting
  if ($cell->{Rich}) {
    foreach my $richTextArray (@{$cell->{Rich}}) {
      my $pos = $richTextArray->[0];
      my $fontObj  = $richTextArray->[1];

      my $sub = substr($cellValue, $lastPos, $pos - $lastPos);
      push (@formatArray, $sub);
      $cellValue = $cell->Value;  # restore full string
      $lastPos = $pos;              # move position pointer

      # Check for change in superscript format
      if ( $super != $fontObj->{Super} ) {
        if ($super) {
          my $str = $formatArray[$#formatArray]; # $#formatArray gives last index
          $str =~ s/\b(a)\b/&alpha;/g;
          $str =~ s/\b(b)\b/&beta;/g;
          $str =~ s/\b(g)\b/&gamma;/g;
          $str =~ s/\b(w)\b/&omega;/g;
          $formatArray[$#formatArray] = $str;

          if ($super == 1) {
            push (@formatArray, '</sup>');
          }
          elsif ($super == 2) {
            push (@formatArray, '</sub>');
          }
        }
        elsif ($fontObj->{Super} == 1) {  # changing from 0 to 1
          push (@formatArray, '<sup>');
        }
        elsif ($fontObj->{Super} == 2) {  # changing from 0 to 2
          push (@formatArray, '<sub>');
        }
        $super = $fontObj->{Super};    # toggle $super
      }
      # Check for change in bold format
      if ( $bold != $fontObj->{Bold} ) {
        if ($bold) {
          push (@formatArray, '</strong>');
          push (@formatArray, '<em>');
          $em = 1;
        }
        else {
          if ($em) {
            push (@formatArray, '</em>');
            $em = 0;
          }
          push (@formatArray, '<strong>');
        }
        $bold = ($bold + 1) % 2;      # toggle $bold
      }
      #Check for change in font face
      if ( $font ne $fontObj->{Name} ) {
        if ( ( $font eq 'Symbol' ) && ( $fontObj->{Name} eq 'Arial') ) {
          # if Symbol font turns to Arial, we need to convert the char
          # to HTML entity code
          my $str = $formatArray[$#formatArray];  # $#formatArray gives last index
          $str =~ s/\b(a)\b/&alpha;/g;
          $str =~ s/\b(b)\b/&beta;/g;
          $str =~ s/\b(g)\b/&gamma;/g;
          $str =~ s/\b(w)\b/&omega;/g;
          $formatArray[$#formatArray] = $str;

          $font = $fontObj->{Name};
        }
        if ( ( $font eq 'Arial' ) && ( $fontObj->{Name} eq 'Symbol') ) {
          $font = $fontObj->{Name};
        }
      }
    }

    # after foreach loop is done, chances are that there is still part of
    # the string left to process
    my $len = length $cellValue;
    my $subFinal = substr($cellValue, $lastPos, $len - $lastPos);
    push (@formatArray, $subFinal);

  }
  else { # lack of rich text info means entire string has same format
    # append plain text string
    push (@formatArray, $cellValue);
  }

  # Check for any chars in symbol font that need to be changed to HTML
  if ($font eq 'Symbol') {
    my $str = $formatArray[$#formatArray];  # $#formatArray gives last index
    $str =~ s/\b(a)\b/&alpha;/g;
    $str =~ s/\b(b)\b/&beta;/g;
    $str =~ s/\b(g)\b/&gamma;/g;
    $str =~ s/\b(w)\b/&omega;/g;
    $formatArray[$#formatArray] = $str;
  }

  # 3. append any necessary closing tags
  if ($super == 1) {
    push (@formatArray, '</sup>');
  }
  elsif ($super == 2) {
    push (@formatArray, '</sub>');
  }
  if ($bold) {   # entire string is bold
    push (@formatArray, '</strong>');
  }
  else {         # string is em
    push (@formatArray, '</em>');
  }

  # 5. patch together the string
  foreach my $substring (@formatArray) {
    $formattedString .= $substring;
  }
  # remove empty <sup> tags
  $formattedString =~ s/<sup>(\s*?)<\/sup>//g;

  # convert '</ ' and '</</' to '&lt;/ ' and '&lt;/</' so that XML tag in grammar isn't interpreted as mark-up
  $formattedString =~ s/<\/(<\/| )/&lt;\/$1/g;

  # fix <<empty>> tags so that they use &laqou; and &raquo;
  $formattedString =~ s/<em>\Wempty\W<\/em>/<em>&laquo;empty&raquo;<\/em>/g;
  
  # remove any extra spaces inside <sup> tags
  $formattedString =~ s/<sup>(.*?)\s+<\/sup>/<sup>$1<\/sup>/g;  # post spaces
  $formattedString =~ s/<sup>\s+(.*?)<\/sup>/<sup>$1<\/sup>/g;  # pre spaces
  
  return $formattedString;
}


# ===============
# Subroutine ModifyTerminalTags
# Purpose: Change HTML tags for terminals from <strong> to <code>
#          in surface syntax
# ================
sub ModifyTerminalTags {
  my @lexemes = @_;
  foreach my $prod ( @grammarList ) {
     # iterate through surface syntax elements
     if (defined $grammar{$prod}[0]) {
       for (my $i = 1; $i <= $grammar{$prod}[0]; $i++) {
         # Use the @matches array to convert <strong> to <code> where needed
         # make a list of all <strong> tag content (don't include tags)
         my @matches = ( $grammar{$prod}[$i] =~ /<strong>(.*?)<\/strong>/g);

         # compare contents to @lexemes. If no match, change <strong> to <code>
         # use \Q to quote $element to avoid "nested quantifiers in regex" error
         # grep used here in scalar context, so it returns number of matches
         foreach my $element (@matches) {
           if ( grep (/\Q$element/, @lexemes) == 0)  {
             $grammar{$prod}[$i] =~ s/<strong>\Q$element<\/strong>/<code>$element<\/code>/;
           }
         }
       } # end for loop through rhs contents
     } # end if (defined...
  } # end "foreach my $prod" loop
}


# ===============
# Subroutine CreateSMLHash
# Purpose: Create hash with SML function names as keys and
#          function bodies as values.
# ===============
sub CreateSMLHash { 
  # iterate through each source file in the @smlFiles array
  foreach my $srcFile (@smlFiles) {
    my $srcPath = $smlSourceDir . '/' . $srcFile; 
    print $srcPath . "\n";
    my $funcName = '';
    my $codeBuffer = '';
    my $commentFlag = 0;
    open (SRC_FILE, "$srcPath");
    while (<SRC_FILE>) {
	  # first 'normalize' source from ast.sml, which indents 
	  # recursive type defs
	  if ($srcPath =~ /ast.sml/ && /^\s*(and.*)/) {
		$_ = $1;
      }
      # Do not change the order of these if and elsif statements
      # unless you know what you are doing. The tail statements
      # assume that comments are not an option
      if (/^(fun|and|type|datatype) (\w+)/) {
        if ($funcName eq '') {
          $funcName = $2;
          $codeBuffer = $_;
        }
        else { # create previous hash entry then reset values
          $smlHash{$funcName} = $codeBuffer;
          push (@funcList, $funcName);
          # reset values
          $funcName = $2;
          $codeBuffer = $_; # start new codeBuffer
        }
      }
      elsif (/^\s*\(\*/) {  # start of comment begins line
        if ($_ =! /\*\)/) { # if end of comment not found, set comment flag
          $commentFlag = 1;
        }
      }
      elsif (/\*\)/ && $commentFlag == 1) { # end of multi-line comment
        $commentFlag = 0;
      }
      elsif ( (/^\w+/) && ($commentFlag == 0) ) { # start of new expression
        if ($funcName ne '') {
          $smlHash{$funcName} = $codeBuffer;
          push (@funcList, $funcName);
          $funcName = '';
          $codeBuffer = '';
        }
      }
      else {
        if ($commentFlag == 0) {
          $codeBuffer .= $_;
        }
      }

    } # end while SRC_FILE loop

    close SRC_FILE;
  } # end foreach loop

} # end sub CreateSMLHash


sub PrintSMLHash {

  open (HASH_FILE, ">smlhash.txt");
  foreach my $func (@funcList) {
    print HASH_FILE "$func => $smlHash{$func}\n";
  }
  close HASH_FILE;
}

# ===============
# Subroutine Merge
# Purpose: Merge SML code & grammar into HTML files
# ================
sub Merge {
  my $mergeBuffer = '';
  my $grammarRangeStart = '';

  open (BFILE, $sourceHTML);
  while(<BFILE>) {
    # merge SML
    if ( $_ =~ /<pre class="code insert_ml">/ ) {
       my $text = $_; 
       my @funcNames = ();

       # parse string and place tag contents into array
       while ( length($text) ) {
         if ( $text =~ /<pre class="code insert_ml">(.*?)<\/pre>/) {
           push (@funcNames, $1);
           $text = $'; # reset $text to everything after the pre tag
         }
         else {
           $text = '';
         }
       }

       # Add sml code for each element of the array
       foreach my $item (@funcNames) { 
         if (defined $smlHash{$item}) {
           my $code = $smlHash{$item};
           $code =~ s/ /&nbsp;/g;
           $code =~ s/\n/<br\/>/g;
           $mergeBuffer .= '<span style="font-family:Arial, Helvetica, sans-serif"><strong>Implementation</strong></span><br/>';
           $mergeBuffer .= "<code>$code</code>";
         }
       }
    }
    # merge single grammar production
    elsif ( $_ =~ /<pre class="code grammar">/ ) {
      my $text = $_;
      my @lhsContents = ();
      
      # parse string and place tag contents into array
      while ( length($text) ) {
        if ( $text =~ /<pre class="code grammar">(.*?)<\/pre>/) {
          push (@lhsContents, $1);
          $text = $'; # reset $text to everything after the pre tag
        }
        else {
          $text = '';
        }
      }
      
      # Add grammar for each element of the array
      foreach my $item (@lhsContents) {
        if ($item !~ /^<em>.*<\/em>$/) {
           $item = '<em>' . $item . '</em>';
        }
        $item =~ s/&lt;/</g;
        $item =~ s/&gt;/>/g;
        $item =~ s/&#945;/&alpha;/g;   # Greek small letter alpha
        $item =~ s/&#946;/&beta;/g;    # Greek small letter beta
        $item =~ s/&#969;/&omega;/g;   # Greek small letter omega

        my $grammar = GenerateGrammar($item);
        if ($grammar ne '') {
	      $mergeBuffer .= '<span style="font-family:Arial, Helvetica, sans-serif"><strong>Syntax</strong><br/>';
          $mergeBuffer .= $grammar;
          $mergeBuffer .= '</span>';
        }
      }

    }
    # merge range of grammar productions
    # grammar_start and grammar_end tags must be on separate lines
    elsif ( $_ =~ /<pre class="code grammar_start">/ ) {
      if ( $_ =~ /<pre class="code grammar_start">(.*?)<\/pre>/) {
        $grammarRangeStart = $1;
      }
    }
    elsif ( $_ =~ /<pre class="code grammar_end">/ ) {
      my $text = $_;
      my @lhsContents = ();
      my $grammarRangeEnd = '';
      my $inRangeFlag = 0;

      if ( $text =~ /<pre class="code grammar_end">(.*?)<\/pre>/) {
	    $grammarRangeEnd = $1;
      }

      # build array of productions by iterating through $grammarList
      foreach my $prod (@grammarList) {

        if ( $prod eq $grammarRangeStart) {
          push (@lhsContents, $prod);
          $inRangeFlag = 1;
        }
        elsif ($inRangeFlag) {
          push (@lhsContents, $prod);
          if ($prod eq $grammarRangeEnd) {
            $inRangeFlag = 0;
          }
        }
      }
	  	
	  $mergeBuffer .= '<span style="font-family:Arial, Helvetica, sans-serif"><strong>Syntax</strong><br/>';
      
      # Add grammar for each element of the array
      foreach my $item (@lhsContents) {
        if ($item !~ /^<em>.*<\/em>$/) {
           $item = '<em>' . $item . '</em>';
        }
        $item =~ s/&lt;/</g;
        $item =~ s/&gt;/>/g;
        $item =~ s/&#945;/&alpha;/g;   # Greek small letter alpha
        $item =~ s/&#946;/&beta;/g;    # Greek small letter beta
        $item =~ s/&#969;/&omega;/g;   # Greek small letter omega

        my $grammar = GenerateGrammar($item);
        if ($grammar ne '') {
          $mergeBuffer .= $grammar;
        }    
      }
      $mergeBuffer .= '</span>';

    }
    # print out complete grammar summary
    elsif ( $_ =~ /<pre class="code grammarSummary">/ ) {

      # Add grammar for each element of the array
      foreach my $prod (@grammarList) {
        if (defined $grammar{$prod}[0]) {
          $mergeBuffer .= GenerateGrammar($prod);
        }
        else { # If first element of hash array undef, this is a header
          if ($prod =~ /SURFACE SYNTAX/) {
            $mergeBuffer .= "<h2>LEXICAL STRUCTURE</h2>\n";
          }
          elsif ($prod =~ /EXPRESSIONS/) {
            $mergeBuffer .= "<h2>SYNTACTIC STRUCTURE</h2>\n<h3>EXPRESSIONS</h3>\n";
          }
          else {
            $prod =~ s/h1>/h3>/g;
            $mergeBuffer .= $prod;
          }
        }
      }

    }
     
    else {
	  $mergeBuffer .= $_;
    } 
  }

  # === Write buffer to file ===
  open (DFILE, ">$outputFile");
  print DFILE $mergeBuffer;
  close DFILE;

}


# ===============
# Subroutine ParseWikiText
# Purpose: Read text in from file and parse
# ================
sub ParseWikiText {
  my $endBuffer = "<html>\n";
  my $tableBuffer = '';     # buffer for table text
  my $inParagraph = 0;      # flag when inside paragraph
  my $inOList = 0;          # flag when inside <ol></ol>
  my $inUList = 0;          # flag when inside <ul></ul>
  my $inTable = 0;          # flag when inside table
  my $inGrammar = 0;        # flag when inside grammar production table
  my $inPreTag = 0;         # flag when inside pre tag
  my $chapterTitle = '';
  open (BFILE, "startBuffer.txt");
    while(<BFILE>) {
     if ( $_ =~ /<title>spec:chapter_\d+_(.+) \[ES4 Wiki\]<\/title>/) {
       $chapterTitle = $1;
       $chapterTitle =~ s/_/ /g;
       $chapterTitle =~ s/^(.)/\u$1/;      # Upper case first letter
       $endBuffer .= "<h1>" . $chapterTitle . "</h1>\n";
     }
     elsif ( $_ =~ /<h1>(.+)<\/h1>/) {
       if ($1 ne $chapterTitle) {
         $endBuffer .= "<h2>" . $1 . "</h2>\n";
       }
     }
     elsif ( $_ =~ /<h2>(.+)<\/h2>/) {
       $endBuffer .= "<h3>" . $1 . "</h3>\n";
     }
     elsif ( $_ =~ /<h3>(.+)<\/h3>/) {
       $endBuffer .= "<h4>" . $1 . "</h4>\n";
     }
     elsif ( $_ =~ /<h4>(.+)<\/h4>/) {
       $endBuffer .= "<h5>" . $1 . "</h5>\n";
     }
     elsif ( $_ =~ /<p>/) {
       $endBuffer .= $_;
       $inParagraph = 1;
     }
     elsif ( $inParagraph ) {
       # check for grammar production, $inParagraph must be exactly 1
       if ( ($inParagraph == 1)
                &&  ( ($_ =~ /<em>.+<\/em>.+&rarr;/)
                   || ($_ =~ /<strong>.+<\/strong>.+&rarr;/)  ) ){
         $inGrammar = 1;
       }
       elsif ($inGrammar) {
         if ($inParagraph == 2) {
           $_ =~ s/^(.*)/&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;$1/;
         }
         else {
           $_ =~ s/(<code>\|.*<\/code>)/&nbsp;&nbsp;&nbsp;$1/;
         }
       }
       
       # increment $inParagraph for line nums or reset if closing tag found
       if ( $_ =~ /<\/p>/) {
         $inParagraph = 0;
         $inGrammar = 0;
       }
       else {
         $inParagraph++;
       }
       
       # convert curly quotes and write to buffer
       my $line = CurlyToHTMLCurly($_);
       $endBuffer .= $line;
     }
     elsif ( $_ =~ /<ul>/) {
       $endBuffer .= $_;
       $inUList++;           # increment to handle nested lists
     }
     elsif ( $inUList ) {
       if ( $_ =~ /<\/ul>/) {
         $inUList--;
       }
       my $line = CurlyToHTMLCurly($_);
       $endBuffer .= $line;
     }
     elsif ( $_ =~ /<ol>/) {
       $endBuffer .= $_;
       $inOList++;           # increment to handle nested lists
     }
     elsif ( $inOList ) {
       if ( $_ =~ /<\/ol>/) {
         $inOList--;
       }
       my $line = CurlyToHTMLCurly($_);
       $endBuffer .= $line;
     }
     elsif ( $_ =~ /<table/) {
       $tableBuffer .= $_;              # place current line in table buffer
       $inTable = 1;                    # set $inTable flag
     }
     elsif ( $inTable ) {
       # check for end of table, clean up if found
       if ( $_ =~ /<\/table/) {
         # add table border
         $tableBuffer =~ s/<table/<table border="1" cellspacing="0" cellpadding="3"/;
         $tableBuffer .= $_;
         $endBuffer .= $tableBuffer;    # write table to general buffer
         $tableBuffer = '';             # reset tableBuffer
         $inTable = 0;                  # reset table flag
        }
       # if not end of table, write line to table buffer
       else {
         my $line = CurlyToHTMLCurly($_);
         $tableBuffer .= $line;
       }
     }
     # merge grammar
     elsif ( $_ =~ /<pre class="code grammar">/ ) {
       my $text = $_;
       my @lhsContents = ();
       
       # parse string and place tag contents into array
       while ( length($text) ) {
         if ( $text =~ /<pre class="code grammar">(.*?)<\/pre>/) {
           push (@lhsContents, $1);
           $text = $'; # reset $text to everything after the pre tag
         }
         else {
           $text = '';
         }
       }
       
       # Add grammar for each element of the array
       foreach my $item (@lhsContents) {
         $item = '<em>' . $item . '</em>';
         $item =~ s/&lt;/</g;
         $item =~ s/&gt;/>/g;
         $item =~ s/&#945;/&alpha;/g;   # Greek small letter alpha
         $item =~ s/&#946;/&beta;/g;    # Greek small letter beta
         $item =~ s/&#969;/&omega;/g;   # Greek small letter omega

         my $grammar = GenerateGrammar($item);
         if ($grammar ne '') {
           $endBuffer .= $grammar;
         }
       }

     }
     # print out complete grammar summary
     elsif ( $_ =~ /<pre class="code grammarSummary">/ ) {

       # Add grammar for each element of the array
       foreach my $prod (@grammarList) {
         if (defined $grammar{$prod}[0]) {
           $endBuffer .= GenerateGrammar($prod);
         }
         else { # If first element of hash array undef, this is a header
           if ($prod =~ /SURFACE SYNTAX/) {
             $endBuffer .= "<h2>LEXICAL STRUCTURE</h2>\n";
           }
           elsif ($prod =~ /EXPRESSIONS/) {
             $endBuffer .= "<h2>SYNTACTIC STRUCTURE</h2>\n<h3>EXPRESSIONS</h3>\n";
           }
           else {
             $prod =~ s/h1>/h3>/g;
             $endBuffer .= $prod;
           }
         }
       }

     }
     # print out section of grammar
     elsif ( $_ =~ /<pre class="code grammarSection">/ ) {
       # ascertain which section (should be tag contents)
       if ($_ =~ /<pre class="code grammarSection">(.*?)<\/pre>/ ) {
         my $section = $1;
         my $inSectionFlag = 0; # currently processing selected section
         # iterate through grammar list
         foreach my $prod (@grammarList) {
           # If first element of hash array undef, this is a header
           if (!defined $grammar{$prod}[0]) {
             if ($prod =~ /$section/i) {
                $inSectionFlag = 1;
             }
             else {
                $inSectionFlag = 0;
             }
           }
           elsif ($inSectionFlag) {
             $endBuffer .= GenerateGrammar($prod);
           }
         }
       }
     }
     # merge smlnj code
     elsif ( $_ =~ /<pre class="code insert_ml">/ ) {
       my $text = $_;
       my @funcNames = ();

       # parse string and place tag contents into array
       while ( length($text) ) {
         if ( $text =~ /<pre class="code insert_ml">(.*?)<\/pre>/) {
           push (@funcNames, $1);
           $text = $'; # reset $text to everything after the pre tag
         }
         else {
           $text = '';
         }
       }

       # Add sml code for each element of the array
       foreach my $item (@funcNames) {
         if (defined $smlHash{$item}) {
           my $code = $smlHash{$item};
           $code =~ s/ /&nbsp;/g;
           $code =~ s/\n/<br\/>/g;
           $endBuffer .= "<code>$code</code>";
         }
       }

     }
     elsif ( $_ =~ /<pre/) {
       my $line = CurlyToStraight($_);
       $endBuffer .= $line;
       if ($_ !~ /<\/pre/) {    # set $inPreTag if line has no closing </pre>
         $inPreTag = 1;
       }
     }
     elsif ( $inPreTag ) {
       if ( $_ =~ /<\/pre/) {
         $inPreTag = 0;
       }
       my $line = CurlyToStraight($_);
       $endBuffer .= $line;
     }
     elsif ( $_ =~ /^<br \/>/ ) {
       $endBuffer .= $_;
     }
 }
  $endBuffer .= '</html>';
  close BFILE;
  
  # === Write buffer to file ===
  open (DFILE, ">wikiExport.doc");
  print DFILE $endBuffer;
  close DFILE;
}

# ===============
# Subroutine CurlyToStraight
# Purpose: Convert "curly" quotes to straight quotes inside <pre /> tags
# ================

sub CurlyToStraight {
  my ($line) = @_;
  $line =~ s/\x{e2}\x{80}\x{99}/\'/g;
  $line =~ s/\x{e2}\x{80}\x{98}/\'/g;
  $line =~ s/\x{e2}\x{80}\x{9c}/\"/g;
  $line =~ s/\x{e2}\x{80}\x{9d}/\"/g;
  return $line;
}

# ===============
# Subroutine CurlyToHTMLCurly
# Purpose: Convert "curly" quotes to HTML curly quotes
# ================

sub CurlyToHTMLCurly {
  my ($line) = @_;
  $line =~ s/\x{e2}\x{80}\x{99}/&#8217;/g;
  $line =~ s/\x{e2}\x{80}\x{98}/&#8216;/g;
  $line =~ s/\x{e2}\x{80}\x{9c}/&#8220;/g;
  $line =~ s/\x{e2}\x{80}\x{9d}/&#8221;/g;
  return $line;
}

# ===============
# Subroutine GenerateGrammar
# Purpose: return surface syntax and abstract syntax for a given production
# ================
sub GenerateGrammar {
  my ($item) = @_;
  my $endBuffer = '';
  my $numSurfaceSyntax = 0;
  my $indent = "&nbsp;&nbsp;&nbsp;&nbsp;";

  $numSurfaceSyntax = $grammar{$item}[0];

  if (defined $numSurfaceSyntax) {

    my $totalSyntax = @{$grammar{$item}};

    # lhs of surface syntax
    $endBuffer .= $item;
    if ($item !~ /=\s+\{.*\}/) {  # add arrow if item is not "a = {isNot, }" etc.
      $endBuffer .= "&nbsp;&nbsp;&nbsp;&rarr; ";
    }
    $endBuffer .= "<br/>\n";

    # first element on rhs of surface syntax (no | symbol)
    if (defined $grammar{$item}[1]) {
      $endBuffer .= "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" . $grammar{$item}[1] . "<br/>\n";

      # remaining surface syntax elements
      for (my $k = 2; $k <= $numSurfaceSyntax; $k++) {
	    # suppress pipe (|) character in cases where a line is part of previous production
	    # place 17 non-breaking spaces instead of pipe character
	    if ($grammar{$item}[$k] =~ /^<(strong|em)>\s\s\s\s\s\s+/) {
          for (my $i= 0; $i<17; $i++) {
            $endBuffer .= "&nbsp;"
          }
        }
        else {
          $endBuffer .= "&nbsp;&nbsp;&nbsp;<code>|</code>&nbsp;"
        }
        $endBuffer .= $grammar{$item}[$k] . "<br/>\n";
      }
      $endBuffer .= "<br/>\n";
    }

    # skip to next item if current item contains no abstract syntax elements
    if (defined $grammar{$item}[$numSurfaceSyntax + 1]) {

      # abstract syntax header
      $endBuffer .= "$indent<code>$grammar{$item}[$numSurfaceSyntax + 1]</code><br/>\n";

      # remaining abstract syntax elements
      for (my $j = $numSurfaceSyntax + 2; $j < $totalSyntax; $j++) {
        $endBuffer .= "$indent&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<code>" . $grammar{$item}[$j] . "</code><br/>\n";
      }
      $endBuffer .= "<br/>\n<br/>\n";
     }
  }
  return $endBuffer;
}
