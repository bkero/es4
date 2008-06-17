#====================================================================
# Name: mergeGrammar.pl
#
# Purpose: Merges HTML file with ES4 Grammar
#
# Author: Francis Cheng
# Date: April 2008 (based on Oct 2006 exportES4Wiki.pl script)
#
# Usage: mergeGrammar [HTML_input_file] [Output_file]
#
# Output File: The output file is identical to the input file, except that
# any <pre> tags with a 'class' attribute set to 'code grammar', 'code grammar_start',
# or 'code grammar_end' are replaced with the corresponding sections of the grammar
# as excerpted from the grammar.xls file in monotone. Only <pre> tags that are 
# within HTML comment tags are processed. This script is designed
# to be run before the stitch.py script.
# 
# IMPORTANT NOTES: 
# 1. The initial <pre> tag must be on the same line as the opening
#    HTML comment tag <!-- and the last <pre> tag must be on the same line
#    as the closing HTML comment tag --> For example:
#    <!-- <pre class="code grammar_start"><em>ArrayInitialiser<sup>noColon</sup></em></pre>
#    <pre class="code grammar_end"><em>SpreadExpression</em></pre>	-->
# 2. Two <br/> tags are prepended to the grammar listing in the output file to every
#    HTML comment block that contains a grammar <pre> tag. You can change the
#    number of <br/> tags with the $spaceBeforeGrammarBlock variable in the
#    CONFIGURATION SECTION.
#
# Other Output: In addition to the merged output file, the PrintGrammarToFile()
# subroutine prints the entire Grammar to a text file. This allows you
# to see how each production is represented in HTML. This HTML markup version
# is the version you must use inside the <pre> tags in the HTML_input_file.
# Change $printGrammarTextFile to 1 in the CONFIGURATION SECTION if you want
# to print this file.
#
# Script notes: This script runs on Perl 5.8 (ActiveState installation)
# and requires the following modules (which can be installed using 
# Perl Package Manager):
# -> ppm install spreadsheet-parseexcel
# -> ppm install OLE::Storage_Lite
# -> ppm install IO::Scalar
#
# This script should be run from its location in the es4 monotone file structure

use strict;
use warnings;
use Spreadsheet::ParseExcel; # for parsing grammar.xls file


# === CONFIGURATION SECTION ====
my $grammarFile = 'grammar.xls'; # expects script to run in root of spec directory
our ($sourceHTML, $outputFile, %grammar, @grammarList);  # globals
my $spaceBeforeGrammarBlock = '<br/><br/>';
my $printGrammarTextFile = 0; # change to 1 if you want to print grammar.txt file

# === Read in command line arguments
if ($ARGV[0] ne "") {
  $sourceHTML = $ARGV[0];
}
else {
  print "ERROR: You must specify the source HTML source directory path.\n";
  exit(0);
}

if ($ARGV[1] ne "") {
  $outputFile = $ARGV[1];
}
# === construct $outputFile string based on input file
elsif ($sourceHTML =~ /\/(\w+)\.html?/) {
  $outputFile = $1 . '_merged.html';
}
else {
  $outputFile = $sourceHTML . '_merged.html';
}


# === Call subroutines

ParseGrammar($grammarFile);

if ($printGrammarTextFile) {
	PrintGrammarToFile();
}

Merge($spaceBeforeGrammarBlock);



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
   my ($grammarXLS) = @_;
   my $myBook = Spreadsheet::ParseExcel::Workbook->Parse($grammarXLS);
   my $ES4Worksheet = $myBook->Worksheet('ES4');
   my $maxRow = $ES4Worksheet->{MaxRow};
   my @lexemes = ();  # array to hold list of lexemes

   my $iCol = 8;  # initial value of $iCol is 8 (column I in the spreadsheet)
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
		   # instances of ... in the .xls file show up as an ampersand
		   # so we need to convert lone instances of ampersand to ...
		   $rhsContent =~ s/<strong>&\s?<\/strong>/<strong>...<\/strong>/;
           push(@rhs, $rhsContent);
           $num++;
         }
         # Place the Abstract Syntax into a list
         my @abstractSyntax;
         # First line of abstract syntax has two-column offset
         my $abstractHeaderCell = $ES4Worksheet->{Cells}[$iRow][$iCol+3];
         if ($abstractHeaderCell && $abstractHeaderCell->Value) {
           push(@abstractSyntax, $abstractHeaderCell->Value);
         }
         # Remainder of abstract syntax has three-column offset
         my $aNum = 0;
         while ($ES4Worksheet->{Cells}[$iRow+1+$aNum][$iCol+4] &&
                $ES4Worksheet->{Cells}[$iRow+1+$aNum][$iCol+4]->Value) {
           push(@abstractSyntax, $ES4Worksheet->{Cells}[$iRow+1+$aNum][$iCol+4]->Value);
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
          $str =~ s/\b(t)\b/&tau;/g;
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
          $str =~ s/\b(t)\b/&tau;/g;
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
    $str =~ s/\b(t)\b/&tau;/g;
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
# Subroutine Merge
# Purpose: Merge grammar into HTML files
# ================
sub Merge {
  my ($spaceBefore) = @_;
  my $mergeBuffer = '';
  my $grammarRangeStart = '';
  my $firstGrammarProduction = 0;

  open (BFILE, $sourceHTML);
  while(<BFILE>) {
	#################################
    # merge single grammar production
    #################################
    if ( $_ =~ /<pre class="code grammar">/ ) {
      my $text = $_;
      my @lhsContents = ();

      # check for start of HTML comment, signifies first grammar prod
      if ($text =~ /<!--/) {
	    $firstGrammarProduction = 1;
	  }
      
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
	      if ($firstGrammarProduction) {
		    $mergeBuffer .= $spaceBefore;
		    $firstGrammarProduction = 0;
		  }
          $mergeBuffer .= '<span>' . $grammar . '</span>';
        }
      }

    }
    ##############################################################
    # merge range of grammar productions
    # grammar_start and grammar_end tags must be on separate lines
    ##############################################################
    elsif ( $_ =~ /<pre class="code grammar_start">/ ) {
      if ( $_ =~ /<pre class="code grammar_start">(.*?)<\/pre>/) {
        $grammarRangeStart = $1;
      }
      # check if this is first grammar prod, which needs space before
      if ($_ =~ /<!--/) {
	    $firstGrammarProduction = 1;
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

      # check if this is start of syntax section
      if ($firstGrammarProduction) {
	    $mergeBuffer .= $spaceBefore;
	    $firstGrammarProduction = 0;
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
          $mergeBuffer .= '<span>' . $grammar;
        }    
      }
      $mergeBuffer .= '</span>';
    }
    ####################################
    # print out complete grammar summary
    ####################################
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
    ##########################
    # Leave original unchanged
    ########################## 
    else {
	  $mergeBuffer .= $_;
    } 
  }

  close BFILE;

  # === Write buffer to file ===
  open (DFILE, ">$outputFile");
  print DFILE $mergeBuffer;
  close DFILE;

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
