#!/usr/bin/perl -w

use Term::ReadLine;
use FileHandle;

my $term = new Term::ReadLine 'ES4 repl';
my $prompt = ">> ";
my $sml = new FileHandle("|sml \@SMLload=es4.heap -rq");
my $line = "";

while ( defined ($line = $term->readline($prompt)) ) {
    $sml->print($line);
    $sml->print("\n");
    $sml->flush();
    if ($line =~ /^:q/) { exit(0); }
    $term->addhistory($line) if ($line =~ /\S/);

    # This is pathetic, but it returns faster than main::sleep(1) and
    # seems to be sufficient.
    system("true");
    system("true");    
    system("true");
}
