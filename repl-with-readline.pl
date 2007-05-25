#!/usr/bin/perl -w

use Term::ReadLine;
use IPC::Open2;

my $term = new Term::ReadLine 'ES4 repl';
my $prompt = ">> ";
my ($sml_response, $sml_call);
my $sml_pid = open2($sml_response, $sml_call, 'sml', '@SMLload=repl.heap', '-q');
my $input = "", $response = "";

while (1) {
    while (1) {
	$response = <$sml_response>;
	if (not defined($response)) { exit(0); }
	last if ($response =~ /<SMLREADY>$/);
	print $response;
    }

    $input = $term->readline($prompt);
    if ((not defined($input)) || $input =~ /^:q/) {
	print $sml_call ":q\n";
	exit(0);
    }
    
    print $sml_call $input, "\n";
}
