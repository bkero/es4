#!/usr/bin/perl -w

my @files = glob("*.sml");
my $defs = {};
my $uses = {};

for my $file (@files) {
    open(FILE, $file);
    my $line = 0;
    while (<FILE>) {
	$line++;    
	if (/^(fun|and) (\w+)/) {
	    $defs->{$2} = "$file:$line";
	}
    }
    close(FILE);
}

for my $file (@files) {
    open(FILE, $file);
    while (<FILE>) {
	# chop off the definitions themselves
	s/^(fun|and) (\w+)//go;
	
	my @words = split(/\b/, $_);
	for my $word (@words) {
	    if (exists ($defs->{$word})) { 
		$uses->{$word}++;
	    }
	}
    }
    close(FILE);
}

my @lines = ();
for my $def (keys(%$defs)) {
    if (not (exists ($uses->{$def}))) {
	push (@lines, sprintf("%s:E fun %s \n", $defs->{$def}, $def));
    }
}

print(sort (@lines));


