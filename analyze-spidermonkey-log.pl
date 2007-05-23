#!/usr/bin/perl -w

my $searching_for_err = 0;
my $should_crash = 0;
my $ecma = '';
my $section = '';
my $test = '';

my $passes = 0;
my $fails = 0;

my $runs = 0;
my $completions = 0;
my $errors = 0;
my $xerrors = 0;
my $alarms = 0;
my $crashes = 0;

my $parseErrs = 0;
my $defnErrs = 0;
my $verifyErrs = 0;

my $evalErrs = 0;
my $machErrs = 0;
my $hostErrs = 0;

while (<>) {
    s/\(near [^\)]*\)//go;
    if (/^sml \@SMLload.*tests\/spidermonkey\/(\w+)\/(\w+)\/(.+)\.js$/) {
	$runs++;
	if ($searching_for_err) {
	    $completions++;
	}
	$searching_for_err = 1;
	$ecma = $1;
	$section = $2;
	$test = $3;
	$should_crash = 0;
	if ($test =~ /-n$/) {
	    $should_crash = 1;
	}
    } elsif ($searching_for_err) {
	if (/\*\*ERROR\*\* *(?:\([^\)]+\))? *(.*)|(^uncaught exception.*)/) {
	    my $err = $1 || $2;
	    if ($should_crash) {
		$xerrors++;
	    } else {
		$errs->{$err}++;
		$errors++;
		if ($err =~ /parseError/) { $parseErrs++; }
		elsif ($err =~ /defnError/) { $defnErrs++; }
		elsif ($err =~ /verifyError/) { $verifyErrs++; }
		elsif ($err =~ /evalError/) { $evalErrs++; }
		elsif ($err =~ /machError/) { $machErrs++; }
		elsif ($err =~ /hostError/) { $hostErrs++; }
	    } 
	    $searching_for_err = 0;
	} elsif (/make: \*\*\* \[run-dumped\] Error 1/) {
	    $crashes++;
	    $searching_for_err = 0;
	} elsif (/Alarm clock/) {
	    $alarms++;
	    $searching_for_err = 0;
	}
    }

    if (/PASSED!/) {
	$passes++;
    }

    if (/FAILED!/) {
	$fails++;
    }
}

if ($searching_for_err) {
    $completions++;
}

my $n = 30;
printf ("---------------------------------------------------\n");
printf ("Top $n crashes:\n");
printf ("---------------------------------------------------\n");

my $j = 1;
my $top = 0;
for my $i (sort {$errs->{$b} <=> $errs->{$a}} keys(%$errs)) {
    printf ("%2d: %5d %.150s\n", $j, $errs->{$i}, $i);
    $top += $errs->{$i};
    if ($j++ == $n) {
	last;
    }
}

sub max { 
    return $_[0] > $_[1] ? $_[0] : $_[1];
}

printf ("---------------------------------------------------\n");
printf ("%d completions, %d xerrors, %d errors, %d alarms, %d unknown crashes\n", $completions, $xerrors, $errors, $alarms, $crashes);
printf ("%d parseErrors, %d defnErrors, %d verifyErrors\n", $parseErrs, $defnErrs, $verifyErrs);
printf ("%d evalErrors, %d machErrors, %d hostErrors\n", $evalErrs, $machErrs, $hostErrs);
printf ("%d PASSED, %d FAILED\n", $passes, $fails);
printf ("---------------------------------------------------\n");
printf ("%d%% of crashes in top $n\n", ($top / max($errors,1)) * 100);
printf ("%d%% of %d runs OK or xerror\n", (($completions + $xerrors) / max(1,$runs)) * 100, $runs);
printf ("%d%% of %d executed cases PASSED\n", ($passes / max(1,($passes + $fails))) * 100, $passes + $fails);
printf ("---------------------------------------------------\n");


