#!/usr/bin/perl -w

# The following licensing terms and conditions apply and must be
# accepted in order to use the Reference Implementation:
# 
#    1. This Reference Implementation is made available to all
# interested persons on the same terms as Ecma makes available its
# standards and technical reports, as set forth at
# http://www.ecma-international.org/publications/.
# 
#    2. All liability and responsibility for any use of this Reference
# Implementation rests with the user, and not with any of the parties
# who contribute to, or who own or hold any copyright in, this Reference
# Implementation.
# 
#    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
# HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
# BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
# OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
# IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# End of Terms and Conditions
# 
# Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
# Software ASA, and others.

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
    #remove all (near ...) text in the errors so all errors can be compared
    s/\(near [^\)]*\)//go;
    if (/sml \@SMLload.*tests\/spidermonkey\/(\w+)\/(\w+)\/(.+)\.js$/) {
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
    # as3 test log
    } elsif (/sml \@SMLload.*tests\/as\/.*\/(.+)\.as$/) {
	$runs++;
	if ($searching_for_err) {
	    $completions++;
	}
	$searching_for_err = 1;
	$test = $1;
	$should_crash = 0;
    } elsif ($searching_for_err) {
	if (/\*\*ERROR\*\* *(?:\([^\)]+\))? *(.*)|(^uncaught exception.*)/) {
	    my $err = $1 || $2;
	    if ($should_crash) {
		$xerrors++;
	    } else {
		$errs->{$err}++;
		$errors++;
		if ($err =~ /ParseError/) { $parseErrs++; }
		elsif ($err =~ /DefnError/) { $defnErrs++; }
		elsif ($err =~ /VerifyError/) { $verifyErrs++; }
		elsif ($err =~ /EvalError/) { $evalErrs++; }
		elsif ($err =~ /MachError/) { $machErrs++; }
		elsif ($err =~ /HostError/) { $hostErrs++; }
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
printf ("%d ParseErrors, %d DefnErrors, %d VerifyErrors\n", $parseErrs, $defnErrs, $verifyErrs);
printf ("%d EvalErrors, %d MachErrors, %d HostErrors\n", $evalErrs, $machErrs, $hostErrs);
printf ("%d PASSED, %d FAILED\n", $passes, $fails);
printf ("---------------------------------------------------\n");
printf ("%d%% of crashes in top $n\n", ($top / max($errors,1)) * 100);
printf ("%d%% of %d runs OK or xerror\n", (($completions + $xerrors) / max(1,$runs)) * 100, $runs);
printf ("%d%% of %d executed cases PASSED\n", ($passes / max(1,($passes + $fails))) * 100, $passes + $fails);
printf ("---------------------------------------------------\n");


