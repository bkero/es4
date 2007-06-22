#!/usr/bin/perl -w

# repl-with-readline.pl - runs the repl with GNU readline
# Copyright (C) 2007 Adobe Systems Inc., The Mozilla Foundation,
# Opera Software ASA, and others.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

use Term::ReadLine;
use IPC::Open2;

my $term = new Term::ReadLine 'ES4 repl';
my $prompt = ">> ";
my ($sml_response, $sml_call);
my $sml_pid = open2($sml_response, $sml_call, 'sml', '@SMLload=es4-dump.heap', '-r', '-I');
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
	while ($response = <$sml_response>) { print($response); }
	exit(0);
    }

    print $sml_call $input, "\n";
}
