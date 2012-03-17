#!/usr/bin/perl -w
use strict;
use utf8;
#Used to generate input file with the correct file ids
open INPUT, ">inputFile.txt"
	or die "Cannot create txt file: $!";
print INPUT "file\tvialNumber\tvialWeight\n";

foreach my $file (glob "*.HST") {	#Cycle through each hst file
	my $filename = substr($file, 0, -4);	#Drop the file extension
	print INPUT "$filename\t\t\n";
}
close INPUT;