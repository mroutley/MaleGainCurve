#!/usr/bin/perl -w
use strict;
use utf8;
#####
#Open file handlers
#####
open INPUT, "<inputFile.txt"	#Contains the vials used & their weights
	or die "Cannot open input file: $!";
open VIALS, "<MGCPollenCountVialWeights.txt"	#Static file containing empty vial weights
	or die "Cannot open vial weight file: $!";
open COUNT, ">outputFile.txt"	#The summary file
	or die "Cannot create txt file: $!";
print COUNT "ID\tsampleCount\tantherCount\n";
#####
#Set defaults & hashes
#####
my $startLine = 37;	#Skip initial lines
my $channels = 128;	#Number of data channels
my $samples = 3;	#How many samples were taken from each vial?
my $sampleVolume = 1;	#How much is sampled?
my %sampleVials;	#Stores the vials used, filename as key
my %sampleWeights;	#Stores the weights of vials with sample, filename as key
my %vialWeights;	#Data from VIALS
#####
#Parse the input file
#####
while (<INPUT>) {
	unless ($. == 1) {	#Skip header
	chomp;
	@. = split;
	@sampleVials{$.[0]} = ($.[1]);
	@sampleWeights{$.[0]} = ($.[2]);
}}
close INPUT;
#####
#Parse the vial file
#####
while (<VIALS>) {
	unless ($. == 1) {	#Skip header
		chomp;
		@. = split;
		@vialWeights{$.[0]} = ($.[1]);
	}
}
close VIALS;
#####
#Parse the hst files
#####
foreach my $file (glob "*.HST") {
	open HST, "<$file"
		or die "Cannot open hst file: $!";
	my $sampleCount=0;	#Pollen counts for each sample
	my $filename = substr($file, 0, -4);	#Drop the file extension
	do {<HST>} until $. == $startLine	#Skip header lines
		or eof;
	while (<HST>) {
		unless ($. > $startLine + $channels) {	#Only process data lines
			chomp;
			$sampleCount += $_;
		}
	}
	my $vial = $sampleVials{$filename};
	my $sampleWeight = $sampleWeights{$filename};
	my $vialWeight = $vialWeights{$vial};
	my $pollenCount = ($sampleCount * ($sampleWeight - $vialWeight))/($samples * $sampleVolume);
	print COUNT "$filename\t$sampleCount\t$pollenCount\n";
	close HST;
}
close COUNT;