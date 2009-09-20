#!/usr/bin/perl

# Insertion of pstricks figs in templates
# by Adrien LELONG (www.lelongdunet.com)
#


use strict;

my $fileName = $ARGV[0];
my $templateName = $ARGV[1];

#Open the source file
open(file, $fileName) or dienice ("%file open failed");
my @data = <file>;
close(file);

open(file, $templateName) or dienice ("%template file open failed");
my @tmplData = <file>;
close(file);

my $lines = scalar(@data); #Number of lines in file
my $tmplLines = scalar(@tmplData); #Number of lines in template
my $line = 0;
my $insertLine = -1;

for($line = 0; $line < $tmplLines; $line++) {
    if($tmplData[$line] =~ m/INSERT_FIG/) {
        $insertLine = $line;
        $line = $tmplLines;
    }
}

if($insertLine == -1) {
    print("%No insertion point in template!\n");
    exit;
}


#Write on standard output
for($line = 0; $line < $insertLine; $line++) {
    print("$tmplData[$line]");
}

for($line = 0; $line < $lines; $line++) {
    print("$data[$line]");
}

for($line = $insertLine + 1; $line < $tmplLines; $line++) {
    print("$tmplData[$line]");
}





