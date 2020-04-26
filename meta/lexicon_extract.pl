#!/usr/bin/perl

use warnings;
use strict;
use List::Util qw(pairs);

system ("wget https://conwaylife.com/ref/lexicon/zip/lex_htm1/lexicon.htm");
my $lexicon = get_all_pattern_string('lexicon.htm');
my @code = map { pattern_to_code($_, $lexicon->{$_}) } (sort keys %$lexicon); 

open my $hs, '>', 'ConwayLexicon.hs';
print $hs get_imports();
print $hs @code;
close $hs;


# Extracts the names and patterns (expressed as strings) from the HTML
# Returns a hash with the pattern name as keys and pattern strings as values
sub get_all_pattern_string {
	my ($file) = @_;

	open(my $fh, '<', $file);
	my ($name, $pattern, %lexicon);

	while(my $line = <$fh>) {
		chomp $line;
		if ($line =~ /:<b>([^<]+)<\/b>/i) {
			$name = $1;

			# Modify name to remove spaces, punctuation and make camel case
			$name =~ s/[\s'-\/\$](\w)/\U$1\L/g;
			$name =~ s/\+/Plus/g;
			$name =~ s/^([a-z])/\U$1\L/g;
			$name = "pattern$name";
		}
		elsif ($line =~ /<pre>/i) {
			$pattern = "";
			$line = <$fh>;
			while ($line =~ /([.O]+)\s*/i) {
				$pattern .= "$1\n";
				$line = <$fh>;
				if ($line =~ /<\/pre>/) {
					$lexicon{$name} = $pattern;
					last;
				}
			}
		}
	}
	close $fh;

	return \%lexicon;
}

# Transforms a pattern from a string representation (O for alive, . for dead)
# of Game of Life to a sparse matrix representation in which only the alive
# cells are documented by their position i,j (line, column) in the matrix
#
# Example:  Glider Pattern  ->  Glider Sparse Matrix  
#                                      (0, 0)
#               000                    (0, 1)
#               0..         ->         (0, 2)
#               .0.                    (1, 0)
#                                      (2, 1)       
       
sub pattern_string_to_sparse_matrix {
	my ($pattern) = @_;

	chomp $pattern;
	my @sparse = ();

	my @lines = split('\n',$pattern);
	for (my $i = 0; $i < scalar(@lines); $i++) {
		my @chars = split('', $lines[$i]);
		for (my $j = 0; $j < scalar(@chars); $j++) {
			if ($chars[$j] eq 'O') {
				push @sparse, ($i, $j);
			}
		}
	}

	return @sparse;
}

# Generates Haskell code that defines a 2D array comonad with that pattern
# as an initial configuration
#
# Example:
#  
#     glider :: GameOfLife
#     glider h w =  U (0, 0) xs
#       where
#           ys = listArray ((0, 0), (h, w)) $ repeat False
#           xs = ys // [ ((0, 0), True)
#                      , ((0, 1), True)
#                      , ((0, 2), True)
#                      , ((1, 0), True)
#                      , ((2, 1), True) ]

sub pattern_to_code {
	my ($name, $pattern) = @_;

	my @sparse = pattern_string_to_sparse_matrix($pattern);

	my $code 
	    = "    $name :: Int -> Int -> GameOfLife\n"
	    . "    $name w h = U (0, 0) xs\n"
	    . "        where\n"
	    . "            ys = listArray ((0, 0), (w, h)) \$ repeat False\n"
	    . "            xs = ys // [";

	foreach (pairs(@sparse)) {
		my ($x, $y) = @$_;
		$code .= " (($x, $y), True)\n                       , ";
	}
	$code =~ s/, $/]\n\n/;

	return $code;
}

# Haskell code for module name and imports
sub get_imports {
    return "module Hascell.ConwayLexicon where\n\n"
	 	 . "    import Hascell.Simulate\n"
		 . "    import Hascell.Conway\n\n"
		 . "    import Data.Array\n\n";
}
