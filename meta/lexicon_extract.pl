#!/usr/bin/perl

use warnings;
use strict;
use List::Util qw(pairs);

system ("wget https://conwaylife.com/ref/lexicon/zip/lex_htm1/lexicon.htm");
my $lexicon = get_all_pattern_string('lexicon.htm');
map { print_coordinates($_, $lexicon->{$_}) } (keys %$lexicon);


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


# Generates a file with the row and column coordinates of each alive cell
sub print_coordinates {
	my ($name, $pattern) = @_;

    open my $out, '>>', "patterns/$name";

	my @sparse = pattern_string_to_sparse_matrix($pattern);

    foreach (pairs(@sparse)) {
		my ($x, $y) = @$_;
        print $out "$x $y\n";
    }

    close $out;
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
