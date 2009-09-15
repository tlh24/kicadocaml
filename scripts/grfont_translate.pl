#!/usr/bin/perl
$narg = $#ARGV + 1 ; 
if ($narg ne 2){
	print "usage: grfont_translate.pl inputfile outputfile\n"; 
	print " e.g. grfont_translate.pl grfonte.h grfonte.ml\n"; 
} else {
	$source = $ARGV[0]; 
	$dest = $ARGV[1]; 
	open(FI, $source); 
	open(FO, "> $dest"); 
	@inlines = <FI> ; 
	foreach $line (@inlines) {
		#change the comment strings. 
		$line =~ s/\/\*/(* /g ; 
		$line =~ s/\*\// *)/g ; 
		$line =~ s/\/\/(.+)$/(* $1 *)/; 
		#remove #defines etc
		$line =~ s/^#.*//; 
		#remove [] - encode in an array directly. 
		$line =~ s/\[\]//g; 
		#replace const SH_CODE .. \n with let .. in
		$line =~ s/const SH_CODE ([^;]+);/let $1 /g; 
		#remove the beginning {Up -- can just start drawing the character. 
		$line =~ s/\{Up,/{/g ; 
		#remove the ending X} -- ocaml arrays have intrinsic length. 
		$line =~ s/,X\}/}/g; 
		$line =~ s/X\}/}/g; 
		#likewise, don't need the Dn keyword - we assume that the pen always
		# goes down at one point after it was lifted. 
		$line =~ s/Dn,//g; 
		#replace the bracket notation with ocaml's [| ... |]
		$line =~ s/\{/[|/g; 
		$line =~ s/\}/|]/g;
		# similarly, replace comma notation (tuple) with array (semicolon)
		$line =~ s/,/;/g; 
		#replace any remaining 'Up' codes with special code. 
		$line =~ s/Up/42;42/g; 
		#combine into int*int tuples. 
		$line =~ s/([-\d]+);([-\d]+)/($1,$2)/g; 
		#replace special code with something more readable. 
		$line =~ s/\(42,42\)/up/g; 
		#try it? 
		print FO $line ; 
		#will have to do a bit of manipulation on the final file, 
		# but it is nothing major 
		# (and nothing that is easily automated in perl)
	}
	close FI ; 
	close FO ; 
}