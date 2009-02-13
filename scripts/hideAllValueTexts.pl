#! /usr/bin/perl

$narg = $#ARGV + 1; 
if( $narg ne 2 ){
	print "please specify *brd file to be read and the file to be written\n"; 
}else{
	$source = $ARGV[0]; 
	local( $/, *FH ) ;
	open(FH, $source);
	$/ = "\n" ;
	@j = <FH>; #slurp entire file, split on newline.
	close FH; 
	$dest = $ARGV[1]; 
	open (FO, ">$dest"); 
	# see e.g. http://www.sunstone.com/pcb-capabilities/manufacturing-capabilities.aspx
	if( 0 ){ #tight tolerance 
		$minimumsizex = 300 ; 
		$minimumsizey = 220 ; 
		$minimumwidth = 50 ;  # this is about what (good) manufacturers can do.. 
	} else {
		$minimumsizex = 400 ; 
		$minimumsizey = 300 ; 
		$minimumwidth = 70 ;  # this is about what (good) manufacturers can do.. 
	}
	$count = 0 ;
	$changed = 0 ; 
	foreach $l (@j) {
		if ( $l =~ /T(\d) ([\d-]+) ([\d-]+) (\d+) (\d+) (\d+) (\d+) (\w+) (\w+) (\d+) "([^"]+)"/ ){
			$sx = $4 ; 
			$sy = $5 ; 
			$width = $7 ; 
			$layer = $10 ; 
			$change = 0; 
			if( $sx < $minimumsizex) { $sx = $minimumsizex ; $change = 1;}
			if( $sy < $minimumsizey) { $sy = $minimumsizey ;  $change = 1;}
			if( $width < $minimumwidth) { $width = $minimumwidth ;  $change = 1;}
			print FO "T$1 $2 $3 $sx $sy $6 $width $8 $9 $layer \"$11\"\n" ;
			$count++; 
			$changed += $change ;
		} else {
			print FO $l ; 
		}
	}
	close FO ; 
	print "checked $count instances; changed $changed\n"; 
}