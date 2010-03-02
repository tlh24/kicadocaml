#!/usr/bin/perl
# script to acutomatically rename gerber output of PCBNEW
use Cwd ; 
$nargs = $#ARGV + 1 ;
if ($nargs > 0) {
	$dir = $ARGV[0]; 
} else {
	print "specify driectory on the command line.";
	$dir = getcwd() ; 
}
opendir(IMD,$dir); 
@files = readdir(IMD); 
closedir(IMD); 
for my $file (@files){
	if( $file =~ /pho$/){
		my $newname = $file ; 
		$newname =~ s/Component/Top/ ; 
		$newname =~ s/Copper/Bot/ ; 
		$newname =~ s/Cmp/Top/ ; 
		$newname =~ s/Cop/Bot/ ; 
		$newname =~ s/SilkS/Silkscreen/ ; 
		$newname =~ s/SoldP/Solderpaste/ ; 
		if( $file ne $newname ){
			$cmd = "mv ".$dir.$file." ".$dir.$newname; 
			print "$cmd\n"; 
			`$cmd`; 
		}
	}
	#`$cmd`; 
}