#!/usr/bin/perl
$key = '?';
open(FILE, $ARGV[0]) || die "Can't open $ARGV[0]";
while ($line = <FILE>) {
	chop($line);
	if (!($line =~ /^\s*$/)) {
		if ($line =~ /begin\(model\((\w+)\)\)/) {
			$key = $1;
		} elsif ($line =~ /end\(model\((\w+)\)\)/) {
			print "\n";
		} else 	{
			if ($line =~ s/neg\((\w+)\(/neg\($1\($key,/) {
				print "$line\n";
			} else {
			       if ($line =~ s/(\w+)\(/$1\($key,/) {
				   print "$line\n";
			        } else {
				$line =~ s/(\w+)/$1\($key\)/;
				print "$line\n";
				}
			}
		}
	}
}
close(FILE);
