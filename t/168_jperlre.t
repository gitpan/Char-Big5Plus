# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{あ} ne "\x82\xa0";

use Char::Big5Plus;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('あ い' =~ /(あ[\s]い)/) {
    if ("-" eq "-") {
        print "ok - 1 $^X $__FILE__ ('あ い' =~ /あ[\\s]い/).\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__ ('あ い' =~ /あ[\\s]い/).\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__ ('あ い' =~ /あ[\\s]い/).\n";
}

__END__
