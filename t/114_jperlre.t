# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{��} ne "\x82\xa0";

use Char::Big5Plus;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('������������' =~ /(����{0,}����)/) {
    if ("$1" eq "������������") {
        print "ok - 1 $^X $__FILE__ ('������������' =~ /����{0,}����/).\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__ ('������������' =~ /����{0,}����/).\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__ ('������������' =~ /����{0,}����/).\n";
}

__END__
