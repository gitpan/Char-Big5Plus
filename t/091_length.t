# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{‚ } ne "\x82\xa0";

use Char::Big5Plus;
print "1..2\n";

my $__FILE__ = __FILE__;

if (length('‚ ‚¢‚¤‚¦‚¨') == 10) {
    print qq{ok - 1 length('‚ ‚¢‚¤‚¦‚¨') == 10 $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 length('‚ ‚¢‚¤‚¦‚¨') == 10 $^X $__FILE__\n};
}

if (Char::Big5Plus::length('‚ ‚¢‚¤‚¦‚¨') == 5) {
    print qq{ok - 2 Char::Big5Plus::length('‚ ‚¢‚¤‚¦‚¨') == 5 $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 Char::Big5Plus::length('‚ ‚¢‚¤‚¦‚¨') == 5 $^X $__FILE__\n};
}

__END__
