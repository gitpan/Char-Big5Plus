# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{あ} ne "\x82\xa0";

use strict;
use Char::Big5Plus;
print "1..2\n";

my $__FILE__ = __FILE__;

if ('あ' =~ qr/(.)/b) {
    if (length($1) == 1) {
        print qq{ok - 1 'あ'=~qr/(.)/b; length(\$1)==1 $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 1 'あ'=~qr/(.)/b; length(\$1)==1 $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 1 'あ'=~qr/(.)/b; length(\$1)==1 $^X $__FILE__\n};
}

if ('あ' =~ qr'(.)'b) {
    if (length($1) == 1) {
        print qq{ok - 2 'あ'=~qr'(.)'b; length(\$1)==1 $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 2 'あ'=~qr'(.)'b; length(\$1)==1 $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 2 'あ'=~qr'(.)'b; length(\$1)==1 $^X $__FILE__\n};
}

__END__

