# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{��} ne "\x82\xa0";

use Char::Big5Plus;
print "1..1\n";

my $__FILE__ = __FILE__;

$a = "�A�\�A";
if ($a !~ s/(�A([�C�E�G])�A)//) {
    print qq{ok - 1 "�A�\�A" !~ s/(�A([�C�E�G])�A)// \$1=(), \$2=() $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 "�A�\�A" !~ s/(�A([�C�\�E])�A)// \$1=($1), \$2=($2) $^X $__FILE__\n};
}

__END__
