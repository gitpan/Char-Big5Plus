# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{あ} ne "\x82\xa0";

use Char::Big5Plus;
print "1..1\n";

$_ = '';

# Search pattern not terminated
# 「サーチパターンが終了しない」
eval { /表/ };
if ($@) {
    print "not ok - 1 eval { /HYO/ }\n";
}
else {
    print "ok - 1 eval { /HYO/ }\n";
}

__END__

Shift-JISテキストを正しく扱う
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm
