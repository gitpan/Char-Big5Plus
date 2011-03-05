# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{あ} ne "\x82\xa0";

use Char::Big5Plus;
print "1..1\n";

# Can't find string terminator '"' anywhere before EOF
# 「終端文字 '"'がファイルの終り EOF までに見つからなかった」
if (qq{"日本語"} eq pack('C8',0x22,0x93,0xfa,0x96,0x7b,0x8c,0xea,0x22)) {
    print qq<ok - 1 qq{"NIHONGO"}\n>;
}
else {
    print qq<not ok - 1 qq{"NIHONGO"}\n>;
}

__END__

Char::Big5Plus.pm の処理結果が以下になることを期待している

if (qq{"日暴{語"} eq pack('C8',0x22,0x93,0xfa,0x96,0x7b,0x8c,0xea,0x22)) {

Shift-JISテキストを正しく扱う
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm
