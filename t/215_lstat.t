# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{��} ne "\x82\xa0";

my $__FILE__ = __FILE__;

use Char::Big5Plus;
print "1..1\n";

my $chcp = '';
if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    $chcp = `chcp`;
}
if ($chcp !~ /932|950/oxms) {
    print "ok - 1 # SKIP $^X $0\n";
    exit;
}

open(FILE,'>F�@�\') || die "Can't open file: F�@�\\n";
print FILE "1\n";
close(FILE);

# lstat
if (lstat('F�@�\')) {
    print "ok - 1 lstat $^X $__FILE__\n";
}
else {
    print "not ok - 1 lstat: $! $^X $__FILE__\n";
}

unlink('F�@�\');

__END__