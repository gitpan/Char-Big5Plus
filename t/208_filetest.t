# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{��} ne "\x82\xa0";

my $__FILE__ = __FILE__;

use Char::Big5Plus;
print "1..6\n";

my $chcp = '';
if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    $chcp = `chcp`;
}
if ($chcp !~ /932|950/oxms) {
    print "ok - 1 # SKIP $^X $__FILE__\n";
    print "ok - 2 # SKIP $^X $__FILE__\n";
    print "ok - 3 # SKIP $^X $__FILE__\n";
    print "ok - 4 # SKIP $^X $__FILE__\n";
    print "ok - 5 # SKIP $^X $__FILE__\n";
    print "ok - 6 # SKIP $^X $__FILE__\n";
    exit;
}

open(FILE,'>F�@�\') || die "Can't open file: F�@�\\n";
print FILE "1\n";
close(FILE);
mkdir('D�@�\', 0777);
open(FILE,'>D�@�\/a.txt') || die "Can't open file: D�@�\/a.txt\n";
print FILE "1\n";
close(FILE);
open(FILE,'>D�@�\/b.txt') || die "Can't open file: D�@�\/b.txt\n";
print FILE "1\n";
close(FILE);
open(FILE,'>D�@�\/c.txt') || die "Can't open file: D�@�\/c.txt\n";
print FILE "1\n";
close(FILE);
open(FILE,'>D�@�\/F�@�\') || die "Can't open file: D�@�\/F�@�\\n";
print FILE "1\n";
close(FILE);
mkdir('D�@�\/D�@�\', 0777);

my $filetest;

$_ = 'F�@�\';

$filetest = -d $_;
if ($filetest) {
    print "not ok - 1 $^X $__FILE__\n";
}
else {
    print "ok - 1 $^X $__FILE__\n";
}

$filetest = -f $_;
if ($filetest) {
    print "ok - 2 $^X $__FILE__\n";
}
else {
    print "not ok - 2 $^X $__FILE__\n";
}

$filetest = -e $_;
if ($filetest) {
    print "ok - 3 $^X $__FILE__\n";
}
else {
    print "not ok - 3 $^X $__FILE__\n";
}

$_ = 'D�@�\';

$filetest = -d $_;
if ($filetest) {
    print "ok - 4 $^X $__FILE__\n";
}
else {
    print "not ok - 4 $^X $__FILE__\n";
}

$filetest = -f $_;
if ($filetest) {
    print "not ok - 5 $^X $__FILE__\n";
}
else {
    print "ok - 5 $^X $__FILE__\n";
}

$filetest = -e $_;
if ($filetest) {
    print "ok - 6 $^X $__FILE__\n";
}
else {
    print "not ok - 6 $^X $__FILE__\n";
}

unlink('F�@�\');
rmdir('D�@�\/D�@�\');
unlink('D�@�\/a.txt');
unlink('D�@�\/b.txt');
unlink('D�@�\/c.txt');
unlink('D�@�\/F�@�\');
rmdir('D�@�\');

__END__

Perl����/Windows�ł̃t�@�C���p�X
http://digit.que.ne.jp/work/wiki.cgi?Perl%E3%83%A1%E3%83%A2%2FWindows%E3%81%A7%E3%81%AE%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%83%91%E3%82%B9

�t�@�C���֘A�R�}���h�̓���m�F
�u�@�\�v�Ƃ����������ϐ�$_�ɓ���A�t�@�C�����Z�q-d�A-f�A-e���Ń`�F�b�N����ƁA����������undef��Ԃ�
