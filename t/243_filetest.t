# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{��} ne "\x82\xa0";

# �t�@�C���e�X�g���^�ɂȂ�ꍇ�� 1 ���Ԃ�e�X�g

my $__FILE__ = __FILE__;

use Char::Ebig5plus;
print "1..9\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    for my $tno (1..9) {
        print "ok - $tno # SKIP $^X $0\n";
    }
    exit;
}

open(FILE,'>file');
close(FILE);

open(FILE,'file');

if ((Char::Ebig5plus::r 'file') == 1) {
    $_ = Char::Ebig5plus::r 'file';
    print "ok - 1 Char::Ebig5plus::r 'file' ($_) == 1 $^X $__FILE__\n";
}
else {
    $_ = Char::Ebig5plus::r 'file';
    print "not ok - 1 Char::Ebig5plus::r 'file' ($_) == 1 $^X $__FILE__\n";
}

if ((Char::Ebig5plus::w 'file') == 1) {
    $_ = Char::Ebig5plus::w 'file';
    print "ok - 2 Char::Ebig5plus::w 'file' ($_) == 1 $^X $__FILE__\n";
}
else {
    $_ = Char::Ebig5plus::w 'file';
    print "not ok - 2 Char::Ebig5plus::w 'file' ($_) == 1 $^X $__FILE__\n";
}

if ((Char::Ebig5plus::o 'file') == 1) {
    $_ = Char::Ebig5plus::o 'file';
    print "ok - 3 Char::Ebig5plus::o 'file' ($_) == 1 $^X $__FILE__\n";
}
else {
    $_ = Char::Ebig5plus::o 'file';
    print "not ok - 3 Char::Ebig5plus::o 'file' ($_) == 1 $^X $__FILE__\n";
}

if ((Char::Ebig5plus::R 'file') == 1) {
    $_ = Char::Ebig5plus::R 'file';
    print "ok - 4 Char::Ebig5plus::R 'file' ($_) == 1 $^X $__FILE__\n";
}
else {
    $_ = Char::Ebig5plus::R 'file';
    print "not ok - 4 Char::Ebig5plus::R 'file' ($_) == 1 $^X $__FILE__\n";
}

if ((Char::Ebig5plus::W 'file') == 1) {
    $_ = Char::Ebig5plus::W 'file';
    print "ok - 5 Char::Ebig5plus::W 'file' ($_) == 1 $^X $__FILE__\n";
}
else {
    $_ = Char::Ebig5plus::W 'file';
    print "not ok - 5 Char::Ebig5plus::W 'file' ($_) == 1 $^X $__FILE__\n";
}

if ((Char::Ebig5plus::O 'file') == 1) {
    $_ = Char::Ebig5plus::O 'file';
    print "ok - 6 Char::Ebig5plus::O 'file' ($_) == 1 $^X $__FILE__\n";
}
else {
    $_ = Char::Ebig5plus::O 'file';
    print "not ok - 6 Char::Ebig5plus::O 'file' ($_) == 1 $^X $__FILE__\n";
}

if ((Char::Ebig5plus::e 'file') == 1) {
    $_ = Char::Ebig5plus::e 'file';
    print "ok - 7 Char::Ebig5plus::e 'file' ($_) == 1 $^X $__FILE__\n";
}
else {
    $_ = Char::Ebig5plus::e 'file';
    print "not ok - 7 Char::Ebig5plus::e 'file' ($_) == 1 $^X $__FILE__\n";
}

if ((Char::Ebig5plus::z 'file') == 1) {
    $_ = Char::Ebig5plus::z 'file';
    print "ok - 8 Char::Ebig5plus::z 'file' ($_) == 1 $^X $__FILE__\n";
}
else {
    $_ = Char::Ebig5plus::z 'file';
    print "not ok - 8 Char::Ebig5plus::z 'file' ($_) == 1 $^X $__FILE__\n";
}

if ((Char::Ebig5plus::f 'file') == 1) {
    $_ = Char::Ebig5plus::f 'file';
    print "ok - 9 Char::Ebig5plus::f 'file' ($_) == 1 $^X $__FILE__\n";
}
else {
    $_ = Char::Ebig5plus::f 'file';
    print "not ok - 9 Char::Ebig5plus::f 'file' ($_) == 1 $^X $__FILE__\n";
}

close(FILE);
unlink('file');

__END__
