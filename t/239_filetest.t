# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{��} ne "\x82\xa0";

# Char::Ebig5plus::X �� -X (Perl�̃t�@�C���e�X�g���Z�q) �̌��ʂ���v���邱�Ƃ̃e�X�g(�Ώۂ̓f�B���N�g��)

my $__FILE__ = __FILE__;

use Char::Ebig5plus;
print "1..22\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    for my $tno (1..22) {
        print "ok - $tno # SKIP $^X $0\n";
    }
    exit;
}

mkdir('directory',0777);

opendir(DIR,'directory');

if (((Char::Ebig5plus::r 'directory') ne '') == ((-r 'directory') ne '')) {
    print "ok - 1 Char::Ebig5plus::r 'directory' == -r 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 1 Char::Ebig5plus::r 'directory' == -r 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::w 'directory') ne '') == ((-w 'directory') ne '')) {
    print "ok - 2 Char::Ebig5plus::w 'directory' == -w 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 2 Char::Ebig5plus::w 'directory' == -w 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::x 'directory') ne '') == ((-x 'directory') ne '')) {
    print "ok - 3 Char::Ebig5plus::x 'directory' == -x 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 3 Char::Ebig5plus::x 'directory' == -x 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::o 'directory') ne '') == ((-o 'directory') ne '')) {
    print "ok - 4 Char::Ebig5plus::o 'directory' == -o 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 4 Char::Ebig5plus::o 'directory' == -o 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::R 'directory') ne '') == ((-R 'directory') ne '')) {
    print "ok - 5 Char::Ebig5plus::R 'directory' == -R 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 5 Char::Ebig5plus::R 'directory' == -R 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::W 'directory') ne '') == ((-W 'directory') ne '')) {
    print "ok - 6 Char::Ebig5plus::W 'directory' == -W 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 6 Char::Ebig5plus::W 'directory' == -W 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::X 'directory') ne '') == ((-X 'directory') ne '')) {
    print "ok - 7 Char::Ebig5plus::X 'directory' == -X 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 7 Char::Ebig5plus::X 'directory' == -X 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::O 'directory') ne '') == ((-O 'directory') ne '')) {
    print "ok - 8 Char::Ebig5plus::O 'directory' == -O 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 8 Char::Ebig5plus::O 'directory' == -O 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::e 'directory') ne '') == ((-e 'directory') ne '')) {
    print "ok - 9 Char::Ebig5plus::e 'directory' == -e 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 9 Char::Ebig5plus::e 'directory' == -e 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::z 'directory') ne '') == ((-z 'directory') ne '')) {
    print "ok - 10 Char::Ebig5plus::z 'directory' == -z 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 10 Char::Ebig5plus::z 'directory' == -z 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::s 'directory') ne '') == ((-s 'directory') ne '')) {
    print "ok - 11 Char::Ebig5plus::s 'directory' == -s 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 11 Char::Ebig5plus::s 'directory' == -s 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::f 'directory') ne '') == ((-f 'directory') ne '')) {
    print "ok - 12 Char::Ebig5plus::f 'directory' == -f 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 12 Char::Ebig5plus::f 'directory' == -f 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::d 'directory') ne '') == ((-d 'directory') ne '')) {
    print "ok - 13 Char::Ebig5plus::d 'directory' == -d 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 13 Char::Ebig5plus::d 'directory' == -d 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::p 'directory') ne '') == ((-p 'directory') ne '')) {
    print "ok - 14 Char::Ebig5plus::p 'directory' == -p 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 14 Char::Ebig5plus::p 'directory' == -p 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::S 'directory') ne '') == ((-S 'directory') ne '')) {
    print "ok - 15 Char::Ebig5plus::S 'directory' == -S 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 15 Char::Ebig5plus::S 'directory' == -S 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::b 'directory') ne '') == ((-b 'directory') ne '')) {
    print "ok - 16 Char::Ebig5plus::b 'directory' == -b 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 16 Char::Ebig5plus::b 'directory' == -b 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::c 'directory') ne '') == ((-c 'directory') ne '')) {
    print "ok - 17 Char::Ebig5plus::c 'directory' == -c 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 17 Char::Ebig5plus::c 'directory' == -c 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::u 'directory') ne '') == ((-u 'directory') ne '')) {
    print "ok - 18 Char::Ebig5plus::u 'directory' == -u 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 18 Char::Ebig5plus::u 'directory' == -u 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::g 'directory') ne '') == ((-g 'directory') ne '')) {
    print "ok - 19 Char::Ebig5plus::g 'directory' == -g 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 19 Char::Ebig5plus::g 'directory' == -g 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::M 'directory') ne '') == ((-M 'directory') ne '')) {
    print "ok - 20 Char::Ebig5plus::M 'directory' == -M 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 20 Char::Ebig5plus::M 'directory' == -M 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::A 'directory') ne '') == ((-A 'directory') ne '')) {
    print "ok - 21 Char::Ebig5plus::A 'directory' == -A 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 21 Char::Ebig5plus::A 'directory' == -A 'directory' $^X $__FILE__\n";
}

if (((Char::Ebig5plus::C 'directory') ne '') == ((-C 'directory') ne '')) {
    print "ok - 22 Char::Ebig5plus::C 'directory' == -C 'directory' $^X $__FILE__\n";
}
else {
    print "not ok - 22 Char::Ebig5plus::C 'directory' == -C 'directory' $^X $__FILE__\n";
}

closedir(DIR);
rmdir('directory');

__END__
