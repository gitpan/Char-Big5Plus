# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{‚ } ne "\x82\xa0";

# ˆø”‚ªÈ—ª‚³‚ê‚½ê‡‚ÌƒeƒXƒg

my $__FILE__ = __FILE__;

use Char::Ebig5plus;
print "1..25\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    for my $tno (1..25) {
        print "ok - $tno # SKIP $^X $0\n";
    }
    exit;
}

open(FILE,'>file');
close(FILE);

open(FILE,'file');

$_ = 'file';
if ((Char::Ebig5plus::r_ ne '') == (-r ne '')) {
    print "ok - 1 Char::Ebig5plus::r_ == -r  $^X $__FILE__\n";
}
else {
    print "not ok - 1 Char::Ebig5plus::r_ == -r  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::w_ ne '') == (-w ne '')) {
    print "ok - 2 Char::Ebig5plus::w_ == -w  $^X $__FILE__\n";
}
else {
    print "not ok - 2 Char::Ebig5plus::w_ == -w  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::x_ ne '') == (-x ne '')) {
    print "ok - 3 Char::Ebig5plus::x_ == -x  $^X $__FILE__\n";
}
else {
    print "not ok - 3 Char::Ebig5plus::x_ == -x  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::o_ ne '') == (-o ne '')) {
    print "ok - 4 Char::Ebig5plus::o_ == -o  $^X $__FILE__\n";
}
else {
    print "not ok - 4 Char::Ebig5plus::o_ == -o  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::R_ ne '') == (-R ne '')) {
    print "ok - 5 Char::Ebig5plus::R_ == -R  $^X $__FILE__\n";
}
else {
    print "not ok - 5 Char::Ebig5plus::R_ == -R  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::W_ ne '') == (-W ne '')) {
    print "ok - 6 Char::Ebig5plus::W_ == -W  $^X $__FILE__\n";
}
else {
    print "not ok - 6 Char::Ebig5plus::W_ == -W  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::X_ ne '') == (-X ne '')) {
    print "ok - 7 Char::Ebig5plus::X_ == -X  $^X $__FILE__\n";
}
else {
    print "not ok - 7 Char::Ebig5plus::X_ == -X  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::O_ ne '') == (-O ne '')) {
    print "ok - 8 Char::Ebig5plus::O_ == -O  $^X $__FILE__\n";
}
else {
    print "not ok - 8 Char::Ebig5plus::O_ == -O  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::e_ ne '') == (-e ne '')) {
    print "ok - 9 Char::Ebig5plus::e_ == -e  $^X $__FILE__\n";
}
else {
    print "not ok - 9 Char::Ebig5plus::e_ == -e  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::z_ ne '') == (-z ne '')) {
    print "ok - 10 Char::Ebig5plus::z_ == -z  $^X $__FILE__\n";
}
else {
    print "not ok - 10 Char::Ebig5plus::z_ == -z  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::s_ ne '') == (-s ne '')) {
    print "ok - 11 Char::Ebig5plus::s_ == -s  $^X $__FILE__\n";
}
else {
    print "not ok - 11 Char::Ebig5plus::s_ == -s  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::f_ ne '') == (-f ne '')) {
    print "ok - 12 Char::Ebig5plus::f_ == -f  $^X $__FILE__\n";
}
else {
    print "not ok - 12 Char::Ebig5plus::f_ == -f  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::d_ ne '') == (-d ne '')) {
    print "ok - 13 Char::Ebig5plus::d_ == -d  $^X $__FILE__\n";
}
else {
    print "not ok - 13 Char::Ebig5plus::d_ == -d  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::p_ ne '') == (-p ne '')) {
    print "ok - 14 Char::Ebig5plus::p_ == -p  $^X $__FILE__\n";
}
else {
    print "not ok - 14 Char::Ebig5plus::p_ == -p  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::S_ ne '') == (-S ne '')) {
    print "ok - 15 Char::Ebig5plus::S_ == -S  $^X $__FILE__\n";
}
else {
    print "not ok - 15 Char::Ebig5plus::S_ == -S  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::b_ ne '') == (-b ne '')) {
    print "ok - 16 Char::Ebig5plus::b_ == -b  $^X $__FILE__\n";
}
else {
    print "not ok - 16 Char::Ebig5plus::b_ == -b  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::c_ ne '') == (-c ne '')) {
    print "ok - 17 Char::Ebig5plus::c_ == -c  $^X $__FILE__\n";
}
else {
    print "not ok - 17 Char::Ebig5plus::c_ == -c  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::u_ ne '') == (-u ne '')) {
    print "ok - 18 Char::Ebig5plus::u_ == -u  $^X $__FILE__\n";
}
else {
    print "not ok - 18 Char::Ebig5plus::u_ == -u  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::g_ ne '') == (-g ne '')) {
    print "ok - 19 Char::Ebig5plus::g_ == -g  $^X $__FILE__\n";
}
else {
    print "not ok - 19 Char::Ebig5plus::g_ == -g  $^X $__FILE__\n";
}

local $^W = 0;
$_ = 'file';
if ((Char::Ebig5plus::k_ ne '') == (-k ne '')) {
    print "ok - 20 Char::Ebig5plus::k_ == -k  $^X $__FILE__\n";
}
else {
    print "not ok - 20 Char::Ebig5plus::k_ == -k  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::T_ ne '') == (-T ne '')) {
    print "ok - 21 Char::Ebig5plus::T_ == -T  $^X $__FILE__\n";
}
else {
    print "not ok - 21 Char::Ebig5plus::T_ == -T  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::B_ ne '') == (-B ne '')) {
    print "ok - 22 Char::Ebig5plus::B_ == -B  $^X $__FILE__\n";
}
else {
    print "not ok - 22 Char::Ebig5plus::B_ == -B  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::M_ ne '') == (-M ne '')) {
    print "ok - 23 Char::Ebig5plus::M_ == -M  $^X $__FILE__\n";
}
else {
    print "not ok - 23 Char::Ebig5plus::M_ == -M  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::A_ ne '') == (-A ne '')) {
    print "ok - 24 Char::Ebig5plus::A_ == -A  $^X $__FILE__\n";
}
else {
    print "not ok - 24 Char::Ebig5plus::A_ == -A  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Ebig5plus::C_ ne '') == (-C ne '')) {
    print "ok - 25 Char::Ebig5plus::C_ == -C  $^X $__FILE__\n";
}
else {
    print "not ok - 25 Char::Ebig5plus::C_ == -C  $^X $__FILE__\n";
}

close(FILE);
unlink('file');

__END__
