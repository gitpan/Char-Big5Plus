# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{あ} ne "\x82\xa0";

# 引数に _ が指定された場合のテスト

my $__FILE__ = __FILE__;

use Char::Ebig5plus;
print "1..23\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    for my $tno (1..23) {
        print "ok - $tno # SKIP $^X $0\n";
    }
    exit;
}

open(FILE,'>file');
close(FILE);

open(FILE,'file');

if (-r ('file')) {
    if (Char::Ebig5plus::r(_)) {
        print "ok - 1 Char::Ebig5plus::r _ == -r _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 1 Char::Ebig5plus::r _ == -r _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::r(_)) {
        print "not ok - 1 Char::Ebig5plus::r _ == -r _ $^X $__FILE__\n";
    }
    else {
        print "ok - 1 Char::Ebig5plus::r _ == -r _ $^X $__FILE__\n";
    }
}

if (-w ('file')) {
    if (Char::Ebig5plus::w(_)) {
        print "ok - 2 Char::Ebig5plus::w _ == -w _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 2 Char::Ebig5plus::w _ == -w _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::w(_)) {
        print "not ok - 2 Char::Ebig5plus::w _ == -w _ $^X $__FILE__\n";
    }
    else {
        print "ok - 2 Char::Ebig5plus::w _ == -w _ $^X $__FILE__\n";
    }
}

if (-x ('file')) {
    if (Char::Ebig5plus::x(_)) {
        print "ok - 3 Char::Ebig5plus::x _ == -x _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 3 Char::Ebig5plus::x _ == -x _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::x(_)) {
        print "not ok - 3 Char::Ebig5plus::x _ == -x _ $^X $__FILE__\n";
    }
    else {
        print "ok - 3 Char::Ebig5plus::x _ == -x _ $^X $__FILE__\n";
    }
}

if (-o ('file')) {
    if (Char::Ebig5plus::o(_)) {
        print "ok - 4 Char::Ebig5plus::o _ == -o _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 4 Char::Ebig5plus::o _ == -o _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::o(_)) {
        print "not ok - 4 Char::Ebig5plus::o _ == -o _ $^X $__FILE__\n";
    }
    else {
        print "ok - 4 Char::Ebig5plus::o _ == -o _ $^X $__FILE__\n";
    }
}

if (-R ('file')) {
    if (Char::Ebig5plus::R(_)) {
        print "ok - 5 Char::Ebig5plus::R _ == -R _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 5 Char::Ebig5plus::R _ == -R _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::R(_)) {
        print "not ok - 5 Char::Ebig5plus::R _ == -R _ $^X $__FILE__\n";
    }
    else {
        print "ok - 5 Char::Ebig5plus::R _ == -R _ $^X $__FILE__\n";
    }
}

if (-W ('file')) {
    if (Char::Ebig5plus::W(_)) {
        print "ok - 6 Char::Ebig5plus::W _ == -W _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 6 Char::Ebig5plus::W _ == -W _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::W(_)) {
        print "not ok - 6 Char::Ebig5plus::W _ == -W _ $^X $__FILE__\n";
    }
    else {
        print "ok - 6 Char::Ebig5plus::W _ == -W _ $^X $__FILE__\n";
    }
}

if (-X ('file')) {
    if (Char::Ebig5plus::X(_)) {
        print "ok - 7 Char::Ebig5plus::X _ == -X _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 7 Char::Ebig5plus::X _ == -X _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::X(_)) {
        print "not ok - 7 Char::Ebig5plus::X _ == -X _ $^X $__FILE__\n";
    }
    else {
        print "ok - 7 Char::Ebig5plus::X _ == -X _ $^X $__FILE__\n";
    }
}

if (-O ('file')) {
    if (Char::Ebig5plus::O(_)) {
        print "ok - 8 Char::Ebig5plus::O _ == -O _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 8 Char::Ebig5plus::O _ == -O _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::O(_)) {
        print "not ok - 8 Char::Ebig5plus::O _ == -O _ $^X $__FILE__\n";
    }
    else {
        print "ok - 8 Char::Ebig5plus::O _ == -O _ $^X $__FILE__\n";
    }
}

if (-e ('file')) {
    if (Char::Ebig5plus::e(_)) {
        print "ok - 9 Char::Ebig5plus::e _ == -e _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 9 Char::Ebig5plus::e _ == -e _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::e(_)) {
        print "not ok - 9 Char::Ebig5plus::e _ == -e _ $^X $__FILE__\n";
    }
    else {
        print "ok - 9 Char::Ebig5plus::e _ == -e _ $^X $__FILE__\n";
    }
}

if (-z ('file')) {
    if (Char::Ebig5plus::z(_)) {
        print "ok - 10 Char::Ebig5plus::z _ == -z _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 10 Char::Ebig5plus::z _ == -z _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::z(_)) {
        print "not ok - 10 Char::Ebig5plus::z _ == -z _ $^X $__FILE__\n";
    }
    else {
        print "ok - 10 Char::Ebig5plus::z _ == -z _ $^X $__FILE__\n";
    }
}

$_ = -s 'file';
if (Char::Ebig5plus::s(_) == $_) {
    print "ok - 11 Char::Ebig5plus::s _ (@{[Char::Ebig5plus::s _]}) == -s 'file' ($_) $^X $__FILE__\n";
}
else {
    print "not ok - 11 Char::Ebig5plus::s _ (@{[Char::Ebig5plus::s _]}) == -s 'file' ($_) $^X $__FILE__\n";
}

if (-f ('file')) {
    if (Char::Ebig5plus::f(_)) {
        print "ok - 12 Char::Ebig5plus::f _ == -f _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 12 Char::Ebig5plus::f _ == -f _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::f(_)) {
        print "not ok - 12 Char::Ebig5plus::f _ == -f _ $^X $__FILE__\n";
    }
    else {
        print "ok - 12 Char::Ebig5plus::f _ == -f _ $^X $__FILE__\n";
    }
}

if (-d ('file')) {
    if (Char::Ebig5plus::d(_)) {
        print "ok - 13 Char::Ebig5plus::d _ == -d _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 13 Char::Ebig5plus::d _ == -d _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::d(_)) {
        print "not ok - 13 Char::Ebig5plus::d _ == -d _ $^X $__FILE__\n";
    }
    else {
        print "ok - 13 Char::Ebig5plus::d _ == -d _ $^X $__FILE__\n";
    }
}

if (-p ('file')) {
    if (Char::Ebig5plus::p(_)) {
        print "ok - 14 Char::Ebig5plus::p _ == -p _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 14 Char::Ebig5plus::p _ == -p _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::p(_)) {
        print "not ok - 14 Char::Ebig5plus::p _ == -p _ $^X $__FILE__\n";
    }
    else {
        print "ok - 14 Char::Ebig5plus::p _ == -p _ $^X $__FILE__\n";
    }
}

if (-S ('file')) {
    if (Char::Ebig5plus::S(_)) {
        print "ok - 15 Char::Ebig5plus::S _ == -S _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 15 Char::Ebig5plus::S _ == -S _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::S(_)) {
        print "not ok - 15 Char::Ebig5plus::S _ == -S _ $^X $__FILE__\n";
    }
    else {
        print "ok - 15 Char::Ebig5plus::S _ == -S _ $^X $__FILE__\n";
    }
}

if (-b ('file')) {
    if (Char::Ebig5plus::b(_)) {
        print "ok - 16 Char::Ebig5plus::b _ == -b _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 16 Char::Ebig5plus::b _ == -b _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::b(_)) {
        print "not ok - 16 Char::Ebig5plus::b _ == -b _ $^X $__FILE__\n";
    }
    else {
        print "ok - 16 Char::Ebig5plus::b _ == -b _ $^X $__FILE__\n";
    }
}

if (-c ('file')) {
    if (Char::Ebig5plus::c(_)) {
        print "ok - 17 Char::Ebig5plus::c _ == -c _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 17 Char::Ebig5plus::c _ == -c _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::c(_)) {
        print "not ok - 17 Char::Ebig5plus::c _ == -c _ $^X $__FILE__\n";
    }
    else {
        print "ok - 17 Char::Ebig5plus::c _ == -c _ $^X $__FILE__\n";
    }
}

if (-u ('file')) {
    if (Char::Ebig5plus::u(_)) {
        print "ok - 18 Char::Ebig5plus::u _ == -u _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 18 Char::Ebig5plus::u _ == -u _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::u(_)) {
        print "not ok - 18 Char::Ebig5plus::u _ == -u _ $^X $__FILE__\n";
    }
    else {
        print "ok - 18 Char::Ebig5plus::u _ == -u _ $^X $__FILE__\n";
    }
}

if (-g ('file')) {
    if (Char::Ebig5plus::g(_)) {
        print "ok - 19 Char::Ebig5plus::g _ == -g _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 19 Char::Ebig5plus::g _ == -g _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::g(_)) {
        print "not ok - 19 Char::Ebig5plus::g _ == -g _ $^X $__FILE__\n";
    }
    else {
        print "ok - 19 Char::Ebig5plus::g _ == -g _ $^X $__FILE__\n";
    }
}

if (-k ('file')) {
    if (Char::Ebig5plus::k(_)) {
        print "ok - 20 Char::Ebig5plus::k _ == -k _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 20 Char::Ebig5plus::k _ == -k _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Ebig5plus::k(_)) {
        print "not ok - 20 Char::Ebig5plus::k _ == -k _ $^X $__FILE__\n";
    }
    else {
        print "ok - 20 Char::Ebig5plus::k _ == -k _ $^X $__FILE__\n";
    }
}

$_ = -M 'file';
if (Char::Ebig5plus::M(_) == $_) {
    print "ok - 21 Char::Ebig5plus::M _ (@{[Char::Ebig5plus::M _]}) == -M 'file' ($_) $^X $__FILE__\n";
}
else {
    print "not ok - 21 Char::Ebig5plus::M _ (@{[Char::Ebig5plus::M _]}) == -M 'file' ($_) $^X $__FILE__\n";
}

$_ = -A 'file';
if (Char::Ebig5plus::A(_) == $_) {
    print "ok - 22 Char::Ebig5plus::A _ (@{[Char::Ebig5plus::A _]}) == -A 'file' ($_) $^X $__FILE__\n";
}
else {
    print "not ok - 22 Char::Ebig5plus::A _ (@{[Char::Ebig5plus::A _]}) == -A 'file' ($_) $^X $__FILE__\n";
}

$_ = -C 'file';
if (Char::Ebig5plus::C(_) == $_) {
    print "ok - 23 Char::Ebig5plus::C _ (@{[Char::Ebig5plus::C _]}) == -C 'file' ($_) $^X $__FILE__\n";
}
else {
    print "not ok - 23 Char::Ebig5plus::C _ (@{[Char::Ebig5plus::C _]}) == -C 'file' ($_) $^X $__FILE__\n";
}

close(FILE);
unlink('file');

__END__
