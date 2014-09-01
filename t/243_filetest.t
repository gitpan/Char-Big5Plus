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
        print "not ok - 6 Char::Ebig5plus:
