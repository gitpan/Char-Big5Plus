package Char::Ebig5plus;
######################################################################
#
# Char::Ebig5plus - Run-time routines for Char/Big5Plus.pm
#
# Copyright (c) 2008, 2009, 2010, 2011 INABA Hitoshi <ina@cpan.org>
#
######################################################################

use 5.00503;
use strict qw(subs vars);

# 12.3. Delaying use Until Runtime
# in Chapter 12. Packages, Libraries, and Modules
# of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
# (and so on)

BEGIN { eval q{ use vars qw($VERSION) } }
$VERSION = sprintf '%d.%02d', q$Revision: 0.78 $ =~ m/(\d+)/xmsg;

BEGIN {
    my $PERL5LIB = __FILE__;

    # DOS-like system
    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        $PERL5LIB =~ s{[^\x81-\xFE/]*$}{Char::Big5Plus};
    }

    # UNIX-like system
    else {
        $PERL5LIB =~ s{[^\x81-\xFE/]*$}{Char::Big5Plus};
    }

    my @inc = ();
    my %inc = ();
    for my $path ($PERL5LIB, @INC) {
        if (not exists $inc{$path}) {
            push @inc, $path;
            $inc{$path} = 1;
        }
    }
    @INC = @inc;
}

BEGIN {

    # instead of utf8.pm
    eval q{
        no warnings qw(redefine);
        *utf8::upgrade   = sub { CORE::length $_[0] };
        *utf8::downgrade = sub { 1 };
        *utf8::encode    = sub {   };
        *utf8::decode    = sub { 1 };
        *utf8::is_utf8   = sub {   };
        *utf8::valid     = sub { 1 };
    };
    if ($@) {
        *utf8::upgrade   = sub { CORE::length $_[0] };
        *utf8::downgrade = sub { 1 };
        *utf8::encode    = sub {   };
        *utf8::decode    = sub { 1 };
        *utf8::is_utf8   = sub {   };
        *utf8::valid     = sub { 1 };
    }
}

# poor Symbol.pm - substitute of real Symbol.pm
BEGIN {
    my $genpkg = "Symbol::";
    my $genseq = 0;

    sub gensym () {
        my $name = "GEN" . $genseq++;
        my $ref = \*{$genpkg . $name};
        delete $$genpkg{$name};
        $ref;
    }

    sub qualify ($;$) {
        my ($name) = @_;
        if (!ref($name) && (Char::Ebig5plus::index($name, '::') == -1) && (Char::Ebig5plus::index($name, "'") == -1)) {
            my $pkg;
            my %global = map {$_ => 1} qw(ARGV ARGVOUT ENV INC SIG STDERR STDIN STDOUT);

            # Global names: special character, "^xyz", or other.
            if ($name =~ /^(([^\x81-\xFEa-z])|(\^[a-z_]+))\z/i || $global{$name}) {
                # RGS 2001-11-05 : translate leading ^X to control-char
                $name =~ s/^\^([a-z_])/'qq(\c'.$1.')'/eei;
                $pkg = "main";
            }
            else {
                $pkg = (@_ > 1) ? $_[1] : caller;
            }
            $name = $pkg . "::" . $name;
        }
        $name;
    }

    sub qualify_to_ref ($;$) {
        no strict qw(refs);
        return \*{ qualify $_[0], @_ > 1 ? $_[1] : caller };
    }
}

# P.714 29.2.39. flock
# in Chapter 29: Functions
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

sub LOCK_SH() {1}
sub LOCK_EX() {2}
sub LOCK_UN() {8}
sub LOCK_NB() {4}

# instead of Carp.pm
sub carp(@);
sub croak(@);
sub cluck(@);
sub confess(@);

my $__FILE__ = __FILE__;

BEGIN {
    if ($^X =~ m/ jperl /oxmsi) {
        die "$0 need perl(not jperl) 5.00503 or later. (\$^X==$^X)";
    }
}

my $your_char = q{[\x81-\xFE][\x00-\xFF]|[\x00-\xFF]};

# regexp of character
my $q_char = qr/$your_char/oxms;

#
# Big5Plus character range per length
#
my %range_tr = ();
my $is_shiftjis_family = 0;
my $is_eucjp_family    = 0;

#
# alias of encoding name
#

BEGIN { eval q{ use vars qw($encoding_alias) } }

if (0) {
}

# Big5HKSCS
elsif (__PACKAGE__ =~ m/ \b Ebig5hkscs \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x80,0xFF],
             ],
        2 => [ [0x81..0xFE],[0x40..0x7E,0xA1..0xFE],
             ],
    );
    $encoding_alias = qr/ \b (?: big5-?hk(?:scs)? | hk(?:scs)?[-_]?big5 ) \b /oxmsi;
}

# Big5Plus
elsif (__PACKAGE__ =~ m/ \b Ebig5plus \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x80,0xFF],
             ],
        2 => [ [0x81..0xFE],[0x40..0x7E,0x80..0xFE],
             ],
    );
    $encoding_alias = qr/ \b (?: big-?5-?(?:plus)? | big5-?et(?:en)? | tca[-_]?big5 ) \b /oxmsi;
}

# GB18030
elsif (__PACKAGE__ =~ m/ \b Egb18030 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x80,0xFF],
             ],
        2 => [ [0x81..0xFE],[0x40..0x7E,0x80..0xFE],
             ],
        4 => [ [0x81..0xFE],[0x30..0x39],[0x81..0xFE],[0x30..0x39],
             ],
    );
    $encoding_alias = qr/ \b (?: euc.*cn | cn.*euc | gbk | cp936 | gb[-_ ]?2312(?!-?raw) | gb18030 ) \b /oxmsi;
}

# GBK
elsif (__PACKAGE__ =~ m/ \b Egbk \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x80,0xFF],
             ],
        2 => [ [0x81..0xFE],[0x40..0x7E,0x80..0xFE],
             ],
    );
    $encoding_alias = qr/ \b (?: euc.*cn | cn.*euc | gbk | cp936 | gb[-_ ]?2312(?!-?raw) ) \b /oxmsi;
}

# HP-15
elsif (__PACKAGE__ =~ m/ \b Ehp15 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x7F,0xA1..0xDF,0xFF],
             ],
        2 => [ [0x80..0xA0,0xE0..0xFE],[0x21..0x7E,0x80..0xFF],
             ],
    );
    $is_shiftjis_family = 1;
    $encoding_alias = qr/ \b (?: hp-?15 ) \b /oxmsi;
}

# INFORMIX V6 ALS
elsif (__PACKAGE__ =~ m/ \b Einformixv6als \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x80,0xA0..0xDF,0xFE..0xFF],
             ],
        2 => [ [0x81..0x9F,0xE0..0xFC],[0x40..0x7E,0x80..0xFC],
             ],
        3 => [ [0xFD..0xFD],[0xA1..0xFE],[0xA1..0xFE],
             ],
    );
    $is_shiftjis_family = 1;
    $encoding_alias = qr/ \b (?: informix(?:v6als)? ) \b /oxmsi;
}

# Shift_JIS
elsif (__PACKAGE__ =~ m/ \b E s j i s \z/oxms) { # escape from build system
    %range_tr = (
        1 => [ [0x00..0x80,0xA0..0xDF,0xFD..0xFF],
             ],
        2 => [ [0x81..0x9F,0xE0..0xFC],[0x40..0x7E,0x80..0xFC],
             ],
    );
    $is_shiftjis_family = 1;
    $encoding_alias = qr/ \b (?: shift.*jis | sjis | windows-31j | cp932 ) \b /oxmsi;
}

# UHC
elsif (__PACKAGE__ =~ m/ \b Euhc \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x80,0xFF],
             ],
        2 => [ [0x81..0xFE],[0x41..0x5A,0x61..0x7A,0x81..0xFE],
             ],
    );
    $encoding_alias = qr/ \b (?: euc.*kr | kr.*euc | (?:x-)?uhc | (?:x-)?windows-949 | ks_c_5601-1987 | cp949 ) \b /oxmsi;
}

# US-ASCII
elsif (__PACKAGE__ =~ m/ \b Eusascii \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: (?:us-?)?ascii ) \b /oxmsi;
}

# Latin-1
elsif (__PACKAGE__ =~ m/ \b Elatin1 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-1 | iec[- ]?8859-1 | latin-?1 ) \b /oxmsi;
}

# Latin-2
elsif (__PACKAGE__ =~ m/ \b Elatin2 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-2 | iec[- ]?8859-2 | latin-?2 ) \b /oxmsi;
}

# Latin-3
elsif (__PACKAGE__ =~ m/ \b Elatin3 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-3 | iec[- ]?8859-3 | latin-?3 ) \b /oxmsi;
}

# Latin-4
elsif (__PACKAGE__ =~ m/ \b Elatin4 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-4 | iec[- ]?8859-4 | latin-?4 ) \b /oxmsi;
}

# Cyrillic
elsif (__PACKAGE__ =~ m/ \b Ecyrillic \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-5 | iec[- ]?8859-5 | cyrillic ) \b /oxmsi;
}

# KOI8-R
elsif (__PACKAGE__ =~ m/ \b Ekoi8r \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: koi8-?r ) \b /oxmsi;
}

# KOI8-U
elsif (__PACKAGE__ =~ m/ \b Ekoi8u \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: koi8-?u ) \b /oxmsi;
}

# Greek
elsif (__PACKAGE__ =~ m/ \b Egreek \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-7 | iec[- ]?8859-7 | greek ) \b /oxmsi;
}

# Latin-5
elsif (__PACKAGE__ =~ m/ \b Elatin5 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-9 | iec[- ]?8859-9 | latin-?5 ) \b /oxmsi;
}

# Latin-6
elsif (__PACKAGE__ =~ m/ \b Elatin6 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-10 | iec[- ]?8859-10 | latin-?6 ) \b /oxmsi;
}

# Latin-7
elsif (__PACKAGE__ =~ m/ \b Elatin7 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-13 | iec[- ]?8859-13 | latin-?7 ) \b /oxmsi;
}

# Latin-8
elsif (__PACKAGE__ =~ m/ \b Elatin8 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-14 | iec[- ]?8859-14 | latin-?8 ) \b /oxmsi;
}

# Latin-9
elsif (__PACKAGE__ =~ m/ \b Elatin9 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-15 | iec[- ]?8859-15 | latin-?9 ) \b /oxmsi;
}

# Latin-10
elsif (__PACKAGE__ =~ m/ \b Elatin10 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-16 | iec[- ]?8859-16 | latin-?10 ) \b /oxmsi;
}

# Windows-1252
elsif (__PACKAGE__ =~ m/ \b Ewindows1252 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: windows-?1252 ) \b /oxmsi;
}

# Windows-1258
elsif (__PACKAGE__ =~ m/ \b Ewindows1258 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: windows-?1258 ) \b /oxmsi;
}

# EUC-JP
elsif (__PACKAGE__ =~ m/ \b Eeucjp \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x8D,0x90..0xA0,0xFF],
             ],
        2 => [ [0x8E..0x8E],[0xA1..0xDF],
               [0xA1..0xFE],[0xA1..0xFE],
             ],
        3 => [ [0x8F..0x8F],[0xA1..0xFE],[0xA1..0xFE],
             ],
    );
    $is_eucjp_family = 1;
    $encoding_alias = qr/ \b (?: euc.*jp | jp.*euc | ujis ) \b /oxmsi;
}

# UTF-2
elsif (__PACKAGE__ =~ m/ \b Eutf2 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x7F],
             ],
        2 => [ [0xC2..0xDF],[0x80..0xBF],
             ],
        3 => [ [0xE0..0xE0],[0xA0..0xBF],[0x80..0xBF],
               [0xE1..0xEC],[0x80..0xBF],[0x80..0xBF],
               [0xED..0xED],[0x80..0x9F],[0x80..0xBF],
               [0xEE..0xEF],[0x80..0xBF],[0x80..0xBF],
             ],
        4 => [ [0xF0..0xF0],[0x90..0xBF],[0x80..0xBF],[0x80..0xBF],
               [0xF1..0xF3],[0x80..0xBF],[0x80..0xBF],[0x80..0xBF],
               [0xF4..0xF4],[0x80..0x8F],[0x80..0xBF],[0x80..0xBF],
             ],
    );
    $encoding_alias = qr/ \b (?: utf-8 | utf-8-strict | utf-?2 ) \b /oxmsi;
}

# Old UTF-8
elsif (__PACKAGE__ =~ m/ \b Eoldutf8 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x7F],
             ],
        2 => [ [0xC0..0xDF],[0x80..0xBF],
             ],
        3 => [ [0xE0..0xEF],[0x80..0xBF],[0x80..0xBF],
             ],
        4 => [ [0xF0..0xF4],[0x80..0xBF],[0x80..0xBF],[0x80..0xBF],
             ],
    );
    $encoding_alias = qr/ \b (?: utf8 | cesu-?8 | modified[ ]?utf-?8 | old[ ]?utf-?8 ) \b /oxmsi;
}

else {
    croak "$0 don't know my package name '" . __PACKAGE__ . "'";
}

#
# Prototypes of subroutines
#
sub import() {}
sub unimport() {}
sub Char::Ebig5plus::split(;$$$);
sub Char::Ebig5plus::tr($$$$;$);
sub Char::Ebig5plus::chop(@);
sub Char::Ebig5plus::index($$;$);
sub Char::Ebig5plus::rindex($$;$);
sub Char::Ebig5plus::lcfirst(@);
sub Char::Ebig5plus::lcfirst_();
sub Char::Ebig5plus::lc(@);
sub Char::Ebig5plus::lc_();
sub Char::Ebig5plus::ucfirst(@);
sub Char::Ebig5plus::ucfirst_();
sub Char::Ebig5plus::uc(@);
sub Char::Ebig5plus::uc_();
sub Char::Ebig5plus::ignorecase(@);
sub Char::Ebig5plus::classic_character_class($);
sub Char::Ebig5plus::capture($);
sub Char::Ebig5plus::chr(;$);
sub Char::Ebig5plus::chr_();
sub Char::Ebig5plus::filetest(@);
sub Char::Ebig5plus::r(;*@);
sub Char::Ebig5plus::w(;*@);
sub Char::Ebig5plus::x(;*@);
sub Char::Ebig5plus::o(;*@);
sub Char::Ebig5plus::R(;*@);
sub Char::Ebig5plus::W(;*@);
sub Char::Ebig5plus::X(;*@);
sub Char::Ebig5plus::O(;*@);
sub Char::Ebig5plus::e(;*@);
sub Char::Ebig5plus::z(;*@);
sub Char::Ebig5plus::s(;*@);
sub Char::Ebig5plus::f(;*@);
sub Char::Ebig5plus::d(;*@);
sub Char::Ebig5plus::l(;*@);
sub Char::Ebig5plus::p(;*@);
sub Char::Ebig5plus::S(;*@);
sub Char::Ebig5plus::b(;*@);
sub Char::Ebig5plus::c(;*@);
sub Char::Ebig5plus::u(;*@);
sub Char::Ebig5plus::g(;*@);
sub Char::Ebig5plus::k(;*@);
sub Char::Ebig5plus::T(;*@);
sub Char::Ebig5plus::B(;*@);
sub Char::Ebig5plus::M(;*@);
sub Char::Ebig5plus::A(;*@);
sub Char::Ebig5plus::C(;*@);
sub Char::Ebig5plus::filetest_(@);
sub Char::Ebig5plus::r_();
sub Char::Ebig5plus::w_();
sub Char::Ebig5plus::x_();
sub Char::Ebig5plus::o_();
sub Char::Ebig5plus::R_();
sub Char::Ebig5plus::W_();
sub Char::Ebig5plus::X_();
sub Char::Ebig5plus::O_();
sub Char::Ebig5plus::e_();
sub Char::Ebig5plus::z_();
sub Char::Ebig5plus::s_();
sub Char::Ebig5plus::f_();
sub Char::Ebig5plus::d_();
sub Char::Ebig5plus::l_();
sub Char::Ebig5plus::p_();
sub Char::Ebig5plus::S_();
sub Char::Ebig5plus::b_();
sub Char::Ebig5plus::c_();
sub Char::Ebig5plus::u_();
sub Char::Ebig5plus::g_();
sub Char::Ebig5plus::k_();
sub Char::Ebig5plus::T_();
sub Char::Ebig5plus::B_();
sub Char::Ebig5plus::M_();
sub Char::Ebig5plus::A_();
sub Char::Ebig5plus::C_();
sub Char::Ebig5plus::glob($);
sub Char::Ebig5plus::glob_();
sub Char::Ebig5plus::lstat(*);
sub Char::Ebig5plus::lstat_();
sub Char::Ebig5plus::opendir(*$);
sub Char::Ebig5plus::stat(*);
sub Char::Ebig5plus::stat_();
sub Char::Ebig5plus::unlink(@);
sub Char::Ebig5plus::chdir(;$);
sub Char::Ebig5plus::do($);
sub Char::Ebig5plus::require(;$);
sub Char::Ebig5plus::telldir(*);

sub Char::Big5Plus::ord(;$);
sub Char::Big5Plus::ord_();
sub Char::Big5Plus::reverse(@);
sub Char::Big5Plus::length(;$);
sub Char::Big5Plus::substr($$;$$);
sub Char::Big5Plus::index($$;$);
sub Char::Big5Plus::rindex($$;$);

#
# @ARGV wildcard globbing
#
if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    if ($ENV{'ComSpec'} =~ / (?: COMMAND\.COM | CMD\.EXE ) \z /oxmsi) {
        my @argv = ();
        for (@ARGV) {
            if (m/\A ' ((?:$q_char)*) ' \z/oxms) {
                push @argv, $1;
            }
            elsif (m/\A (?:$q_char)*? [*?] /oxms and (my @glob = Char::Ebig5plus::glob($_))) {
                push @argv, @glob;
            }
            else {
                push @argv, $_;
            }
        }
        @ARGV = @argv;
    }
}

#
# Big5Plus split
#
sub Char::Ebig5plus::split(;$$$) {

    # P.794 29.2.161. split
    # in Chapter 29: Functions
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    my $pattern = $_[0];
    my $string  = $_[1];
    my $limit   = $_[2];

    # if $string is omitted, the function splits the $_ string
    if (not defined $string) {
        if (defined $_) {
            $string = $_;
        }
        else {
            $string = '';
        }
    }

    my @split = ();

    # when string is empty
    if ($string eq '') {

        # resulting list value in list context
        if (wantarray) {
            return @split;
        }

        # count of substrings in scalar context
        else {
            carp "$0: Use of implicit split to \@_ is deprecated" if $^W;
            @_ = @split;
            return scalar @_;
        }
    }

    # if $limit is negative, it is treated as if an arbitrarily large $limit has been specified
    if ((not defined $limit) or ($limit <= 0)) {

        # if $pattern is also omitted or is the literal space, " ", the function splits
        # on whitespace, /\s+/, after skipping any leading whitespace
        # (and so on)

        if ((not defined $pattern) or ($pattern eq ' ')) {
            $string =~ s/ \A \s+ //oxms;

            # P.1024 Appendix W.10 Multibyte Processing
            # of ISBN 1-56592-224-7 CJKV Information Processing
            # (and so on)

            # the //m modifier is assumed when you split on the pattern /^/
            # (and so on)

            while ($string =~ s/\A((?:$q_char)*?)\s+//m) {

                # if the $pattern contains parentheses, then the substring matched by each pair of parentheses
                # is included in the resulting list, interspersed with the fields that are ordinarily returned
                # (and so on)

                local $@;
                for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                    push @split, eval '$' . $digit;
                }
            }
        }

        # a pattern capable of matching either the null string or something longer than the
        # null string will split the value of $string into separate characters wherever it
        # matches the null string between characters
        # (and so on)

        elsif ('' =~ m/ \A $pattern \z /xms) {
            while ($string =~ s/\A((?:$q_char)+?)$pattern//m) {
                local $@;
                for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                    push @split, eval '$' . $digit;
                }
            }
        }

        else {
            while ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                local $@;
                for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                    push @split, eval '$' . $digit;
                }
            }
        }
    }

    else {
        if ((not defined $pattern) or ($pattern eq ' ')) {
            $string =~ s/ \A \s+ //oxms;
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)*?)\s+//m) {
                    local $@;
                    for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                        push @split, eval '$' . $digit;
                    }
                }
            }
        }
        elsif ('' =~ m/ \A $pattern \z /xms) {
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)+?)$pattern//m) {
                    local $@;
                    for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                        push @split, eval '$' . $digit;
                    }
                }
            }
        }
        else {
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                    local $@;
                    for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                        push @split, eval '$' . $digit;
                    }
                }
            }
        }
    }

    push @split, $string;

    # if $limit is omitted or zero, trailing null fields are stripped from the result
    if ((not defined $limit) or ($limit == 0)) {
        while ((scalar(@split) >= 1) and ($split[-1] eq '')) {
            pop @split;
        }
    }

    # resulting list value in list context
    if (wantarray) {
        return @split;
    }

    # count of substrings in scalar context
    else {
        carp "$0: Use of implicit split to \@_ is deprecated" if $^W;
        @_ = @split;
        return scalar @_;
    }
}

#
# Big5Plus transliteration (tr///)
#
sub Char::Ebig5plus::tr($$$$;$) {

    my $bind_operator   = $_[1];
    my $searchlist      = $_[2];
    my $replacementlist = $_[3];
    my $modifier        = $_[4] || '';

    if ($modifier =~ m/r/oxms) {
        if ($bind_operator =~ m/ !~ /oxms) {
            croak "$0: Using !~ with tr///r doesn't make sense";
        }
    }

    my @char            = $_[0] =~ m/\G ($q_char) /oxmsg;
    my @searchlist      = _charlist_tr($searchlist);
    my @replacementlist = _charlist_tr($replacementlist);

    my %tr = ();
    for (my $i=0; $i <= $#searchlist; $i++) {
        if (not exists $tr{$searchlist[$i]}) {
            if (defined $replacementlist[$i] and ($replacementlist[$i] ne '')) {
                $tr{$searchlist[$i]} = $replacementlist[$i];
            }
            elsif ($modifier =~ m/d/oxms) {
                $tr{$searchlist[$i]} = '';
            }
            elsif (defined $replacementlist[-1] and ($replacementlist[-1] ne '')) {
                $tr{$searchlist[$i]} = $replacementlist[-1];
            }
            else {
                $tr{$searchlist[$i]} = $searchlist[$i];
            }
        }
    }

    my $tr = 0;
    my $replaced = '';
    if ($modifier =~ m/c/oxms) {
        while (defined(my $char = shift @char)) {
            if (not exists $tr{$char}) {
                if (defined $replacementlist[0]) {
                    $replaced .= $replacementlist[0];
                }
                $tr++;
                if ($modifier =~ m/s/oxms) {
                    while (@char and (not exists $tr{$char[0]})) {
                        shift @char;
                        $tr++;
                    }
                }
            }
            else {
                $replaced .= $char;
            }
        }
    }
    else {
        while (defined(my $char = shift @char)) {
            if (exists $tr{$char}) {
                $replaced .= $tr{$char};
                $tr++;
                if ($modifier =~ m/s/oxms) {
                    while (@char and (exists $tr{$char[0]}) and ($tr{$char[0]} eq $tr{$char})) {
                        shift @char;
                        $tr++;
                    }
                }
            }
            else {
                $replaced .= $char;
            }
        }
    }

    if ($modifier =~ m/r/oxms) {
        return $replaced;
    }
    else {
        $_[0] = $replaced;
        if ($bind_operator =~ m/ !~ /oxms) {
            return not $tr;
        }
        else {
            return $tr;
        }
    }
}

#
# Big5Plus chop
#
sub Char::Ebig5plus::chop(@) {

    my $chop;
    if (@_ == 0) {
        my @char = m/\G ($q_char) /oxmsg;
        $chop = pop @char;
        $_ = join '', @char;
    }
    else {
        for (@_) {
            my @char = m/\G ($q_char) /oxmsg;
            $chop = pop @char;
            $_ = join '', @char;
        }
    }
    return $chop;
}

#
# Big5Plus index by octet
#
sub Char::Ebig5plus::index($$;$) {

    my($str,$substr,$position) = @_;
    $position ||= 0;
    my $pos = 0;

    while ($pos < CORE::length($str)) {
        if (CORE::substr($str,$pos,CORE::length($substr)) eq $substr) {
            if ($pos >= $position) {
                return $pos;
            }
        }
        if (CORE::substr($str,$pos) =~ m/\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return -1;
}

#
# Big5Plus reverse index
#
sub Char::Ebig5plus::rindex($$;$) {

    my($str,$substr,$position) = @_;
    $position ||= CORE::length($str) - 1;
    my $pos = 0;
    my $rindex = -1;

    while (($pos < CORE::length($str)) and ($pos <= $position)) {
        if (CORE::substr($str,$pos,CORE::length($substr)) eq $substr) {
            $rindex = $pos;
        }
        if (CORE::substr($str,$pos) =~ m/\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return $rindex;
}

#
# Big5Plus lower case
#
{
    # P.132 4.8.2. Lexically Scoped Variables: my
    # in Chapter 4: Statements and Declarations
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.
    # (and so on)

    my %lc = ();
    @lc{qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)} =
        qw(a b c d e f g h i j k l m n o p q r s t u v w x y z);

    if (0) {
    }

    elsif (__PACKAGE__ =~ m/ \b Eusascii \z/oxms) {
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin1 \z/oxms) {
        %lc = (%lc,
            "\xC0" => "\xE0", # LATIN LETTER A WITH GRAVE
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH TILDE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER A WITH RING ABOVE
            "\xC6" => "\xE6", # LATIN LETTER AE
            "\xC7" => "\xE7", # LATIN LETTER C WITH CEDILLA
            "\xC8" => "\xE8", # LATIN LETTER E WITH GRAVE
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER I WITH GRAVE
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH DIAERESIS
            "\xD0" => "\xF0", # LATIN LETTER ETH (Icelandic)
            "\xD1" => "\xF1", # LATIN LETTER N WITH TILDE
            "\xD2" => "\xF2", # LATIN LETTER O WITH GRAVE
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH TILDE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD8" => "\xF8", # LATIN LETTER O WITH STROKE
            "\xD9" => "\xF9", # LATIN LETTER U WITH GRAVE
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER Y WITH ACUTE
            "\xDE" => "\xFE", # LATIN LETTER THORN (Icelandic)
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin2 \z/oxms) {
        %lc = (%lc,
            "\xA1" => "\xB1", # LATIN LETTER A WITH OGONEK
            "\xA3" => "\xB3", # LATIN LETTER L WITH STROKE
            "\xA5" => "\xB5", # LATIN LETTER L WITH CARON
            "\xA6" => "\xB6", # LATIN LETTER S WITH ACUTE
            "\xA9" => "\xB9", # LATIN LETTER S WITH CARON
            "\xAA" => "\xBA", # LATIN LETTER S WITH CEDILLA
            "\xAB" => "\xBB", # LATIN LETTER T WITH CARON
            "\xAC" => "\xBC", # LATIN LETTER Z WITH ACUTE
            "\xAE" => "\xBE", # LATIN LETTER Z WITH CARON
            "\xAF" => "\xBF", # LATIN LETTER Z WITH DOT ABOVE
            "\xC0" => "\xE0", # LATIN LETTER R WITH ACUTE
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH BREVE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER L WITH ACUTE
            "\xC6" => "\xE6", # LATIN LETTER C WITH ACUTE
            "\xC7" => "\xE7", # LATIN LETTER C WITH CEDILLA
            "\xC8" => "\xE8", # LATIN LETTER C WITH CARON
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH OGONEK
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER E WITH CARON
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER D WITH CARON
            "\xD0" => "\xF0", # LATIN LETTER D WITH STROKE
            "\xD1" => "\xF1", # LATIN LETTER N WITH ACUTE
            "\xD2" => "\xF2", # LATIN LETTER N WITH CARON
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH DOUBLE ACUTE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD8" => "\xF8", # LATIN LETTER R WITH CARON
            "\xD9" => "\xF9", # LATIN LETTER U WITH RING ABOVE
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH DOUBLE ACUTE
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER Y WITH ACUTE
            "\xDE" => "\xFE", # LATIN LETTER T WITH CEDILLA
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin3 \z/oxms) {
        %lc = (%lc,
            "\xA1" => "\xB1", # LATIN LETTER H WITH STROKE
            "\xA6" => "\xB6", # LATIN LETTER H WITH CIRCUMFLEX
            "\xAA" => "\xBA", # LATIN LETTER S WITH CEDILLA
            "\xAB" => "\xBB", # LATIN LETTER G WITH BREVE
            "\xAC" => "\xBC", # LATIN LETTER J WITH CIRCUMFLEX
            "\xAF" => "\xBF", # LATIN LETTER Z WITH DOT ABOVE
            "\xC0" => "\xE0", # LATIN LETTER A WITH GRAVE
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER C WITH DOT ABOVE
            "\xC6" => "\xE6", # LATIN LETTER C WITH CIRCUMFLEX
            "\xC7" => "\xE7", # LATIN LETTER C WITH CEDILLA
            "\xC8" => "\xE8", # LATIN LETTER E WITH GRAVE
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER I WITH GRAVE
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH DIAERESIS
            "\xD1" => "\xF1", # LATIN LETTER N WITH TILDE
            "\xD2" => "\xF2", # LATIN LETTER O WITH GRAVE
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER G WITH DOT ABOVE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD8" => "\xF8", # LATIN LETTER G WITH CIRCUMFLEX
            "\xD9" => "\xF9", # LATIN LETTER U WITH GRAVE
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER U WITH BREVE
            "\xDE" => "\xFE", # LATIN LETTER S WITH CIRCUMFLEX
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin4 \z/oxms) {
        %lc = (%lc,
            "\xA1" => "\xB1", # LATIN LETTER A WITH OGONEK
            "\xA3" => "\xB3", # LATIN LETTER R WITH CEDILLA
            "\xA5" => "\xB5", # LATIN LETTER I WITH TILDE
            "\xA6" => "\xB6", # LATIN LETTER L WITH CEDILLA
            "\xA9" => "\xB9", # LATIN LETTER S WITH CARON
            "\xAA" => "\xBA", # LATIN LETTER E WITH MACRON
            "\xAB" => "\xBB", # LATIN LETTER G WITH CEDILLA
            "\xAC" => "\xBC", # LATIN LETTER T WITH STROKE
            "\xAE" => "\xBE", # LATIN LETTER Z WITH CARON
            "\xBD" => "\xBF", # LATIN LETTER ENG
            "\xC0" => "\xE0", # LATIN LETTER A WITH MACRON
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH TILDE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER A WITH RING ABOVE
            "\xC6" => "\xE6", # LATIN LETTER AE
            "\xC7" => "\xE7", # LATIN LETTER I WITH OGONEK
            "\xC8" => "\xE8", # LATIN LETTER C WITH CARON
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH OGONEK
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER E WITH DOT ABOVE
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH MACRON
            "\xD0" => "\xF0", # LATIN LETTER D WITH STROKE
            "\xD1" => "\xF1", # LATIN LETTER N WITH CEDILLA
            "\xD2" => "\xF2", # LATIN LETTER O WITH MACRON
            "\xD3" => "\xF3", # LATIN LETTER K WITH CEDILLA
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH TILDE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD8" => "\xF8", # LATIN LETTER O WITH STROKE
            "\xD9" => "\xF9", # LATIN LETTER U WITH OGONEK
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER U WITH TILDE
            "\xDE" => "\xFE", # LATIN LETTER U WITH MACRON
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ecyrillic \z/oxms) {
        %lc = (%lc,
            "\xA1" => "\xF1", # CYRILLIC LETTER IO
            "\xA2" => "\xF2", # CYRILLIC LETTER DJE
            "\xA3" => "\xF3", # CYRILLIC LETTER GJE
            "\xA4" => "\xF4", # CYRILLIC LETTER UKRAINIAN IE
            "\xA5" => "\xF5", # CYRILLIC LETTER DZE
            "\xA6" => "\xF6", # CYRILLIC LETTER BYELORUSSIAN-UKRAINIAN I
            "\xA7" => "\xF7", # CYRILLIC LETTER YI
            "\xA8" => "\xF8", # CYRILLIC LETTER JE
            "\xA9" => "\xF9", # CYRILLIC LETTER LJE
            "\xAA" => "\xFA", # CYRILLIC LETTER NJE
            "\xAB" => "\xFB", # CYRILLIC LETTER TSHE
            "\xAC" => "\xFC", # CYRILLIC LETTER KJE
            "\xAE" => "\xFE", # CYRILLIC LETTER SHORT U
            "\xAF" => "\xFF", # CYRILLIC LETTER DZHE
            "\xB0" => "\xD0", # CYRILLIC LETTER A
            "\xB1" => "\xD1", # CYRILLIC LETTER BE
            "\xB2" => "\xD2", # CYRILLIC LETTER VE
            "\xB3" => "\xD3", # CYRILLIC LETTER GHE
            "\xB4" => "\xD4", # CYRILLIC LETTER DE
            "\xB5" => "\xD5", # CYRILLIC LETTER IE
            "\xB6" => "\xD6", # CYRILLIC LETTER ZHE
            "\xB7" => "\xD7", # CYRILLIC LETTER ZE
            "\xB8" => "\xD8", # CYRILLIC LETTER I
            "\xB9" => "\xD9", # CYRILLIC LETTER SHORT I
            "\xBA" => "\xDA", # CYRILLIC LETTER KA
            "\xBB" => "\xDB", # CYRILLIC LETTER EL
            "\xBC" => "\xDC", # CYRILLIC LETTER EM
            "\xBD" => "\xDD", # CYRILLIC LETTER EN
            "\xBE" => "\xDE", # CYRILLIC LETTER O
            "\xBF" => "\xDF", # CYRILLIC LETTER PE
            "\xC0" => "\xE0", # CYRILLIC LETTER ER
            "\xC1" => "\xE1", # CYRILLIC LETTER ES
            "\xC2" => "\xE2", # CYRILLIC LETTER TE
            "\xC3" => "\xE3", # CYRILLIC LETTER U
            "\xC4" => "\xE4", # CYRILLIC LETTER EF
            "\xC5" => "\xE5", # CYRILLIC LETTER HA
            "\xC6" => "\xE6", # CYRILLIC LETTER TSE
            "\xC7" => "\xE7", # CYRILLIC LETTER CHE
            "\xC8" => "\xE8", # CYRILLIC LETTER SHA
            "\xC9" => "\xE9", # CYRILLIC LETTER SHCHA
            "\xCA" => "\xEA", # CYRILLIC LETTER HARD SIGN
            "\xCB" => "\xEB", # CYRILLIC LETTER YERU
            "\xCC" => "\xEC", # CYRILLIC LETTER SOFT SIGN
            "\xCD" => "\xED", # CYRILLIC LETTER E
            "\xCE" => "\xEE", # CYRILLIC LETTER YU
            "\xCF" => "\xEF", # CYRILLIC LETTER YA
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ekoi8r \z/oxms) {
        %lc = (%lc,
            "\xB3" => "\xA3", # CYRILLIC LETTER IO
            "\xE0" => "\xC0", # CYRILLIC LETTER IU
            "\xE1" => "\xC1", # CYRILLIC LETTER A
            "\xE2" => "\xC2", # CYRILLIC LETTER BE
            "\xE3" => "\xC3", # CYRILLIC LETTER TSE
            "\xE4" => "\xC4", # CYRILLIC LETTER DE
            "\xE5" => "\xC5", # CYRILLIC LETTER IE
            "\xE6" => "\xC6", # CYRILLIC LETTER EF
            "\xE7" => "\xC7", # CYRILLIC LETTER GE
            "\xE8" => "\xC8", # CYRILLIC LETTER KHA
            "\xE9" => "\xC9", # CYRILLIC LETTER II
            "\xEA" => "\xCA", # CYRILLIC LETTER SHORT II
            "\xEB" => "\xCB", # CYRILLIC LETTER KA
            "\xEC" => "\xCC", # CYRILLIC LETTER EL
            "\xED" => "\xCD", # CYRILLIC LETTER EM
            "\xEE" => "\xCE", # CYRILLIC LETTER EN
            "\xEF" => "\xCF", # CYRILLIC LETTER O
            "\xF0" => "\xD0", # CYRILLIC LETTER PE
            "\xF1" => "\xD1", # CYRILLIC LETTER IA
            "\xF2" => "\xD2", # CYRILLIC LETTER ER
            "\xF3" => "\xD3", # CYRILLIC LETTER ES
            "\xF4" => "\xD4", # CYRILLIC LETTER TE
            "\xF5" => "\xD5", # CYRILLIC LETTER U
            "\xF6" => "\xD6", # CYRILLIC LETTER ZHE
            "\xF7" => "\xD7", # CYRILLIC LETTER VE
            "\xF8" => "\xD8", # CYRILLIC LETTER SOFT SIGN
            "\xF9" => "\xD9", # CYRILLIC LETTER YERI
            "\xFA" => "\xDA", # CYRILLIC LETTER ZE
            "\xFB" => "\xDB", # CYRILLIC LETTER SHA
            "\xFC" => "\xDC", # CYRILLIC LETTER REVERSED E
            "\xFD" => "\xDD", # CYRILLIC LETTER SHCHA
            "\xFE" => "\xDE", # CYRILLIC LETTER CHE
            "\xFF" => "\xDF", # CYRILLIC LETTER HARD SIGN
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ekoi8u \z/oxms) {
        %lc = (%lc,
            "\xB3" => "\xA3", # CYRILLIC LETTER IO
            "\xB4" => "\xA4", # CYRILLIC LETTER UKRAINIAN IE
            "\xB6" => "\xA6", # CYRILLIC LETTER BYELORUSSIAN-UKRAINIAN I
            "\xB7" => "\xA7", # CYRILLIC LETTER YI (UKRAINIAN)
            "\xBD" => "\xAD", # CYRILLIC LETTER GHE WITH UPTURN
            "\xE0" => "\xC0", # CYRILLIC LETTER YU
            "\xE1" => "\xC1", # CYRILLIC LETTER A
            "\xE2" => "\xC2", # CYRILLIC LETTER BE
            "\xE3" => "\xC3", # CYRILLIC LETTER TSE
            "\xE4" => "\xC4", # CYRILLIC LETTER DE
            "\xE5" => "\xC5", # CYRILLIC LETTER IE
            "\xE6" => "\xC6", # CYRILLIC LETTER EF
            "\xE7" => "\xC7", # CYRILLIC LETTER GHE
            "\xE8" => "\xC8", # CYRILLIC LETTER KHA
            "\xE9" => "\xC9", # CYRILLIC LETTER I
            "\xEA" => "\xCA", # CYRILLIC LETTER SHORT I
            "\xEB" => "\xCB", # CYRILLIC LETTER KA
            "\xEC" => "\xCC", # CYRILLIC LETTER EL
            "\xED" => "\xCD", # CYRILLIC LETTER EM
            "\xEE" => "\xCE", # CYRILLIC LETTER EN
            "\xEF" => "\xCF", # CYRILLIC LETTER O
            "\xF0" => "\xD0", # CYRILLIC LETTER PE
            "\xF1" => "\xD1", # CYRILLIC LETTER YA
            "\xF2" => "\xD2", # CYRILLIC LETTER ER
            "\xF3" => "\xD3", # CYRILLIC LETTER ES
            "\xF4" => "\xD4", # CYRILLIC LETTER TE
            "\xF5" => "\xD5", # CYRILLIC LETTER U
            "\xF6" => "\xD6", # CYRILLIC LETTER ZHE
            "\xF7" => "\xD7", # CYRILLIC LETTER VE
            "\xF8" => "\xD8", # CYRILLIC LETTER SOFT SIGN
            "\xF9" => "\xD9", # CYRILLIC LETTER YERU
            "\xFA" => "\xDA", # CYRILLIC LETTER ZE
            "\xFB" => "\xDB", # CYRILLIC LETTER SHA
            "\xFC" => "\xDC", # CYRILLIC LETTER E
            "\xFD" => "\xDD", # CYRILLIC LETTER SHCHA
            "\xFE" => "\xDE", # CYRILLIC LETTER CHE
            "\xFF" => "\xDF", # CYRILLIC LETTER HARD SIGN
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Egreek \z/oxms) {
        %lc = (%lc,
            "\xB6" => "\xDC", # GREEK LETTER ALPHA WITH TONOS
            "\xB8" => "\xDD", # GREEK LETTER EPSILON WITH TONOS
            "\xB9" => "\xDE", # GREEK LETTER ETA WITH TONOS
            "\xBA" => "\xDF", # GREEK LETTER IOTA WITH TONOS
            "\xBC" => "\xFC", # GREEK LETTER OMICRON WITH TONOS
            "\xBE" => "\xFD", # GREEK LETTER UPSILON WITH TONOS
            "\xBF" => "\xFE", # GREEK LETTER OMEGA WITH TONOS
            "\xC1" => "\xE1", # GREEK LETTER ALPHA
            "\xC2" => "\xE2", # GREEK LETTER BETA
            "\xC3" => "\xE3", # GREEK LETTER GAMMA
            "\xC4" => "\xE4", # GREEK LETTER DELTA
            "\xC5" => "\xE5", # GREEK LETTER EPSILON
            "\xC6" => "\xE6", # GREEK LETTER ZETA
            "\xC7" => "\xE7", # GREEK LETTER ETA
            "\xC8" => "\xE8", # GREEK LETTER THETA
            "\xC9" => "\xE9", # GREEK LETTER IOTA
            "\xCA" => "\xEA", # GREEK LETTER KAPPA
            "\xCB" => "\xEB", # GREEK LETTER LAMDA
            "\xCC" => "\xEC", # GREEK LETTER MU
            "\xCD" => "\xED", # GREEK LETTER NU
            "\xCE" => "\xEE", # GREEK LETTER XI
            "\xCF" => "\xEF", # GREEK LETTER OMICRON
            "\xD0" => "\xF0", # GREEK LETTER PI
            "\xD1" => "\xF1", # GREEK LETTER RHO
            "\xD3" => "\xF3", # GREEK LETTER SIGMA
            "\xD4" => "\xF4", # GREEK LETTER TAU
            "\xD5" => "\xF5", # GREEK LETTER UPSILON
            "\xD6" => "\xF6", # GREEK LETTER PHI
            "\xD7" => "\xF7", # GREEK LETTER CHI
            "\xD8" => "\xF8", # GREEK LETTER PSI
            "\xD9" => "\xF9", # GREEK LETTER OMEGA
            "\xDA" => "\xFA", # GREEK LETTER IOTA WITH DIALYTIKA
            "\xDB" => "\xFB", # GREEK LETTER UPSILON WITH DIALYTIKA
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin5 \z/oxms) {
        %lc = (%lc,
            "\xC0" => "\xE0", # LATIN LETTER A WITH GRAVE
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH TILDE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER A WITH RING ABOVE
            "\xC6" => "\xE6", # LATIN LETTER AE
            "\xC7" => "\xE7", # LATIN LETTER C WITH CEDILLA
            "\xC8" => "\xE8", # LATIN LETTER E WITH GRAVE
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER I WITH GRAVE
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH DIAERESIS
            "\xD0" => "\xF0", # LATIN LETTER G WITH BREVE
            "\xD1" => "\xF1", # LATIN LETTER N WITH TILDE
            "\xD2" => "\xF2", # LATIN LETTER O WITH GRAVE
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH TILDE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD8" => "\xF8", # LATIN LETTER O WITH STROKE
            "\xD9" => "\xF9", # LATIN LETTER U WITH GRAVE
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDE" => "\xFE", # LATIN LETTER S WITH CEDILLA
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin6 \z/oxms) {
        %lc = (%lc,
            "\xA1" => "\xB1", # LATIN LETTER A WITH OGONEK
            "\xA2" => "\xB2", # LATIN LETTER E WITH MACRON
            "\xA3" => "\xB3", # LATIN LETTER G WITH CEDILLA
            "\xA4" => "\xB4", # LATIN LETTER I WITH MACRON
            "\xA5" => "\xB5", # LATIN LETTER I WITH TILDE
            "\xA6" => "\xB6", # LATIN LETTER K WITH CEDILLA
            "\xA8" => "\xB8", # LATIN LETTER L WITH CEDILLA
            "\xA9" => "\xB9", # LATIN LETTER D WITH STROKE
            "\xAA" => "\xBA", # LATIN LETTER S WITH CARON
            "\xAB" => "\xBB", # LATIN LETTER T WITH STROKE
            "\xAC" => "\xBC", # LATIN LETTER Z WITH CARON
            "\xAE" => "\xBE", # LATIN LETTER U WITH MACRON
            "\xAF" => "\xBF", # LATIN LETTER ENG
            "\xC0" => "\xE0", # LATIN LETTER A WITH MACRON
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH TILDE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER A WITH RING ABOVE
            "\xC6" => "\xE6", # LATIN LETTER AE
            "\xC7" => "\xE7", # LATIN LETTER I WITH OGONEK
            "\xC8" => "\xE8", # LATIN LETTER C WITH CARON
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH OGONEK
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER E WITH DOT ABOVE
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH DIAERESIS
            "\xD0" => "\xF0", # LATIN LETTER ETH (Icelandic)
            "\xD1" => "\xF1", # LATIN LETTER N WITH CEDILLA
            "\xD2" => "\xF2", # LATIN LETTER O WITH MACRON
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH TILDE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD7" => "\xF7", # LATIN LETTER U WITH TILDE
            "\xD8" => "\xF8", # LATIN LETTER O WITH STROKE
            "\xD9" => "\xF9", # LATIN LETTER U WITH OGONEK
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER Y WITH ACUTE
            "\xDE" => "\xFE", # LATIN LETTER THORN (Icelandic)
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin7 \z/oxms) {
        %lc = (%lc,
            "\xA8" => "\xB8", # LATIN LETTER O WITH STROKE
            "\xAA" => "\xBA", # LATIN LETTER R WITH CEDILLA
            "\xAF" => "\xBF", # LATIN LETTER AE
            "\xC0" => "\xE0", # LATIN LETTER A WITH OGONEK
            "\xC1" => "\xE1", # LATIN LETTER I WITH OGONEK
            "\xC2" => "\xE2", # LATIN LETTER A WITH MACRON
            "\xC3" => "\xE3", # LATIN LETTER C WITH ACUTE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER A WITH RING ABOVE
            "\xC6" => "\xE6", # LATIN LETTER E WITH OGONEK
            "\xC7" => "\xE7", # LATIN LETTER E WITH MACRON
            "\xC8" => "\xE8", # LATIN LETTER C WITH CARON
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER Z WITH ACUTE
            "\xCB" => "\xEB", # LATIN LETTER E WITH DOT ABOVE
            "\xCC" => "\xEC", # LATIN LETTER G WITH CEDILLA
            "\xCD" => "\xED", # LATIN LETTER K WITH CEDILLA
            "\xCE" => "\xEE", # LATIN LETTER I WITH MACRON
            "\xCF" => "\xEF", # LATIN LETTER L WITH CEDILLA
            "\xD0" => "\xF0", # LATIN LETTER S WITH CARON
            "\xD1" => "\xF1", # LATIN LETTER N WITH ACUTE
            "\xD2" => "\xF2", # LATIN LETTER N WITH CEDILLA
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH MACRON
            "\xD5" => "\xF5", # LATIN LETTER O WITH TILDE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD8" => "\xF8", # LATIN LETTER U WITH OGONEK
            "\xD9" => "\xF9", # LATIN LETTER L WITH STROKE
            "\xDA" => "\xFA", # LATIN LETTER S WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH MACRON
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER Z WITH DOT ABOVE
            "\xDE" => "\xFE", # LATIN LETTER Z WITH CARON
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin8 \z/oxms) {
        %lc = (%lc,
            "\xA1" => "\xA2", # LATIN LETTER B WITH DOT ABOVE
            "\xA4" => "\xA5", # LATIN LETTER C WITH DOT ABOVE
            "\xA6" => "\xAB", # LATIN LETTER D WITH DOT ABOVE
            "\xA8" => "\xB8", # LATIN LETTER W WITH GRAVE
            "\xAA" => "\xBA", # LATIN LETTER W WITH ACUTE
            "\xAC" => "\xBC", # LATIN LETTER Y WITH GRAVE
            "\xAF" => "\xFF", # LATIN LETTER Y WITH DIAERESIS
            "\xB0" => "\xB1", # LATIN LETTER F WITH DOT ABOVE
            "\xB2" => "\xB3", # LATIN LETTER G WITH DOT ABOVE
            "\xB4" => "\xB5", # LATIN LETTER M WITH DOT ABOVE
            "\xB7" => "\xB9", # LATIN LETTER P WITH DOT ABOVE
            "\xBB" => "\xBF", # LATIN LETTER S WITH DOT ABOVE
            "\xBD" => "\xBE", # LATIN LETTER W WITH DIAERESIS
            "\xC0" => "\xE0", # LATIN LETTER A WITH GRAVE
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH TILDE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER A WITH RING ABOVE
            "\xC6" => "\xE6", # LATIN LETTER AE
            "\xC7" => "\xE7", # LATIN LETTER C WITH CEDILLA
            "\xC8" => "\xE8", # LATIN LETTER E WITH GRAVE
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER I WITH GRAVE
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH DIAERESIS
            "\xD0" => "\xF0", # LATIN LETTER W WITH CIRCUMFLEX
            "\xD1" => "\xF1", # LATIN LETTER N WITH TILDE
            "\xD2" => "\xF2", # LATIN LETTER O WITH GRAVE
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH TILDE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD7" => "\xF7", # LATIN LETTER T WITH DOT ABOVE
            "\xD8" => "\xF8", # LATIN LETTER O WITH STROKE
            "\xD9" => "\xF9", # LATIN LETTER U WITH GRAVE
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER Y WITH ACUTE
            "\xDE" => "\xFE", # LATIN LETTER Y WITH CIRCUMFLEX
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin9 \z/oxms) {
        %lc = (%lc,
            "\xA6" => "\xA8", # LATIN LETTER S WITH CARON
            "\xB4" => "\xB8", # LATIN LETTER Z WITH CARON
            "\xBC" => "\xBD", # LATIN LIGATURE OE
            "\xBE" => "\xFF", # LATIN LETTER Y WITH DIAERESIS
            "\xC0" => "\xE0", # LATIN LETTER A WITH GRAVE
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH TILDE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER A WITH RING ABOVE
            "\xC6" => "\xE6", # LATIN LETTER AE
            "\xC7" => "\xE7", # LATIN LETTER C WITH CEDILLA
            "\xC8" => "\xE8", # LATIN LETTER E WITH GRAVE
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER I WITH GRAVE
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH DIAERESIS
            "\xD0" => "\xF0", # LATIN LETTER ETH
            "\xD1" => "\xF1", # LATIN LETTER N WITH TILDE
            "\xD2" => "\xF2", # LATIN LETTER O WITH GRAVE
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH TILDE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD8" => "\xF8", # LATIN LETTER O WITH STROKE
            "\xD9" => "\xF9", # LATIN LETTER U WITH GRAVE
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER Y WITH ACUTE
            "\xDE" => "\xFE", # LATIN LETTER THORN
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin10 \z/oxms) {
        %lc = (%lc,
            "\xA1" => "\xA2", # LATIN LETTER A WITH OGONEK
            "\xA3" => "\xB3", # LATIN LETTER L WITH STROKE
            "\xA6" => "\xA8", # LATIN LETTER S WITH CARON
            "\xAA" => "\xBA", # LATIN LETTER S WITH COMMA BELOW
            "\xAC" => "\xAE", # LATIN LETTER Z WITH ACUTE
            "\xAF" => "\xBF", # LATIN LETTER Z WITH DOT ABOVE
            "\xB2" => "\xB9", # LATIN LETTER C WITH CARON
            "\xB4" => "\xB8", # LATIN LETTER Z WITH CARON
            "\xBC" => "\xBD", # LATIN LIGATURE OE
            "\xBE" => "\xFF", # LATIN LETTER Y WITH DIAERESIS
            "\xC0" => "\xE0", # LATIN LETTER A WITH GRAVE
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH BREVE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER C WITH ACUTE
            "\xC6" => "\xE6", # LATIN LETTER AE
            "\xC7" => "\xE7", # LATIN LETTER C WITH CEDILLA
            "\xC8" => "\xE8", # LATIN LETTER E WITH GRAVE
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER I WITH GRAVE
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH DIAERESIS
            "\xD0" => "\xF0", # LATIN LETTER D WITH STROKE
            "\xD1" => "\xF1", # LATIN LETTER N WITH ACUTE
            "\xD2" => "\xF2", # LATIN LETTER O WITH GRAVE
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH DOUBLE ACUTE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD7" => "\xF7", # LATIN LETTER S WITH ACUTE
            "\xD8" => "\xF8", # LATIN LETTER U WITH DOUBLE ACUTE
            "\xD9" => "\xF9", # LATIN LETTER U WITH GRAVE
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER E WITH OGONEK
            "\xDE" => "\xFE", # LATIN LETTER T WITH COMMA BELOW
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ewindows1252 \z/oxms) {
        %lc = (%lc,
            "\x8A" => "\x9A", # LATIN LETTER S WITH CARON
            "\x8C" => "\x9C", # LATIN LIGATURE OE
            "\x8E" => "\x9E", # LATIN LETTER Z WITH CARON
            "\x9F" => "\xFF", # LATIN LETTER Y WITH DIAERESIS
            "\xC0" => "\xE0", # LATIN LETTER A WITH GRAVE
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH TILDE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER A WITH RING ABOVE
            "\xC6" => "\xE6", # LATIN LETTER AE
            "\xC7" => "\xE7", # LATIN LETTER C WITH CEDILLA
            "\xC8" => "\xE8", # LATIN LETTER E WITH GRAVE
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCC" => "\xEC", # LATIN LETTER I WITH GRAVE
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH DIAERESIS
            "\xD0" => "\xF0", # LATIN LETTER ETH
            "\xD1" => "\xF1", # LATIN LETTER N WITH TILDE
            "\xD2" => "\xF2", # LATIN LETTER O WITH GRAVE
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH TILDE
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD8" => "\xF8", # LATIN LETTER O WITH STROKE
            "\xD9" => "\xF9", # LATIN LETTER U WITH GRAVE
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER Y WITH ACUTE
            "\xDE" => "\xFE", # LATIN LETTER THORN
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ewindows1258 \z/oxms) {
        %lc = (%lc,
            "\x8C" => "\x9C", # LATIN LIGATURE OE
            "\x9F" => "\xFF", # LATIN LETTER Y WITH DIAERESIS
            "\xC0" => "\xE0", # LATIN LETTER A WITH GRAVE
            "\xC1" => "\xE1", # LATIN LETTER A WITH ACUTE
            "\xC2" => "\xE2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xC3" => "\xE3", # LATIN LETTER A WITH BREVE
            "\xC4" => "\xE4", # LATIN LETTER A WITH DIAERESIS
            "\xC5" => "\xE5", # LATIN LETTER A WITH RING ABOVE
            "\xC6" => "\xE6", # LATIN LETTER AE
            "\xC7" => "\xE7", # LATIN LETTER C WITH CEDILLA
            "\xC8" => "\xE8", # LATIN LETTER E WITH GRAVE
            "\xC9" => "\xE9", # LATIN LETTER E WITH ACUTE
            "\xCA" => "\xEA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xCB" => "\xEB", # LATIN LETTER E WITH DIAERESIS
            "\xCD" => "\xED", # LATIN LETTER I WITH ACUTE
            "\xCE" => "\xEE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xCF" => "\xEF", # LATIN LETTER I WITH DIAERESIS
            "\xD0" => "\xF0", # LATIN LETTER D WITH STROKE
            "\xD1" => "\xF1", # LATIN LETTER N WITH TILDE
            "\xD3" => "\xF3", # LATIN LETTER O WITH ACUTE
            "\xD4" => "\xF4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xD5" => "\xF5", # LATIN LETTER O WITH HORN
            "\xD6" => "\xF6", # LATIN LETTER O WITH DIAERESIS
            "\xD8" => "\xF8", # LATIN LETTER O WITH STROKE
            "\xD9" => "\xF9", # LATIN LETTER U WITH GRAVE
            "\xDA" => "\xFA", # LATIN LETTER U WITH ACUTE
            "\xDB" => "\xFB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xDC" => "\xFC", # LATIN LETTER U WITH DIAERESIS
            "\xDD" => "\xFD", # LATIN LETTER U WITH HORN
        );
    }

    # lower case first with parameter
    sub Char::Ebig5plus::lcfirst(@) {
        if (@_) {
            my $s = shift @_;
            if (@_ and wantarray) {
                return Char::Ebig5plus::lc(CORE::substr($s,0,1)) . CORE::substr($s,1), @_;
            }
            else {
                return Char::Ebig5plus::lc(CORE::substr($s,0,1)) . CORE::substr($s,1);
            }
        }
        else {
            return Char::Ebig5plus::lc(CORE::substr($_,0,1)) . CORE::substr($_,1);
        }
    }

    # lower case first without parameter
    sub Char::Ebig5plus::lcfirst_() {
        return Char::Ebig5plus::lc(CORE::substr($_,0,1)) . CORE::substr($_,1);
    }

    # lower case with parameter
    sub Char::Ebig5plus::lc(@) {
        if (@_) {
            my $s = shift @_;
            if (@_ and wantarray) {
                return join('', map {defined($lc{$_}) ? $lc{$_} : $_} ($s =~ m/\G ($q_char) /oxmsg)), @_;
            }
            else {
                return join('', map {defined($lc{$_}) ? $lc{$_} : $_} ($s =~ m/\G ($q_char) /oxmsg));
            }
        }
        else {
            return Char::Ebig5plus::lc_();
        }
    }

    # lower case without parameter
    sub Char::Ebig5plus::lc_() {
        my $s = $_;
        return join '', map {defined($lc{$_}) ? $lc{$_} : $_} ($s =~ m/\G ($q_char) /oxmsg);
    }
}

#
# Big5Plus upper case
#
{
    my %uc = ();
    @uc{qw(a b c d e f g h i j k l m n o p q r s t u v w x y z)} =
        qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z);

    if (0) {
    }

    elsif (__PACKAGE__ =~ m/ \b Eusascii \z/oxms) {
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin1 \z/oxms) {
        %uc = (%uc,
            "\xE0" => "\xC0", # LATIN LETTER A WITH GRAVE
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH TILDE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER A WITH RING ABOVE
            "\xE6" => "\xC6", # LATIN LETTER AE
            "\xE7" => "\xC7", # LATIN LETTER C WITH CEDILLA
            "\xE8" => "\xC8", # LATIN LETTER E WITH GRAVE
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER I WITH GRAVE
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH DIAERESIS
            "\xF0" => "\xD0", # LATIN LETTER ETH (Icelandic)
            "\xF1" => "\xD1", # LATIN LETTER N WITH TILDE
            "\xF2" => "\xD2", # LATIN LETTER O WITH GRAVE
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH TILDE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF8" => "\xD8", # LATIN LETTER O WITH STROKE
            "\xF9" => "\xD9", # LATIN LETTER U WITH GRAVE
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER Y WITH ACUTE
            "\xFE" => "\xDE", # LATIN LETTER THORN (Icelandic)
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin2 \z/oxms) {
        %uc = (%uc,
            "\xB1" => "\xA1", # LATIN LETTER A WITH OGONEK
            "\xB3" => "\xA3", # LATIN LETTER L WITH STROKE
            "\xB5" => "\xA5", # LATIN LETTER L WITH CARON
            "\xB6" => "\xA6", # LATIN LETTER S WITH ACUTE
            "\xB9" => "\xA9", # LATIN LETTER S WITH CARON
            "\xBA" => "\xAA", # LATIN LETTER S WITH CEDILLA
            "\xBB" => "\xAB", # LATIN LETTER T WITH CARON
            "\xBC" => "\xAC", # LATIN LETTER Z WITH ACUTE
            "\xBE" => "\xAE", # LATIN LETTER Z WITH CARON
            "\xBF" => "\xAF", # LATIN LETTER Z WITH DOT ABOVE
            "\xE0" => "\xC0", # LATIN LETTER R WITH ACUTE
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH BREVE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER L WITH ACUTE
            "\xE6" => "\xC6", # LATIN LETTER C WITH ACUTE
            "\xE7" => "\xC7", # LATIN LETTER C WITH CEDILLA
            "\xE8" => "\xC8", # LATIN LETTER C WITH CARON
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH OGONEK
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER E WITH CARON
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER D WITH CARON
            "\xF0" => "\xD0", # LATIN LETTER D WITH STROKE
            "\xF1" => "\xD1", # LATIN LETTER N WITH ACUTE
            "\xF2" => "\xD2", # LATIN LETTER N WITH CARON
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH DOUBLE ACUTE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF8" => "\xD8", # LATIN LETTER R WITH CARON
            "\xF9" => "\xD9", # LATIN LETTER U WITH RING ABOVE
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH DOUBLE ACUTE
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER Y WITH ACUTE
            "\xFE" => "\xDE", # LATIN LETTER T WITH CEDILLA
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin3 \z/oxms) {
        %uc = (%uc,
            "\xB1" => "\xA1", # LATIN LETTER H WITH STROKE
            "\xB6" => "\xA6", # LATIN LETTER H WITH CIRCUMFLEX
            "\xBA" => "\xAA", # LATIN LETTER S WITH CEDILLA
            "\xBB" => "\xAB", # LATIN LETTER G WITH BREVE
            "\xBC" => "\xAC", # LATIN LETTER J WITH CIRCUMFLEX
            "\xBF" => "\xAF", # LATIN LETTER Z WITH DOT ABOVE
            "\xE0" => "\xC0", # LATIN LETTER A WITH GRAVE
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER C WITH DOT ABOVE
            "\xE6" => "\xC6", # LATIN LETTER C WITH CIRCUMFLEX
            "\xE7" => "\xC7", # LATIN LETTER C WITH CEDILLA
            "\xE8" => "\xC8", # LATIN LETTER E WITH GRAVE
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER I WITH GRAVE
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH DIAERESIS
            "\xF1" => "\xD1", # LATIN LETTER N WITH TILDE
            "\xF2" => "\xD2", # LATIN LETTER O WITH GRAVE
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER G WITH DOT ABOVE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF8" => "\xD8", # LATIN LETTER G WITH CIRCUMFLEX
            "\xF9" => "\xD9", # LATIN LETTER U WITH GRAVE
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER U WITH BREVE
            "\xFE" => "\xDE", # LATIN LETTER S WITH CIRCUMFLEX
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin4 \z/oxms) {
        %uc = (%uc,
            "\xB1" => "\xA1", # LATIN LETTER A WITH OGONEK
            "\xB3" => "\xA3", # LATIN LETTER R WITH CEDILLA
            "\xB5" => "\xA5", # LATIN LETTER I WITH TILDE
            "\xB6" => "\xA6", # LATIN LETTER L WITH CEDILLA
            "\xB9" => "\xA9", # LATIN LETTER S WITH CARON
            "\xBA" => "\xAA", # LATIN LETTER E WITH MACRON
            "\xBB" => "\xAB", # LATIN LETTER G WITH CEDILLA
            "\xBC" => "\xAC", # LATIN LETTER T WITH STROKE
            "\xBE" => "\xAE", # LATIN LETTER Z WITH CARON
            "\xBF" => "\xBD", # LATIN LETTER ENG
            "\xE0" => "\xC0", # LATIN LETTER A WITH MACRON
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH TILDE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER A WITH RING ABOVE
            "\xE6" => "\xC6", # LATIN LETTER AE
            "\xE7" => "\xC7", # LATIN LETTER I WITH OGONEK
            "\xE8" => "\xC8", # LATIN LETTER C WITH CARON
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH OGONEK
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER E WITH DOT ABOVE
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH MACRON
            "\xF0" => "\xD0", # LATIN LETTER D WITH STROKE
            "\xF1" => "\xD1", # LATIN LETTER N WITH CEDILLA
            "\xF2" => "\xD2", # LATIN LETTER O WITH MACRON
            "\xF3" => "\xD3", # LATIN LETTER K WITH CEDILLA
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH TILDE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF8" => "\xD8", # LATIN LETTER O WITH STROKE
            "\xF9" => "\xD9", # LATIN LETTER U WITH OGONEK
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER U WITH TILDE
            "\xFE" => "\xDE", # LATIN LETTER U WITH MACRON
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ecyrillic \z/oxms) {
        %uc = (%uc,
            "\xD0" => "\xB0", # CYRILLIC LETTER A
            "\xD1" => "\xB1", # CYRILLIC LETTER BE
            "\xD2" => "\xB2", # CYRILLIC LETTER VE
            "\xD3" => "\xB3", # CYRILLIC LETTER GHE
            "\xD4" => "\xB4", # CYRILLIC LETTER DE
            "\xD5" => "\xB5", # CYRILLIC LETTER IE
            "\xD6" => "\xB6", # CYRILLIC LETTER ZHE
            "\xD7" => "\xB7", # CYRILLIC LETTER ZE
            "\xD8" => "\xB8", # CYRILLIC LETTER I
            "\xD9" => "\xB9", # CYRILLIC LETTER SHORT I
            "\xDA" => "\xBA", # CYRILLIC LETTER KA
            "\xDB" => "\xBB", # CYRILLIC LETTER EL
            "\xDC" => "\xBC", # CYRILLIC LETTER EM
            "\xDD" => "\xBD", # CYRILLIC LETTER EN
            "\xDE" => "\xBE", # CYRILLIC LETTER O
            "\xDF" => "\xBF", # CYRILLIC LETTER PE
            "\xE0" => "\xC0", # CYRILLIC LETTER ER
            "\xE1" => "\xC1", # CYRILLIC LETTER ES
            "\xE2" => "\xC2", # CYRILLIC LETTER TE
            "\xE3" => "\xC3", # CYRILLIC LETTER U
            "\xE4" => "\xC4", # CYRILLIC LETTER EF
            "\xE5" => "\xC5", # CYRILLIC LETTER HA
            "\xE6" => "\xC6", # CYRILLIC LETTER TSE
            "\xE7" => "\xC7", # CYRILLIC LETTER CHE
            "\xE8" => "\xC8", # CYRILLIC LETTER SHA
            "\xE9" => "\xC9", # CYRILLIC LETTER SHCHA
            "\xEA" => "\xCA", # CYRILLIC LETTER HARD SIGN
            "\xEB" => "\xCB", # CYRILLIC LETTER YERU
            "\xEC" => "\xCC", # CYRILLIC LETTER SOFT SIGN
            "\xED" => "\xCD", # CYRILLIC LETTER E
            "\xEE" => "\xCE", # CYRILLIC LETTER YU
            "\xEF" => "\xCF", # CYRILLIC LETTER YA
            "\xF1" => "\xA1", # CYRILLIC LETTER IO
            "\xF2" => "\xA2", # CYRILLIC LETTER DJE
            "\xF3" => "\xA3", # CYRILLIC LETTER GJE
            "\xF4" => "\xA4", # CYRILLIC LETTER UKRAINIAN IE
            "\xF5" => "\xA5", # CYRILLIC LETTER DZE
            "\xF6" => "\xA6", # CYRILLIC LETTER BYELORUSSIAN-UKRAINIAN I
            "\xF7" => "\xA7", # CYRILLIC LETTER YI
            "\xF8" => "\xA8", # CYRILLIC LETTER JE
            "\xF9" => "\xA9", # CYRILLIC LETTER LJE
            "\xFA" => "\xAA", # CYRILLIC LETTER NJE
            "\xFB" => "\xAB", # CYRILLIC LETTER TSHE
            "\xFC" => "\xAC", # CYRILLIC LETTER KJE
            "\xFE" => "\xAE", # CYRILLIC LETTER SHORT U
            "\xFF" => "\xAF", # CYRILLIC LETTER DZHE
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ekoi8r \z/oxms) {
        %uc = (%uc,
            "\xA3" => "\xB3", # CYRILLIC LETTER IO
            "\xC0" => "\xE0", # CYRILLIC LETTER IU
            "\xC1" => "\xE1", # CYRILLIC LETTER A
            "\xC2" => "\xE2", # CYRILLIC LETTER BE
            "\xC3" => "\xE3", # CYRILLIC LETTER TSE
            "\xC4" => "\xE4", # CYRILLIC LETTER DE
            "\xC5" => "\xE5", # CYRILLIC LETTER IE
            "\xC6" => "\xE6", # CYRILLIC LETTER EF
            "\xC7" => "\xE7", # CYRILLIC LETTER GE
            "\xC8" => "\xE8", # CYRILLIC LETTER KHA
            "\xC9" => "\xE9", # CYRILLIC LETTER II
            "\xCA" => "\xEA", # CYRILLIC LETTER SHORT II
            "\xCB" => "\xEB", # CYRILLIC LETTER KA
            "\xCC" => "\xEC", # CYRILLIC LETTER EL
            "\xCD" => "\xED", # CYRILLIC LETTER EM
            "\xCE" => "\xEE", # CYRILLIC LETTER EN
            "\xCF" => "\xEF", # CYRILLIC LETTER O
            "\xD0" => "\xF0", # CYRILLIC LETTER PE
            "\xD1" => "\xF1", # CYRILLIC LETTER IA
            "\xD2" => "\xF2", # CYRILLIC LETTER ER
            "\xD3" => "\xF3", # CYRILLIC LETTER ES
            "\xD4" => "\xF4", # CYRILLIC LETTER TE
            "\xD5" => "\xF5", # CYRILLIC LETTER U
            "\xD6" => "\xF6", # CYRILLIC LETTER ZHE
            "\xD7" => "\xF7", # CYRILLIC LETTER VE
            "\xD8" => "\xF8", # CYRILLIC LETTER SOFT SIGN
            "\xD9" => "\xF9", # CYRILLIC LETTER YERI
            "\xDA" => "\xFA", # CYRILLIC LETTER ZE
            "\xDB" => "\xFB", # CYRILLIC LETTER SHA
            "\xDC" => "\xFC", # CYRILLIC LETTER REVERSED E
            "\xDD" => "\xFD", # CYRILLIC LETTER SHCHA
            "\xDE" => "\xFE", # CYRILLIC LETTER CHE
            "\xDF" => "\xFF", # CYRILLIC LETTER HARD SIGN
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ekoi8u \z/oxms) {
        %uc = (%uc,
            "\xA3" => "\xB3", # CYRILLIC LETTER IO
            "\xA4" => "\xB4", # CYRILLIC LETTER UKRAINIAN IE
            "\xA6" => "\xB6", # CYRILLIC LETTER BYELORUSSIAN-UKRAINIAN I
            "\xA7" => "\xB7", # CYRILLIC LETTER YI (UKRAINIAN)
            "\xAD" => "\xBD", # CYRILLIC LETTER GHE WITH UPTURN
            "\xC0" => "\xE0", # CYRILLIC LETTER YU
            "\xC1" => "\xE1", # CYRILLIC LETTER A
            "\xC2" => "\xE2", # CYRILLIC LETTER BE
            "\xC3" => "\xE3", # CYRILLIC LETTER TSE
            "\xC4" => "\xE4", # CYRILLIC LETTER DE
            "\xC5" => "\xE5", # CYRILLIC LETTER IE
            "\xC6" => "\xE6", # CYRILLIC LETTER EF
            "\xC7" => "\xE7", # CYRILLIC LETTER GHE
            "\xC8" => "\xE8", # CYRILLIC LETTER KHA
            "\xC9" => "\xE9", # CYRILLIC LETTER I
            "\xCA" => "\xEA", # CYRILLIC LETTER SHORT I
            "\xCB" => "\xEB", # CYRILLIC LETTER KA
            "\xCC" => "\xEC", # CYRILLIC LETTER EL
            "\xCD" => "\xED", # CYRILLIC LETTER EM
            "\xCE" => "\xEE", # CYRILLIC LETTER EN
            "\xCF" => "\xEF", # CYRILLIC LETTER O
            "\xD0" => "\xF0", # CYRILLIC LETTER PE
            "\xD1" => "\xF1", # CYRILLIC LETTER YA
            "\xD2" => "\xF2", # CYRILLIC LETTER ER
            "\xD3" => "\xF3", # CYRILLIC LETTER ES
            "\xD4" => "\xF4", # CYRILLIC LETTER TE
            "\xD5" => "\xF5", # CYRILLIC LETTER U
            "\xD6" => "\xF6", # CYRILLIC LETTER ZHE
            "\xD7" => "\xF7", # CYRILLIC LETTER VE
            "\xD8" => "\xF8", # CYRILLIC LETTER SOFT SIGN
            "\xD9" => "\xF9", # CYRILLIC LETTER YERU
            "\xDA" => "\xFA", # CYRILLIC LETTER ZE
            "\xDB" => "\xFB", # CYRILLIC LETTER SHA
            "\xDC" => "\xFC", # CYRILLIC LETTER E
            "\xDD" => "\xFD", # CYRILLIC LETTER SHCHA
            "\xDE" => "\xFE", # CYRILLIC LETTER CHE
            "\xDF" => "\xFF", # CYRILLIC LETTER HARD SIGN
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Egreek \z/oxms) {
        %uc = (%uc,
            "\xDC" => "\xB6", # GREEK LETTER ALPHA WITH TONOS
            "\xDD" => "\xB8", # GREEK LETTER EPSILON WITH TONOS
            "\xDE" => "\xB9", # GREEK LETTER ETA WITH TONOS
            "\xDF" => "\xBA", # GREEK LETTER IOTA WITH TONOS
            "\xE1" => "\xC1", # GREEK LETTER ALPHA
            "\xE2" => "\xC2", # GREEK LETTER BETA
            "\xE3" => "\xC3", # GREEK LETTER GAMMA
            "\xE4" => "\xC4", # GREEK LETTER DELTA
            "\xE5" => "\xC5", # GREEK LETTER EPSILON
            "\xE6" => "\xC6", # GREEK LETTER ZETA
            "\xE7" => "\xC7", # GREEK LETTER ETA
            "\xE8" => "\xC8", # GREEK LETTER THETA
            "\xE9" => "\xC9", # GREEK LETTER IOTA
            "\xEA" => "\xCA", # GREEK LETTER KAPPA
            "\xEB" => "\xCB", # GREEK LETTER LAMDA
            "\xEC" => "\xCC", # GREEK LETTER MU
            "\xED" => "\xCD", # GREEK LETTER NU
            "\xEE" => "\xCE", # GREEK LETTER XI
            "\xEF" => "\xCF", # GREEK LETTER OMICRON
            "\xF0" => "\xD0", # GREEK LETTER PI
            "\xF1" => "\xD1", # GREEK LETTER RHO
            "\xF3" => "\xD3", # GREEK LETTER SIGMA
            "\xF4" => "\xD4", # GREEK LETTER TAU
            "\xF5" => "\xD5", # GREEK LETTER UPSILON
            "\xF6" => "\xD6", # GREEK LETTER PHI
            "\xF7" => "\xD7", # GREEK LETTER CHI
            "\xF8" => "\xD8", # GREEK LETTER PSI
            "\xF9" => "\xD9", # GREEK LETTER OMEGA
            "\xFA" => "\xDA", # GREEK LETTER IOTA WITH DIALYTIKA
            "\xFB" => "\xDB", # GREEK LETTER UPSILON WITH DIALYTIKA
            "\xFC" => "\xBC", # GREEK LETTER OMICRON WITH TONOS
            "\xFD" => "\xBE", # GREEK LETTER UPSILON WITH TONOS
            "\xFE" => "\xBF", # GREEK LETTER OMEGA WITH TONOS
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin5 \z/oxms) {
        %uc = (%uc,
            "\xE0" => "\xC0", # LATIN LETTER A WITH GRAVE
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH TILDE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER A WITH RING ABOVE
            "\xE6" => "\xC6", # LATIN LETTER AE
            "\xE7" => "\xC7", # LATIN LETTER C WITH CEDILLA
            "\xE8" => "\xC8", # LATIN LETTER E WITH GRAVE
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER I WITH GRAVE
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH DIAERESIS
            "\xF0" => "\xD0", # LATIN LETTER G WITH BREVE
            "\xF1" => "\xD1", # LATIN LETTER N WITH TILDE
            "\xF2" => "\xD2", # LATIN LETTER O WITH GRAVE
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH TILDE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF8" => "\xD8", # LATIN LETTER O WITH STROKE
            "\xF9" => "\xD9", # LATIN LETTER U WITH GRAVE
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFE" => "\xDE", # LATIN LETTER S WITH CEDILLA
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin6 \z/oxms) {
        %uc = (%uc,
            "\xB1" => "\xA1", # LATIN LETTER A WITH OGONEK
            "\xB2" => "\xA2", # LATIN LETTER E WITH MACRON
            "\xB3" => "\xA3", # LATIN LETTER G WITH CEDILLA
            "\xB4" => "\xA4", # LATIN LETTER I WITH MACRON
            "\xB5" => "\xA5", # LATIN LETTER I WITH TILDE
            "\xB6" => "\xA6", # LATIN LETTER K WITH CEDILLA
            "\xB8" => "\xA8", # LATIN LETTER L WITH CEDILLA
            "\xB9" => "\xA9", # LATIN LETTER D WITH STROKE
            "\xBA" => "\xAA", # LATIN LETTER S WITH CARON
            "\xBB" => "\xAB", # LATIN LETTER T WITH STROKE
            "\xBC" => "\xAC", # LATIN LETTER Z WITH CARON
            "\xBE" => "\xAE", # LATIN LETTER U WITH MACRON
            "\xBF" => "\xAF", # LATIN LETTER ENG
            "\xE0" => "\xC0", # LATIN LETTER A WITH MACRON
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH TILDE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER A WITH RING ABOVE
            "\xE6" => "\xC6", # LATIN LETTER AE
            "\xE7" => "\xC7", # LATIN LETTER I WITH OGONEK
            "\xE8" => "\xC8", # LATIN LETTER C WITH CARON
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH OGONEK
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER E WITH DOT ABOVE
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH DIAERESIS
            "\xF0" => "\xD0", # LATIN LETTER ETH (Icelandic)
            "\xF1" => "\xD1", # LATIN LETTER N WITH CEDILLA
            "\xF2" => "\xD2", # LATIN LETTER O WITH MACRON
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH TILDE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF7" => "\xD7", # LATIN LETTER U WITH TILDE
            "\xF8" => "\xD8", # LATIN LETTER O WITH STROKE
            "\xF9" => "\xD9", # LATIN LETTER U WITH OGONEK
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER Y WITH ACUTE
            "\xFE" => "\xDE", # LATIN LETTER THORN (Icelandic)
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin7 \z/oxms) {
        %uc = (%uc,
            "\xB8" => "\xA8", # LATIN LETTER O WITH STROKE
            "\xBA" => "\xAA", # LATIN LETTER R WITH CEDILLA
            "\xBF" => "\xAF", # LATIN LETTER AE
            "\xE0" => "\xC0", # LATIN LETTER A WITH OGONEK
            "\xE1" => "\xC1", # LATIN LETTER I WITH OGONEK
            "\xE2" => "\xC2", # LATIN LETTER A WITH MACRON
            "\xE3" => "\xC3", # LATIN LETTER C WITH ACUTE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER A WITH RING ABOVE
            "\xE6" => "\xC6", # LATIN LETTER E WITH OGONEK
            "\xE7" => "\xC7", # LATIN LETTER E WITH MACRON
            "\xE8" => "\xC8", # LATIN LETTER C WITH CARON
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER Z WITH ACUTE
            "\xEB" => "\xCB", # LATIN LETTER E WITH DOT ABOVE
            "\xEC" => "\xCC", # LATIN LETTER G WITH CEDILLA
            "\xED" => "\xCD", # LATIN LETTER K WITH CEDILLA
            "\xEE" => "\xCE", # LATIN LETTER I WITH MACRON
            "\xEF" => "\xCF", # LATIN LETTER L WITH CEDILLA
            "\xF0" => "\xD0", # LATIN LETTER S WITH CARON
            "\xF1" => "\xD1", # LATIN LETTER N WITH ACUTE
            "\xF2" => "\xD2", # LATIN LETTER N WITH CEDILLA
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH MACRON
            "\xF5" => "\xD5", # LATIN LETTER O WITH TILDE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF8" => "\xD8", # LATIN LETTER U WITH OGONEK
            "\xF9" => "\xD9", # LATIN LETTER L WITH STROKE
            "\xFA" => "\xDA", # LATIN LETTER S WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH MACRON
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER Z WITH DOT ABOVE
            "\xFE" => "\xDE", # LATIN LETTER Z WITH CARON
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin8 \z/oxms) {
        %uc = (%uc,
            "\xA2" => "\xA1", # LATIN LETTER B WITH DOT ABOVE
            "\xA5" => "\xA4", # LATIN LETTER C WITH DOT ABOVE
            "\xAB" => "\xA6", # LATIN LETTER D WITH DOT ABOVE
            "\xB1" => "\xB0", # LATIN LETTER F WITH DOT ABOVE
            "\xB3" => "\xB2", # LATIN LETTER G WITH DOT ABOVE
            "\xB5" => "\xB4", # LATIN LETTER M WITH DOT ABOVE
            "\xB8" => "\xA8", # LATIN LETTER W WITH GRAVE
            "\xB9" => "\xB7", # LATIN LETTER P WITH DOT ABOVE
            "\xBA" => "\xAA", # LATIN LETTER W WITH ACUTE
            "\xBC" => "\xAC", # LATIN LETTER Y WITH GRAVE
            "\xBE" => "\xBD", # LATIN LETTER W WITH DIAERESIS
            "\xBF" => "\xBB", # LATIN LETTER S WITH DOT ABOVE
            "\xE0" => "\xC0", # LATIN LETTER A WITH GRAVE
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH TILDE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER A WITH RING ABOVE
            "\xE6" => "\xC6", # LATIN LETTER AE
            "\xE7" => "\xC7", # LATIN LETTER C WITH CEDILLA
            "\xE8" => "\xC8", # LATIN LETTER E WITH GRAVE
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER I WITH GRAVE
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH DIAERESIS
            "\xF0" => "\xD0", # LATIN LETTER W WITH CIRCUMFLEX
            "\xF1" => "\xD1", # LATIN LETTER N WITH TILDE
            "\xF2" => "\xD2", # LATIN LETTER O WITH GRAVE
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH TILDE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF7" => "\xD7", # LATIN LETTER T WITH DOT ABOVE
            "\xF8" => "\xD8", # LATIN LETTER O WITH STROKE
            "\xF9" => "\xD9", # LATIN LETTER U WITH GRAVE
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER Y WITH ACUTE
            "\xFE" => "\xDE", # LATIN LETTER Y WITH CIRCUMFLEX
            "\xFF" => "\xAF", # LATIN LETTER Y WITH DIAERESIS
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin9 \z/oxms) {
        %uc = (%uc,
            "\xA8" => "\xA6", # LATIN LETTER S WITH CARON
            "\xB8" => "\xB4", # LATIN LETTER Z WITH CARON
            "\xBD" => "\xBC", # LATIN LIGATURE OE
            "\xE0" => "\xC0", # LATIN LETTER A WITH GRAVE
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH TILDE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER A WITH RING ABOVE
            "\xE6" => "\xC6", # LATIN LETTER AE
            "\xE7" => "\xC7", # LATIN LETTER C WITH CEDILLA
            "\xE8" => "\xC8", # LATIN LETTER E WITH GRAVE
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER I WITH GRAVE
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH DIAERESIS
            "\xF0" => "\xD0", # LATIN LETTER ETH
            "\xF1" => "\xD1", # LATIN LETTER N WITH TILDE
            "\xF2" => "\xD2", # LATIN LETTER O WITH GRAVE
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH TILDE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF8" => "\xD8", # LATIN LETTER O WITH STROKE
            "\xF9" => "\xD9", # LATIN LETTER U WITH GRAVE
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER Y WITH ACUTE
            "\xFE" => "\xDE", # LATIN LETTER THORN
            "\xFF" => "\xBE", # LATIN LETTER Y WITH DIAERESIS
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Elatin10 \z/oxms) {
        %uc = (%uc,
            "\xA2" => "\xA1", # LATIN LETTER A WITH OGONEK
            "\xA8" => "\xA6", # LATIN LETTER S WITH CARON
            "\xAE" => "\xAC", # LATIN LETTER Z WITH ACUTE
            "\xB3" => "\xA3", # LATIN LETTER L WITH STROKE
            "\xB8" => "\xB4", # LATIN LETTER Z WITH CARON
            "\xB9" => "\xB2", # LATIN LETTER C WITH CARON
            "\xBA" => "\xAA", # LATIN LETTER S WITH COMMA BELOW
            "\xBD" => "\xBC", # LATIN LIGATURE OE
            "\xBF" => "\xAF", # LATIN LETTER Z WITH DOT ABOVE
            "\xE0" => "\xC0", # LATIN LETTER A WITH GRAVE
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH BREVE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER C WITH ACUTE
            "\xE6" => "\xC6", # LATIN LETTER AE
            "\xE7" => "\xC7", # LATIN LETTER C WITH CEDILLA
            "\xE8" => "\xC8", # LATIN LETTER E WITH GRAVE
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER I WITH GRAVE
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH DIAERESIS
            "\xF0" => "\xD0", # LATIN LETTER D WITH STROKE
            "\xF1" => "\xD1", # LATIN LETTER N WITH ACUTE
            "\xF2" => "\xD2", # LATIN LETTER O WITH GRAVE
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH DOUBLE ACUTE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF7" => "\xD7", # LATIN LETTER S WITH ACUTE
            "\xF8" => "\xD8", # LATIN LETTER U WITH DOUBLE ACUTE
            "\xF9" => "\xD9", # LATIN LETTER U WITH GRAVE
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER E WITH OGONEK
            "\xFE" => "\xDE", # LATIN LETTER T WITH COMMA BELOW
            "\xFF" => "\xBE", # LATIN LETTER Y WITH DIAERESIS
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ewindows1252 \z/oxms) {
        %uc = (%uc,
            "\x9A" => "\x8A", # LATIN LETTER S WITH CARON
            "\x9C" => "\x8C", # LATIN LIGATURE OE
            "\x9E" => "\x8E", # LATIN LETTER Z WITH CARON
            "\xE0" => "\xC0", # LATIN LETTER A WITH GRAVE
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH TILDE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER A WITH RING ABOVE
            "\xE6" => "\xC6", # LATIN LETTER AE
            "\xE7" => "\xC7", # LATIN LETTER C WITH CEDILLA
            "\xE8" => "\xC8", # LATIN LETTER E WITH GRAVE
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xEC" => "\xCC", # LATIN LETTER I WITH GRAVE
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH DIAERESIS
            "\xF0" => "\xD0", # LATIN LETTER ETH
            "\xF1" => "\xD1", # LATIN LETTER N WITH TILDE
            "\xF2" => "\xD2", # LATIN LETTER O WITH GRAVE
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH TILDE
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF8" => "\xD8", # LATIN LETTER O WITH STROKE
            "\xF9" => "\xD9", # LATIN LETTER U WITH GRAVE
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER Y WITH ACUTE
            "\xFE" => "\xDE", # LATIN LETTER THORN
            "\xFF" => "\x9F", # LATIN LETTER Y WITH DIAERESIS
        );
    }

    elsif (__PACKAGE__ =~ m/ \b Ewindows1258 \z/oxms) {
        %uc = (%uc,
            "\x9C" => "\x8C", # LATIN LIGATURE OE
            "\xFF" => "\x9F", # LATIN LETTER Y WITH DIAERESIS
            "\xE0" => "\xC0", # LATIN LETTER A WITH GRAVE
            "\xE1" => "\xC1", # LATIN LETTER A WITH ACUTE
            "\xE2" => "\xC2", # LATIN LETTER A WITH CIRCUMFLEX
            "\xE3" => "\xC3", # LATIN LETTER A WITH BREVE
            "\xE4" => "\xC4", # LATIN LETTER A WITH DIAERESIS
            "\xE5" => "\xC5", # LATIN LETTER A WITH RING ABOVE
            "\xE6" => "\xC6", # LATIN LETTER AE
            "\xE7" => "\xC7", # LATIN LETTER C WITH CEDILLA
            "\xE8" => "\xC8", # LATIN LETTER E WITH GRAVE
            "\xE9" => "\xC9", # LATIN LETTER E WITH ACUTE
            "\xEA" => "\xCA", # LATIN LETTER E WITH CIRCUMFLEX
            "\xEB" => "\xCB", # LATIN LETTER E WITH DIAERESIS
            "\xED" => "\xCD", # LATIN LETTER I WITH ACUTE
            "\xEE" => "\xCE", # LATIN LETTER I WITH CIRCUMFLEX
            "\xEF" => "\xCF", # LATIN LETTER I WITH DIAERESIS
            "\xF0" => "\xD0", # LATIN LETTER D WITH STROKE
            "\xF1" => "\xD1", # LATIN LETTER N WITH TILDE
            "\xF3" => "\xD3", # LATIN LETTER O WITH ACUTE
            "\xF4" => "\xD4", # LATIN LETTER O WITH CIRCUMFLEX
            "\xF5" => "\xD5", # LATIN LETTER O WITH HORN
            "\xF6" => "\xD6", # LATIN LETTER O WITH DIAERESIS
            "\xF8" => "\xD8", # LATIN LETTER O WITH STROKE
            "\xF9" => "\xD9", # LATIN LETTER U WITH GRAVE
            "\xFA" => "\xDA", # LATIN LETTER U WITH ACUTE
            "\xFB" => "\xDB", # LATIN LETTER U WITH CIRCUMFLEX
            "\xFC" => "\xDC", # LATIN LETTER U WITH DIAERESIS
            "\xFD" => "\xDD", # LATIN LETTER U WITH HORN
        );
    }

    # upper case first with parameter
    sub Char::Ebig5plus::ucfirst(@) {
        if (@_) {
            my $s = shift @_;
            if (@_ and wantarray) {
                return Char::Ebig5plus::uc(CORE::substr($s,0,1)) . CORE::substr($s,1), @_;
            }
            else {
                return Char::Ebig5plus::uc(CORE::substr($s,0,1)) . CORE::substr($s,1);
            }
        }
        else {
            return Char::Ebig5plus::uc(CORE::substr($_,0,1)) . CORE::substr($_,1);
        }
    }

    # upper case first without parameter
    sub Char::Ebig5plus::ucfirst_() {
        return Char::Ebig5plus::uc(CORE::substr($_,0,1)) . CORE::substr($_,1);
    }

    # upper case with parameter
    sub Char::Ebig5plus::uc(@) {
        if (@_) {
            my $s = shift @_;
            if (@_ and wantarray) {
                return join('', map {defined($uc{$_}) ? $uc{$_} : $_} ($s =~ m/\G ($q_char) /oxmsg)), @_;
            }
            else {
                return join('', map {defined($uc{$_}) ? $uc{$_} : $_} ($s =~ m/\G ($q_char) /oxmsg));
            }
        }
        else {
            return Char::Ebig5plus::uc_();
        }
    }

    # upper case without parameter
    sub Char::Ebig5plus::uc_() {
        my $s = $_;
        return join '', map {defined($uc{$_}) ? $uc{$_} : $_} ($s =~ m/\G ($q_char) /oxmsg);
    }
}

#
# Big5Plus regexp capture
#
{
    # 10.3. Creating Persistent Private Variables
    # in Chapter 10. Subroutines
    # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.

    my $last_s_matched = 0;

    sub Char::Ebig5plus::capture($) {
        if ($last_s_matched and ($_[0] =~ m/\A [1-9][0-9]* \z/oxms)) {
            return $_[0] + 1;
        }
        return $_[0];
    }

    # Big5Plus regexp mark last m// or qr// matched
    sub Char::Ebig5plus::m_matched() {
        $last_s_matched = 0;
    }

    # Big5Plus regexp mark last s/// or qr matched
    sub Char::Ebig5plus::s_matched() {
        $last_s_matched = 1;
    }

    # which matched of m// or s/// at last

    # P.854 31.17. use re
    # in Chapter 31. Pragmatic Modules
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    @Char::Ebig5plus::m_matched = (qr/(?{Char::Ebig5plus::m_matched})/);
    @Char::Ebig5plus::s_matched = (qr/(?{Char::Ebig5plus::s_matched})/);
}

#
# Big5Plus regexp ignore case modifier
#
sub Char::Ebig5plus::ignorecase(@) {

    my @string = @_;
    my $metachar = qr/[\@\\|[\]{]/oxms;

    # ignore case of $scalar or @array
    for my $string (@string) {

        # split regexp
        my @char = $string =~ m{\G(
            \[\^ |
                \\? (?:$q_char)
        )}oxmsg;

        # unescape character
        for (my $i=0; $i <= $#char; $i++) {
            next if not defined $char[$i];

            # open character class [...]
            if ($char[$i] eq '[') {
                my $left = $i;

                # [] make die "unmatched [] in regexp ..."

                if ($char[$i+1] eq ']') {
                    $i++;
                }

                while (1) {
                    if (++$i > $#char) {
                        croak "$0: unmatched [] in regexp";
                    }
                    if ($char[$i] eq ']') {
                        my $right = $i;
                        my @charlist = charlist_qr(@char[$left+1..$right-1], 'i');

                        # escape character
                        for my $char (@charlist) {

                            # do not use quotemeta here
                            if ($char =~ m/\A ([\x80-\xFF].*) ($metachar) \z/oxms) {
                                $char = $1 . '\\' . $2;
                            }
                            elsif ($char =~ m/\A [.|)] \z/oxms) {
                                $char = $1 . '\\' . $char;
                            }
                        }

                        # [...]
                        splice @char, $left, $right-$left+1, '(?:' . join('|', @charlist) . ')';

                        $i = $left;
                        last;
                    }
                }
            }

            # open character class [^...]
            elsif ($char[$i] eq '[^') {
                my $left = $i;

                # [^] make die "unmatched [] in regexp ..."

                if ($char[$i+1] eq ']') {
                    $i++;
                }

                while (1) {
                    if (++$i > $#char) {
                        croak "$0: unmatched [] in regexp";
                    }
                    if ($char[$i] eq ']') {
                        my $right = $i;
                        my @charlist = charlist_not_qr(@char[$left+1..$right-1], 'i');

                        # escape character
                        for my $char (@charlist) {

                            # do not use quotemeta here
                            if ($char =~ m/\A ([\x80-\xFF].*) ($metachar) \z/oxms) {
                                $char = $1 . '\\' . $2;
                            }
                            elsif ($char =~ m/\A [.|)] \z/oxms) {
                                $char = '\\' . $char;
                            }
                        }

                        # [^...]
                        splice @char, $left, $right-$left+1, '(?!' . join('|', @charlist) . ")(?:$your_char)";

                        $i = $left;
                        last;
                    }
                }
            }

            # rewrite classic character class or escape character
            elsif (my $char = classic_character_class($char[$i])) {
                $char[$i] = $char;
            }

            # /i modifier
            elsif ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) {
                my $uc = Char::Ebig5plus::uc($char[$i]);
                my $lc = Char::Ebig5plus::lc($char[$i]);
                if ($uc ne $lc) {
                    $char[$i] = '[' . $uc . $lc . ']';
                }
            }
        }

        # characterize
        for (my $i=0; $i <= $#char; $i++) {
            next if not defined $char[$i];

            # escape last octet of multiple octet
            if ($char[$i] =~ m/\A ([\x80-\xFF].*) ($metachar) \z/oxms) {
                $char[$i] = $1 . '\\' . $2;
            }

            # quote character before ? + * {
            elsif (($i >= 1) and ($char[$i] =~ m/\A [\?\+\*\{] \z/oxms)) {
                if ($char[$i-1] !~ m/\A [\x00-\xFF] \z/oxms) {
                    $char[$i-1] = '(?:' . $char[$i-1] . ')';
                }
            }
        }

        $string = join '', @char;
    }

    # make regexp string
    return @string;
}

#
# classic character class ( \D \S \W \d \s \w \C \X \H \V \h \v \R \N \b \B )
#
sub classic_character_class($) {
    my($char) = @_;

    return {
        '\D' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE0-9])',
        '\S' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x0A\x0C\x0D\x20])',
        '\W' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE0-9A-Z_a-z])',
        '\d' => '[0-9]',
                 # \t  \n  \f  \r space
        '\s' => '[\x09\x0A\x0C\x0D\x20]',
        '\w' => '[0-9A-Z_a-z]',
        '\C' => '[\x00-\xFF]',
        '\X' => 'X',

        # \h \v \H \V
        #
        # P.114 Character Class Shortcuts
        # in Chapter 7: In the World of Regular Expressions
        # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition

        '\H' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x20])',
        '\V' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x0C\x0A\x0D])',
        '\h' => '[\x09\x20]',
        '\v' => '[\x0C\x0A\x0D]',
        '\R' => '(?:\x0D\x0A|[\x0A\x0D])',

        # \N
        #
        # http://perldoc.perl.org/perlre.html
        # Character Classes and other Special Escapes
        # Any character but \n (experimental). Not affected by /s modifier

        '\N' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x0A])',

        # \b \B
        #
        # P.131 Word boundaries: \b, \B, \<, \>, ...
        # in Chapter 3: Overview of Regular Expression Features and Flavors
        # of ISBN 0-596-00289-0 Mastering Regular Expressions, Second edition

        # '\b' => '(?:(?<=\A|\W)(?=\w)|(?<=\w)(?=\W|\z))',
        '\b' => '(?:\A(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[0-9A-Z_a-z])|(?<=[0-9A-Z_a-z])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]|\z))',

        # '\B' => '(?:(?<=\w)(?=\w)|(?<=\W)(?=\W))',
        '\B' => '(?:(?<=[0-9A-Z_a-z])(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]))',

    }->{$char} || '';
}

#
# prepare Big5Plus characters per length
#

# 1 octet characters
my @chars1 = ();
sub chars1 {
    if (@chars1) {
        return @chars1;
    }
    if (exists $range_tr{1}) {
        my @ranges = @{ $range_tr{1} };
        while (my @range = splice(@ranges,0,1)) {
            for my $oct0 (@{$range[0]}) {
                push @chars1, pack 'C', $oct0;
            }
        }
    }
    return @chars1;
}

# 2 octets characters
my @chars2 = ();
sub chars2 {
    if (@chars2) {
        return @chars2;
    }
    if (exists $range_tr{2}) {
        my @ranges = @{ $range_tr{2} };
        while (my @range = splice(@ranges,0,2)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    push @chars2, pack 'CC', $oct0,$oct1;
                }
            }
        }
    }
    return @chars2;
}

# 3 octets characters
my @chars3 = ();
sub chars3 {
    if (@chars3) {
        return @chars3;
    }
    if (exists $range_tr{3}) {
        my @ranges = @{ $range_tr{3} };
        while (my @range = splice(@ranges,0,3)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    for my $oct2 (@{$range[2]}) {
                        push @chars3, pack 'CCC', $oct0,$oct1,$oct2;
                    }
                }
            }
        }
    }
    return @chars3;
}

# 4 octets characters
my @chars4 = ();
sub chars4 {
    if (@chars4) {
        return @chars4;
    }
    if (exists $range_tr{4}) {
        my @ranges = @{ $range_tr{4} };
        while (my @range = splice(@ranges,0,4)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    for my $oct2 (@{$range[2]}) {
                        for my $oct3 (@{$range[3]}) {
                            push @chars4, pack 'CCCC', $oct0,$oct1,$oct2,$oct3;
                        }
                    }
                }
            }
        }
    }
    return @chars4;
}

# minimum value of each octet
my @minchar = ();
sub minchar {
    if (defined $minchar[$_[0]]) {
        return $minchar[$_[0]];
    }
    $minchar[$_[0]] = (&{(sub {}, \&chars1, \&chars2, \&chars3, \&chars4)[$_[0]]})[0];
}

# maximum value of each octet
my @maxchar = ();
sub maxchar {
    if (defined $maxchar[$_[0]]) {
        return $maxchar[$_[0]];
    }
    $maxchar[$_[0]] = (&{(sub {}, \&chars1, \&chars2, \&chars3, \&chars4)[$_[0]]})[-1];
}

#
# Big5Plus open character list for tr
#
sub _charlist_tr {

    local $_ = shift @_;

    # unescape character
    my @char = ();
    while (not m/\G \z/oxmsgc) {
        if (m/\G (\\0?55|\\x2[Dd]|\\-) /oxmsgc) {
            push @char, '\-';
        }
        elsif (m/\G \\ ([0-7]{2,3}) /oxmsgc) {
            push @char, CORE::chr(oct $1);
        }
        elsif (m/\G \\x ([0-9A-Fa-f]{1,2}) /oxmsgc) {
            push @char, CORE::chr(hex $1);
        }
        elsif (m/\G \\c ([\x40-\x5F]) /oxmsgc) {
            push @char, CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif (m/\G (\\ [0nrtfbae]) /oxmsgc) {
            push @char, {
                '\0' => "\0",
                '\n' => "\n",
                '\r' => "\r",
                '\t' => "\t",
                '\f' => "\f",
                '\b' => "\x08", # \b means backspace in character class
                '\a' => "\a",
                '\e' => "\e",
            }->{$1};
        }
        elsif (m/\G \\ ($q_char) /oxmsgc) {
            push @char, $1;
        }
        elsif (m/\G ($q_char) /oxmsgc) {
            push @char, $1;
        }
    }

    # join separated multiple octet
    @char = join('',@char) =~ m/\G (\\-|$q_char) /oxmsg;

    # unescape '-'
    my @i = ();
    for my $i (0 .. $#char) {
        if ($char[$i] eq '\-') {
            $char[$i] = '-';
        }
        elsif ($char[$i] eq '-') {
            if ((0 < $i) and ($i < $#char)) {
                push @i, $i;
            }
        }
    }

    # open character list (reverse for splice)
    for my $i (CORE::reverse @i) {
        my @range = ();

        # range error
        if ((length($char[$i-1]) > length($char[$i+1])) or ($char[$i-1] gt $char[$i+1])) {
            croak "$0: invalid [] range \"\\x" . unpack('H*',$char[$i-1]) . '-\\x' . unpack('H*',$char[$i+1]) . '" in regexp';
        }

        # range of multiple octet code
        if (length($char[$i-1]) == 1) {
            if (length($char[$i+1]) == 1) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars1();
            }
            elsif (length($char[$i+1]) == 2) {
                push @range, grep {$char[$i-1] le $_}                           &chars1();
                push @range, grep {$_ le $char[$i+1]}                           &chars2();
            }
            elsif (length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           &chars1();
                push @range,                                                    &chars2();
                push @range, grep {$_ le $char[$i+1]}                           &chars3();
            }
            elsif (length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           &chars1();
                push @range,                                                    &chars2();
                push @range,                                                    &chars3();
                push @range, grep {$_ le $char[$i+1]}                           &chars4();
            }
        }
        elsif (length($char[$i-1]) == 2) {
            if (length($char[$i+1]) == 2) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars2();
            }
            elsif (length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           &chars2();
                push @range, grep {$_ le $char[$i+1]}                           &chars3();
            }
            elsif (length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           &chars2();
                push @range,                                                    &chars3();
                push @range, grep {$_ le $char[$i+1]}                           &chars4();
            }
        }
        elsif (length($char[$i-1]) == 3) {
            if (length($char[$i+1]) == 3) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars3();
            }
            elsif (length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           &chars3();
                push @range, grep {$_ le $char[$i+1]}                           &chars4();
            }
        }
        elsif (length($char[$i-1]) == 4) {
            if (length($char[$i+1]) == 4) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars4();
            }
        }

        splice @char, $i-1, 3, @range;
    }

    return @char;
}

#
# Big5Plus octet range
#
sub _octets {

    my $modifier = pop @_;
    my $length = shift;

    my($a) = unpack 'C', $_[0];
    my($z) = unpack 'C', $_[1];

    # single octet code
    if ($length == 1) {

        # single octet and ignore case
        if (((caller(1))[3] ne 'Char::Ebig5plus::_octets') and ($modifier =~ m/i/oxms)) {
            if ($a == $z) {
                return sprintf('(?i:\x%02X)',          $a);
            }
            elsif (($a+1) == $z) {
                return sprintf('(?i:[\x%02X\x%02X])',  $a, $z);
            }
            else {
                return sprintf('(?i:[\x%02X-\x%02X])', $a, $z);
            }
        }

        # not ignore case or one of multiple octet
        else {
            if ($a == $z) {
                return sprintf('\x%02X',          $a);
            }
            elsif (($a+1) == $z) {
                return sprintf('[\x%02X\x%02X]',  $a, $z);
            }
            else {
                return sprintf('[\x%02X-\x%02X]', $a, $z);
            }
        }
    }

    # double octet code of Shift_JIS family
    elsif (($length == 2) and $is_shiftjis_family and ($a <= 0x9F) and (0xE0 <= $z)) {
        my(undef,$a2) = unpack 'CC', $_[0];
        my(undef,$z2) = unpack 'CC', $_[1];
        my $octets1;
        my $octets2;

        if ($a == 0x9F) {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]',                            0x9F,$a2);
        }
        elsif (($a+1) == 0x9F) {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]|\x%02X[\x00-\xFF]',          $a,  $a2,$a+1);
        }
        elsif (($a+2) == 0x9F) {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]|[\x%02X\x%02X][\x00-\xFF]',  $a,  $a2,$a+1,$a+2);
        }
        else {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]|[\x%02X-\x%02X][\x00-\xFF]', $a,  $a2,$a+1,$a+2);
        }

        if ($z == 0xE0) {
            $octets2 = sprintf('\x%02X[\x00-\x%02X]',                                      $z,$z2);
        }
        elsif (($z-1) == 0xE0) {
            $octets2 = sprintf('\x%02X[\x00-\xFF]|\x%02X[\x00-\x%02X]',               $z-1,$z,$z2);
        }
        elsif (($z-2) == 0xE0) {
            $octets2 = sprintf('[\x%02X\x%02X][\x00-\xFF]|\x%02X[\x00X-\x%02X]', $z-2,$z-1,$z,$z2);
        }
        else {
            $octets2 = sprintf('[\x%02X-\x%02X][\x00-\xFF]|\x%02X[\x00-\x%02X]', 0xE0,$z-1,$z,$z2);
        }

        return "(?:$octets1|$octets2)";
    }

    # double octet code of EUC-JP family
    elsif (($length == 2) and $is_eucjp_family and ($a == 0x8E) and (0xA1 <= $z)) {
        my(undef,$a2) = unpack 'CC', $_[0];
        my(undef,$z2) = unpack 'CC', $_[1];
        my $octets1;
        my $octets2;

        $octets1 = sprintf('\x%02X[\x%02X-\xFF]',                                0x8E,$a2);

        if ($z == 0xA1) {
            $octets2 = sprintf('\x%02X[\x00-\x%02X]',                                      $z,$z2);
        }
        elsif (($z-1) == 0xA1) {
            $octets2 = sprintf('\x%02X[\x00-\xFF]|\x%02X[\x00-\x%02X]',               $z-1,$z,$z2);
        }
        elsif (($z-2) == 0xA1) {
            $octets2 = sprintf('[\x%02X\x%02X][\x00-\xFF]|\x%02X[\x00X-\x%02X]', $z-2,$z-1,$z,$z2);
        }
        else {
            $octets2 = sprintf('[\x%02X-\x%02X][\x00-\xFF]|\x%02X[\x00-\x%02X]', 0xA1,$z-1,$z,$z2);
        }

        return "(?:$octets1|$octets2)";
    }

    # multiple octet code
    else {
        my(undef,$aa) = unpack 'Ca*', $_[0];
        my(undef,$zz) = unpack 'Ca*', $_[1];

        if ($a == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                $zz,                $modifier)),
            ) . ')';
        }
        elsif (($a+1) == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',              $z,    _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
        elsif (($a+2) == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',         $a+1,       _octets($length-1,&minchar($length-1),&maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',              $z,    _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
        elsif (($a+3) == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('[\x%02X\x%02X]%s', $a+1,$z-1,  _octets($length-1,&minchar($length-1),&maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',              $z,    _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
        else {
            return '(?:' . join('|',
                sprintf('\x%02X%s',          $a,        _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('[\x%02X-\x%02X]%s', $a+1,$z-1, _octets($length-1,&minchar($length-1),&maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',               $z,   _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
    }
}

#
# Big5Plus open character list for qr and not qr
#
sub _charlist {

    my $modifier = pop @_;
    my @char = @_;

    # unescape character
    for (my $i=0; $i <= $#char; $i++) {

        # escape - to ...
        if ($char[$i] eq '-') {
            if ((0 < $i) and ($i < $#char)) {
                $char[$i] = '...';
            }
        }

        # octal escape sequence
        elsif ($char[$i] =~ m/\A \\o \{ ([0-7]+) \} \z/oxms) {
            $char[$i] = octchr($1);
        }

        # hexadecimal escape sequence
        elsif ($char[$i] =~ m/\A \\x \{ ([0-9A-Fa-f]+) \} \z/oxms) {
            $char[$i] = hexchr($1);
        }

        # \N{CHARNAME} --> N{CHARNAME}
        elsif ($char[$i] =~ m/\A \\ ( N\{ ([^\x81-\xFE0-9\}][^\x81-\xFE\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        # \p{PROPERTY} --> p{PROPERTY}
        elsif ($char[$i] =~ m/\A \\ ( p\{ ([^\x81-\xFE0-9\}][^\x81-\xFE\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        # \P{PROPERTY} --> P{PROPERTY}
        elsif ($char[$i] =~ m/\A \\ ( P\{ ([^\x81-\xFE0-9\}][^\x81-\xFE\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        # \p, \P, \X --> p, P, X
        elsif ($char[$i] =~ m/\A \\ ( [pPX] ) \z/oxms) {
            $char[$i] = $1;
        }

        elsif ($char[$i] =~ m/\A \\ ([0-7]{2,3}) \z/oxms) {
            $char[$i] = CORE::chr oct $1;
        }
        elsif ($char[$i] =~ m/\A \\x ([0-9A-Fa-f]{1,2}) \z/oxms) {
            $char[$i] = CORE::chr hex $1;
        }
        elsif ($char[$i] =~ m/\A \\c ([\x40-\x5F]) \z/oxms) {
            $char[$i] = CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif ($char[$i] =~ m/\A (\\ [0nrtfbaedswDSWHVhvR]) \z/oxms) {
            $char[$i] = {
                '\0' => "\0",
                '\n' => "\n",
                '\r' => "\r",
                '\t' => "\t",
                '\f' => "\f",
                '\b' => "\x08", # \b means backspace in character class
                '\a' => "\a",
                '\e' => "\e",
                '\d' => '[0-9]',
                '\s' => '[\x09\x0A\x0C\x0D\x20]',
                '\w' => '[0-9A-Z_a-z]',
                '\D' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE0-9])',
                '\S' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x0A\x0C\x0D\x20])',
                '\W' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE0-9A-Z_a-z])',

                '\H' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x20])',
                '\V' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x0C\x0A\x0D])',
                '\h' => '[\x09\x20]',
                '\v' => '[\x0C\x0A\x0D]',
                '\R' => '(?:\x0D\x0A|[\x0A\x0D])',

            }->{$1};
        }

        # POSIX-style character classes
        elsif ($char[$i] =~ m/\A ( \[\: \^? (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] ) \z/oxms) {
            $char[$i] = {

                '[:alnum:]'   => '[\x30-\x39\x41-\x5A\x61-\x7A]',
                '[:alpha:]'   => '[\x41-\x5A\x61-\x7A]',
                '[:ascii:]'   => '[\x00-\x7F]',
                '[:blank:]'   => '[\x09\x20]',
                '[:cntrl:]'   => '[\x00-\x1F\x7F]',
                '[:digit:]'   => '[\x30-\x39]',
                '[:graph:]'   => '[\x21-\x7F]',
                '[:lower:]'   => '[\x61-\x7A]',
                '[:print:]'   => '[\x20-\x7F]',
                '[:punct:]'   => '[\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E]',
                '[:space:]'   => '[\x09\x0A\x0B\x0C\x0D\x20]',
                '[:upper:]'   => '[\x41-\x5A]',
                '[:word:]'    => '[\x30-\x39\x41-\x5A\x5F\x61-\x7A]',
                '[:xdigit:]'  => '[\x30-\x39\x41-\x46\x61-\x66]',

                '[:^alnum:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x30-\x39\x41-\x5A\x61-\x7A])',
                '[:^alpha:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x41-\x5A\x61-\x7A])',
                '[:^ascii:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x00-\x7F])',
                '[:^blank:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x20])',
                '[:^cntrl:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x00-\x1F\x7F])',
                '[:^digit:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x30-\x39])',
                '[:^graph:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x21-\x7F])',
                '[:^lower:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x61-\x7A])',
                '[:^print:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x20-\x7F])',
                '[:^punct:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E])',
                '[:^space:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x0A\x0B\x0C\x0D\x20])',
                '[:^upper:]'  => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x41-\x5A])',
                '[:^word:]'   => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x30-\x39\x41-\x5A\x5F\x61-\x7A])',
                '[:^xdigit:]' => '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x30-\x39\x41-\x46\x61-\x66])',

            }->{$1};
        }
        elsif ($char[$i] =~ m/\A \\ ($q_char) \z/oxms) {
            $char[$i] = $1;
        }
    }

    # open character list
    my @singleoctet = ();
    my @charlist    = ();
    for (my $i=0; $i <= $#char; ) {

        # escaped -
        if (defined($char[$i+1]) and ($char[$i+1] eq '...')) {
            $i += 1;
            next;
        }
        elsif ($char[$i] eq '...') {

            # range error
            if ((length($char[$i-1]) > length($char[$i+1])) or ($char[$i-1] gt $char[$i+1])) {
                croak "$0: invalid [] range \"\\x" . unpack('H*',$char[$i-1]) . '-\\x' . unpack('H*',$char[$i+1]) . '" in regexp';
            }

            # range of single octet code and not ignore case
            if ((length($char[$i-1]) == 1) and (length($char[$i+1]) == 1) and ($modifier !~ m/i/oxms)) {
                my $a = unpack 'C', $char[$i-1];
                my $z = unpack 'C', $char[$i+1];

                if ($a == $z) {
                    push @singleoctet, sprintf('\x%02X',        $a);
                }
                elsif (($a+1) == $z) {
                    push @singleoctet, sprintf('\x%02X\x%02X',  $a, $z);
                }
                else {
                    push @singleoctet, sprintf('\x%02X-\x%02X', $a, $z);
                }
            }

            # range of multiple octet code
            elsif (length($char[$i-1]) == length($char[$i+1])) {
                push @charlist, _octets(length($char[$i-1]), $char[$i-1], $char[$i+1], $modifier);
            }
            elsif (length($char[$i-1]) == 1) {
                if (length($char[$i+1]) == 2) {
                    push @charlist,
                        _octets(1, $char[$i-1], &maxchar(1), $modifier),
                        _octets(2, &minchar(2), $char[$i+1], $modifier);
                }
                elsif (length($char[$i+1]) == 3) {
                    push @charlist,
                        _octets(1, $char[$i-1], &maxchar(1), $modifier),
                        _octets(2, &minchar(2), &maxchar(2), $modifier),
                        _octets(3, &minchar(3), $char[$i+1], $modifier);
                }
                elsif (length($char[$i+1]) == 4) {
                    push @charlist,
                        _octets(1, $char[$i-1], &maxchar(1), $modifier),
                        _octets(2, &minchar(2), &maxchar(2), $modifier),
                        _octets(3, &minchar(3), &maxchar(3), $modifier),
                        _octets(4, &minchar(4), $char[$i+1], $modifier);
                }
            }
            elsif (length($char[$i-1]) == 2) {
                if (length($char[$i+1]) == 3) {
                    push @charlist,
                        _octets(2, $char[$i-1], &maxchar(2), $modifier),
                        _octets(3, &minchar(3), $char[$i+1], $modifier);
                }
                elsif (length($char[$i+1]) == 4) {
                    push @charlist,
                        _octets(2, $char[$i-1], &maxchar(2), $modifier),
                        _octets(3, &minchar(3), &maxchar(3), $modifier),
                        _octets(4, &minchar(4), $char[$i+1], $modifier);
                }
            }
            elsif (length($char[$i-1]) == 3) {
                if (length($char[$i+1]) == 4) {
                    push @charlist,
                        _octets(3, $char[$i-1], &maxchar(3), $modifier),
                        _octets(4, &minchar(4), $char[$i+1], $modifier);
                }
            }
            else {
                croak "$0: invalid [] range \"\\x" . unpack('H*',$char[$i-1]) . '-\\x' . unpack('H*',$char[$i+1]) . '" in regexp';
            }

            $i += 2;
        }

        # /i modifier
        elsif ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) {
            if ($modifier =~ m/i/oxms) {
                my $uc = Char::Ebig5plus::uc($char[$i]);
                my $lc = Char::Ebig5plus::lc($char[$i]);
                if ($uc ne $lc) {
                    push @singleoctet, $uc, $lc;
                }
                else {
                    push @singleoctet, $char[$i];
                }
            }
            else {
                push @singleoctet, $char[$i];
            }
            $i += 1;
        }

        # single character of single octet code

        # \h \v
        #
        # P.114 Character Class Shortcuts
        # in Chapter 7: In the World of Regular Expressions
        # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition

        elsif ($char[$i] =~ m/\A (?: \\h ) \z/oxms) {
            push @singleoctet, "\t", "\x20";
            $i += 1;
        }
        elsif ($char[$i] =~ m/\A (?: \\v ) \z/oxms) {
            push @singleoctet, "\f","\n","\r";
            $i += 1;
        }
        elsif ($char[$i] =~ m/\A (?: \\d | \\s | \\w ) \z/oxms) {
            push @singleoctet, $char[$i];
            $i += 1;
        }

        # single character of multiple octet code
        else {
            push @charlist, $char[$i];
            $i += 1;
        }
    }

    # quote metachar
    for (@singleoctet) {
        if (m/\A \n \z/oxms) {
            $_ = '\n';
        }
        elsif (m/\A \r \z/oxms) {
            $_ = '\r';
        }
        elsif (m/\A ([\x00-\x20\x7F-\xFF]) \z/oxms) {
            $_ = sprintf('\x%02X', CORE::ord $1);
        }
        elsif (m/\A [\x00-\xFF] \z/oxms) {
            $_ = quotemeta $_;
        }
    }
    for (@charlist) {
        if (m/\A ([\x81-\xFE]) ([\x00-\xFF]) \z/oxms) {
            $_ = $1 . quotemeta $2;
        }
    }

    # return character list
    return \@singleoctet, \@charlist;
}

#
# Big5Plus octal escape sequence
#
sub octchr {
    my($octdigit) = @_;

    my @binary = ();
    for my $octal (split(//,$octdigit)) {
        push @binary, {
            '0' => '000',
            '1' => '001',
            '2' => '010',
            '3' => '011',
            '4' => '100',
            '5' => '101',
            '6' => '110',
            '7' => '111',
        }->{$octal};
    }
    my $binary = join '', @binary;

    my $octchr = {
        #                1234567
        1 => pack('B*', "0000000$binary"),
        2 => pack('B*', "000000$binary"),
        3 => pack('B*', "00000$binary"),
        4 => pack('B*', "0000$binary"),
        5 => pack('B*', "000$binary"),
        6 => pack('B*', "00$binary"),
        7 => pack('B*', "0$binary"),
        0 => pack('B*', "$binary"),

    }->{CORE::length($binary) % 8};

    return $octchr;
}

#
# Big5Plus hexadecimal escape sequence
#
sub hexchr {
    my($hexdigit) = @_;

    my $hexchr = {
        1 => pack('H*', "0$hexdigit"),
        0 => pack('H*', "$hexdigit"),

    }->{CORE::length($_[0]) % 2};

    return $hexchr;
}

#
# Big5Plus open character list for qr
#
sub charlist_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $charlist) = _charlist(@char, $modifier);
    my @singleoctet = @$singleoctet;
    my @charlist    = @$charlist;

    # return character list
    if (scalar(@singleoctet) == 0) {
    }
    elsif (scalar(@singleoctet) >= 2) {
        push @charlist, '[' . join('',@singleoctet) . ']';
    }
    elsif ($singleoctet[0] =~ m/ . - . /oxms) {
        push @charlist, '[' . $singleoctet[0] . ']';
    }
    else {
        push @charlist, $singleoctet[0];
    }
    if (scalar(@charlist) >= 2) {
        return '(?:' . join('|', @charlist) . ')';
    }
    else {
        return $charlist[0];
    }
}

#
# Big5Plus open character list for not qr
#
sub charlist_not_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $charlist) = _charlist(@char, $modifier);
    my @singleoctet = @$singleoctet;
    my @charlist    = @$charlist;

    # return character list
    if (scalar(@charlist) >= 1) {
        if (scalar(@singleoctet) >= 1) {

            # any character other than multiple octet and single octet character class
            return '(?!' . join('|', @charlist) . ')(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE'. join('', @singleoctet) . '])';
        }
        else {

            # any character other than multiple octet character class
            return '(?!' . join('|', @charlist) . ")(?:$your_char)";
        }
    }
    else {
        if (scalar(@singleoctet) >= 1) {

            # any character other than single octet character class
            return                                 '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE'. join('', @singleoctet) . '])';
        }
        else {

            # any character
            return                                 "(?:$your_char)";
        }
    }
}

#
# Big5Plus order to character (with parameter)
#
sub Char::Ebig5plus::chr(;$) {

    my $c = @_ ? $_[0] : $_;

    if ($c == 0x00) {
        return "\x00";
    }
    else {
        my @chr = ();
        while ($c > 0) {
            unshift @chr, ($c % 0x100);
            $c = int($c / 0x100);
        }
        return pack 'C*', @chr;
    }
}

#
# Big5Plus order to character (without parameter)
#
sub Char::Ebig5plus::chr_() {

    my $c = $_;

    if ($c == 0x00) {
        return "\x00";
    }
    else {
        my @chr = ();
        while ($c > 0) {
            unshift @chr, ($c % 0x100);
            $c = int($c / 0x100);
        }
        return pack 'C*', @chr;
    }
}

#
# Big5Plus stacked file test expr
#
sub Char::Ebig5plus::filetest(@) {

    my $file     = pop @_;
    my $filetest = substr(pop @_, 1);

    unless (eval qq{Char::Ebig5plus::$filetest(\$file)}) {
        return '';
    }
    for my $filetest (reverse @_) {
        unless (eval qq{ $filetest _ }) {
            return '';
        }
    }
    return 1;
}

#
# Big5Plus file test -r expr
#
sub Char::Ebig5plus::r(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -r (Char::Ebig5plus::r)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-r _,@_) : -r _;
    }

    # P.908 32.39. Symbol
    # in Chapter 32: Standard Modules
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.
    # (and so on)

    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-r $fh,@_) : -r $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-r _,@_) : -r _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-r _,@_) : -r _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $r = -r $fh;
                close $fh;
                return wantarray ? ($r,@_) : $r;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -w expr
#
sub Char::Ebig5plus::w(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -w (Char::Ebig5plus::w)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-w _,@_) : -w _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-w $fh,@_) : -w $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-w _,@_) : -w _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-w _,@_) : -w _;
        }
        else {
            my $fh = gensym();
            if (open $fh, ">>$_") {
                my $w = -w $fh;
                close $fh;
                return wantarray ? ($w,@_) : $w;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -x expr
#
sub Char::Ebig5plus::x(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -x (Char::Ebig5plus::x)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-x _,@_) : -x _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-x $fh,@_) : -x $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-x _,@_) : -x _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-x _,@_) : -x _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $dummy_for_underline_cache = -x $fh;
                close $fh;
            }

            # filename is not .COM .EXE .BAT .CMD
            return wantarray ? ('',@_) : '';
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -o expr
#
sub Char::Ebig5plus::o(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -o (Char::Ebig5plus::o)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-o _,@_) : -o _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-o $fh,@_) : -o $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-o _,@_) : -o _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-o _,@_) : -o _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $o = -o $fh;
                close $fh;
                return wantarray ? ($o,@_) : $o;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -R expr
#
sub Char::Ebig5plus::R(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -R (Char::Ebig5plus::R)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-R _,@_) : -R _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-R $fh,@_) : -R $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-R _,@_) : -R _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-R _,@_) : -R _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $R = -R $fh;
                close $fh;
                return wantarray ? ($R,@_) : $R;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -W expr
#
sub Char::Ebig5plus::W(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -W (Char::Ebig5plus::W)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-W _,@_) : -W _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-W $fh,@_) : -W $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-W _,@_) : -W _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-W _,@_) : -W _;
        }
        else {
            my $fh = gensym();
            if (open $fh, ">>$_") {
                my $W = -W $fh;
                close $fh;
                return wantarray ? ($W,@_) : $W;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -X expr
#
sub Char::Ebig5plus::X(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -X (Char::Ebig5plus::X)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-X _,@_) : -X _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-X $fh,@_) : -X $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-X _,@_) : -X _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-X _,@_) : -X _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $dummy_for_underline_cache = -X $fh;
                close $fh;
            }

            # filename is not .COM .EXE .BAT .CMD
            return wantarray ? ('',@_) : '';
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -O expr
#
sub Char::Ebig5plus::O(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -O (Char::Ebig5plus::O)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-O _,@_) : -O _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-O $fh,@_) : -O $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-O _,@_) : -O _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-O _,@_) : -O _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $O = -O $fh;
                close $fh;
                return wantarray ? ($O,@_) : $O;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -e expr
#
sub Char::Ebig5plus::e(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -e (Char::Ebig5plus::e)' if @_ and not wantarray;

    local $^W = 0;

    my $fh = qualify_to_ref $_;
    if ($_ eq '_') {
        return wantarray ? (-e _,@_) : -e _;
    }

    # return false if directory handle
    elsif (defined Char::Ebig5plus::telldir($fh)) {
        return wantarray ? ('',@_) : '';
    }

    # return true if file handle
    elsif (fileno $fh) {
        return wantarray ? (1,@_) : 1;
    }

    elsif (-e $_) {
        return wantarray ? (1,@_) : 1;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (1,@_) : 1;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $e = -e $fh;
                close $fh;
                return wantarray ? ($e,@_) : $e;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -z expr
#
sub Char::Ebig5plus::z(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -z (Char::Ebig5plus::z)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-z _,@_) : -z _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-z $fh,@_) : -z $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-z _,@_) : -z _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-z _,@_) : -z _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $z = -z $fh;
                close $fh;
                return wantarray ? ($z,@_) : $z;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -s expr
#
sub Char::Ebig5plus::s(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -s (Char::Ebig5plus::s)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-s _,@_) : -s _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-s $fh,@_) : -s $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-s _,@_) : -s _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-s _,@_) : -s _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $s = -s $fh;
                close $fh;
                return wantarray ? ($s,@_) : $s;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -f expr
#
sub Char::Ebig5plus::f(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -f (Char::Ebig5plus::f)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-f _,@_) : -f _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-f $fh,@_) : -f $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-f _,@_) : -f _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? ('',@_) : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $f = -f $fh;
                close $fh;
                return wantarray ? ($f,@_) : $f;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -d expr
#
sub Char::Ebig5plus::d(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -d (Char::Ebig5plus::d)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-d _,@_) : -d _;
    }

    # return false if file handle or directory handle
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? ('',@_) : '';
    }
    elsif (-e $_) {
        return wantarray ? (-d _,@_) : -d _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        return wantarray ? (-d "$_/.",@_) : -d "$_/.";
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -l expr
#
sub Char::Ebig5plus::l(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -l (Char::Ebig5plus::l)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-l _,@_) : -l _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-l $fh,@_) : -l $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-l _,@_) : -l _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-l _,@_) : -l _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $l = -l $fh;
                close $fh;
                return wantarray ? ($l,@_) : $l;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -p expr
#
sub Char::Ebig5plus::p(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -p (Char::Ebig5plus::p)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-p _,@_) : -p _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-p $fh,@_) : -p $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-p _,@_) : -p _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-p _,@_) : -p _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $p = -p $fh;
                close $fh;
                return wantarray ? ($p,@_) : $p;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -S expr
#
sub Char::Ebig5plus::S(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -S (Char::Ebig5plus::S)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-S _,@_) : -S _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-S $fh,@_) : -S $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-S _,@_) : -S _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-S _,@_) : -S _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $S = -S $fh;
                close $fh;
                return wantarray ? ($S,@_) : $S;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -b expr
#
sub Char::Ebig5plus::b(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -b (Char::Ebig5plus::b)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-b _,@_) : -b _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-b $fh,@_) : -b $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-b _,@_) : -b _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-b _,@_) : -b _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $b = -b $fh;
                close $fh;
                return wantarray ? ($b,@_) : $b;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -c expr
#
sub Char::Ebig5plus::c(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -c (Char::Ebig5plus::c)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-c _,@_) : -c _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-c $fh,@_) : -c $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-c _,@_) : -c _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-c _,@_) : -c _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $c = -c $fh;
                close $fh;
                return wantarray ? ($c,@_) : $c;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -u expr
#
sub Char::Ebig5plus::u(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -u (Char::Ebig5plus::u)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-u _,@_) : -u _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-u $fh,@_) : -u $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-u _,@_) : -u _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-u _,@_) : -u _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $u = -u $fh;
                close $fh;
                return wantarray ? ($u,@_) : $u;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -g expr
#
sub Char::Ebig5plus::g(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -g (Char::Ebig5plus::g)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-g _,@_) : -g _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-g $fh,@_) : -g $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-g _,@_) : -g _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-g _,@_) : -g _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $g = -g $fh;
                close $fh;
                return wantarray ? ($g,@_) : $g;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -k expr
#
sub Char::Ebig5plus::k(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -k (Char::Ebig5plus::k)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? ('',@_) : '';
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? ('',@_) : '';
    }
    elsif ($] =~ m/^5\.008/oxms) {
        return wantarray ? ('',@_) : '';
    }
    return wantarray ? ($_,@_) : $_;
}

#
# Big5Plus file test -T expr
#
sub Char::Ebig5plus::T(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -T (Char::Ebig5plus::T)' if @_ and not wantarray;
    my $T = 1;

    my $fh = qualify_to_ref $_;
    if (fileno $fh) {

        if (defined Char::Ebig5plus::telldir($fh)) {
            return wantarray ? (undef,@_) : undef;
        }

        # P.813 29.2.176. tell
        # in Chapter 29: Functions
        # of ISBN 0-596-00027-8 Programming Perl Third Edition.
        # (and so on)

        my $systell = sysseek $fh, 0, 1;

        if (sysread $fh, my $block, 512) {

            # P.163 Binary file check in Little Perl Parlor 16
            # of Book No. T1008901080816 ZASSHI 08901-8 UNIX MAGAZINE 1993 Aug VOL8#8
            # (and so on)

            if ($block =~ /[\000\377]/oxms) {
                $T = '';
            }
            elsif (($block =~ tr/\000-\007\013\016-\032\034-\037\377//) * 10 > CORE::length $block) {
                $T = '';
            }
        }

        # 0 byte or eof
        else {
            $T = 1;
        }

        sysseek $fh, $systell, 0;
    }
    else {
        if (-d $_ or -d "$_/.") {
            return wantarray ? (undef,@_) : undef;
        }

        $fh = gensym();
        unless (open $fh, $_) {
            return wantarray ? (undef,@_) : undef;
        }
        if (sysread $fh, my $block, 512) {
            if ($block =~ /[\000\377]/oxms) {
                $T = '';
            }
            elsif (($block =~ tr/\000-\007\013\016-\032\034-\037\377//) * 10 > CORE::length $block) {
                $T = '';
            }
        }

        # 0 byte or eof
        else {
            $T = 1;
        }
        close $fh;
    }

    my $dummy_for_underline_cache = -T $_;
    return wantarray ? ($T,@_) : $T;
}

#
# Big5Plus file test -B expr
#
sub Char::Ebig5plus::B(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -B (Char::Ebig5plus::B)' if @_ and not wantarray;
    my $B = '';

    my $fh = qualify_to_ref $_;
    if (fileno $fh) {

        if (defined Char::Ebig5plus::telldir($fh)) {
            return wantarray ? (undef,@_) : undef;
        }

        my $systell = sysseek $fh, 0, 1;

        if (sysread $fh, my $block, 512) {
            if ($block =~ /[\000\377]/oxms) {
                $B = 1;
            }
            elsif (($block =~ tr/\000-\007\013\016-\032\034-\037\377//) * 10 > CORE::length $block) {
                $B = 1;
            }
        }

        # 0 byte or eof
        else {
            $B = 1;
        }

        sysseek $fh, $systell, 0;
    }
    else {
        if (-d $_ or -d "$_/.") {
            return wantarray ? (undef,@_) : undef;
        }

        $fh = gensym();
        unless (open $fh, $_) {
            return wantarray ? (undef,@_) : undef;
        }
        if (sysread $fh, my $block, 512) {
            if ($block =~ /[\000\377]/oxms) {
                $B = 1;
            }
            elsif (($block =~ tr/\000-\007\013\016-\032\034-\037\377//) * 10 > CORE::length $block) {
                $B = 1;
            }
        }

        # 0 byte or eof
        else {
            $B = 1;
        }
        close $fh;
    }

    my $dummy_for_underline_cache = -B $_;
    return wantarray ? ($B,@_) : $B;
}

#
# Big5Plus file test -M expr
#
sub Char::Ebig5plus::M(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -M (Char::Ebig5plus::M)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-M _,@_) : -M _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-M $fh,@_) : -M $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-M _,@_) : -M _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-M _,@_) : -M _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = CORE::stat $fh;
                close $fh;
                my $M = ($^T - $mtime) / (24*60*60);
                return wantarray ? ($M,@_) : $M;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -A expr
#
sub Char::Ebig5plus::A(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -A (Char::Ebig5plus::A)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-A _,@_) : -A _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-A $fh,@_) : -A $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-A _,@_) : -A _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-A _,@_) : -A _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = CORE::stat $fh;
                close $fh;
                my $A = ($^T - $atime) / (24*60*60);
                return wantarray ? ($A,@_) : $A;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus file test -C expr
#
sub Char::Ebig5plus::C(;*@) {

    local $_ = shift if @_;
    croak 'Too many arguments for -C (Char::Ebig5plus::C)' if @_ and not wantarray;

    if ($_ eq '_') {
        return wantarray ? (-C _,@_) : -C _;
    }
    elsif (fileno(my $fh = qualify_to_ref $_)) {
        return wantarray ? (-C $fh,@_) : -C $fh;
    }
    elsif (-e $_) {
        return wantarray ? (-C _,@_) : -C _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return wantarray ? (-C _,@_) : -C _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = CORE::stat $fh;
                close $fh;
                my $C = ($^T - $ctime) / (24*60*60);
                return wantarray ? ($C,@_) : $C;
            }
        }
    }
    return wantarray ? (undef,@_) : undef;
}

#
# Big5Plus stacked file test $_
#
sub Char::Ebig5plus::filetest_(@) {

    my $filetest = substr(pop @_, 1);

    unless (eval qq{Char::Ebig5plus::${filetest}_}) {
        return '';
    }
    for my $filetest (reverse @_) {
        unless (eval qq{ $filetest _ }) {
            return '';
        }
    }
    return 1;
}

#
# Big5Plus file test -r $_
#
sub Char::Ebig5plus::r_() {

    if (-e $_) {
        return -r _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -r _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $r = -r $fh;
                close $fh;
                return $r ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -w $_
#
sub Char::Ebig5plus::w_() {

    if (-e $_) {
        return -w _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -w _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, ">>$_") {
                my $w = -w $fh;
                close $fh;
                return $w ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -x $_
#
sub Char::Ebig5plus::x_() {

    if (-e $_) {
        return -x _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -x _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $dummy_for_underline_cache = -x $fh;
                close $fh;
            }

            # filename is not .COM .EXE .BAT .CMD
            return '';
        }
    }
    return;
}

#
# Big5Plus file test -o $_
#
sub Char::Ebig5plus::o_() {

    if (-e $_) {
        return -o _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -o _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $o = -o $fh;
                close $fh;
                return $o ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -R $_
#
sub Char::Ebig5plus::R_() {

    if (-e $_) {
        return -R _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -R _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $R = -R $fh;
                close $fh;
                return $R ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -W $_
#
sub Char::Ebig5plus::W_() {

    if (-e $_) {
        return -W _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -W _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, ">>$_") {
                my $W = -W $fh;
                close $fh;
                return $W ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -X $_
#
sub Char::Ebig5plus::X_() {

    if (-e $_) {
        return -X _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -X _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $dummy_for_underline_cache = -X $fh;
                close $fh;
            }

            # filename is not .COM .EXE .BAT .CMD
            return '';
        }
    }
    return;
}

#
# Big5Plus file test -O $_
#
sub Char::Ebig5plus::O_() {

    if (-e $_) {
        return -O _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -O _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $O = -O $fh;
                close $fh;
                return $O ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -e $_
#
sub Char::Ebig5plus::e_() {

    if (-e $_) {
        return 1;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return 1;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $e = -e $fh;
                close $fh;
                return $e ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -z $_
#
sub Char::Ebig5plus::z_() {

    if (-e $_) {
        return -z _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -z _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $z = -z $fh;
                close $fh;
                return $z ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -s $_
#
sub Char::Ebig5plus::s_() {

    if (-e $_) {
        return -s _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -s _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $s = -s $fh;
                close $fh;
                return $s;
            }
        }
    }
    return;
}

#
# Big5Plus file test -f $_
#
sub Char::Ebig5plus::f_() {

    if (-e $_) {
        return -f _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $f = -f $fh;
                close $fh;
                return $f ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -d $_
#
sub Char::Ebig5plus::d_() {

    if (-e $_) {
        return -d _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        return -d "$_/." ? 1 : '';
    }
    return;
}

#
# Big5Plus file test -l $_
#
sub Char::Ebig5plus::l_() {

    if (-e $_) {
        return -l _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -l _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $l = -l $fh;
                close $fh;
                return $l ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -p $_
#
sub Char::Ebig5plus::p_() {

    if (-e $_) {
        return -p _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -p _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $p = -p $fh;
                close $fh;
                return $p ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -S $_
#
sub Char::Ebig5plus::S_() {

    if (-e $_) {
        return -S _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -S _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $S = -S $fh;
                close $fh;
                return $S ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -b $_
#
sub Char::Ebig5plus::b_() {

    if (-e $_) {
        return -b _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -b _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $b = -b $fh;
                close $fh;
                return $b ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -c $_
#
sub Char::Ebig5plus::c_() {

    if (-e $_) {
        return -c _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -c _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $c = -c $fh;
                close $fh;
                return $c ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -u $_
#
sub Char::Ebig5plus::u_() {

    if (-e $_) {
        return -u _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -u _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $u = -u $fh;
                close $fh;
                return $u ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -g $_
#
sub Char::Ebig5plus::g_() {

    if (-e $_) {
        return -g _ ? 1 : '';
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -g _ ? 1 : '';
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my $g = -g $fh;
                close $fh;
                return $g ? 1 : '';
            }
        }
    }
    return;
}

#
# Big5Plus file test -k $_
#
sub Char::Ebig5plus::k_() {

    if ($] =~ m/^5\.008/oxms) {
        return wantarray ? ('',@_) : '';
    }
    return wantarray ? ($_,@_) : $_;
}

#
# Big5Plus file test -T $_
#
sub Char::Ebig5plus::T_() {

    my $T = 1;

    if (-d $_ or -d "$_/.") {
        return;
    }
    my $fh = gensym();
    unless (open $fh, $_) {
        return;
    }

    if (sysread $fh, my $block, 512) {
        if ($block =~ /[\000\377]/oxms) {
            $T = '';
        }
        elsif (($block =~ tr/\000-\007\013\016-\032\034-\037\377//) * 10 > CORE::length $block) {
            $T = '';
        }
    }

    # 0 byte or eof
    else {
        $T = 1;
    }
    close $fh;

    my $dummy_for_underline_cache = -T $_;
    return $T;
}

#
# Big5Plus file test -B $_
#
sub Char::Ebig5plus::B_() {

    my $B = '';

    if (-d $_ or -d "$_/.") {
        return;
    }
    my $fh = gensym();
    unless (open $fh, $_) {
        return;
    }

    if (sysread $fh, my $block, 512) {
        if ($block =~ /[\000\377]/oxms) {
            $B = 1;
        }
        elsif (($block =~ tr/\000-\007\013\016-\032\034-\037\377//) * 10 > CORE::length $block) {
            $B = 1;
        }
    }

    # 0 byte or eof
    else {
        $B = 1;
    }
    close $fh;

    my $dummy_for_underline_cache = -B $_;
    return $B;
}

#
# Big5Plus file test -M $_
#
sub Char::Ebig5plus::M_() {

    if (-e $_) {
        return -M _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -M _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = CORE::stat $fh;
                close $fh;
                my $M = ($^T - $mtime) / (24*60*60);
                return $M;
            }
        }
    }
    return;
}

#
# Big5Plus file test -A $_
#
sub Char::Ebig5plus::A_() {

    if (-e $_) {
        return -A _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -A _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = CORE::stat $fh;
                close $fh;
                my $A = ($^T - $atime) / (24*60*60);
                return $A;
            }
        }
    }
    return;
}

#
# Big5Plus file test -C $_
#
sub Char::Ebig5plus::C_() {

    if (-e $_) {
        return -C _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        if (-d "$_/.") {
            return -C _;
        }
        else {
            my $fh = gensym();
            if (open $fh, $_) {
                my($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = CORE::stat $fh;
                close $fh;
                my $C = ($^T - $ctime) / (24*60*60);
                return $C;
            }
        }
    }
    return;
}

#
# Big5Plus path globbing (with parameter)
#
sub Char::Ebig5plus::glob($) {

    return _dosglob(@_);
}

#
# Big5Plus path globbing (without parameter)
#
sub Char::Ebig5plus::glob_() {

    return _dosglob();
}

#
# Big5Plus path globbing from File::DosGlob module
#
my %iter;
my %entries;
sub _dosglob {

    # context (keyed by second cxix argument provided by core)
    my($expr,$cxix) = @_;

    # glob without args defaults to $_
    $expr = $_ if not defined $expr;

    # represents the current user's home directory
    #
    # 7.3. Expanding Tildes in Filenames
    # in Chapter 7. File Access
    # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
    #
    # and File::HomeDir, File::HomeDir::Windows module

    # DOS-like system
    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        $expr =~ s{ \A ~ (?= [^/\\] ) }
                  { $ENV{'HOME'} || $ENV{'USERPROFILE'} || "$ENV{'HOMEDRIVE'}$ENV{'HOMEPATH'}" }oxmse;
    }

    # UNIX-like system
    else {
        $expr =~ s{ \A ~ ( (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE/])* ) }
                  { $1 ? (getpwnam($1))[7] : ($ENV{'HOME'} || $ENV{'LOGDIR'} || (getpwuid($<))[7]) }oxmse;
    }

    # assume global context if not provided one
    $cxix = '_G_' if not defined $cxix;
    $iter{$cxix} = 0 if not exists $iter{$cxix};

    # if we're just beginning, do it all first
    if ($iter{$cxix} == 0) {
            $entries{$cxix} = [ _do_glob(1, _parse_line($expr)) ];
    }

    # chuck it all out, quick or slow
    if (wantarray) {
        delete $iter{$cxix};
        return @{delete $entries{$cxix}};
    }
    else {
        if ($iter{$cxix} = scalar @{$entries{$cxix}}) {
            return shift @{$entries{$cxix}};
        }
        else {
            # return undef for EOL
            delete $iter{$cxix};
            delete $entries{$cxix};
            return undef;
        }
    }
}

#
# Big5Plus path globbing subroutine
#
sub _do_glob {

    my($cond,@expr) = @_;
    my @glob = ();
    my $fix_drive_relative_paths = 0;

OUTER:
    for my $expr (@expr) {
        next OUTER if not defined $expr;
        next OUTER if $expr eq '';

        my @matched = ();
        my @globdir = ();
        my $head    = '.';
        my $pathsep = '/';
        my $tail;

        # if argument is within quotes strip em and do no globbing
        if ($expr =~ m/\A " ((?:$q_char)*) " \z/oxms) {
            $expr = $1;
            if ($cond eq 'd') {
                if (Char::Ebig5plus::d $expr) {
                    push @glob, $expr;
                }
            }
            else {
                if (Char::Ebig5plus::e $expr) {
                    push @glob, $expr;
                }
            }
            next OUTER;
        }

        # wildcards with a drive prefix such as h:*.pm must be changed
        # to h:./*.pm to expand correctly
        if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
            if ($expr =~ s# \A ((?:[A-Za-z]:)?) ([\x81-\xFE][\x00-\xFF]|[^\x81-\xFE/\\]) #$1./$2#oxms) {
                $fix_drive_relative_paths = 1;
            }
        }

        if (($head, $tail) = _parse_path($expr,$pathsep)) {
            if ($tail eq '') {
                push @glob, $expr;
                next OUTER;
            }
            if ($head =~ m/ \A (?:$q_char)*? [*?] /oxms) {
                if (@globdir = _do_glob('d', $head)) {
                    push @glob, _do_glob($cond, map {"$_$pathsep$tail"} @globdir);
                    next OUTER;
                }
            }
            if ($head eq '' or $head =~ m/\A [A-Za-z]: \z/oxms) {
                $head .= $pathsep;
            }
            $expr = $tail;
        }

        # If file component has no wildcards, we can avoid opendir
        if ($expr !~ m/ \A (?:$q_char)*? [*?] /oxms) {
            if ($head eq '.') {
                $head = '';
            }
            if ($head ne '' and ($head =~ m/ \G ($q_char) /oxmsg)[-1] ne $pathsep) {
                $head .= $pathsep;
            }
            $head .= $expr;
            if ($cond eq 'd') {
                if (Char::Ebig5plus::d $head) {
                    push @glob, $head;
                }
            }
            else {
                if (Char::Ebig5plus::e $head) {
                    push @glob, $head;
                }
            }
            next OUTER;
        }
        Char::Ebig5plus::opendir(*DIR, $head) or next OUTER;
        my @leaf = readdir DIR;
        closedir DIR;

        if ($head eq '.') {
            $head = '';
        }
        if ($head ne '' and ($head =~ m/ \G ($q_char) /oxmsg)[-1] ne $pathsep) {
            $head .= $pathsep;
        }

        my $pattern = '';
        while ($expr =~ m/ \G ($q_char) /oxgc) {
            my $char = $1;
            if ($char eq '*') {
                $pattern .= "(?:$your_char)*",
            }
            elsif ($char eq '?') {
                $pattern .= "(?:$your_char)?",  # DOS style
#               $pattern .= "(?:$your_char)",   # UNIX style
            }
            elsif ((my $uc = Char::Ebig5plus::uc($char)) ne $char) {
                $pattern .= $uc;
            }
            else {
                $pattern .= quotemeta $char;
            }
        }
        my $matchsub = sub { Char::Ebig5plus::uc($_[0]) =~ m{\A $pattern \z}xms };

#       if ($@) {
#           print STDERR "$0: $@\n";
#           next OUTER;
#       }

INNER:
        for my $leaf (@leaf) {
            if ($leaf eq '.' or $leaf eq '..') {
                next INNER;
            }
            if ($cond eq 'd' and not Char::Ebig5plus::d "$head$leaf") {
                next INNER;
            }

            if (&$matchsub($leaf)) {
                push @matched, "$head$leaf";
                next INNER;
            }

            # [DOS compatibility special case]
            # Failed, add a trailing dot and try again, but only...

            if (Char::Ebig5plus::index($leaf,'.') == -1 and   # if name does not have a dot in it *and*
                CORE::length($leaf) <= 8 and        # name is shorter than or equal to 8 chars *and*
                Char::Ebig5plus::index($pattern,'\\.') != -1  # pattern has a dot.
            ) {
                if (&$matchsub("$leaf.")) {
                    push @matched, "$head$leaf";
                    next INNER;
                }
            }
        }
        if (@matched) {
            push @glob, @matched;
        }
    }
    if ($fix_drive_relative_paths) {
        for my $glob (@glob) {
            $glob =~ s# \A ([A-Za-z]:) \./ #$1#oxms;
        }
    }
    return @glob;
}

#
# Big5Plus parse line
#
sub _parse_line {

    my($line) = @_;

    $line .= ' ';
    my @piece = ();
    while ($line =~ m{
        " ( (?: [\x81-\xFE][\x00-\xFF]|[^\x81-\xFE"]   )*  ) " \s+ |
          ( (?: [\x81-\xFE][\x00-\xFF]|[^\x81-\xFE"\s] )*  )   \s+
        }oxmsg
    ) {
        push @piece, defined($1) ? $1 : $2;
    }
    return @piece;
}

#
# Big5Plus parse path
#
sub _parse_path {

    my($path,$pathsep) = @_;

    $path .= '/';
    my @subpath = ();
    while ($path =~ m{
        ((?: [\x81-\xFE][\x00-\xFF]|[^\x81-\xFE/\\] )+?) [/\\] }oxmsg
    ) {
        push @subpath, $1;
    }

    my $tail = pop @subpath;
    my $head = join $pathsep, @subpath;
    return $head, $tail;
}

#
# Big5Plus file lstat (with parameter)
#
sub Char::Ebig5plus::lstat(*) {

    local $_ = shift if @_;

    if (-e $_) {
        return CORE::lstat _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        my $fh = gensym();
        if (open $fh, $_) {
            my @lstat = CORE::stat $fh; # not CORE::lstat
            close $fh;
            return @lstat;
        }
    }
    return;
}

#
# Big5Plus file lstat (without parameter)
#
sub Char::Ebig5plus::lstat_() {

    if (-e $_) {
        return CORE::lstat _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        my $fh = gensym();
        if (open $fh, $_) {
            my @lstat = CORE::stat $fh; # not CORE::lstat
            close $fh;
            return @lstat;
        }
    }
    return;
}

#
# Big5Plus path opendir
#
sub Char::Ebig5plus::opendir(*$) {

    my $dh = qualify_to_ref $_[0];
    if (CORE::opendir $dh, $_[1]) {
        return 1;
    }
    elsif (_MSWin32_5Cended_path($_[1])) {
        if (CORE::opendir $dh, "$_[1]/.") {
            return 1;
        }
    }
    return;
}

#
# Big5Plus file stat (with parameter)
#
sub Char::Ebig5plus::stat(*) {

    local $_ = shift if @_;

    my $fh = qualify_to_ref $_;
    if (fileno $fh) {
        return CORE::stat $fh;
    }
    elsif (-e $_) {
        return CORE::stat _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        my $fh = gensym();
        if (open $fh, $_) {
            my @stat = CORE::stat $fh;
            close $fh;
            return @stat;
        }
    }
    return;
}

#
# Big5Plus file stat (without parameter)
#
sub Char::Ebig5plus::stat_() {

    my $fh = qualify_to_ref $_;
    if (fileno $fh) {
        return CORE::stat $fh;
    }
    elsif (-e $_) {
        return CORE::stat _;
    }
    elsif (_MSWin32_5Cended_path($_)) {
        my $fh = gensym();
        if (open $fh, $_) {
            my @stat = CORE::stat $fh;
            close $fh;
            return @stat;
        }
    }
    return;
}

#
# Big5Plus path unlink
#
sub Char::Ebig5plus::unlink(@) {

    local @_ = ($_) unless @_;

    my $unlink = 0;
    for (@_) {
        if (CORE::unlink) {
            $unlink++;
        }
        elsif (_MSWin32_5Cended_path($_)) {
            my @char = m/\G ($q_char) /oxmsg;
            my $file = join '', map {{'/' => '\\'}->{$_} || $_} @char;
            if ($file =~ m/ \A (?:$q_char)*? [ ] /oxms) {
                $file = qq{"$file"};
            }

            # P.565 23.1.2. Cleaning Up Your Environment
            # in Chapter 23: Security
            # of ISBN 0-596-00027-8 Programming Perl Third Edition.
            # (and so on)

            # local $ENV{'PATH'} = '.';
            local @ENV{qw(IFS CDPATH ENV BASH_ENV)};

            system qq{del $file >NUL 2>NUL};

            my $fh = gensym();
            if (open $fh, $_) {
                close $fh;
            }
            else {
                $unlink++;
            }
        }
    }
    return $unlink;
}

#
# Big5Plus chdir
#
sub Char::Ebig5plus::chdir(;$) {

    if (@_ == 0) {
        return CORE::chdir;
    }

    my($dir) = @_;

    if (_MSWin32_5Cended_path($dir)) {
        if (not Char::Ebig5plus::d $dir) {
            return 0;
        }

        if ($] =~ m/^5\.005/oxms) {
            return CORE::chdir $dir;
        }
        elsif ($] =~ m/^(?:5\.006|5\.008000)/oxms) {
            if ($^O eq 'MSWin32') {
                local $@;
                my $chdir = eval q{
                    CORE::require 'jacode.pl';

                    # P.676 ${^WIDE_SYSTEM_CALLS}
                    # in Chapter 28: Special Names
                    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

                    local ${^WIDE_SYSTEM_CALLS} = 1;
                    return CORE::chdir jcode::utf8($dir,'sjis');
                };
                if (not $@) {
                    return $chdir;
                }
            }
        }

        local $@;
        my $shortdir = '';
        my $chdir = eval q{
            use Win32;
            $shortdir = Win32::GetShortPathName($dir);
            if ($shortdir ne $dir) {
                return CORE::chdir $shortdir;
            }
            else {
                return 0;
            }
        };
        if ($@) {
            my @char = $dir =~ m/\G ($q_char) /oxmsg;
            while ($char[-1] eq "\x5C") {
                pop @char;
            }
            $dir = join '', @char;
            croak "perl$] can't chdir to $dir (chr(0x5C) ended path), Win32.pm module may help you";
        }
        elsif ($shortdir eq $dir) {
            my @char = $dir =~ m/\G ($q_char) /oxmsg;
            while ($char[-1] eq "\x5C") {
                pop @char;
            }
            $dir = join '', @char;
            croak "perl$] can't chdir to $dir (chr(0x5C) ended path)";
        }
        return $chdir;
    }
    else {
        return CORE::chdir $dir;
    }
}

#
# Big5Plus chr(0x5C) ended path on MSWin32
#
sub _MSWin32_5Cended_path {

    if ((@_ >= 1) and ($_[0] ne '')) {
        if ($^O =~ m/\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
            my @char = $_[0] =~ /\G ($q_char) /oxmsg;
            if ($char[-1] =~ m/ \x5C \z/oxms) {
                return 1;
            }
        }
    }
    return;
}

#
# do Big5Plus file
#
sub Char::Ebig5plus::do($) {

    my($filename) = @_;

    my $realfilename;
    my $result;
ITER_DO:
    {
        for my $prefix (@INC) {
            if ($^O eq 'MacOS') {
                $realfilename = "$prefix$filename";
            }
            else {
                $realfilename = "$prefix/$filename";
            }

            if (Char::Ebig5plus::f($realfilename)) {

                my $script = '';

                if (Char::Ebig5plus::e("$realfilename.e")) {
                    my $e_mtime      = (Char::Ebig5plus::stat("$realfilename.e"))[9];
                    my $mtime        = (Char::Ebig5plus::stat($realfilename))[9];
                    my $module_mtime = (Char::Ebig5plus::stat(__FILE__))[9];
                    if (($e_mtime < $mtime) or ($mtime < $module_mtime)) {
                        Char::Ebig5plus::unlink "$realfilename.e";
                    }
                }

                if (Char::Ebig5plus::e("$realfilename.e")) {
                    my $fh = gensym();
                    if (open $fh, "$realfilename.e") {
                        if ($^O eq 'MacOS') {
                            eval q{
                                CORE::require Mac::Files;
                                Mac::Files::FSpSetFLock("$realfilename.e");
                            };
                        }
                        elsif (exists $ENV{'SJIS_NONBLOCK'}) {

                            # 7.18. Locking a File
                            # in Chapter 7. File Access
                            # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
                            # (and so on)

                            eval q{
                                unless (flock($fh, LOCK_SH | LOCK_NB)) {
                                    carp "$__FILE__: Can't immediately read-lock the file: $realfilename.e";
                                    exit;
                                }
                            };
                        }
                        else {
                            eval q{ flock($fh, LOCK_SH) };
                        }
                        local $/ = undef; # slurp mode
                        $script = <$fh>;
                        if ($^O eq 'MacOS') {
                            eval q{
                                CORE::require Mac::Files;
                                Mac::Files::FSpRstFLock("$realfilename.e");
                            };
                        }
                        close $fh;
                    }
                }
                else {
                    my $fh = gensym();
                    open $fh, $realfilename;
                    local $/ = undef; # slurp mode
                    $script = <$fh>;
                    close $fh;

                    if ($script =~ m/^ \s* use \s+ Char::Big5Plus \s* ([^\x81-\xFE;]*) ; \s* \n? $/oxms) {
                        CORE::require Char::Big5Plus;
                        $script = Char::Big5Plus::escape_script($script);
                        my $fh = gensym();
                        if ((eval q{ use Fcntl qw(O_WRONLY O_APPEND O_CREAT); 1 } and CORE::sysopen($fh, "$realfilename.e", &O_WRONLY|&O_APPEND|&O_CREAT))
                            or open($fh, ">>$realfilename.e")
                        ) {
                            if ($^O eq 'MacOS') {
                                eval q{
                                    CORE::require Mac::Files;
                                    Mac::Files::FSpSetFLock("$realfilename.e");
                                };
                            }
                            elsif (exists $ENV{'SJIS_NONBLOCK'}) {
                                eval q{
                                    unless (flock($fh, LOCK_EX | LOCK_NB)) {
                                        carp "$__FILE__: Can't immediately write-lock the file: $realfilename.e";
                                        exit;
                                    }
                                };
                            }
                            else {
                                eval q{ flock($fh, LOCK_EX) };
                            }
                            truncate($fh, 0) or croak "$__FILE__: Can't truncate file: $realfilename.e";
                            seek($fh, 0, 0)  or croak "$__FILE__: Can't seek file: $realfilename.e";
                            print {$fh} $script;
                            if ($^O eq 'MacOS') {
                                eval q{
                                    CORE::require Mac::Files;
                                    Mac::Files::FSpRstFLock("$realfilename.e");
                                };
                            }
                            close $fh;
                        }
                    }
                }

                no strict;
                local $@;
                $result = eval $script;

                last ITER_DO;
            }
        }
    }
    $INC{$filename} = $realfilename;
    return $result;
}

#
# require Big5Plus file
#

# require
# in Chapter 3: Functions
# of ISBN 1-56592-149-6 Programming Perl, Second Edition.

sub Char::Ebig5plus::require(;$) {

    local $_ = shift if @_;
    return 1 if $INC{$_};

    # jcode.pl
    # ftp://ftp.iij.ad.jp/pub/IIJ/dist/utashiro/perl/

    # jacode.pl
    # http://search.cpan.org/dist/jacode/

    if (m{ \b (?: jcode\.pl | jacode\.pl ) \z }oxms) {
        return CORE::require($_);
    }

    my $realfilename;
    my $result;
ITER_REQUIRE:
    {
        for my $prefix (@INC) {
            if ($^O eq 'MacOS') {
                $realfilename = "$prefix$_";
            }
            else {
                $realfilename = "$prefix/$_";
            }

            if (Char::Ebig5plus::f($realfilename)) {

                my $script = '';

                if (Char::Ebig5plus::e("$realfilename.e")) {
                    my $e_mtime      = (Char::Ebig5plus::stat("$realfilename.e"))[9];
                    my $mtime        = (Char::Ebig5plus::stat($realfilename))[9];
                    my $module_mtime = (Char::Ebig5plus::stat(__FILE__))[9];
                    if (($e_mtime < $mtime) or ($mtime < $module_mtime)) {
                        Char::Ebig5plus::unlink "$realfilename.e";
                    }
                }

                if (Char::Ebig5plus::e("$realfilename.e")) {
                    my $fh = gensym();
                    open($fh, "$realfilename.e") or croak "Can't open file: $realfilename.e";
                    if ($^O eq 'MacOS') {
                        eval q{
                            CORE::require Mac::Files;
                            Mac::Files::FSpSetFLock("$realfilename.e");
                        };
                    }
                    elsif (exists $ENV{'SJIS_NONBLOCK'}) {
                        eval q{
                            unless (flock($fh, LOCK_SH | LOCK_NB)) {
                                carp "$__FILE__: Can't immediately read-lock the file: $realfilename.e";
                                exit;
                            }
                        };
                    }
                    else {
                        eval q{ flock($fh, LOCK_SH) };
                    }
                    local $/ = undef; # slurp mode
                    $script = <$fh>;
                    if ($^O eq 'MacOS') {
                        eval q{
                            CORE::require Mac::Files;
                            Mac::Files::FSpRstFLock("$realfilename.e");
                        };
                    }
                    close($fh) or croak "Can't close file: $realfilename";
                }
                else {
                    my $fh = gensym();
                    open($fh, $realfilename) or croak "Can't open file: $realfilename";
                    local $/ = undef; # slurp mode
                    $script = <$fh>;
                    close($fh) or croak "Can't close file: $realfilename";

                    if ($script =~ m/^ \s* use \s+ Char::Big5Plus \s* ([^\x81-\xFE;]*) ; \s* \n? $/oxms) {
                        CORE::require Char::Big5Plus;
                        $script = Char::Big5Plus::escape_script($script);
                        my $fh = gensym();
                        if (eval q{ use Fcntl qw(O_WRONLY O_APPEND O_CREAT); 1 } and CORE::sysopen($fh,"$realfilename.e",&O_WRONLY|&O_APPEND|&O_CREAT)) {
                        }
                        else {
                            open($fh, ">>$realfilename.e") or croak "Can't write open file: $realfilename.e";
                        }
                        if ($^O eq 'MacOS') {
                            eval q{
                                CORE::require Mac::Files;
                                Mac::Files::FSpSetFLock("$realfilename.e");
                            };
                        }
                        elsif (exists $ENV{'SJIS_NONBLOCK'}) {
                            eval q{
                                unless (flock($fh, LOCK_EX | LOCK_NB)) {
                                    carp "$__FILE__: Can't immediately write-lock the file: $realfilename.e";
                                    exit;
                                }
                            };
                        }
                        else {
                            eval q{ flock($fh, LOCK_EX) };
                        }
                        truncate($fh, 0) or croak "$__FILE__: Can't truncate file: $realfilename.e";
                        seek($fh, 0, 0)  or croak "$__FILE__: Can't seek file: $realfilename.e";
                        print {$fh} $script;
                        if ($^O eq 'MacOS') {
                            eval q{
                                CORE::require Mac::Files;
                                Mac::Files::FSpRstFLock("$realfilename.e");
                            };
                        }
                        close($fh) or croak "Can't close file: $realfilename";
                    }
                }

                no strict;
                local $@;
                $result = eval $script;

                last ITER_REQUIRE;
            }
        }
        croak "Can't find $_ in \@INC";
    }
    croak $@ if $@;
    croak "$_ did not return true value" unless $result;
    $INC{$_} = $realfilename;
    return $result;
}

#
# Big5Plus telldir avoid warning
#
sub Char::Ebig5plus::telldir(*) {

    local $^W = 0;

    return CORE::telldir $_[0];
}

#
# Big5Plus character to order (with parameter)
#
sub Char::Big5Plus::ord(;$) {

    local $_ = shift if @_;

    if (m/\A ($q_char) /oxms) {
        my @ord = unpack 'C*', $1;
        my $ord = 0;
        while (my $o = shift @ord) {
            $ord = $ord * 0x100 + $o;
        }
        return $ord;
    }
    else {
        return CORE::ord $_;
    }
}

#
# Big5Plus character to order (without parameter)
#
sub Char::Big5Plus::ord_() {

    if (m/\A ($q_char) /oxms) {
        my @ord = unpack 'C*', $1;
        my $ord = 0;
        while (my $o = shift @ord) {
            $ord = $ord * 0x100 + $o;
        }
        return $ord;
    }
    else {
        return CORE::ord $_;
    }
}

#
# Big5Plus reverse
#
sub Char::Big5Plus::reverse(@) {

    if (wantarray) {
        return CORE::reverse @_;
    }
    else {
        return join '', CORE::reverse(join('',@_) =~ m/\G ($q_char) /oxmsg);
    }
}

#
# Big5Plus length by character
#
sub Char::Big5Plus::length(;$) {

    local $_ = shift if @_;

    local @_ = m/\G ($q_char) /oxmsg;
    return scalar @_;
}

#
# Big5Plus substr by character
#
sub Char::Big5Plus::substr($$;$$) {

    my @char = $_[0] =~ m/\G ($q_char) /oxmsg;

    # substr($string,$offset,$length,$replacement)
    if (@_ == 4) {
        my(undef,$offset,$length,$replacement) = @_;
        my $substr = join '', splice(@char, $offset, $length, $replacement);
        $_[0] = join '', @char;
        return $substr;
    }

    # substr($string,$offset,$length)
    elsif (@_ == 3) {
        my(undef,$offset,$length) = @_;
        if ($length == 0) {
            return '';
        }
        if ($offset >= 0) {
            return join '', (@char[$offset            .. $#char])[0 .. $length-1];
        }
        else {
            return join '', (@char[($#char+$offset+1) .. $#char])[0 .. $length-1];
        }
    }

    # substr($string,$offset)
    else {
        my(undef,$offset) = @_;
        if ($offset >= 0) {
            return join '', @char[$offset            .. $#char];
        }
        else {
            return join '', @char[($#char+$offset+1) .. $#char];
        }
    }
}

#
# Big5Plus index by character
#
sub Char::Big5Plus::index($$;$) {

    my $index;
    if (@_ == 3) {
        $index = Char::Ebig5plus::index($_[0], $_[1], CORE::length(Char::Big5Plus::substr($_[0], 0, $_[2])));
    }
    else {
        $index = Char::Ebig5plus::index($_[0], $_[1]);
    }

    if ($index == -1) {
        return -1;
    }
    else {
        return Char::Big5Plus::length(CORE::substr $_[0], 0, $index);
    }
}

#
# Big5Plus rindex by character
#
sub Char::Big5Plus::rindex($$;$) {

    my $rindex;
    if (@_ == 3) {
        $rindex = Char::Ebig5plus::rindex($_[0], $_[1], CORE::length(Char::Big5Plus::substr($_[0], 0, $_[2])));
    }
    else {
        $rindex = Char::Ebig5plus::rindex($_[0], $_[1]);
    }

    if ($rindex == -1) {
        return -1;
    }
    else {
        return Char::Big5Plus::length(CORE::substr $_[0], 0, $rindex);
    }
}

#
# instead of Carp::carp
#
sub carp(@) {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
}

#
# instead of Carp::croak
#
sub croak(@) {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
    die "\n";
}

#
# instead of Carp::cluck
#
sub cluck(@) {
    my $i = 0;
    my @cluck = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @cluck, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR reverse @cluck;
    print STDERR "\n";
    carp @_;
}

#
# instead of Carp::confess
#
sub confess(@) {
    my $i = 0;
    my @confess = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @confess, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR reverse @confess;
    print STDERR "\n";
    croak @_;
}

1;

__END__

=pod

=head1 NAME

Char::Ebig5plus - Run-time routines for Char/Big5Plus.pm

=head1 SYNOPSIS

  use Char::Ebig5plus;

    Char::Ebig5plus::split(...);
    Char::Ebig5plus::tr(...);
    Char::Ebig5plus::chop(...);
    Char::Ebig5plus::index(...);
    Char::Ebig5plus::rindex(...);
    Char::Ebig5plus::lc(...);
    Char::Ebig5plus::lc_;
    Char::Ebig5plus::lcfirst(...);
    Char::Ebig5plus::lcfirst_;
    Char::Ebig5plus::uc(...);
    Char::Ebig5plus::uc_;
    Char::Ebig5plus::ucfirst(...);
    Char::Ebig5plus::ucfirst_;
    Char::Ebig5plus::ignorecase(...);
    Char::Ebig5plus::capture(...);
    Char::Ebig5plus::chr(...);
    Char::Ebig5plus::chr_;
    Char::Ebig5plus::X ...;
    Char::Ebig5plus::X_;
    Char::Ebig5plus::glob(...);
    Char::Ebig5plus::glob_;
    Char::Ebig5plus::lstat(...);
    Char::Ebig5plus::lstat_;
    Char::Ebig5plus::opendir(...);
    Char::Ebig5plus::stat(...);
    Char::Ebig5plus::stat_;
    Char::Ebig5plus::unlink(...);
    Char::Ebig5plus::chdir(...);
    Char::Ebig5plus::do(...);
    Char::Ebig5plus::require(...);
    Char::Ebig5plus::telldir(...);

  # "no Char::Ebig5plus;" not supported

=head1 ABSTRACT

This module is a run-time routines of the Char/Big5Plus.pm.
Because the Char/Big5Plus.pm automatically uses this module, you need not use directly.

=head1 BUGS AND LIMITATIONS

Please patches and report problems to author are welcome.

=head1 HISTORY

This Char::Ebig5plus module first appeared in ActivePerl Build 522 Built under
MSWin32 Compiled at Nov 2 1999 09:52:28

=head1 AUTHOR

INABA Hitoshi E<lt>ina@cpan.orgE<gt>

This project was originated by INABA Hitoshi.
For any questions, use E<lt>ina@cpan.orgE<gt> so we can share
this file.

=head1 LICENSE AND COPYRIGHT

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=head1 EXAMPLES

=over 2

=item Split string

  @split = Char::Ebig5plus::split(/pattern/,$string,$limit);
  @split = Char::Ebig5plus::split(/pattern/,$string);
  @split = Char::Ebig5plus::split(/pattern/);
  @split = Char::Ebig5plus::split('',$string,$limit);
  @split = Char::Ebig5plus::split('',$string);
  @split = Char::Ebig5plus::split('');
  @split = Char::Ebig5plus::split();
  @split = Char::Ebig5plus::split;

  Scans a Big5Plus $string for delimiters that match pattern and splits the Big5Plus
  $string into a list of substrings, returning the resulting list value in list
  context, or the count of substrings in scalar context. The delimiters are
  determined by repeated pattern matching, using the regular expression given in
  pattern, so the delimiters may be of any size and need not be the same Big5Plus
  $string on every match. If the pattern doesn't match at all, Char::Ebig5plus::split returns
  the original Big5Plus $string as a single substring. If it matches once, you get
  two substrings, and so on.
  If $limit is specified and is not negative, the function splits into no more than
  that many fields. If $limit is negative, it is treated as if an arbitrarily large
  $limit has been specified. If $limit is omitted, trailing null fields are stripped
  from the result (which potential users of pop would do well to remember).
  If Big5Plus $string is omitted, the function splits the $_ Big5Plus string.
  If $patten is also omitted, the function splits on whitespace, /\s+/, after
  skipping any leading whitespace.
  If the pattern contains parentheses, then the substring matched by each pair of
  parentheses is included in the resulting list, interspersed with the fields that
  are ordinarily returned.

=item Transliteration

  $tr = Char::Ebig5plus::tr($variable,$bind_operator,$searchlist,$replacementlist,$modifier);
  $tr = Char::Ebig5plus::tr($variable,$bind_operator,$searchlist,$replacementlist);

  This function scans a Big5Plus string character by character and replaces all
  occurrences of the characters found in $searchlist with the corresponding character
  in $replacementlist. It returns the number of characters replaced or deleted.
  If no Big5Plus string is specified via =~ operator, the $_ variable is translated.
  $modifier are:

  Modifier   Meaning
  ------------------------------------------------------
  c          Complement $searchlist
  d          Delete found but unreplaced characters
  s          Squash duplicate replaced characters
  ------------------------------------------------------

=item Chop string

  $chop = Char::Ebig5plus::chop(@list);
  $chop = Char::Ebig5plus::chop();
  $chop = Char::Ebig5plus::chop;

  Chops off the last character of a Big5Plus string contained in the variable (or
  Big5Plus strings in each element of a @list) and returns the character chopped.
  The Char::Ebig5plus::chop operator is used primarily to remove the newline from the end of
  an input record but is more efficient than s/\n$//. If no argument is given, the
  function chops the $_ variable.

=item Index string

  $pos = Char::Ebig5plus::index($string,$substr,$position);
  $pos = Char::Ebig5plus::index($string,$substr);

  Returns the position of the first occurrence of $substr in Big5Plus $string.
  The start, if specified, specifies the $position to start looking in the Big5Plus
  $string. Positions are integer numbers based at 0. If the substring is not found,
  the Char::Ebig5plus::index function returns -1.

=item Reverse index string

  $pos = Char::Ebig5plus::rindex($string,$substr,$position);
  $pos = Char::Ebig5plus::rindex($string,$substr);

  Works just like Char::Ebig5plus::index except that it returns the position of the last
  occurence of $substr in Big5Plus $string (a reverse index). The function returns
  -1 if not found. $position, if specified, is the rightmost position that may be
  returned, i.e., how far in the Big5Plus string the function can search.

=item Lower case string

  $lc = Char::Ebig5plus::lc($string);
  $lc = Char::Ebig5plus::lc_;

  Returns a lowercase version of Big5Plus string (or $_, if omitted). This is the
  internal function implementing the \L escape in double-quoted strings.

=item Lower case first character of string

  $lcfirst = Char::Ebig5plus::lcfirst($string);
  $lcfirst = Char::Ebig5plus::lcfirst_;

  Returns a version of Big5Plus string (or $_, if omitted) with the first character
  lowercased. This is the internal function implementing the \l escape in double-
  quoted strings.

=item Upper case string

  $uc = Char::Ebig5plus::uc($string);
  $uc = Char::Ebig5plus::uc_;

  Returns an uppercased version of Big5Plus string (or $_, if string is omitted).
  This is the internal function implementing the \U escape in double-quoted
  strings.

=item Upper case first character of string

  $ucfirst = Char::Ebig5plus::ucfirst($string);
  $ucfirst = Char::Ebig5plus::ucfirst_;

  Returns a version of Big5Plus string (or $_, if omitted) with the first character
  uppercased. This is the internal function implementing the \u escape in double-
  quoted strings.

=item Make ignore case string

  @ignorecase = Char::Ebig5plus::ignorecase(@string);

  This function is internal use to m/ /i, s/ / /i, split / /i and qr/ /i.

=item Make capture number

  $capturenumber = Char::Ebig5plus::capture($string);

  This function is internal use to m/ /, s/ / /, split / / and qr/ /.

=item Make character

  $chr = Char::Ebig5plus::chr($code);
  $chr = Char::Ebig5plus::chr_;

  This function returns the character represented by that $code in the character
  set. For example, Char::Ebig5plus::chr(65) is "A" in either ASCII or Big5Plus, not Unicode,
  and Char::Ebig5plus::chr(0x82a0) is a Big5Plus HIRAGANA LETTER A. For the reverse of
  Char::Ebig5plus::chr, use Char::Big5Plus::ord.

=item File test operator -X

  A file test operator is an unary operator that tests a pathname or a filehandle.
  If $string is omitted, it uses $_ by function Char::Ebig5plus::r_.
  The following functions function when the pathname ends with chr(0x5C) on MSWin32.

  $test = Char::Ebig5plus::r $string;
  $test = Char::Ebig5plus::r_;

  Returns 1 when true case or '' when false case.
  Returns undef unless successful.

  Function and Prototype     Meaning
  ------------------------------------------------------------------------------
  Char::Ebig5plus::r(*), Char::Ebig5plus::r_()   File is readable by effective uid/gid
  Char::Ebig5plus::w(*), Char::Ebig5plus::w_()   File is writable by effective uid/gid
  Char::Ebig5plus::x(*), Char::Ebig5plus::x_()   File is executable by effective uid/gid
  Char::Ebig5plus::o(*), Char::Ebig5plus::o_()   File is owned by effective uid
  Char::Ebig5plus::R(*), Char::Ebig5plus::R_()   File is readable by real uid/gid
  Char::Ebig5plus::W(*), Char::Ebig5plus::W_()   File is writable by real uid/gid
  Char::Ebig5plus::X(*), Char::Ebig5plus::X_()   File is executable by real uid/gid
  Char::Ebig5plus::O(*), Char::Ebig5plus::O_()   File is owned by real uid
  Char::Ebig5plus::e(*), Char::Ebig5plus::e_()   File exists
  Char::Ebig5plus::z(*), Char::Ebig5plus::z_()   File has zero size
  Char::Ebig5plus::f(*), Char::Ebig5plus::f_()   File is a plain file
  Char::Ebig5plus::d(*), Char::Ebig5plus::d_()   File is a directory
  Char::Ebig5plus::l(*), Char::Ebig5plus::l_()   File is a symbolic link
  Char::Ebig5plus::p(*), Char::Ebig5plus::p_()   File is a named pipe (FIFO)
  Char::Ebig5plus::S(*), Char::Ebig5plus::S_()   File is a socket
  Char::Ebig5plus::b(*), Char::Ebig5plus::b_()   File is a block special file
  Char::Ebig5plus::c(*), Char::Ebig5plus::c_()   File is a character special file
  Char::Ebig5plus::u(*), Char::Ebig5plus::u_()   File has setuid bit set
  Char::Ebig5plus::g(*), Char::Ebig5plus::g_()   File has setgid bit set
  Char::Ebig5plus::k(*), Char::Ebig5plus::k_()   File has sticky bit set
  ------------------------------------------------------------------------------

  Returns 1 when true case or '' when false case.
  Returns undef unless successful.

  The Char::Ebig5plus::T, Char::Ebig5plus::T_, Char::Ebig5plus::B and Char::Ebig5plus::B_ work as follows. The first block
  or so of the file is examined for strange chatracters such as
  [\000-\007\013\016-\032\034-\037\377] (that don't look like Big5Plus). If more
  than 10% of the bytes appear to be strange, it's a *maybe* binary file;
  otherwise, it's a *maybe* text file. Also, any file containing ASCII NUL(\0) or
  \377 in the first block is considered a binary file. If Char::Ebig5plus::T or Char::Ebig5plus::B is
  used on a filehandle, the current input (standard I/O or "stdio") buffer is
  examined rather than the first block of the file. Both Char::Ebig5plus::T and Char::Ebig5plus::B
  return 1 as true on an empty file, or on a file at EOF (end-of-file) when testing
  a filehandle. Both Char::Ebig5plus::T and Char::Ebig5plus::B deosn't work when given the special
  filehandle consisting of a solitary underline.

  Function and Prototype     Meaning
  ------------------------------------------------------------------------------
  Char::Ebig5plus::T(*), Char::Ebig5plus::T_()   File is a text file
  Char::Ebig5plus::B(*), Char::Ebig5plus::B_()   File is a binary file (opposite of -T)
  ------------------------------------------------------------------------------

  Returns useful value if successful, or undef unless successful.

  $value = Char::Ebig5plus::s $string;
  $value = Char::Ebig5plus::s_;

  Function and Prototype     Meaning
  ------------------------------------------------------------------------------
  Char::Ebig5plus::s(*), Char::Ebig5plus::s_()   File has nonzero size (returns size)
  Char::Ebig5plus::M(*), Char::Ebig5plus::M_()   Age of file (at startup) in days since modification
  Char::Ebig5plus::A(*), Char::Ebig5plus::A_()   Age of file (at startup) in days since last access
  Char::Ebig5plus::C(*), Char::Ebig5plus::C_()   Age of file (at startup) in days since inode change
  ------------------------------------------------------------------------------

=item Filename expansion (globbing)

  @glob = Char::Ebig5plus::glob($string);
  @glob = Char::Ebig5plus::glob_;

  Performs filename expansion (DOS-like globbing) on $string, returning the next
  successive name on each call. If $string is omitted, $_ is globbed instead.
  This function function when the pathname ends with chr(0x5C) on MSWin32.

  For example, C<<..\\l*b\\file/*glob.p?>> on MSWin32 or UNIX will work as
  expected (in that it will find something like '..\lib\File/DosGlob.pm'
  alright).
  Note that all path components are
  case-insensitive, and that backslashes and forward slashes are both accepted,
  and preserved. You may have to double the backslashes if you are putting them in
  literally, due to double-quotish parsing of the pattern by perl.
  A tilde ("~") expands to the current user's home directory.

  Spaces in the argument delimit distinct patterns, so C<glob('*.exe *.dll')> globs
  all filenames that end in C<.exe> or C<.dll>. If you want to put in literal spaces
  in the glob pattern, you can escape them with either double quotes.
  e.g. C<glob('c:/"Program Files"/*/*.dll')>.

=item Statistics about link

  @lstat = Char::Ebig5plus::lstat($file);
  @lstat = Char::Ebig5plus::lstat_;

  Like Char::Ebig5plus::stat, returns information on file, except that if file is a symbolic
  link, Char::Ebig5plus::lstat returns information about the link; Char::Ebig5plus::stat returns
  information about the file pointed to by the link. (If symbolic links are
  unimplemented on your system, a normal Char::Ebig5plus::stat is done instead.) If file is
  omitted, returns information on file given in $_.
  This function function when the filename ends with chr(0x5C) on MSWin32.

=item Open directory handle

  $rc = Char::Ebig5plus::opendir(DIR,$dir);

  Opens a directory for processing by readdir, telldir, seekdir, rewinddir and
  closedir. The function returns true if successful.
  This function function when the directory name ends with chr(0x5C) on MSWin32.

=item Statistics about file

  @stat = Char::Ebig5plus::stat($file);
  @stat = Char::Ebig5plus::stat_;

  Returns a 13-element list giving the statistics for a file, indicated by either
  a filehandle or an expression that gives its name. It's typically used as
  follows:

  ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
      $atime,$mtime,$ctime,$blksize,$blocks) = Char::Ebig5plus::stat($file);

  Not all fields are supported on all filesystem types. Here are the meanings of
  the fields:

  Field     Meaning
  -----------------------------------------------------------------
  dev       Device number of filesystem
  ino       Inode number
  mode      File mode (type and permissions)
  nlink     Nunmer of (hard) links to the file
  uid       Numeric user ID of file's owner
  gid       Numeric group ID of file's owner
  rdev      The device identifier (special files only)
  size      Total size of file, in bytes
  atime     Last access time since the epoch
  mtime     Last modification time since the epoch
  ctime     Inode change time (not creation time!) since the epoch
  blksize   Preferred blocksize for file system I/O
  blocks    Actual number of blocks allocated
  -----------------------------------------------------------------

  $dev and $ino, token together, uniquely identify a file. The $blksize and
  $blocks are likely defined only on BSD-derived filesystem. The $blocks field
  (if defined) is reported in 512-byte blocks.
  If stat is passed the special filehandle consisting of an underline, no
  actual stat is done, but the current contents of the stat structure from the
  last stat or stat-based file test (the -x operators) is returned.
  If file is omitted, returns information on file given in $_.
  This function function when the filename ends with chr(0x5C) on MSWin32.

=item Deletes a list of files.

  $unlink = Char::Ebig5plus::unlink(@list);
  $unlink = Char::Ebig5plus::unlink($file);
  $unlink = Char::Ebig5plus::unlink;

  Delete a list of files. (Under Unix, it will remove a link to a file, but the
  file may still exist if another link references it.) If list is omitted, it
  unlinks the file given in $_. The function returns the number of files
  successfully deleted.
  This function function when the filename ends with chr(0x5C) on MSWin32.

=item Changes the working directory.

  $chdir = Char::Ebig5plus::chdir($dirname);
  $chdir = Char::Ebig5plus::chdir;

  Changes the working directory to $dirname, if possible. If $dirname is omitted,
  it changes to the home directory. The function returns 1 upon success, 0
  otherwise (and puts the error code into $!).

  This function can't function when the $dirname ends with chr(0x5C) on perl5.006,
  perl5.008, perl5.010, perl5.012, perl5.014 on MSWin32.

=item do file

  $return = Char::Ebig5plus::do($file);

  The do FILE form uses the value of FILE as a filename and executes the contents
  of the file as a Perl script. Its primary use is (or rather was) to include
  subroutines from a Perl subroutine library, so that:

  Char::Ebig5plus::do('stat.pl');

  is rather like: 

  scalar eval `cat stat.pl`;   # `type stat.pl` on Windows

  except that Char::Ebig5plus::do is more efficient, more concise, keeps track of the current
  filename for error messages, searches all the directories listed in the @INC
  array, and updates %INC if the file is found.
  It also differs in that code evaluated with Char::Ebig5plus::do FILE can not see lexicals in
  the enclosing scope, whereas code in eval FILE does. It's the same, however, in
  that it reparses the file every time you call it -- so you might not want to do
  this inside a loop unless the filename itself changes at each loop iteration.

  If Char::Ebig5plus::do can't read the file, it returns undef and sets $! to the error. If 
  Char::Ebig5plus::do can read the file but can't compile it, it returns undef and sets an
  error message in $@. If the file is successfully compiled, do returns the value of
  the last expression evaluated.

  Inclusion of library modules (which have a mandatory .pm suffix) is better done
  with the use and require operators, which also Char::Ebig5plus::do error checking and raise
  an exception if there's a problem. They also offer other benefits: they avoid
  duplicate loading, help with object-oriented programming, and provide hints to the
  compiler on function prototypes.

  But Char::Ebig5plus::do FILE is still useful for such things as reading program configuration
  files. Manual error checking can be done this way:

  # read in config files: system first, then user
  for $file ("/usr/share/proggie/defaults.rc", "$ENV{HOME}/.someprogrc") {
      unless ($return = Char::Ebig5plus::do($file)) {
          warn "couldn't parse $file: $@" if $@;
          warn "couldn't Char::Ebig5plus::do($file): $!" unless defined $return;
          warn "couldn't run $file"            unless $return;
      }
  }

  A long-running daemon could periodically examine the timestamp on its configuration
  file, and if the file has changed since it was last read in, the daemon could use
  Char::Ebig5plus::do to reload that file. This is more tidily accomplished with Char::Ebig5plus::do than
  with Char::Ebig5plus::require.

=item require file

  Char::Ebig5plus::require($file);
  Char::Ebig5plus::require();

  This function asserts a dependency of some kind on its argument. If an argument is not
  supplied, $_ is used.

  If the argument is a string, Char::Ebig5plus::require loads and executes the Perl code found in
  the separate file whose name is given by the string. This is similar to performing a
  Char::Ebig5plus::do on a file, except that Char::Ebig5plus::require checks to see whether the library
  file has been loaded already and raises an exception if any difficulties are
  encountered. (It can thus be used to express file dependencies without worrying about
  duplicate compilation.) Like its cousins Char::Ebig5plus::do and use, Char::Ebig5plus::require knows how
  to search the include path stored in the @INC array and to update %INC upon success.

  The file must return true as the last value to indicate successful execution of any
  initialization code, so it's customary to end such a file with 1; unless you're sure
  it'll return true otherwise.

  See also do file.

=item current position of the readdir

  $telldir = Char::Ebig5plus::telldir(DIRHANDLE);

  This function returns the current position of the readdir routines on DIRHANDLE.
  This value may be given to seekdir to access a particular location in a directory.
  The function has the same caveats about possible directory compaction as the
  corresponding system library routine. This function might not be implemented
  everywhere that readdir is. Even if it is, no calculation may be done with the
  return value. It's just an opaque value, meaningful only to seekdir.

=cut
