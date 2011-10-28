package Char::Big5Plus;
######################################################################
#
# Char::Big5Plus - Source code filter to Big5Plus script
#
# Copyright (c) 2008, 2009, 2010, 2011 INABA Hitoshi <ina@cpan.org>
#
######################################################################

use 5.00503;
use strict;
use Char::Ebig5plus;

# 12.3. Delaying use Until Runtime
# in Chapter 12. Packages, Libraries, and Modules
# of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
# (and so on)

BEGIN { eval q{ use vars qw($VERSION) } }

$VERSION = sprintf '%d.%02d', q$Revision: 0.78 $ =~ m/(\d+)/oxmsg;

# poor Symbol.pm - substitute of real Symbol.pm
BEGIN {
    my $genpkg = "Symbol::";
    my $genseq = 0;
    sub gensym () {
        my $name = "GEN" . $genseq++;
        no strict qw(refs);
        my $ref = \*{$genpkg . $name};
        delete $$genpkg{$name};
        $ref;
    }
}

# P.714 29.2.39. flock
# in Chapter 29: Functions
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

sub LOCK_SH() {1}
sub LOCK_EX() {2}
sub LOCK_UN() {8}
sub LOCK_NB() {4}

# P.707 29.2.33. exec
# in Chapter 29: Functions
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

$| = 1;

BEGIN {
    if ($^X =~ m/ jperl /oxmsi) {
        die __FILE__, ": need perl(not jperl) 5.00503 or later. (\$^X==$^X)";
    }
}

sub import() {}
sub unimport() {}
sub Char::Big5Plus::escape_script;

# regexp of character
my $your_char = q{[\x81-\xFE][\x00-\xFF]|[\x00-\xFF]};
my $qq_char   = qr/\\c[\x40-\x5F]|\\?(?:$your_char)/oxms;
my  $q_char   = qr/$your_char/oxms;

# P.1023 Appendix W.9 Multibyte Anchoring
# of ISBN 1-56592-224-7 CJKV Information Processing

my $your_gap = '';
$your_gap = q{\G(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE])*?};

BEGIN { eval q{ use vars qw($nest) } }

# regexp of nested parens in qqXX

# P.340 Matching Nested Constructs with Embedded Code
# in Chapter 7: Perl
# of ISBN 0-596-00289-0 Mastering Regular Expressions, Second edition

my $qq_paren   = qr{(?{local $nest=0}) (?>(?:
                    \\c[\x40-\x5F] |
                    \\? [\x81-\xFE][\x00-\xFF] |
                    \\ [\x00-\xFF] |
                    [^()] |
                             \(  (?{$nest++}) |
                             \)  (?(?{$nest>0})(?{$nest--})|(?!)))*) (?(?{$nest!=0})(?!))
                 }xms;
my $qq_brace   = qr{(?{local $nest=0}) (?>(?:
                    \\c[\x40-\x5F] |
                    \\? [\x81-\xFE][\x00-\xFF] |
                    \\ [\x00-\xFF] |
                    [^{}] |
                             \{  (?{$nest++}) |
                             \}  (?(?{$nest>0})(?{$nest--})|(?!)))*) (?(?{$nest!=0})(?!))
                 }xms;
my $qq_bracket = qr{(?{local $nest=0}) (?>(?:
                    \\c[\x40-\x5F] |
                    \\? [\x81-\xFE][\x00-\xFF] |
                    \\ [\x00-\xFF] |
                    [^[\]] |
                             \[  (?{$nest++}) |
                             \]  (?(?{$nest>0})(?{$nest--})|(?!)))*) (?(?{$nest!=0})(?!))
                 }xms;
my $qq_angle   = qr{(?{local $nest=0}) (?>(?:
                    \\c[\x40-\x5F] |
                    \\? [\x81-\xFE][\x00-\xFF] |
                    \\ [\x00-\xFF] |
                    [^<>] |
                             \<  (?{$nest++}) |
                             \>  (?(?{$nest>0})(?{$nest--})|(?!)))*) (?(?{$nest!=0})(?!))
                 }xms;
my $qq_scalar  = qr{(?: \{ (?:$qq_brace)*? \} |
                       (?: ::)? (?:
                             [a-zA-Z_][a-zA-Z_0-9]*
                       (?: ::[a-zA-Z_][a-zA-Z_0-9]* )* (?: \[ (?: \$\[ | \$\] | $qq_char )*? \] | \{ (?:$qq_brace)*? \} )*
                                         (?: (?: -> )? (?: \[ (?: \$\[ | \$\] | $qq_char )*? \] | \{ (?:$qq_brace)*? \} ) )*
                   ))
                 }xms;
my $qq_variable = qr{(?: \{ (?:$qq_brace)*? \}                    |
                        (?: ::)? (?:
                              [0-9]+                              |
                              [^\x81-\xFEa-zA-Z_0-9\[\]] |
                              ^[A-Z]                              |
                              [a-zA-Z_][a-zA-Z_0-9]*
                        (?: ::[a-zA-Z_][a-zA-Z_0-9]* )* (?: \[ (?: \$\[ | \$\] | $qq_char )*? \] | \{ (?:$qq_brace)*? \} )*
                                          (?: (?: -> )? (?: \[ (?: \$\[ | \$\] | $qq_char )*? \] | \{ (?:$qq_brace)*? \} ) )*
                    ))
                  }xms;

# regexp of nested parens in qXX
my $q_paren    = qr{(?{local $nest=0}) (?>(?:
                    [\x81-\xFE][\x00-\xFF] |
                    [^()] |
                             \(  (?{$nest++}) |
                             \)  (?(?{$nest>0})(?{$nest--})|(?!)))*) (?(?{$nest!=0})(?!))
                 }xms;
my $q_brace    = qr{(?{local $nest=0}) (?>(?:
                    [\x81-\xFE][\x00-\xFF] |
                    [^{}] |
                             \{  (?{$nest++}) |
                             \}  (?(?{$nest>0})(?{$nest--})|(?!)))*) (?(?{$nest!=0})(?!))
                 }xms;
my $q_bracket  = qr{(?{local $nest=0}) (?>(?:
                    [\x81-\xFE][\x00-\xFF] |
                    [^[\]] |
                             \[  (?{$nest++}) |
                             \]  (?(?{$nest>0})(?{$nest--})|(?!)))*) (?(?{$nest!=0})(?!))
                 }xms;
my $q_angle    = qr{(?{local $nest=0}) (?>(?:
                    [\x81-\xFE][\x00-\xFF] |
                    [^<>] |
                             \<  (?{$nest++}) |
                             \>  (?(?{$nest>0})(?{$nest--})|(?!)))*) (?(?{$nest!=0})(?!))
                 }xms;

# P.854 31.17. use re
# in Chapter 31. Pragmatic Modules
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

my $use_re_eval = '';
my $m_matched   = '';
my $s_matched   = '';
$m_matched   = q{@Char::Ebig5plus::m_matched};
$s_matched   = q{@Char::Ebig5plus::s_matched};

my $tr_variable   = '';   # variable of tr///
my $sub_variable  = '';   # variable of s///
my $bind_operator = '';   # =~ or !~
BEGIN { eval q{ use vars qw($slash) } }
                          # when 'm//', '/' means regexp match 'm//' and '?' means regexp match '??'
                          # when 'div', '/' means division operator and '?' means conditional operator (condition ? then : else)
my @heredoc = ();         # here document
my @heredoc_delimiter = ();
my $here_script = '';     # here script
my $function_ord;         # ord()   to ord()   or Char::Big5Plus::ord()
my $function_ord_;        # ord     to ord     or Char::Big5Plus::ord_
my $function_reverse;     # reverse to reverse or Char::Big5Plus::reverse

my $ignore_modules = join('|', qw(
    utf8
    bytes
    charnames
    I18N::Japanese
    I18N::Collate
    I18N::JExt
    File::DosGlob
    Wild
    Wildcard
    Japanese
));

# in Chapter 8: Standard Modules
# of ISBN 0-596-00241-6 Perl in a Nutshell, Second Edition

# when this script is main program
if ($0 eq __FILE__) {

    # show usage
    unless (@ARGV) {
        die <<END;
$0: usage

perl $0 Big5Plus_script.pl > Escaped_script.pl.e
END
    }

    print Char::Big5Plus::escape_script($ARGV[0]);
    exit 0;
}

my $__PACKAGE__ = __PACKAGE__;
my $__FILE__    = __FILE__;
my($package,$filename,$line,$subroutine,$hasargs,$wantarray,$evaltext,$is_require,$hints,$bitmask) = caller 0;

# called any package not main
if ($package ne 'main') {
    die <<END;
$__FILE__: escape by manually command '$^X $__FILE__ "$filename" > "$__PACKAGE__::$filename"'
and rewrite "use $package;" to "use $__PACKAGE__::$package;" of script "$0".
END
}

if (Char::Ebig5plus::e("$filename.e")) {
    if (exists $ENV{'SJIS_DEBUG'}) {
        Char::Ebig5plus::unlink "$filename.e";
    }
    elsif (Char::Ebig5plus::z("$filename.e")) {
        Char::Ebig5plus::unlink "$filename.e";
    }
    else {
        my $e_mtime   = (Char::Ebig5plus::stat("$filename.e"))[9];
        my $mtime     = (Char::Ebig5plus::stat($filename))[9];
        my $__mtime__ = (Char::Ebig5plus::stat($__FILE__))[9];
        if (($e_mtime < $mtime) or ($mtime < $__mtime__)) {
            Char::Ebig5plus::unlink "$filename.e";
        }
    }
}

if (not Char::Ebig5plus::e("$filename.e")) {
    my $fh = gensym();

    if (eval q{ use Fcntl qw(O_WRONLY O_APPEND O_CREAT); 1 } and CORE::sysopen($fh,"$filename.e",&O_WRONLY|&O_APPEND|&O_CREAT)) {
    }
    else {
        open($fh, ">>$filename.e") or die "$__FILE__: Can't write open file: $filename.e";
    }

    if (0) {
    }
    elsif (exists $ENV{'SJIS_NONBLOCK'}) {

        # 7.18. Locking a File
        # in Chapter 7. File Access
        # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
        # (and so on)

        eval q{
            unless (flock($fh, LOCK_EX | LOCK_NB)) {
                warn "$__FILE__: Can't immediately write-lock the file: $filename.e";
                exit;
            }
        };
    }
    else {
        eval q{ flock($fh, LOCK_EX) };
    }

    truncate($fh, 0) or die "$__FILE__: Can't truncate file: $filename.e";
    seek($fh, 0, 0)  or die "$__FILE__: Can't seek file: $filename.e";

    my $e_script = Char::Big5Plus::escape_script($filename);
    print {$fh} $e_script;

    my $mode = (Char::Ebig5plus::stat($filename))[2] & 0777;
    chmod $mode, "$filename.e";

    close($fh) or die "$__FILE__: Can't close file: $filename.e";
}

# P.565 23.1.2. Cleaning Up Your Environment
# in Chapter 23: Security
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

# local $ENV{'PATH'} = '.';
local @ENV{qw(IFS CDPATH ENV BASH_ENV)};

my $fh = gensym();
open($fh, "$filename.e") or die "$__FILE__: Can't read open file: $filename.e";

if (0) {
}
elsif (exists $ENV{'SJIS_NONBLOCK'}) {
    eval q{
        unless (flock($fh, LOCK_SH | LOCK_NB)) {
            warn "$__FILE__: Can't immediately read-lock the file: $filename.e";
            exit;
        }
    };
}
else {
    eval q{ flock($fh, LOCK_SH) };
}

my @switch = ();
if ($^W) {
    push @switch, '-w';
}

# DOS-like system
if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    exit system
        _escapeshellcmd_MSWin32($^X),

# -I switch can not treat space included path
#       (map { '-I' . _escapeshellcmd_MSWin32($_) } @INC),
        (map { '-I' .                         $_  } @INC),

        @switch,
        '--',
        map { _escapeshellcmd_MSWin32($_) } "$filename.e", @ARGV;
}

# UNIX-like system
else {
    exit system
        _escapeshellcmd($^X),
        (map { '-I' . _escapeshellcmd($_) } @INC),
        @switch,
        '--',
        map { _escapeshellcmd($_) } "$filename.e", @ARGV;
}

# escape shell command line on DOS-like system
sub _escapeshellcmd_MSWin32 {
    my($word) = @_;
    if ($word =~ m/ $your_gap [ ] /oxms) {
        return qq{"$word"};
    }
    else {
        return $word;
    }
}

# escape shell command line on UNIX-like system
sub _escapeshellcmd {
    my($word) = @_;
    $word =~ s/([\t\n\r\x20!"#\$%&'()*+;<=>?\[\\\]^`{|}~\x7F\xFF])/\\$1/g;
    return $word;
}

# escape Big5Plus script
sub Char::Big5Plus::escape_script {
    my($script) = @_;
    my $e_script = '';

    # read Big5Plus script
    my $fh = gensym();
    open($fh, $script) or die "$__FILE__: Can't open file: $script";
    local $/ = undef; # slurp mode
    $_ = <$fh>;
    close($fh) or die "$__FILE__: Can't close file: $script";

    if (m/^ use Char::Ebig5plus(?:\s+[0-9\.]*)?\s*; $/oxms) {
        return $_;
    }
    else {

        # #! shebang line
        if (s/\A(#!.+?\n)//oms) {
            my $head = $1;
            $head =~ s/\bjperl\b/perl/gi;
            $e_script .= $head;
        }

        # DOS-like system header
        if (s/\A(\@rem\s*=\s*'.*?'\s*;\s*\n)//oms) {
            my $head = $1;
            $head =~ s/\bjperl\b/perl/gi;
            $e_script .= $head;
        }

        # P.618 Generating Perl in Other Languages
        # in Chapter 24: Common Practices
        # of ISBN 0-596-00027-8 Programming Perl Third Edition.

        if (s/(.*^#\s*line\s+\d+(?:\s+"(?:$q_char)+?")?\s*\n)//oms) {
            my $head = $1;
            $head =~ s/\bjperl\b/perl/gi;
            $e_script .= $head;
        }

        # P.210 5.10.3.3. Match-time code evaluation
        # in Chapter 5: Pattern Matching
        # of ISBN 0-596-00027-8 Programming Perl Third Edition.

        $e_script .= sprintf("use Char::Ebig5plus %s;\n%s", $Char::Ebig5plus::VERSION, $use_re_eval); # require run-time routines version

        # use Char::Big5Plus version qw(ord reverse);
        $function_ord     = 'ord';
        $function_ord_    = 'ord';
        $function_reverse = 'reverse';
        if (s/^ \s* use \s+ Char::Big5Plus \s* ([^\x81-\xFE;]*) ; \s* \n? $//oxms) {

            # require version
            my $list = $1;
            if ($list =~ s/\A ([0-9]+(?:\.[0-9]*)) \s* //oxms) {
                my $version = $1;
                if ($version > $VERSION) {
                    die "$__FILE__: version $version required--this is only version $VERSION";
                }
            }

            # demand ord and reverse
            if ($list !~ m/\A \s* \z/oxms) {
                local $@;
                my @list = eval $list;
                for (@list) {
                    $function_ord     = 'Char::Big5Plus::ord'     if m/\A ord \z/oxms;
                    $function_ord_    = 'Char::Big5Plus::ord_'    if m/\A ord \z/oxms;
                    $function_reverse = 'Char::Big5Plus::reverse' if m/\A reverse \z/oxms;
                }
            }
        }
    }

    $slash = 'm//';

    # Yes, I studied study yesterday.

    # P.359 The Study Function
    # in Chapter 7: Perl
    # of ISBN 0-596-00289-0 Mastering Regular Expressions, Second edition

    study $_;

    # while all script

    # one member of Tag-team
    #
    # P.315 "Tag-team" matching with /gc
    # in Chapter 7: Perl
    # of ISBN 0-596-00289-0 Mastering Regular Expressions, Second edition

    while (not /\G \z/oxgc) { # member
        $e_script .= escape();
    }

    return $e_script;
}

# escape Big5Plus part of script
sub escape {

# \n output here document

    # another member of Tag-team
    #
    # P.315 "Tag-team" matching with /gc
    # in Chapter 7: Perl
    # of ISBN 0-596-00289-0 Mastering Regular Expressions, Second edition

    if (/\G ( \n ) /oxgc) { # another member (and so on)
        my $heredoc = '';
        if (scalar(@heredoc_delimiter) >= 1) {
            $slash = 'm//';

            $heredoc = join '', @heredoc;
            @heredoc = ();

            # skip here document
            for my $heredoc_delimiter (@heredoc_delimiter) {
                /\G .*? \n $heredoc_delimiter \n/xmsgc;
            }
            @heredoc_delimiter = ();

            $here_script = '';
        }
        return "\n" . $heredoc;
    }

# ignore space, comment
    elsif (/\G (\s+|\#.*) /oxgc) { return $1; }

# if (, elsif (, unless (, while (, until (, given ( and when (

    # given, when
    #
    # P.225 The given Statement
    # in Chapter 15: Smart Matching and given-when
    # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition

    elsif (/\G ( (?: if | elsif | unless | while | until | given | when ) \s* \( ) /oxgc) {
        $slash = 'm//';
        return $1;
    }

# scalar variable ($scalar = ...) =~ tr///;
# scalar variable ($scalar = ...) =~ s///;

    # state
    #
    # P.68 Persistent, Private Variables
    # in Chapter 4: Subroutines
    # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition
    # (and so on)

    elsif (/\G ( \( \s* (?: local \b | my \b | our \b | state \b )? \s* \$ $qq_scalar ) /oxgc) {
        my $e_string = e_string($1);

        if (/\G ( \s* = $qq_paren \) ) ( \s* (?: =~ | !~ ) \s* ) (?= (?: tr|y) \b ) /oxgc) {
            $tr_variable = $e_string . e_string($1);
            $bind_operator = $2;
            $slash = 'm//';
            return '';
        }
        elsif (/\G ( \s* = $qq_paren \) ) ( \s* (?: =~ | !~ ) \s* ) (?= s \b ) /oxgc) {
            $sub_variable = $e_string . e_string($1);
            $bind_operator = $2;
            $slash = 'm//';
            return '';
        }
        else {
            $slash = 'div';
            return $e_string;
        }
    }

# scalar variable $scalar =~ tr///;
# scalar variable $scalar =~ s///;
    elsif (/\G ( \$ $qq_scalar ) /oxgc) {
        my $scalar = e_string($1);

        if (/\G ( \s* (?: =~ | !~ ) \s* ) (?= (?: tr|y) \b ) /oxgc) {
            $tr_variable = $scalar;
            $bind_operator = $1;
            $slash = 'm//';
            return '';
        }
        elsif (/\G ( \s* (?: =~ | !~ ) \s* ) (?= s \b ) /oxgc) {
            $sub_variable = $scalar;
            $bind_operator = $1;
            $slash = 'm//';
            return '';
        }
        else {
            $slash = 'div';
            return $scalar;
        }
    }

    # end of statement
    elsif (/\G ( [,;] ) /oxgc) {
        $slash = 'm//';

        # clear tr/// variable
        $tr_variable  = '';

        # clear s/// variable
        $sub_variable  = '';

        $bind_operator = '';

        return $1;
    }

# bareword
    elsif (/\G ( \{ \s* (?: tr|index|rindex|reverse) \s* \} ) /oxmsgc) {
        return $1;
    }

# $0 --> $0
    elsif (/\G ( \$ 0 ) /oxmsgc) {
        $slash = 'div';
        return $1;
    }
    elsif (/\G ( \$ \{ \s* 0 \s* \} ) /oxmsgc) {
        $slash = 'div';
        return $1;
    }

# $$ --> $$
    elsif (/\G ( \$ \$ ) (?![\w\{]) /oxmsgc) {
        $slash = 'div';
        return $1;
    }

# $1, $2, $3 --> $2, $3, $4 (only when multibyte anchoring is enable)
    elsif (/\G \$ ([1-9][0-9]*) /oxmsgc) {
        $slash = 'div';
        return e_capture($1);
    }
    elsif (/\G \$ \{ \s* ([1-9][0-9]*) \s* \} /oxmsgc) {
        $slash = 'div';
        return e_capture($1);
    }

# $$foo[ ... ] --> $ $foo->[ ... ]
    elsif (/\G \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \[ .+? \] ) /oxmsgc) {
        $slash = 'div';
        return e_capture($1.'->'.$2);
    }

# $$foo{ ... } --> $ $foo->{ ... }
    elsif (/\G \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \{ .+? \} ) /oxmsgc) {
        $slash = 'div';
        return e_capture($1.'->'.$2);
    }

# $$foo
    elsif (/\G \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) /oxmsgc) {
        $slash = 'div';
        return e_capture($1);
    }

# ${ foo }
    elsif (/\G \$ \s* \{ ( \s* [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* \s* ) \} /oxmsgc) {
        $slash = 'div';
        return '${' . $1 . '}';
    }

# ${ ... }
    elsif (/\G \$ \s* \{ \s* ( $qq_brace ) \s* \} /oxmsgc) {
        $slash = 'div';
        return e_capture($1);
    }

# variable or function
    #                  $ @ % & *     $ #
    elsif (/\G ( (?: [\$\@\%\&\*] | \$\# | -> | \b sub \b) \s* (?: split|chop|index|rindex|lc|uc|chr|ord|reverse|tr|y|q|qq|qx|qw|m|s|qr|glob|lstat|opendir|stat|unlink|chdir) ) \b /oxmsgc) {
        $slash = 'div';
        return $1;
    }
    #                $ $ $ $ $ $ $ $ $ $ $ $ $ $ $
    #                $ @ # \ ' " ` / ? ( ) [ ] < >
    elsif (/\G ( \$[\$\@\#\\\'\"\`\/\?\(\)\[\]\<\>] ) /oxmsgc) {
        $slash = 'div';
        return $1;
    }

# while (<FILEHANDLE>)
    elsif (/\G \b (while \s* \( \s* <[\$]?[A-Za-z_][A-Za-z_0-9]*> \s* \)) \b /oxgc) {
        return $1;
    }

# while (<WILDCARD>) --- glob

    # avoid "Error: Runtime exception" of perl version 5.005_03

    elsif (/\G \b while \s* \( \s* < ((?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE>\0\a\e\f\n\r\t])+?) > \s* \) \b /oxgc) {
        return 'while ($_ = Char::Ebig5plus::glob("' . $1 . '"))';
    }

# while (glob)
    elsif (/\G \b while \s* \( \s* glob \s* \) /oxgc) {
        return 'while ($_ = Char::Ebig5plus::glob_)';
    }

# while (glob(WILDCARD))
    elsif (/\G \b while \s* \( \s* glob \b /oxgc) {
        return 'while ($_ = Char::Ebig5plus::glob';
    }

# doit if, doit unless, doit while, doit until, doit for, doit when
    elsif (m{\G \b ( if | unless | while | until | for | when ) \b }oxgc) { $slash = 'm//'; return $1;  }

# functions of package Char::Ebig5plus
    elsif (m{\G \b (CORE::(?:split|chop|index|rindex|lc|uc|chr|ord|reverse|open|binmode)) \b }oxgc) { $slash = 'm//'; return $1; }
    elsif (m{\G \b bytes::substr \b (?! \s* => )                }oxgc) { $slash = 'm//'; return 'substr';              }
    elsif (m{\G \b chop \b          (?! \s* => )                }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::chop';         }
    elsif (m{\G \b bytes::index \b  (?! \s* => )                }oxgc) { $slash = 'm//'; return 'index';               }
    elsif (m{\G \b Char::Big5Plus::index \b   (?! \s* => )                }oxgc) { $slash = 'm//'; return 'Char::Big5Plus::index';         }
    elsif (m{\G \b index \b         (?! \s* => )                }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::index';        }
    elsif (m{\G \b bytes::rindex \b (?! \s* => )                }oxgc) { $slash = 'm//'; return 'rindex';              }
    elsif (m{\G \b Char::Big5Plus::rindex \b  (?! \s* => )                }oxgc) { $slash = 'm//'; return 'Char::Big5Plus::rindex';        }
    elsif (m{\G \b rindex \b        (?! \s* => )                }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::rindex';       }
    elsif (m{\G \b lc      (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::lc';           }
    elsif (m{\G \b lcfirst (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::lcfirst';      }
    elsif (m{\G \b uc      (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::uc';           }
    elsif (m{\G \b ucfirst (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::ucfirst';      }

    # stacked file test operators
    #
    # P.179 File Test Operators
    # in Chapter 12: File Tests
    # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition
    # (and so on)

    elsif (m{\G (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+)
                                                          \s* (\") ((?:$qq_char)+?)             (\") }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_qq('',  $2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\#) ((?:$qq_char)+?)             (\#) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\() ((?:$qq_paren)+?)            (\)) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\{) ((?:$qq_brace)+?)            (\}) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\[) ((?:$qq_bracket)+?)          (\]) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\<) ((?:$qq_angle)+?)            (\>) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\S) ((?:$qq_char)+?)             (\3) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; }

    elsif (m{\G (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+)
                                                          \s* (\') ((?:\\\1|\\\\|$q_char)+?)    (\') }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_q ('',  $2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\#) ((?:\\\#|\\\\|$q_char)+?)    (\#) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\() ((?:\\\)|\\\\|$q_paren)+?)   (\)) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\{) ((?:\\\}|\\\\|$q_brace)+?)   (\}) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\[) ((?:\\\]|\\\\|$q_bracket)+?) (\]) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\<) ((?:\\\>|\\\\|$q_angle)+?)   (\>) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\S) ((?:\\\1|\\\\|$q_char)+?)    (\3) }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; }

    elsif (m{\G (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+) (\$ \w+(?: ::\w+)* (?: (?: ->)? (?: \( (?:$qq_paren)*? \) | \{ (?:$qq_brace)+? \} | \[ (?:$qq_bracket)+? \] ) )*) }oxgc)
                                                                                                            { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1),$2)"; }
    elsif (m{\G (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+) \( ((?:$qq_paren)*?) \) }oxgc)
                                                                                                            { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1),$2)"; }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) (?= [a-z]+) }oxgc)                                   { $slash = 'm//'; return "Char::Ebig5plus::filetest qw($1),";    }
    elsif (m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) (\w+) }oxgc)                                         { $slash = 'm//'; return "Char::Ebig5plus::filetest(qw($1),$2)"; }

    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+    \s* (\") ((?:$qq_char)+?)             (\") }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_qq('',  $2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\#) ((?:$qq_char)+?)             (\#) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\() ((?:$qq_paren)+?)            (\)) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\{) ((?:$qq_brace)+?)            (\}) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\[) ((?:$qq_bracket)+?)          (\]) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\<) ((?:$qq_angle)+?)            (\>) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\S) ((?:$qq_char)+?)             (\3) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; }

    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+    \s* (\') ((?:\\\1|\\\\|$q_char)+?)    (\') }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_q ('',  $2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\#) ((?:\\\#|\\\\|$q_char)+?)    (\#) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\() ((?:\\\)|\\\\|$q_paren)+?)   (\)) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\{) ((?:\\\}|\\\\|$q_brace)+?)   (\}) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\[) ((?:\\\]|\\\\|$q_bracket)+?) (\]) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\<) ((?:\\\>|\\\\|$q_angle)+?)   (\>) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\S) ((?:\\\1|\\\\|$q_char)+?)    (\3) }oxgc)    { $slash = 'm//'; return "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; }

    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s* (\$ \w+(?: ::\w+)* (?: (?: ->)? (?: \( (?:$qq_paren)*? \) | \{ (?:$qq_brace)+? \} | \[ (?:$qq_bracket)+? \] ) )*) }oxgc)
                                                                                                            { $slash = 'm//'; return "Char::Ebig5plus::$1($2)";      }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s* \( ((?:$qq_paren)*?) \) }oxgc)                          { $slash = 'm//'; return "Char::Ebig5plus::$1($2)";      }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) (?= \s+ [a-z]+) }oxgc)                                      { $slash = 'm//'; return "Char::Ebig5plus::$1";          }
    elsif (m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ (\w+) }oxgc)                                            { $slash = 'm//'; return "Char::Ebig5plus::$1(::"."$2)"; }
    elsif (m{\G -(t)                            \s+ (\w+) }oxgc)                                            { $slash = 'm//'; return "-t $2";              }
    elsif (m{\G \b lstat         (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::lstat';             }
    elsif (m{\G \b stat          (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::stat';              }

    # "-s '' ..." means file test "-s 'filename' ..." (not means "- s/// ...")
    elsif (m{\G -s                               \s+    \s* (\") ((?:$qq_char)+?)             (\") }oxgc)    { $slash = 'm//'; return '-s ' . e_qq('',  $1,$3,$2); }
    elsif (m{\G -s                               \s+ qq \s* (\#) ((?:$qq_char)+?)             (\#) }oxgc)    { $slash = 'm//'; return '-s ' . e_qq('qq',$1,$3,$2); }
    elsif (m{\G -s                               \s+ qq \s* (\() ((?:$qq_paren)+?)            (\)) }oxgc)    { $slash = 'm//'; return '-s ' . e_qq('qq',$1,$3,$2); }
    elsif (m{\G -s                               \s+ qq \s* (\{) ((?:$qq_brace)+?)            (\}) }oxgc)    { $slash = 'm//'; return '-s ' . e_qq('qq',$1,$3,$2); }
    elsif (m{\G -s                               \s+ qq \s* (\[) ((?:$qq_bracket)+?)          (\]) }oxgc)    { $slash = 'm//'; return '-s ' . e_qq('qq',$1,$3,$2); }
    elsif (m{\G -s                               \s+ qq \s* (\<) ((?:$qq_angle)+?)            (\>) }oxgc)    { $slash = 'm//'; return '-s ' . e_qq('qq',$1,$3,$2); }
    elsif (m{\G -s                               \s+ qq \s* (\S) ((?:$qq_char)+?)             (\3) }oxgc)    { $slash = 'm//'; return '-s ' . e_qq('qq',$1,$3,$2); }

    elsif (m{\G -s                               \s+    \s* (\') ((?:\\\1|\\\\|$q_char)+?)    (\') }oxgc)    { $slash = 'm//'; return '-s ' . e_q ('',  $1,$3,$2); }
    elsif (m{\G -s                               \s+ q  \s* (\#) ((?:\\\#|\\\\|$q_char)+?)    (\#) }oxgc)    { $slash = 'm//'; return '-s ' . e_q ('q', $1,$3,$2); }
    elsif (m{\G -s                               \s+ q  \s* (\() ((?:\\\)|\\\\|$q_paren)+?)   (\)) }oxgc)    { $slash = 'm//'; return '-s ' . e_q ('q', $1,$3,$2); }
    elsif (m{\G -s                               \s+ q  \s* (\{) ((?:\\\}|\\\\|$q_brace)+?)   (\}) }oxgc)    { $slash = 'm//'; return '-s ' . e_q ('q', $1,$3,$2); }
    elsif (m{\G -s                               \s+ q  \s* (\[) ((?:\\\]|\\\\|$q_bracket)+?) (\]) }oxgc)    { $slash = 'm//'; return '-s ' . e_q ('q', $1,$3,$2); }
    elsif (m{\G -s                               \s+ q  \s* (\<) ((?:\\\>|\\\\|$q_angle)+?)   (\>) }oxgc)    { $slash = 'm//'; return '-s ' . e_q ('q', $1,$3,$2); }
    elsif (m{\G -s                               \s+ q  \s* (\S) ((?:\\\1|\\\\|$q_char)+?)    (\3) }oxgc)    { $slash = 'm//'; return '-s ' . e_q ('q', $1,$3,$2); }

    elsif (m{\G -s                               \s* (\$ \w+(?: ::\w+)* (?: (?: ->)? (?: \( (?:$qq_paren)*? \) | \{ (?:$qq_brace)+? \} | \[ (?:$qq_bracket)+? \] ) )*) }oxgc)
                                                                                                             { $slash = 'm//'; return "-s $1";   }
    elsif (m{\G -s                               \s* \( ((?:$qq_paren)*?) \) }oxgc)                          { $slash = 'm//'; return "-s ($1)"; }
    elsif (m{\G -s                               (?= \s+ [a-z]+) }oxgc)                                      { $slash = 'm//'; return '-s';      }
    elsif (m{\G -s                               \s+ (\w+) }oxgc)                                            { $slash = 'm//'; return "-s $1";   }

    elsif (m{\G \b bytes::length (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'length';                   }
    elsif (m{\G \b bytes::chr    (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'chr';                      }
    elsif (m{\G \b chr           (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::chr';               }
    elsif (m{\G \b bytes::ord    (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'div'; return 'ord';                      }
    elsif (m{\G \b ord           (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'div'; return $function_ord;              }
    elsif (m{\G \b glob          (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::glob';              }
    elsif (m{\G \b lc \b         (?! \s* => )                         }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::lc_';               }
    elsif (m{\G \b lcfirst \b    (?! \s* => )                         }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::lcfirst_';          }
    elsif (m{\G \b uc \b         (?! \s* => )                         }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::uc_';               }
    elsif (m{\G \b ucfirst \b    (?! \s* => )                         }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::ucfirst_';          }
    elsif (m{\G    (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+)
                           \b    (?! \s* => )                         }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::filetest_(qw($1))"; }
    elsif (m{\G    -([rwxoRWXOezsfdlpSbcugkTBMAC])
                           \b    (?! \s* => )                         }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::${1}_";             }
    elsif (m{\G \b lstat \b      (?! \s* => )                         }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::lstat_';            }
    elsif (m{\G \b stat \b       (?! \s* => )                         }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::stat_';             }
    elsif (m{\G    -s \b         (?! \s* => )                         }oxgc) { $slash = 'm//'; return '-s ';                      }

    elsif (m{\G \b bytes::length \b (?! \s* => )                      }oxgc) { $slash = 'm//'; return 'length';                   }
    elsif (m{\G \b bytes::chr \b    (?! \s* => )                      }oxgc) { $slash = 'm//'; return 'chr';                      }
    elsif (m{\G \b chr \b           (?! \s* => )                      }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::chr_';              }
    elsif (m{\G \b bytes::ord \b    (?! \s* => )                      }oxgc) { $slash = 'div'; return 'ord';                      }
    elsif (m{\G \b ord \b           (?! \s* => )                      }oxgc) { $slash = 'div'; return $function_ord_;             }
    elsif (m{\G \b glob \b          (?! \s* => )                      }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::glob_';             }
    elsif (m{\G \b reverse \b       (?! \s* => )                      }oxgc) { $slash = 'm//'; return $function_reverse;          }
    elsif (m{\G \b opendir (\s* \( \s*) (?=[A-Za-z_])                 }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::opendir$1*";        }
    elsif (m{\G \b opendir (\s+)        (?=[A-Za-z_])                 }oxgc) { $slash = 'm//'; return "Char::Ebig5plus::opendir$1*";        }
    elsif (m{\G \b unlink \b     (?! \s* => )                         }oxgc) { $slash = 'm//'; return 'Char::Ebig5plus::unlink';            }

# chdir
    elsif (m{\G \b (chdir) \b    (?! \s* => ) }oxgc) {
        $slash = 'm//';

        my $e = 'Char::Ebig5plus::chdir';

        while (/\G ( \s+ | \( | \#.* ) /oxgc) {
            $e .= $1;
        }

# end of chdir
        if    (/\G (?= [,;\)\}\]] )          /oxgc) { return $e;                 }

# chdir scalar value
        elsif (/\G ( [\$\@\&\*] $qq_scalar ) /oxgc) { return $e . e_string($1);  }

# chdir qq//
        elsif (/\G \b (qq) \b /oxgc) {
            if (/\G (\#) ((?:$qq_char)*?) (\#) /oxgc)                        { return $e . e_chdir('qq',$1,$3,$2);   } # qq# #  --> qr # #
            else {
                while (not /\G \z/oxgc) {
                    if    (/\G (\s+|\#.*)                             /oxgc) { $e .= $1; }
                    elsif (/\G (\()          ((?:$qq_paren)*?)   (\)) /oxgc) { return $e . e_chdir('qq',$1,$3,$2);   } # qq ( ) --> qr ( )
                    elsif (/\G (\{)          ((?:$qq_brace)*?)   (\}) /oxgc) { return $e . e_chdir('qq',$1,$3,$2);   } # qq { } --> qr { }
                    elsif (/\G (\[)          ((?:$qq_bracket)*?) (\]) /oxgc) { return $e . e_chdir('qq',$1,$3,$2);   } # qq [ ] --> qr [ ]
                    elsif (/\G (\<)          ((?:$qq_angle)*?)   (\>) /oxgc) { return $e . e_chdir('qq',$1,$3,$2);   } # qq < > --> qr < >
                    elsif (/\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) /oxgc) { return $e . e_chdir('qq','{','}',$2); } # qq | | --> qr { }
                    elsif (/\G (\S)          ((?:$qq_char)*?)    (\1) /oxgc) { return $e . e_chdir('qq',$1,$3,$2);   } # qq * * --> qr * *
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }
        }

# chdir q//
        elsif (/\G \b (q) \b /oxgc) {
            if (/\G (\#) ((?:\\\#|\\\\|$q_char)*?) (\#) /oxgc)                    { return $e . e_chdir_q('q',$1,$3,$2);   } # q# #  --> qr # #
            else {
                while (not /\G \z/oxgc) {
                    if    (/\G (\s+|\#.*)                                  /oxgc) { $e .= $1; }
                    elsif (/\G (\() ((?:\\\\|\\\)|\\\(|$q_paren)*?)   (\)) /oxgc) { return $e . e_chdir_q('q',$1,$3,$2);   } # q ( ) --> qr ( )
                    elsif (/\G (\{) ((?:\\\\|\\\}|\\\{|$q_brace)*?)   (\}) /oxgc) { return $e . e_chdir_q('q',$1,$3,$2);   } # q { } --> qr { }
                    elsif (/\G (\[) ((?:\\\\|\\\]|\\\[|$q_bracket)*?) (\]) /oxgc) { return $e . e_chdir_q('q',$1,$3,$2);   } # q [ ] --> qr [ ]
                    elsif (/\G (\<) ((?:\\\\|\\\>|\\\<|$q_angle)*?)   (\>) /oxgc) { return $e . e_chdir_q('q',$1,$3,$2);   } # q < > --> qr < >
                    elsif (/\G ([*\-:?\\^|])       ((?:$q_char)*?)    (\1) /oxgc) { return $e . e_chdir_q('q','{','}',$2); } # q | | --> qr { }
                    elsif (/\G (\S) ((?:\\\\|\\\1|     $q_char)*?)    (\1) /oxgc) { return $e . e_chdir_q('q',$1,$3,$2);   } # q * * --> qr * *
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }
        }

# chdir ''
        elsif (/\G (\') /oxgc) {
            my $q_string = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\\\\)    /oxgc) { $q_string .= $1; }
                elsif (/\G (\\\')    /oxgc) { $q_string .= $1; }
                elsif (/\G \'        /oxgc)                                       { return $e . e_chdir_q('',"'","'",$q_string); }
                elsif (/\G ($q_char) /oxgc) { $q_string .= $1; }
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }

# chdir ""
        elsif (/\G (\") /oxgc) {
            my $qq_string = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\\\\)    /oxgc) { $qq_string .= $1; }
                elsif (/\G (\\\")    /oxgc) { $qq_string .= $1; }
                elsif (/\G \"        /oxgc)                                       { return $e . e_chdir('','"','"',$qq_string); }
                elsif (/\G ($q_char) /oxgc) { $qq_string .= $1; }
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }
    }

# split
    elsif (m{\G \b (split) \b (?! \s* => ) }oxgc) {
        $slash = 'm//';

        my $e = 'Char::Ebig5plus::split';

        while (/\G ( \s+ | \( | \#.* ) /oxgc) {
            $e .= $1;
        }

# end of split
        if    (/\G (?= [,;\)\}\]] )          /oxgc) { return $e;                 }

# split scalar value
        elsif (/\G ( [\$\@\&\*] $qq_scalar ) /oxgc) { return $e . e_string($1);  }

# split literal space
        elsif (/\G \b qq       (\#) [ ] (\#) /oxgc) { return $e . qq  {qq$1 $2}; }
        elsif (/\G \b qq (\s*) (\() [ ] (\)) /oxgc) { return $e . qq{$1qq$2 $3}; }
        elsif (/\G \b qq (\s*) (\{) [ ] (\}) /oxgc) { return $e . qq{$1qq$2 $3}; }
        elsif (/\G \b qq (\s*) (\[) [ ] (\]) /oxgc) { return $e . qq{$1qq$2 $3}; }
        elsif (/\G \b qq (\s*) (\<) [ ] (\>) /oxgc) { return $e . qq{$1qq$2 $3}; }
        elsif (/\G \b qq (\s*) (\S) [ ] (\2) /oxgc) { return $e . qq{$1qq$2 $3}; }
        elsif (/\G \b q        (\#) [ ] (\#) /oxgc) { return $e . qq   {q$1 $2}; }
        elsif (/\G \b q  (\s*) (\() [ ] (\)) /oxgc) { return $e . qq {$1q$2 $3}; }
        elsif (/\G \b q  (\s*) (\{) [ ] (\}) /oxgc) { return $e . qq {$1q$2 $3}; }
        elsif (/\G \b q  (\s*) (\[) [ ] (\]) /oxgc) { return $e . qq {$1q$2 $3}; }
        elsif (/\G \b q  (\s*) (\<) [ ] (\>) /oxgc) { return $e . qq {$1q$2 $3}; }
        elsif (/\G \b q  (\s*) (\S) [ ] (\2) /oxgc) { return $e . qq {$1q$2 $3}; }
        elsif (/\G                ' [ ] '    /oxgc) { return $e . qq     {' '};  }
        elsif (/\G                " [ ] "    /oxgc) { return $e . qq     {" "};  }

# split qq//
        elsif (/\G \b (qq) \b /oxgc) {
            if (/\G (\#) ((?:$qq_char)*?) (\#) /oxgc)                        { return $e . e_split('qr',$1,$3,$2,'');   } # qq# #  --> qr # #
            else {
                while (not /\G \z/oxgc) {
                    if    (/\G (\s+|\#.*)                             /oxgc) { $e .= $1; }
                    elsif (/\G (\()          ((?:$qq_paren)*?)   (\)) /oxgc) { return $e . e_split('qr',$1,$3,$2,'');   } # qq ( ) --> qr ( )
                    elsif (/\G (\{)          ((?:$qq_brace)*?)   (\}) /oxgc) { return $e . e_split('qr',$1,$3,$2,'');   } # qq { } --> qr { }
                    elsif (/\G (\[)          ((?:$qq_bracket)*?) (\]) /oxgc) { return $e . e_split('qr',$1,$3,$2,'');   } # qq [ ] --> qr [ ]
                    elsif (/\G (\<)          ((?:$qq_angle)*?)   (\>) /oxgc) { return $e . e_split('qr',$1,$3,$2,'');   } # qq < > --> qr < >
                    elsif (/\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) /oxgc) { return $e . e_split('qr','{','}',$2,''); } # qq | | --> qr { }
                    elsif (/\G (\S)          ((?:$qq_char)*?)    (\1) /oxgc) { return $e . e_split('qr',$1,$3,$2,'');   } # qq * * --> qr * *
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }
        }

# split qr//
        elsif (/\G \b (qr) \b /oxgc) {
            if (/\G (\#) ((?:$qq_char)*?) (\#) ([imosxpadlu]*) /oxgc)                        { return $e . e_split  ('qr',$1,$3,$2,$4);   } # qr# #
            else {
                while (not /\G \z/oxgc) {
                    if    (/\G (\s+|\#.*)                                             /oxgc) { $e .= $1; }
                    elsif (/\G (\()          ((?:$qq_paren)*?)   (\)) ([imosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # qr ( )
                    elsif (/\G (\{)          ((?:$qq_brace)*?)   (\}) ([imosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # qr { }
                    elsif (/\G (\[)          ((?:$qq_bracket)*?) (\]) ([imosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # qr [ ]
                    elsif (/\G (\<)          ((?:$qq_angle)*?)   (\>) ([imosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # qr < >
                    elsif (/\G (\')          ((?:$qq_char)*?)    (\') ([imosxpadlu]*) /oxgc) { return $e . e_split_q('qr',$1, $3, $2,$4); } # qr ' '
                    elsif (/\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) ([imosxpadlu]*) /oxgc) { return $e . e_split  ('qr','{','}',$2,$4); } # qr | | --> qr { }
                    elsif (/\G (\S)          ((?:$qq_char)*?)    (\1) ([imosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # qr * *
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }
        }

# split q//
        elsif (/\G \b (q) \b /oxgc) {
            if (/\G (\#) ((?:\\\#|\\\\|$q_char)*?) (\#) /oxgc)                    { return $e . e_split_q('qr',$1,$3,$2,'');   } # q# #  --> qr # #
            else {
                while (not /\G \z/oxgc) {
                    if    (/\G (\s+|\#.*)                                  /oxgc) { $e .= $1; }
                    elsif (/\G (\() ((?:\\\\|\\\)|\\\(|$q_paren)*?)   (\)) /oxgc) { return $e . e_split_q('qr',$1,$3,$2,'');   } # q ( ) --> qr ( )
                    elsif (/\G (\{) ((?:\\\\|\\\}|\\\{|$q_brace)*?)   (\}) /oxgc) { return $e . e_split_q('qr',$1,$3,$2,'');   } # q { } --> qr { }
                    elsif (/\G (\[) ((?:\\\\|\\\]|\\\[|$q_bracket)*?) (\]) /oxgc) { return $e . e_split_q('qr',$1,$3,$2,'');   } # q [ ] --> qr [ ]
                    elsif (/\G (\<) ((?:\\\\|\\\>|\\\<|$q_angle)*?)   (\>) /oxgc) { return $e . e_split_q('qr',$1,$3,$2,'');   } # q < > --> qr < >
                    elsif (/\G ([*\-:?\\^|])       ((?:$q_char)*?)    (\1) /oxgc) { return $e . e_split_q('qr','{','}',$2,''); } # q | | --> qr { }
                    elsif (/\G (\S) ((?:\\\\|\\\1|     $q_char)*?)    (\1) /oxgc) { return $e . e_split_q('qr',$1,$3,$2,'');   } # q * * --> qr * *
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }
        }

# split m//
        elsif (/\G \b (m) \b /oxgc) {
            if (/\G (\#) ((?:$qq_char)*?) (\#) ([cgimosxpadlu]*) /oxgc)                        { return $e . e_split  ('qr',$1,$3,$2,$4);   } # m# #  --> qr # #
            else {
                while (not /\G \z/oxgc) {
                    if    (/\G (\s+|\#.*)                                               /oxgc) { $e .= $1; }
                    elsif (/\G (\()          ((?:$qq_paren)*?)   (\)) ([cgimosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # m ( ) --> qr ( )
                    elsif (/\G (\{)          ((?:$qq_brace)*?)   (\}) ([cgimosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # m { } --> qr { }
                    elsif (/\G (\[)          ((?:$qq_bracket)*?) (\]) ([cgimosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # m [ ] --> qr [ ]
                    elsif (/\G (\<)          ((?:$qq_angle)*?)   (\>) ([cgimosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # m < > --> qr < >
                    elsif (/\G (\')          ((?:$qq_char)*?)    (\') ([cgimosxpadlu]*) /oxgc) { return $e . e_split_q('qr',$1, $3, $2,$4); } # m ' ' --> qr ' '
                    elsif (/\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) ([cgimosxpadlu]*) /oxgc) { return $e . e_split  ('qr','{','}',$2,$4); } # m | | --> qr { }
                    elsif (/\G (\S)          ((?:$qq_char)*?)    (\1) ([cgimosxpadlu]*) /oxgc) { return $e . e_split  ('qr',$1, $3, $2,$4); } # m * * --> qr * *
                }
                die "$__FILE__: Search pattern not terminated";
            }
        }

# split ''
        elsif (/\G (\') /oxgc) {
            my $q_string = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\\\\)    /oxgc) { $q_string .= $1; }
                elsif (/\G (\\\')    /oxgc) { $q_string .= $1; }                               # splitqr'' --> split qr''
                elsif (/\G \'        /oxgc)                                                    { return $e . e_split_q(q{ qr},"'","'",$q_string,''); } # ' ' --> qr ' '
                elsif (/\G ($q_char) /oxgc) { $q_string .= $1; }
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }

# split ""
        elsif (/\G (\") /oxgc) {
            my $qq_string = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\\\\)    /oxgc) { $qq_string .= $1; }
                elsif (/\G (\\\")    /oxgc) { $qq_string .= $1; }                              # splitqr"" --> split qr""
                elsif (/\G \"        /oxgc)                                                    { return $e . e_split(q{ qr},'"','"',$qq_string,''); } # " " --> qr " "
                elsif (/\G ($q_char) /oxgc) { $qq_string .= $1; }
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }

# split //
        elsif (/\G (\/) /oxgc) {
            my $regexp = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\\\\)               /oxgc) { $regexp .= $1; }
                elsif (/\G (\\\/)               /oxgc) { $regexp .= $1; }                      # splitqr// --> split qr//
                elsif (/\G \/ ([cgimosxpadlu]*) /oxgc)                                         { return $e . e_split(q{ qr}, '/','/',$regexp,$1); } # / / --> qr / /
                elsif (/\G ($q_char)            /oxgc) { $regexp .= $1; }
            }
            die "$__FILE__: Search pattern not terminated";
        }
    }

# tr/// or y///

    # about [cdsrbB]* (/B modifier)
    #
    # P.559 appendix C
    # of ISBN 4-89052-384-7 Programming perl
    # (Japanese title is: Perl puroguramingu)

    elsif (/\G \b (tr|y) \b /oxgc) {
        my $ope = $1;

        #        $1   $2               $3   $4               $5   $6
        if (/\G (\#) ((?:$qq_char)*?) (\#) ((?:$qq_char)*?) (\#) ([cdsrbB]*) /oxgc) { # tr# # #
            my @tr = ($tr_variable,$2);
            return e_tr(@tr,'',$4,$6);
        }
        else {
            my $e = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\s+|\#.*)                  /oxgc) { $e .= $1; }
                elsif (/\G (\() ((?:$qq_paren)*?) (\)) /oxgc) {
                    my @tr = ($tr_variable,$2);
                    while (not /\G \z/oxgc) {
                        if    (/\G (\s+|\#.*)                                /oxgc) { $e .= $1; }
                        elsif (/\G (\() ((?:$qq_paren)*?)   (\)) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr ( ) ( )
                        elsif (/\G (\{) ((?:$qq_brace)*?)   (\}) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr ( ) { }
                        elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr ( ) [ ]
                        elsif (/\G (\<) ((?:$qq_angle)*?)   (\>) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr ( ) < >
                        elsif (/\G (\S) ((?:$qq_char)*?)    (\1) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr ( ) * *
                    }
                    die "$__FILE__: Transliteration replacement not terminated";
                }
                elsif (/\G (\{) ((?:$qq_brace)*?) (\}) /oxgc) {
                    my @tr = ($tr_variable,$2);
                    while (not /\G \z/oxgc) {
                        if    (/\G (\s+|\#.*)                                /oxgc) { $e .= $1; }
                        elsif (/\G (\() ((?:$qq_paren)*?)   (\)) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr { } ( )
                        elsif (/\G (\{) ((?:$qq_brace)*?)   (\}) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr { } { }
                        elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr { } [ ]
                        elsif (/\G (\<) ((?:$qq_angle)*?)   (\>) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr { } < >
                        elsif (/\G (\S) ((?:$qq_char)*?)    (\1) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr { } * *
                    }
                    die "$__FILE__: Transliteration replacement not terminated";
                }
                elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) /oxgc) {
                    my @tr = ($tr_variable,$2);
                    while (not /\G \z/oxgc) {
                        if    (/\G (\s+|\#.*)                                /oxgc) { $e .= $1; }
                        elsif (/\G (\() ((?:$qq_paren)*?)   (\)) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr [ ] ( )
                        elsif (/\G (\{) ((?:$qq_brace)*?)   (\}) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr [ ] { }
                        elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr [ ] [ ]
                        elsif (/\G (\<) ((?:$qq_angle)*?)   (\>) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr [ ] < >
                        elsif (/\G (\S) ((?:$qq_char)*?)    (\1) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr [ ] * *
                    }
                    die "$__FILE__: Transliteration replacement not terminated";
                }
                elsif (/\G (\<) ((?:$qq_angle)*?) (\>) /oxgc) {
                    my @tr = ($tr_variable,$2);
                    while (not /\G \z/oxgc) {
                        if    (/\G (\s+|\#.*)                                /oxgc) { $e .= $1; }
                        elsif (/\G (\() ((?:$qq_paren)*?)   (\)) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr < > ( )
                        elsif (/\G (\{) ((?:$qq_brace)*?)   (\}) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr < > { }
                        elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr < > [ ]
                        elsif (/\G (\<) ((?:$qq_angle)*?)   (\>) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr < > < >
                        elsif (/\G (\S) ((?:$qq_char)*?)    (\1) ([cdsrbB]*) /oxgc) { return e_tr(@tr,$e,$2,$4); } # tr < > * *
                    }
                    die "$__FILE__: Transliteration replacement not terminated";
                }
                #           $1   $2               $3   $4               $5   $6
                elsif (/\G (\S) ((?:$qq_char)*?) (\1) ((?:$qq_char)*?) (\1) ([cdsrbB]*) /oxgc) { # tr * * *
                    my @tr = ($tr_variable,$2);
                    return e_tr(@tr,'',$4,$6);
                }
            }
            die "$__FILE__: Transliteration pattern not terminated";
        }
    }

# qq//
    elsif (/\G \b (qq) \b /oxgc) {
        my $ope = $1;

#       if (/\G (\#) ((?:$qq_char)*?) (\#) /oxgc) { return e_qq($ope,$1,$3,$2); } # qq# #
        if (/\G (\#) /oxgc) {                                                     # qq# #
            my $qq_string = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\\\\)     /oxgc) { $qq_string .= $1;                     }
                elsif (/\G (\\\#)     /oxgc) { $qq_string .= $1;                     }
                elsif (/\G (\#)       /oxgc) { return e_qq($ope,'#','#',$qq_string); }
                elsif (/\G ($qq_char) /oxgc) { $qq_string .= $1;                     }
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }

        else {
            my $e = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\s+|\#.*)                  /oxgc) { $e .= $1; }

#               elsif (/\G (\() ((?:$qq_paren)*?) (\)) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qq ( )
                elsif (/\G (\() /oxgc) {                                                           # qq ( )
                    my $qq_string = '';
                    local $nest = 1;
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)     /oxgc) { $qq_string .= $1;                          }
                        elsif (/\G (\\\))     /oxgc) { $qq_string .= $1;                          }
                        elsif (/\G (\()       /oxgc) { $qq_string .= $1; $nest++;                 }
                        elsif (/\G (\))       /oxgc) {
                            if (--$nest == 0)        { return $e . e_qq($ope,'(',')',$qq_string); }
                            else                     { $qq_string .= $1;                          }
                        }
                        elsif (/\G ($qq_char) /oxgc) { $qq_string .= $1;                          }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }

#               elsif (/\G (\{) ((?:$qq_brace)*?) (\}) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qq { }
                elsif (/\G (\{) /oxgc) {                                                           # qq { }
                    my $qq_string = '';
                    local $nest = 1;
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)     /oxgc) { $qq_string .= $1;                          }
                        elsif (/\G (\\\})     /oxgc) { $qq_string .= $1;                          }
                        elsif (/\G (\{)       /oxgc) { $qq_string .= $1; $nest++;                 }
                        elsif (/\G (\})       /oxgc) {
                            if (--$nest == 0)        { return $e . e_qq($ope,'{','}',$qq_string); }
                            else                     { $qq_string .= $1;                          }
                        }
                        elsif (/\G ($qq_char) /oxgc) { $qq_string .= $1;                          }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }

#               elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qq [ ]
                elsif (/\G (\[) /oxgc) {                                                             # qq [ ]
                    my $qq_string = '';
                    local $nest = 1;
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)     /oxgc) { $qq_string .= $1;                          }
                        elsif (/\G (\\\])     /oxgc) { $qq_string .= $1;                          }
                        elsif (/\G (\[)       /oxgc) { $qq_string .= $1; $nest++;                 }
                        elsif (/\G (\])       /oxgc) {
                            if (--$nest == 0)        { return $e . e_qq($ope,'[',']',$qq_string); }
                            else                     { $qq_string .= $1;                          }
                        }
                        elsif (/\G ($qq_char) /oxgc) { $qq_string .= $1;                          }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }

#               elsif (/\G (\<) ((?:$qq_angle)*?) (\>) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qq < >
                elsif (/\G (\<) /oxgc) {                                                           # qq < >
                    my $qq_string = '';
                    local $nest = 1;
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)     /oxgc) { $qq_string .= $1;                          }
                        elsif (/\G (\\\>)     /oxgc) { $qq_string .= $1;                          }
                        elsif (/\G (\<)       /oxgc) { $qq_string .= $1; $nest++;                 }
                        elsif (/\G (\>)       /oxgc) {
                            if (--$nest == 0)        { return $e . e_qq($ope,'<','>',$qq_string); }
                            else                     { $qq_string .= $1;                          }
                        }
                        elsif (/\G ($qq_char) /oxgc) { $qq_string .= $1;                          }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }

#               elsif (/\G (\S) ((?:$qq_char)*?) (\1) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qq * *
                elsif (/\G (\S) /oxgc) {                                                          # qq * *
                    my $delimiter = $1;
                    my $qq_string = '';
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)             /oxgc) { $qq_string .= $1;                                        }
                        elsif (/\G (\\\Q$delimiter\E) /oxgc) { $qq_string .= $1;                                        }
                        elsif (/\G (\Q$delimiter\E)   /oxgc) { return $e . e_qq($ope,$delimiter,$delimiter,$qq_string); }
                        elsif (/\G ($qq_char)         /oxgc) { $qq_string .= $1;                                        }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }
    }

# qr//
    elsif (/\G \b (qr) \b /oxgc) {
        my $ope = $1;
        if (/\G (\#) ((?:$qq_char)*?) (\#) ([imosxpadlu]*) /oxgc) { # qr# # #
            return e_qr($ope,$1,$3,$2,$4);
        }
        else {
            my $e = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\s+|\#.*)                                             /oxgc) { $e .= $1; }
                elsif (/\G (\()          ((?:$qq_paren)*?)   (\)) ([imosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # qr ( )
                elsif (/\G (\{)          ((?:$qq_brace)*?)   (\}) ([imosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # qr { }
                elsif (/\G (\[)          ((?:$qq_bracket)*?) (\]) ([imosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # qr [ ]
                elsif (/\G (\<)          ((?:$qq_angle)*?)   (\>) ([imosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # qr < >
                elsif (/\G (\')          ((?:$qq_char)*?)    (\') ([imosxpadlu]*) /oxgc) { return $e . e_qr_q($ope,$1, $3, $2,$4); } # qr ' '
                elsif (/\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) ([imosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,'{','}',$2,$4); } # qr | | --> qr { }
                elsif (/\G (\S)          ((?:$qq_char)*?)    (\1) ([imosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # qr * *
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }
    }

# qw//
    elsif (/\G \b (qw) \b /oxgc) {
        my $ope = $1;
        if (/\G (\#) (.*?) (\#) /oxmsgc) { # qw# #
            return e_qw($ope,$1,$3,$2);
        }
        else {
            my $e = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\s+|\#.*)                            /oxgc)   { $e .= $1; }

                elsif (/\G (\()          ([^(]*?)           (\)) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw ( )
                elsif (/\G (\()          ((?:$q_paren)*?)   (\)) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw ( )

                elsif (/\G (\{)          ([^{]*?)           (\}) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw { }
                elsif (/\G (\{)          ((?:$q_brace)*?)   (\}) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw { }

                elsif (/\G (\[)          ([^[]*?)           (\]) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw [ ]
                elsif (/\G (\[)          ((?:$q_bracket)*?) (\]) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw [ ]

                elsif (/\G (\<)          ([^<]*?)           (\>) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw < >
                elsif (/\G (\<)          ((?:$q_angle)*?)   (\>) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw < >

                elsif (/\G ([\x21-\x3F]) (.*?)              (\1) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw * *
                elsif (/\G (\S)          ((?:$q_char)*?)    (\1) /oxmsgc) { return $e . e_qw($ope,$1,$3,$2); } # qw * *
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }
    }

# qx//
    elsif (/\G \b (qx) \b /oxgc) {
        my $ope = $1;
        if (/\G (\#) ((?:$qq_char)*?) (\#) /oxgc) { # qx# #
            return e_qq($ope,$1,$3,$2);
        }
        else {
            my $e = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\s+|\#.*)                    /oxgc) { $e .= $1; }
                elsif (/\G (\() ((?:$qq_paren)*?)   (\)) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qx ( )
                elsif (/\G (\{) ((?:$qq_brace)*?)   (\}) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qx { }
                elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qx [ ]
                elsif (/\G (\<) ((?:$qq_angle)*?)   (\>) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qx < >
                elsif (/\G (\') ((?:$qq_char)*?)    (\') /oxgc) { return $e . e_q ($ope,$1,$3,$2); } # qx ' '
                elsif (/\G (\S) ((?:$qq_char)*?)    (\1) /oxgc) { return $e . e_qq($ope,$1,$3,$2); } # qx * *
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }
    }

# q//
    elsif (/\G \b (q) \b /oxgc) {
        my $ope = $1;

#       if (/\G (\#) ((?:\\\#|\\\\|$q_char)*?) (\#) /oxgc) { return e_q($ope,$1,$3,$2); } # q# #

        # avoid "Error: Runtime exception" of perl version 5.005_03
        # (and so on)

        if (/\G (\#) /oxgc) {                                                             # q# #
            my $q_string = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\\\\)    /oxgc) { $q_string .= $1;                    }
                elsif (/\G (\\\#)    /oxgc) { $q_string .= $1;                    }
                elsif (/\G (\#)      /oxgc) { return e_q($ope,'#','#',$q_string); }
                elsif (/\G ($q_char) /oxgc) { $q_string .= $1;                    }
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }

        else {
            my $e = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\s+|\#.*)                           /oxgc) { $e .= $1; }

#               elsif (/\G (\() ((?:\\\)|\\\\|$q_paren)*?) (\)) /oxgc) { return $e . e_q($ope,$1,$3,$2); } # q ( )
                elsif (/\G (\() /oxgc) {                                                                   # q ( )
                    my $q_string = '';
                    local $nest = 1;
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\\\))    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\\\()    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\()      /oxgc) { $q_string .= $1; $nest++;                }
                        elsif (/\G (\))      /oxgc) {
                            if (--$nest == 0)       { return $e . e_q($ope,'(',')',$q_string); }
                            else                    { $q_string .= $1;                         }
                        }
                        elsif (/\G ($q_char) /oxgc) { $q_string .= $1;                         }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }

#               elsif (/\G (\{) ((?:\\\}|\\\\|$q_brace)*?) (\}) /oxgc) { return $e . e_q($ope,$1,$3,$2); } # q { }
                elsif (/\G (\{) /oxgc) {                                                                   # q { }
                    my $q_string = '';
                    local $nest = 1;
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\\\})    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\\\{)    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\{)      /oxgc) { $q_string .= $1; $nest++;                }
                        elsif (/\G (\})      /oxgc) {
                            if (--$nest == 0)       { return $e . e_q($ope,'{','}',$q_string); }
                            else                    { $q_string .= $1;                         }
                        }
                        elsif (/\G ($q_char) /oxgc) { $q_string .= $1;                         }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }

#               elsif (/\G (\[) ((?:\\\]|\\\\|$q_bracket)*?) (\]) /oxgc) { return $e . e_q($ope,$1,$3,$2); } # q [ ]
                elsif (/\G (\[) /oxgc) {                                                                     # q [ ]
                    my $q_string = '';
                    local $nest = 1;
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\\\])    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\\\[)    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\[)      /oxgc) { $q_string .= $1; $nest++;                }
                        elsif (/\G (\])      /oxgc) {
                            if (--$nest == 0)       { return $e . e_q($ope,'[',']',$q_string); }
                            else                    { $q_string .= $1;                         }
                        }
                        elsif (/\G ($q_char) /oxgc) { $q_string .= $1;                         }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }

#               elsif (/\G (\<) ((?:\\\>|\\\\|$q_angle)*?) (\>) /oxgc) { return $e . e_q($ope,$1,$3,$2); } # q < >
                elsif (/\G (\<) /oxgc) {                                                                   # q < >
                    my $q_string = '';
                    local $nest = 1;
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\\\>)    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\\\<)    /oxgc) { $q_string .= $1;                         }
                        elsif (/\G (\<)      /oxgc) { $q_string .= $1; $nest++;                }
                        elsif (/\G (\>)      /oxgc) {
                            if (--$nest == 0)       { return $e . e_q($ope,'<','>',$q_string); }
                            else                    { $q_string .= $1;                         }
                        }
                        elsif (/\G ($q_char) /oxgc) { $q_string .= $1;                         }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }

#               elsif (/\G (\S) ((?:\\\1|\\\\|$q_char)*?) (\1) /oxgc) { return $e . e_q($ope,$1,$3,$2); } # q * *
                elsif (/\G (\S) /oxgc) {                                                                  # q * *
                    my $delimiter = $1;
                    my $q_string = '';
                    while (not /\G \z/oxgc) {
                        if    (/\G (\\\\)             /oxgc) { $q_string .= $1;                                       }
                        elsif (/\G (\\\Q$delimiter\E) /oxgc) { $q_string .= $1;                                       }
                        elsif (/\G (\Q$delimiter\E)   /oxgc) { return $e . e_q($ope,$delimiter,$delimiter,$q_string); }
                        elsif (/\G ($q_char)          /oxgc) { $q_string .= $1;                                       }
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }
            }
            die "$__FILE__: Can't find string terminator anywhere before EOF";
        }
    }

# m//
    elsif (/\G \b (m) \b /oxgc) {
        my $ope = $1;
        if (/\G (\#) ((?:$qq_char)*?) (\#) ([cgimosxpadlu]*) /oxgc) { # m# #
            return e_qr($ope,$1,$3,$2,$4);
        }
        else {
            my $e = '';
            while (not /\G \z/oxgc) {
                if    (/\G (\s+|\#.*)                                               /oxgc) { $e .= $1; }
                elsif (/\G (\()          ((?:$qq_paren)*?)   (\)) ([cgimosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # m ( )
                elsif (/\G (\{)          ((?:$qq_brace)*?)   (\}) ([cgimosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # m { }
                elsif (/\G (\[)          ((?:$qq_bracket)*?) (\]) ([cgimosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # m [ ]
                elsif (/\G (\<)          ((?:$qq_angle)*?)   (\>) ([cgimosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # m < >
                elsif (/\G (\')          ((?:$qq_char)*?)    (\') ([cgimosxpadlu]*) /oxgc) { return $e . e_qr_q($ope,$1, $3, $2,$4); } # m ' '
                elsif (/\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) ([cgimosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,'{','}',$2,$4); } # m | | --> m { }
                elsif (/\G (\S)          ((?:$qq_char)*?)    (\1) ([cgimosxpadlu]*) /oxgc) { return $e . e_qr  ($ope,$1, $3, $2,$4); } # m * *
            }
            die "$__FILE__: Search pattern not terminated";
        }
    }

# s///

    # about [cegimosxpradlu]* (/cg modifier)
    #
    # P.67 Pattern-Matching Operators
    # of ISBN 0-596-00241-6 Perl in a Nutshell, Second Edition.

    elsif (/\G \b (s) \b /oxgc) {
        my $ope = $1;

        #        $1   $2               $3   $4               $5   $6
        if (/\G (\#) ((?:$qq_char)*?) (\#) ((?:$qq_char)*?) (\#) ([cegimosxpradlu]*) /oxgc) { # s# # #
            return e_sub($sub_variable,$1,$2,$3,$3,$4,$5,$6);
        }
        else {
            my $e = '';
            while (not /\G \z/oxgc) {
                if (/\G (\s+|\#.*) /oxgc) { $e .= $1; }
                elsif (/\G (\() ((?:$qq_paren)*?) (\)) /oxgc) {
                    my @s = ($1,$2,$3);
                    while (not /\G \z/oxgc) {
                        if    (/\G (\s+|\#.*)                                        /oxgc) { $e .= $1; }
                        #           $1   $2                  $3   $4
                        elsif (/\G (\() ((?:$qq_paren)*?)   (\)) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\{) ((?:$qq_brace)*?)   (\}) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\<) ((?:$qq_angle)*?)   (\>) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\') ((?:$qq_char)*?)    (\') ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\$) ((?:$qq_char)*?)    (\$) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\:) ((?:$qq_char)*?)    (\:) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\@) ((?:$qq_char)*?)    (\@) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\S) ((?:$qq_char)*?)    (\1) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                    }
                    die "$__FILE__: Substitution replacement not terminated";
                }
                elsif (/\G (\{) ((?:$qq_brace)*?) (\}) /oxgc) {
                    my @s = ($1,$2,$3);
                    while (not /\G \z/oxgc) {
                        if    (/\G (\s+|\#.*)                                        /oxgc) { $e .= $1; }
                        #           $1   $2                  $3   $4
                        elsif (/\G (\() ((?:$qq_paren)*?)   (\)) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\{) ((?:$qq_brace)*?)   (\}) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\<) ((?:$qq_angle)*?)   (\>) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\') ((?:$qq_char)*?)    (\') ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\$) ((?:$qq_char)*?)    (\$) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\:) ((?:$qq_char)*?)    (\:) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\@) ((?:$qq_char)*?)    (\@) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\S) ((?:$qq_char)*?)    (\1) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                    }
                    die "$__FILE__: Substitution replacement not terminated";
                }
                elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) /oxgc) {
                    my @s = ($1,$2,$3);
                    while (not /\G \z/oxgc) {
                        if    (/\G (\s+|\#.*)                                        /oxgc) { $e .= $1; }
                        #           $1   $2                  $3   $4
                        elsif (/\G (\() ((?:$qq_paren)*?)   (\)) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\{) ((?:$qq_brace)*?)   (\}) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\<) ((?:$qq_angle)*?)   (\>) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\') ((?:$qq_char)*?)    (\') ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\$) ((?:$qq_char)*?)    (\$) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\S) ((?:$qq_char)*?)    (\1) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                    }
                    die "$__FILE__: Substitution replacement not terminated";
                }
                elsif (/\G (\<) ((?:$qq_angle)*?) (\>) /oxgc) {
                    my @s = ($1,$2,$3);
                    while (not /\G \z/oxgc) {
                        if    (/\G (\s+|\#.*)                                        /oxgc) { $e .= $1; }
                        #           $1   $2                  $3   $4
                        elsif (/\G (\() ((?:$qq_paren)*?)   (\)) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\{) ((?:$qq_brace)*?)   (\}) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\[) ((?:$qq_bracket)*?) (\]) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\<) ((?:$qq_angle)*?)   (\>) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\') ((?:$qq_char)*?)    (\') ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\$) ((?:$qq_char)*?)    (\$) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\:) ((?:$qq_char)*?)    (\:) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\@) ((?:$qq_char)*?)    (\@) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                        elsif (/\G (\S) ((?:$qq_char)*?)    (\1) ([cegimosxpradlu]*) /oxgc) { return e_sub($sub_variable,@s,$1,$2,$3,$4); }
                    }
                    die "$__FILE__: Substitution replacement not terminated";
                }
                #           $1   $2               $3   $4               $5   $6
                elsif (/\G (\') ((?:$qq_char)*?) (\') ((?:$qq_char)*?) (\') ([cegimosxpradlu]*) /oxgc) {
                    return e_sub($sub_variable,$1,$2,$3,$3,$4,$5,$6);
                }
                #           $1            $2               $3   $4               $5   $6
                elsif (/\G ([*\-:?\\^|]) ((?:$qq_char)*?) (\1) ((?:$qq_char)*?) (\1) ([cegimosxpradlu]*) /oxgc) {
                    return e_sub($sub_variable,'{',$2,'}','{',$4,'}',$6); # s | | | --> s { } { }
                }
                #           $1   $2               $3   $4               $5   $6
                elsif (/\G (\$) ((?:$qq_char)*?) (\1) ((?:$qq_char)*?) (\1) ([cegimosxpradlu]*) /oxgc) {
                    return e_sub($sub_variable,$1,$2,$3,$3,$4,$5,$6);
                }
                #           $1   $2               $3   $4               $5   $6
                elsif (/\G (\S) ((?:$qq_char)*?) (\1) ((?:$qq_char)*?) (\1) ([cegimosxpradlu]*) /oxgc) {
                    return e_sub($sub_variable,$1,$2,$3,$3,$4,$5,$6);
                }
            }
            die "$__FILE__: Substitution pattern not terminated";
        }
    }

# do
    elsif (/\G \b do (?= \s* \{ )                    /oxmsgc)                                  { return 'do';                }
    elsif (/\G \b do (?= \s+ (?: q|qq|qx) \b)        /oxmsgc)                                  { return 'Char::Ebig5plus::do';         }
    elsif (/\G \b do (?= \s+ \w+)                    /oxmsgc)                                  { return 'do';                }
    elsif (/\G \b do (?= \s* \$ \w+ (?: ::\w+)* \( ) /oxmsgc)                                  { return 'do';                }
    elsif (/\G \b do \b                              /oxmsgc)                                  { return 'Char::Ebig5plus::do';         }

# require ignore module
    elsif (/\G \b require (\s+ (?:$ignore_modules) .*? ;) ([ \t]* [#\n])                  /oxmsgc) { return "# require$1$2";   }
    elsif (/\G \b require (\s+ (?:$ignore_modules) .*? ;) ([ \t]* [^\x81-\xFE#]) /oxmsgc) { return "# require$1\n$2"; }
    elsif (/\G \b require (\s+ (?:$ignore_modules)) \b                                    /oxmsgc) { return "# require$1";     }

# require
    elsif (/\G \b require \s+ (v? [0-9]+(?: [._][0-9]+)*) \s* ; /oxmsgc)                       { return "require $1;";       }
    elsif (/\G \b require \s+ (\w+(?: ::\w+)*)            \s* ; /oxmsgc)                       { return e_require($1);       }
    elsif (/\G \b require                                 \s* ; /oxmsgc)                       { return 'Char::Ebig5plus::require;';   }
    elsif (/\G \b require \b                                    /oxmsgc)                       { return 'Char::Ebig5plus::require';    }

# use strict; --> use strict; no strict qw(refs);
    elsif (/\G \b use (\s+ strict .*? ;) ([ \t]* [#\n])                  /oxmsgc)              { return "use$1 no strict qw(refs);$2";   }
    elsif (/\G \b use (\s+ strict .*? ;) ([ \t]* [^\x81-\xFE#]) /oxmsgc)              { return "use$1 no strict qw(refs);\n$2"; }
    elsif (/\G \b use (\s+ strict) \b                                    /oxmsgc)              { return "use$1; no strict qw(refs)";     }

# use 5.12.0; --> use 5.12.0; no strict qw(refs);
    elsif (/\G \b use (\s+ v?5\.([0-9]{1,3})(?:\.[0-9]+)? .*? ;)         /oxmsgc)              {
        if ($2 >= 12) {
            return "use$1 no strict qw(refs);";
        }
        else {
            return "use$1";
        }
    }

# ignore use module
    elsif (/\G \b use (\s+ (?:$ignore_modules) .*? ;) ([ \t]* [#\n])                  /oxmsgc) { return "# use$1$2";         }
    elsif (/\G \b use (\s+ (?:$ignore_modules) .*? ;) ([ \t]* [^\x81-\xFE#]) /oxmsgc) { return "# use$1\n$2";       }
    elsif (/\G \b use (\s+ (?:$ignore_modules)) \b                                    /oxmsgc) { return "# use$1";           }

# use without import
    elsif (/\G \b use \s+ (v? [0-9]+(?: [._][0-9]+)*)                           \s* ; /oxmsgc) { return "use $1;";           }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s*        (\()          \s* \) \s* ; /oxmsgc) { return e_use_noimport($1);  }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\()          \s* \) \s* ; /oxmsgc) { return e_use_noimport($1);  }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\{)          \s* \} \s* ; /oxmsgc) { return e_use_noimport($1);  }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\[)          \s* \] \s* ; /oxmsgc) { return e_use_noimport($1);  }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\<)          \s* \> \s* ; /oxmsgc) { return e_use_noimport($1);  }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* ([\x21-\x3F]) \s* \2 \s* ; /oxmsgc) { return e_use_noimport($1);  }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\S)          \s* \2 \s* ; /oxmsgc) { return e_use_noimport($1);  }

# ignore no module
    elsif (/\G \b no  (\s+ (?:$ignore_modules) .*? ;) ([ \t]* [#\n])                  /oxmsgc) { return "# no$1$2";          }
    elsif (/\G \b no  (\s+ (?:$ignore_modules) .*? ;) ([ \t]* [^\x81-\xFE#]) /oxmsgc) { return "# no$1\n$2";        }
    elsif (/\G \b no  (\s+ (?:$ignore_modules)) \b                                    /oxmsgc) { return "# no$1";            }

# no without unimport
    elsif (/\G \b no  \s+ (v? [0-9]+(?: [._][0-9]+)*)                           \s* ; /oxmsgc) { return "no $1;";            }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s*        (\()          \s* \) \s* ; /oxmsgc) { return e_no_nounimport($1); }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\()          \s* \) \s* ; /oxmsgc) { return e_no_nounimport($1); }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\{)          \s* \} \s* ; /oxmsgc) { return e_no_nounimport($1); }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\[)          \s* \] \s* ; /oxmsgc) { return e_no_nounimport($1); }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\<)          \s* \> \s* ; /oxmsgc) { return e_no_nounimport($1); }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* ([\x21-\x3F]) \s* \2 \s* ; /oxmsgc) { return e_no_nounimport($1); }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ qw \s* (\S)          \s* \2 \s* ; /oxmsgc) { return e_no_nounimport($1); }

# use with import no parameter
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*)                                 \s* ; /oxmsgc) { return e_use_noparam($1);   }

# no with unimport no parameter
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*)                                 \s* ; /oxmsgc) { return e_no_noparam($1);    }

# use with import parameters
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s* (                (\()          [^\x81-\xFE)]* \)) \s* ; /oxmsgc) { return e_use($1,$2); }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s* (                (\')          [^\x81-\xFE']* \') \s* ; /oxmsgc) { return e_use($1,$2); }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s* (                (\")          [^\x81-\xFE"]* \") \s* ; /oxmsgc) { return e_use($1,$2); }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\()          [^\x81-\xFE)]* \)) \s* ; /oxmsgc) { return e_use($1,$2); }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\{)          (?:$q_char)*?           \}) \s* ; /oxmsgc) { return e_use($1,$2); }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\[)          (?:$q_char)*?           \]) \s* ; /oxmsgc) { return e_use($1,$2); }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\<)          [^\x81-\xFE>]* \>) \s* ; /oxmsgc) { return e_use($1,$2); }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* ([\x21-\x3F]) .*?                     \3) \s* ; /oxmsgc) { return e_use($1,$2); }
    elsif (/\G \b use \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\S)          (?:$q_char)*?           \3) \s* ; /oxmsgc) { return e_use($1,$2); }

# no with unimport parameters
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s* (                (\()          [^\x81-\xFE)]* \)) \s* ; /oxmsgc) { return e_no($1,$2);  }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s* (                (\')          [^\x81-\xFE']* \') \s* ; /oxmsgc) { return e_no($1,$2);  }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s* (                (\")          [^\x81-\xFE"]* \") \s* ; /oxmsgc) { return e_no($1,$2);  }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\()          [^\x81-\xFE)]* \)) \s* ; /oxmsgc) { return e_no($1,$2);  }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\{)          (?:$q_char)*?           \}) \s* ; /oxmsgc) { return e_no($1,$2);  }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\[)          (?:$q_char)*?           \]) \s* ; /oxmsgc) { return e_no($1,$2);  }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\<)          [^\x81-\xFE>]* \>) \s* ; /oxmsgc) { return e_no($1,$2);  }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* ([\x21-\x3F]) .*?                     \3) \s* ; /oxmsgc) { return e_no($1,$2);  }
    elsif (/\G \b no  \s+ ([A-Z]\w*(?: ::\w+)*) \s+ ((?:q|qq|qw) \s* (\S)          (?:$q_char)*?           \3) \s* ; /oxmsgc) { return e_no($1,$2);  }

# ''
    elsif (/\G (?<![\w\$\@\%\&\*]) (\') /oxgc) {
        my $q_string = '';
        while (not /\G \z/oxgc) {
            if    (/\G (\\\\)    /oxgc)            { $q_string .= $1;                   }
            elsif (/\G (\\\')    /oxgc)            { $q_string .= $1;                   }
            elsif (/\G \'        /oxgc)            { return e_q('', "'","'",$q_string); }
            elsif (/\G ($q_char) /oxgc)            { $q_string .= $1;                   }
        }
        die "$__FILE__: Can't find string terminator anywhere before EOF";
    }

# ""
    elsif (/\G (\") /oxgc) {
        my $qq_string = '';
        while (not /\G \z/oxgc) {
            if    (/\G (\\\\)    /oxgc)            { $qq_string .= $1;                    }
            elsif (/\G (\\\")    /oxgc)            { $qq_string .= $1;                    }
            elsif (/\G \"        /oxgc)            { return e_qq('', '"','"',$qq_string); }
            elsif (/\G ($q_char) /oxgc)            { $qq_string .= $1;                    }
        }
        die "$__FILE__: Can't find string terminator anywhere before EOF";
    }

# ``
    elsif (/\G (\`) /oxgc) {
        my $qx_string = '';
        while (not /\G \z/oxgc) {
            if    (/\G (\\\\)    /oxgc)            { $qx_string .= $1;                    }
            elsif (/\G (\\\`)    /oxgc)            { $qx_string .= $1;                    }
            elsif (/\G \`        /oxgc)            { return e_qq('', '`','`',$qx_string); }
            elsif (/\G ($q_char) /oxgc)            { $qx_string .= $1;                    }
        }
        die "$__FILE__: Can't find string terminator anywhere before EOF";
    }

# //   --- not divide operator (num / num), not defined-or
    elsif (($slash eq 'm//') and /\G (\/) /oxgc) {
        my $regexp = '';
        while (not /\G \z/oxgc) {
            if    (/\G (\\\\)               /oxgc) { $regexp .= $1;                       }
            elsif (/\G (\\\/)               /oxgc) { $regexp .= $1;                       }
            elsif (/\G \/ ([cgimosxpadlu]*) /oxgc) { return e_qr('', '/','/',$regexp,$1); }
            elsif (/\G ($q_char)            /oxgc) { $regexp .= $1;                       }
        }
        die "$__FILE__: Search pattern not terminated";
    }

# ??   --- not conditional operator (condition ? then : else)
    elsif (($slash eq 'm//') and /\G (\?) /oxgc) {
        my $regexp = '';
        while (not /\G \z/oxgc) {
            if    (/\G (\\\\)               /oxgc) { $regexp .= $1;                       }
            elsif (/\G (\\\?)               /oxgc) { $regexp .= $1;                       }
            elsif (/\G \? ([cgimosxpadlu]*) /oxgc) { return e_qr('', '?','?',$regexp,$1); }
            elsif (/\G ($q_char)            /oxgc) { $regexp .= $1;                       }
        }
        die "$__FILE__: Search pattern not terminated";
    }

# << (bit shift)   --- not here document
    elsif (/\G ( << \s* ) (?= [0-9\$\@\&] ) /oxgc) { $slash = 'm//'; return $1;           }

# <<'HEREDOC'
    elsif (/\G ( << '([a-zA-Z_0-9]*)' ) /oxgc) {
        $slash = 'm//';
        my $here_quote = $1;
        my $delimiter  = $2;

        # get here document
        if ($here_script eq '') {
            $here_script = CORE::substr $_, pos $_;
            $here_script =~ s/.*?\n//oxm;
        }
        if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
            push @heredoc, $1 . qq{\n$delimiter\n};
            push @heredoc_delimiter, $delimiter;
        }
        else {
            die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
        }
        return $here_quote;
    }

# <<\HEREDOC

    # P.66 2.6.6. "Here" Documents
    # in Chapter 2: Bits and Pieces
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    elsif (/\G ( << \\([a-zA-Z_0-9]+) ) /oxgc) {
        $slash = 'm//';
        my $here_quote = $1;
        my $delimiter  = $2;

        # get here document
        if ($here_script eq '') {
            $here_script = CORE::substr $_, pos $_;
            $here_script =~ s/.*?\n//oxm;
        }
        if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
            push @heredoc, $1 . qq{\n$delimiter\n};
            push @heredoc_delimiter, $delimiter;
        }
        else {
            die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
        }
        return $here_quote;
    }

# <<"HEREDOC"
    elsif (/\G ( << "([a-zA-Z_0-9]*)" ) /oxgc) {
        $slash = 'm//';
        my $here_quote = $1;
        my $delimiter  = $2;

        # get here document
        if ($here_script eq '') {
            $here_script = CORE::substr $_, pos $_;
            $here_script =~ s/.*?\n//oxm;
        }
        if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
            push @heredoc, e_heredoc($1) . qq{\n$delimiter\n};
            push @heredoc_delimiter, $delimiter;
        }
        else {
            die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
        }
        return $here_quote;
    }

# <<HEREDOC
    elsif (/\G ( << ([a-zA-Z_0-9]+) ) /oxgc) {
        $slash = 'm//';
        my $here_quote = $1;
        my $delimiter  = $2;

        # get here document
        if ($here_script eq '') {
            $here_script = CORE::substr $_, pos $_;
            $here_script =~ s/.*?\n//oxm;
        }
        if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
            push @heredoc, e_heredoc($1) . qq{\n$delimiter\n};
            push @heredoc_delimiter, $delimiter;
        }
        else {
            die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
        }
        return $here_quote;
    }

# <<`HEREDOC`
    elsif (/\G ( << `([a-zA-Z_0-9]*)` ) /oxgc) {
        $slash = 'm//';
        my $here_quote = $1;
        my $delimiter  = $2;

        # get here document
        if ($here_script eq '') {
            $here_script = CORE::substr $_, pos $_;
            $here_script =~ s/.*?\n//oxm;
        }
        if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
            push @heredoc, e_heredoc($1) . qq{\n$delimiter\n};
            push @heredoc_delimiter, $delimiter;
        }
        else {
            die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
        }
        return $here_quote;
    }

# <<= <=> <= < operator
    elsif (/\G (<<=|<=>|<=|<) (?= \s* [A-Za-z_0-9'"`\$\@\&\*\(\+\-] )/oxgc) {
        return $1;
    }

# <FILEHANDLE>
    elsif (/\G (<[\$]?[A-Za-z_][A-Za-z_0-9]*>) /oxgc) {
        return $1;
    }

# <WILDCARD> --- glob

    # avoid "Error: Runtime exception" of perl version 5.005_03

    elsif (/\G < ((?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE>\0\a\e\f\n\r\t])+?) > /oxgc) {
        return 'Char::Ebig5plus::glob("' . $1 . '")';
    }

# __DATA__
    elsif (/\G ^ ( __DATA__ \n .*) \z /oxmsgc) { return $1; }

# __END__
    elsif (/\G ^ ( __END__  \n .*) \z /oxmsgc) { return $1; }

# \cD Control-D

    # P.68 2.6.8. Other Literal Tokens
    # in Chapter 2: Bits and Pieces
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    elsif (/\G   ( \cD         .*) \z /oxmsgc) { return $1; }

# \cZ Control-Z
    elsif (/\G   ( \cZ         .*) \z /oxmsgc) { return $1; }

    # any operator before div
    elsif (/\G (
            -- | \+\+ |
            [\)\}\]]

            ) /oxgc) { $slash = 'div'; return $1; }

    # any operator before m//

    # //, //= (defined-or)
    #
    # P.164 Logical Operators
    # in Chapter 10: More Control Structures
    # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition
    # (and so on)

    # ~~
    #
    # P.221 The Smart Match Operator
    # in Chapter 15: Smart Matching and given-when
    # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition
    # (and so on)

    elsif (/\G (

            !~~ | !~ | != | ! |
            %= | % |
            &&= | && | &= | & |
            -= | -> | - |
            :\s*= |
            : |
            <<= | <=> | <= | < |
            == | => | =~ | = |
            >>= | >> | >= | > |
            \*\*= | \*\* | \*= | \* |
            \+= | \+ |
            \.\.\. | \.\. | \.= | \. |
            \/\/= | \/\/ |
            \/= | \/ |
            \? |
            \\ |
            \^= | \^ |
            \b x= |
            \|\|= | \|\| | \|= | \| |
            ~~ | ~ |
            \b(?: and | cmp | eq | ge | gt | le | lt | ne | not | or | xor | x )\b |
            \b(?: print )\b |

            [,;\(\{\[]

            ) /oxgc) { $slash = 'm//'; return $1; }

    # other any character
    elsif (/\G ($q_char) /oxgc) { $slash = 'div'; return $1; }

    # system error
    else {
        die "$__FILE__: oops, this shouldn't happen!";
    }
}

# escape Big5Plus string
sub e_string {
    my($string) = @_;
    my $e_string = '';

    local $slash = 'm//';

    # P.1024 Appendix W.10 Multibyte Processing
    # of ISBN 1-56592-224-7 CJKV Information Processing
    # (and so on)

    my @char = $string =~ m/ \G (\\?(?:$q_char)) /oxmsg;

    # without { ... }
    if (not (grep(m/\A \{ \z/xms, @char) and grep(m/\A \} \z/xms, @char))) {
        if ($string !~ /<</oxms) {
            return $string;
        }
    }

E_STRING_LOOP:
    while ($string !~ /\G \z/oxgc) {

# bareword
        if ($string =~ /\G ( \{ \s* (?: tr|index|rindex|reverse) \s* \} ) /oxmsgc) {
            $e_string .= $1;
            $slash = 'div';
        }

# $0 --> $0
        elsif ($string =~ /\G ( \$ 0 ) /oxmsgc) {
            $e_string .= $1;
            $slash = 'div';
        }
        elsif ($string =~ /\G ( \$ \{ \s* 0 \s* \} ) /oxmsgc) {
            $e_string .= $1;
            $slash = 'div';
        }

# $$ --> $$
        elsif ($string =~ /\G ( \$ \$ ) (?![\w\{]) /oxmsgc) {
            $e_string .= $1;
            $slash = 'div';
        }

# $1, $2, $3 --> $2, $3, $4 (only when multibyte anchoring is enable)
        elsif ($string =~ /\G \$ ([1-9][0-9]*) /oxmsgc) {
            $e_string .= e_capture($1);
            $slash = 'div';
        }
        elsif ($string =~ /\G \$ \{ \s* ([1-9][0-9]*) \s* \} /oxmsgc) {
            $e_string .= e_capture($1);
            $slash = 'div';
        }

# $$foo[ ... ] --> $ $foo->[ ... ]
        elsif ($string =~ /\G \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \[ .+? \] ) /oxmsgc) {
            $e_string .= e_capture($1.'->'.$2);
            $slash = 'div';
        }

# $$foo{ ... } --> $ $foo->{ ... }
        elsif ($string =~ /\G \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \{ .+? \} ) /oxmsgc) {
            $e_string .= e_capture($1.'->'.$2);
            $slash = 'div';
        }

# $$foo
        elsif ($string =~ /\G \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) /oxmsgc) {
            $e_string .= e_capture($1);
            $slash = 'div';
        }

# ${ foo }
        elsif ($string =~ /\G \$ \s* \{ ( \s* [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* \s* ) \} /oxmsgc) {
            $e_string .= '${' . $1 . '}';
            $slash = 'div';
        }

# ${ ... }
        elsif ($string =~ /\G \$ \s* \{ \s* ( $qq_brace ) \s* \} /oxmsgc) {
            $e_string .= e_capture($1);
            $slash = 'div';
        }

# variable or function
        #                             $ @ % & *     $ #
        elsif ($string =~ /\G ( (?: [\$\@\%\&\*] | \$\# | -> | \b sub \b) \s* (?: split|chop|index|rindex|lc|uc|chr|ord|reverse|tr|y|q|qq|qx|qw|m|s|qr|glob|lstat|opendir|stat|unlink|chdir) ) \b /oxmsgc) {
            $e_string .= $1;
            $slash = 'div';
        }
        #                           $ $ $ $ $ $ $ $ $ $ $ $ $ $ $
        #                           $ @ # \ ' " ` / ? ( ) [ ] < >
        elsif ($string =~ /\G ( \$[\$\@\#\\\'\"\`\/\?\(\)\[\]\<\>] ) /oxmsgc) {
            $e_string .= $1;
            $slash = 'div';
        }

# functions of package Char::Ebig5plus
        elsif ($string =~ m{\G \b (CORE::(?:split|chop|index|rindex|lc|uc|chr|ord|reverse|open|binmode)) \b }oxgc) { $e_string .= $1; $slash = 'm//'; }
        elsif ($string =~ m{\G \b bytes::substr \b                             }oxgc) { $e_string .= 'substr';         $slash = 'm//'; }
        elsif ($string =~ m{\G \b chop \b                                      }oxgc) { $e_string .= 'Char::Ebig5plus::chop';    $slash = 'm//'; }
        elsif ($string =~ m{\G \b bytes::index \b                              }oxgc) { $e_string .= 'index';          $slash = 'm//'; }
        elsif ($string =~ m{\G \b Char::Big5Plus::index \b                               }oxgc) { $e_string .= 'Char::Big5Plus::index';    $slash = 'm//'; }
        elsif ($string =~ m{\G \b index \b                                     }oxgc) { $e_string .= 'Char::Ebig5plus::index';   $slash = 'm//'; }
        elsif ($string =~ m{\G \b bytes::rindex \b                             }oxgc) { $e_string .= 'rindex';         $slash = 'm//'; }
        elsif ($string =~ m{\G \b Char::Big5Plus::rindex \b                              }oxgc) { $e_string .= 'Char::Big5Plus::rindex';   $slash = 'm//'; }
        elsif ($string =~ m{\G \b rindex \b                                    }oxgc) { $e_string .= 'Char::Ebig5plus::rindex';  $slash = 'm//'; }
        elsif ($string =~ m{\G \b lc      (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'Char::Ebig5plus::lc';      $slash = 'm//'; }
        elsif ($string =~ m{\G \b lcfirst (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'Char::Ebig5plus::lcfirst'; $slash = 'm//'; }
        elsif ($string =~ m{\G \b uc      (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'Char::Ebig5plus::uc';      $slash = 'm//'; }
        elsif ($string =~ m{\G \b ucfirst (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'Char::Ebig5plus::ucfirst'; $slash = 'm//'; }
        elsif ($string =~ m{\G (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+)
                                                                         \s* (\") ((?:$qq_char)+?)             (\") }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_qq('',  $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\#) ((?:$qq_char)+?)             (\#) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\() ((?:$qq_paren)+?)            (\)) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\{) ((?:$qq_brace)+?)            (\}) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\[) ((?:$qq_bracket)+?)          (\]) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\<) ((?:$qq_angle)+?)            (\>) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) qq \s* (\S) ((?:$qq_char)+?)             (\3) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }

        elsif ($string =~ m{\G (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+)
                                                                         \s* (\') ((?:\\\1|\\\\|$q_char)+?)    (\') }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_q ('',  $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\#) ((?:\\\#|\\\\|$q_char)+?)    (\#) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\() ((?:\\\)|\\\\|$q_paren)+?)   (\)) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\{) ((?:\\\}|\\\\|$q_brace)+?)   (\}) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\[) ((?:\\\]|\\\\|$q_bracket)+?) (\]) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\<) ((?:\\\>|\\\\|$q_angle)+?)   (\>) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) q  \s* (\S) ((?:\\\1|\\\\|$q_char)+?)    (\3) }oxgc) { $e_string .= "Char::Ebig5plus::filetest(qw($1)," . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }

        elsif ($string =~ m{\G (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+) (\$ \w+(?: ::\w+)* (?: (?: ->)? (?: \( (?:$qq_paren)*? \) | \{ (?:$qq_brace)+? \} | \[ (?:$qq_bracket)+? \] ) )*) }oxgc)
                                                                                                                           { $e_string .= "Char::Ebig5plus::filetest(qw($1),$2)"; $slash = 'm//'; }
        elsif ($string =~ m{\G (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+) \( ((?:$qq_paren)*?) \) }oxgc)
                                                                                                                           { $e_string .= "Char::Ebig5plus::filetest(qw($1),$2)"; $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) (?= [a-z]+) }oxgc)                                   { $e_string .= "Char::Ebig5plus::filetest qw($1),";    $slash = 'm//'; }
        elsif ($string =~ m{\G ((?:-[rwxoRWXOezfdlpSbcugkTB]\s+){2,}) (\w+) }oxgc)                                         { $e_string .= "Char::Ebig5plus::filetest(qw($1),$2)"; $slash = 'm//'; }

        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+    \s* (\") ((?:$qq_char)+?)             (\") }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_qq('',  $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\#) ((?:$qq_char)+?)             (\#) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\() ((?:$qq_paren)+?)            (\)) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\{) ((?:$qq_brace)+?)            (\}) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\[) ((?:$qq_bracket)+?)          (\]) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\<) ((?:$qq_angle)+?)            (\>) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ qq \s* (\S) ((?:$qq_char)+?)             (\3) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_qq('qq',$2,$4,$3) . ")"; $slash = 'm//'; }

        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+    \s* (\') ((?:\\\1|\\\\|$q_char)+?)    (\') }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_q ('',  $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\#) ((?:\\\#|\\\\|$q_char)+?)    (\#) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\() ((?:\\\)|\\\\|$q_paren)+?)   (\)) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\{) ((?:\\\}|\\\\|$q_brace)+?)   (\}) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\[) ((?:\\\]|\\\\|$q_bracket)+?) (\]) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\<) ((?:\\\>|\\\\|$q_angle)+?)   (\>) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ q  \s* (\S) ((?:\\\1|\\\\|$q_char)+?)    (\3) }oxgc)    { $e_string .= "Char::Ebig5plus::$1(" . e_q ('q', $2,$4,$3) . ")"; $slash = 'm//'; }

        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s* (\$ \w+(?: ::\w+)* (?: (?: ->)? (?: \( (?:$qq_paren)*? \) | \{ (?:$qq_brace)+? \} | \[ (?:$qq_bracket)+? \] ) )*) }oxgc)
                                                                                                                           { $e_string .= "Char::Ebig5plus::$1($2)";      $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s* \( ((?:$qq_paren)*?) \) }oxgc)                          { $e_string .= "Char::Ebig5plus::$1($2)";      $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) (?= \s+ [a-z]+) }oxgc)                                      { $e_string .= "Char::Ebig5plus::$1";          $slash = 'm//'; }
        elsif ($string =~ m{\G -([rwxoRWXOezsfdlpSbcugkTBMAC]) \s+ (\w+) }oxgc)                                            { $e_string .= "Char::Ebig5plus::$1(::"."$2)"; $slash = 'm//'; }
        elsif ($string =~ m{\G -(t)                            \s+ (\w+) }oxgc)                                            { $e_string .= "-t $2";              $slash = 'm//'; }
        elsif ($string =~ m{\G \b lstat         (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'Char::Ebig5plus::lstat';             $slash = 'm//'; }
        elsif ($string =~ m{\G \b stat          (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'Char::Ebig5plus::stat';              $slash = 'm//'; }

        # "-s '' ..." means file test "-s 'filename' ..." (not means "- s/// ...")
        elsif ($string =~ m{\G -s                               \s+    \s* (\") ((?:$qq_char)+?)             (\") }oxgc)    { $e_string .= '-s ' . e_qq('',  $1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ qq \s* (\#) ((?:$qq_char)+?)             (\#) }oxgc)    { $e_string .= '-s ' . e_qq('qq',$1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ qq \s* (\() ((?:$qq_paren)+?)            (\)) }oxgc)    { $e_string .= '-s ' . e_qq('qq',$1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ qq \s* (\{) ((?:$qq_brace)+?)            (\}) }oxgc)    { $e_string .= '-s ' . e_qq('qq',$1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ qq \s* (\[) ((?:$qq_bracket)+?)          (\]) }oxgc)    { $e_string .= '-s ' . e_qq('qq',$1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ qq \s* (\<) ((?:$qq_angle)+?)            (\>) }oxgc)    { $e_string .= '-s ' . e_qq('qq',$1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ qq \s* (\S) ((?:$qq_char)+?)             (\3) }oxgc)    { $e_string .= '-s ' . e_qq('qq',$1,$3,$2); $slash = 'm//'; }

        elsif ($string =~ m{\G -s                               \s+    \s* (\') ((?:\\\1|\\\\|$q_char)+?)    (\') }oxgc)    { $e_string .= '-s ' . e_q ('',  $1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ q  \s* (\#) ((?:\\\#|\\\\|$q_char)+?)    (\#) }oxgc)    { $e_string .= '-s ' . e_q ('q', $1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ q  \s* (\() ((?:\\\)|\\\\|$q_paren)+?)   (\)) }oxgc)    { $e_string .= '-s ' . e_q ('q', $1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ q  \s* (\{) ((?:\\\}|\\\\|$q_brace)+?)   (\}) }oxgc)    { $e_string .= '-s ' . e_q ('q', $1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ q  \s* (\[) ((?:\\\]|\\\\|$q_bracket)+?) (\]) }oxgc)    { $e_string .= '-s ' . e_q ('q', $1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ q  \s* (\<) ((?:\\\>|\\\\|$q_angle)+?)   (\>) }oxgc)    { $e_string .= '-s ' . e_q ('q', $1,$3,$2); $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ q  \s* (\S) ((?:\\\1|\\\\|$q_char)+?)    (\3) }oxgc)    { $e_string .= '-s ' . e_q ('q', $1,$3,$2); $slash = 'm//'; }

        elsif ($string =~ m{\G -s                               \s* (\$ \w+(?: ::\w+)* (?: (?: ->)? (?: \( (?:$qq_paren)*? \) | \{ (?:$qq_brace)+? \} | \[ (?:$qq_bracket)+? \] ) )*) }oxgc)
                                                                                                                            { $e_string .= "-s $1";   $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s* \( ((?:$qq_paren)*?) \) }oxgc)                          { $e_string .= "-s ($1)"; $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               (?= \s+ [a-z]+) }oxgc)                                      { $e_string .= '-s';      $slash = 'm//'; }
        elsif ($string =~ m{\G -s                               \s+ (\w+) }oxgc)                                            { $e_string .= "-s $1";   $slash = 'm//'; }

        elsif ($string =~ m{\G \b bytes::length (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'length';                   $slash = 'm//'; }
        elsif ($string =~ m{\G \b bytes::chr    (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'chr';                      $slash = 'm//'; }
        elsif ($string =~ m{\G \b chr           (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'Char::Ebig5plus::chr';               $slash = 'm//'; }
        elsif ($string =~ m{\G \b bytes::ord    (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'ord';                      $slash = 'div'; }
        elsif ($string =~ m{\G \b ord           (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= $function_ord;              $slash = 'div'; }
        elsif ($string =~ m{\G \b glob          (?= \s+[A-Za-z_]|\s*['"`\$\@\&\*\(]) }oxgc) { $e_string .= 'Char::Ebig5plus::glob';              $slash = 'm//'; }
        elsif ($string =~ m{\G \b lc \b                                              }oxgc) { $e_string .= 'Char::Ebig5plus::lc_';               $slash = 'm//'; }
        elsif ($string =~ m{\G \b lcfirst \b                                         }oxgc) { $e_string .= 'Char::Ebig5plus::lcfirst_';          $slash = 'm//'; }
        elsif ($string =~ m{\G \b uc \b                                              }oxgc) { $e_string .= 'Char::Ebig5plus::uc_';               $slash = 'm//'; }
        elsif ($string =~ m{\G \b ucfirst \b                                         }oxgc) { $e_string .= 'Char::Ebig5plus::ucfirst_';          $slash = 'm//'; }
        elsif ($string =~ m{\G    (-[rwxoRWXOezfdlpSbcugkTB](?:\s+-[rwxoRWXOezfdlpSbcugkTB])+)
                                                                  \b                 }oxgc) { $e_string .= "Char::Ebig5plus::filetest_(qw($1))"; $slash = 'm//'; }
        elsif ($string =~ m{\G    -([rwxoRWXOezsfdlpSbcugkTBMAC]) \b                 }oxgc) { $e_string .= "Char::Ebig5plus::${1}_";             $slash = 'm//'; }
        elsif ($string =~ m{\G \b lstat \b                                           }oxgc) { $e_string .= 'Char::Ebig5plus::lstat_';            $slash = 'm//'; }
        elsif ($string =~ m{\G \b stat \b                                            }oxgc) { $e_string .= 'Char::Ebig5plus::stat_';             $slash = 'm//'; }
        elsif ($string =~ m{\G    -s                              \b                 }oxgc) { $e_string .= '-s ';                      $slash = 'm//'; }

        elsif ($string =~ m{\G \b bytes::length \b                                   }oxgc) { $e_string .= 'length';                   $slash = 'm//'; }
        elsif ($string =~ m{\G \b bytes::chr \b                                      }oxgc) { $e_string .= 'chr';                      $slash = 'm//'; }
        elsif ($string =~ m{\G \b chr \b                                             }oxgc) { $e_string .= 'Char::Ebig5plus::chr_';              $slash = 'm//'; }
        elsif ($string =~ m{\G \b bytes::ord \b                                      }oxgc) { $e_string .= 'ord';                      $slash = 'div'; }
        elsif ($string =~ m{\G \b ord \b                                             }oxgc) { $e_string .= $function_ord_;             $slash = 'div'; }
        elsif ($string =~ m{\G \b glob \b                                            }oxgc) { $e_string .= 'Char::Ebig5plus::glob_';             $slash = 'm//'; }
        elsif ($string =~ m{\G \b reverse \b                                         }oxgc) { $e_string .= $function_reverse;          $slash = 'm//'; }
        elsif ($string =~ m{\G \b opendir (\s* \( \s*) (?=[A-Za-z_])                 }oxgc) { $e_string .= "Char::Ebig5plus::opendir$1*";        $slash = 'm//'; }
        elsif ($string =~ m{\G \b opendir (\s+)        (?=[A-Za-z_])                 }oxgc) { $e_string .= "Char::Ebig5plus::opendir$1*";        $slash = 'm//'; }
        elsif ($string =~ m{\G \b unlink \b                                          }oxgc) { $e_string .= 'Char::Ebig5plus::unlink';            $slash = 'm//'; }

# chdir
        elsif ($string =~ m{\G \b (chdir) \b (?! \s* => ) }oxgc) {
            $slash = 'm//';

            my $e_string = 'Char::Ebig5plus::chdir';

            while ($string =~ /\G ( \s+ | \( | \#.* ) /oxgc) {
                $e_string .= $1;
            }

# end of chdir
            if    ($string =~ /\G (?= [,;\)\}\]] )          /oxgc) { return $e_string;                               }

# chdir scalar value
            elsif ($string =~ /\G ( [\$\@\&\*] $qq_scalar ) /oxgc) { $e_string .= e_string($1);  next E_STRING_LOOP; }

# chdir qq//
            elsif ($string =~ /\G \b (qq) \b /oxgc) {
                if ($string =~ /\G (\#) ((?:$qq_char)*?) (\#) /oxgc)                             { $e_string .= e_chdir('qq',$1,$3,$2);   next E_STRING_LOOP; } # qq# #  --> qr # #
                else {
                    while ($string !~ /\G \z/oxgc) {
                        if    ($string =~ /\G (\s+|\#.*)                             /oxgc)      { $e_string .= $1; }
                        elsif ($string =~ /\G (\()          ((?:$qq_paren)*?)   (\)) /oxgc)      { $e_string .= e_chdir('qq',$1,$3,$2);   next E_STRING_LOOP; } # qq ( ) --> qr ( )
                        elsif ($string =~ /\G (\{)          ((?:$qq_brace)*?)   (\}) /oxgc)      { $e_string .= e_chdir('qq',$1,$3,$2);   next E_STRING_LOOP; } # qq { } --> qr { }
                        elsif ($string =~ /\G (\[)          ((?:$qq_bracket)*?) (\]) /oxgc)      { $e_string .= e_chdir('qq',$1,$3,$2);   next E_STRING_LOOP; } # qq [ ] --> qr [ ]
                        elsif ($string =~ /\G (\<)          ((?:$qq_angle)*?)   (\>) /oxgc)      { $e_string .= e_chdir('qq',$1,$3,$2);   next E_STRING_LOOP; } # qq < > --> qr < >
                        elsif ($string =~ /\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) /oxgc)      { $e_string .= e_chdir('qq','{','}',$2); next E_STRING_LOOP; } # qq | | --> qr { }
                        elsif ($string =~ /\G (\S)          ((?:$qq_char)*?)    (\1) /oxgc)      { $e_string .= e_chdir('qq',$1,$3,$2);   next E_STRING_LOOP; } # qq * * --> qr * *
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }
            }

# chdir q//
            elsif ($string =~ /\G \b (q) \b /oxgc) {
                if ($string =~ /\G (\#) ((?:\\\#|\\\\|$q_char)*?) (\#) /oxgc)                    { $e_string .= e_chdir_q('q',$1,$3,$2);   next E_STRING_LOOP; } # q# #  --> qr # #
                else {
                    while ($string !~ /\G \z/oxgc) {
                        if    ($string =~ /\G (\s+|\#.*)                                  /oxgc) { $e_string .= $1; }
                        elsif ($string =~ /\G (\() ((?:\\\\|\\\)|\\\(|$q_paren)*?)   (\)) /oxgc) { $e_string .= e_chdir_q('q',$1,$3,$2);   next E_STRING_LOOP; } # q ( ) --> qr ( )
                        elsif ($string =~ /\G (\{) ((?:\\\\|\\\}|\\\{|$q_brace)*?)   (\}) /oxgc) { $e_string .= e_chdir_q('q',$1,$3,$2);   next E_STRING_LOOP; } # q { } --> qr { }
                        elsif ($string =~ /\G (\[) ((?:\\\\|\\\]|\\\[|$q_bracket)*?) (\]) /oxgc) { $e_string .= e_chdir_q('q',$1,$3,$2);   next E_STRING_LOOP; } # q [ ] --> qr [ ]
                        elsif ($string =~ /\G (\<) ((?:\\\\|\\\>|\\\<|$q_angle)*?)   (\>) /oxgc) { $e_string .= e_chdir_q('q',$1,$3,$2);   next E_STRING_LOOP; } # q < > --> qr < >
                        elsif ($string =~ /\G ([*\-:?\\^|])       ((?:$q_char)*?)    (\1) /oxgc) { $e_string .= e_chdir_q('q','{','}',$2); next E_STRING_LOOP; } # q | | --> qr { }
                        elsif ($string =~ /\G (\S) ((?:\\\\|\\\1|     $q_char)*?)    (\1) /oxgc) { $e_string .= e_chdir_q('q',$1,$3,$2);   next E_STRING_LOOP; } # q * * --> qr * *
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }
            }

# chdir ''
            elsif ($string =~ /\G (\') /oxgc) {
                my $q_string = '';
                while ($string !~ /\G \z/oxgc) {
                    if    ($string =~ /\G (\\\\)    /oxgc) { $q_string .= $1; }
                    elsif ($string =~ /\G (\\\')    /oxgc) { $q_string .= $1; }
                    elsif ($string =~ /\G \'        /oxgc)                                       { $e_string .= e_chdir_q('',"'","'",$q_string); next E_STRING_LOOP; }
                    elsif ($string =~ /\G ($q_char) /oxgc) { $q_string .= $1; }
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }

# chdir ""
            elsif ($string =~ /\G (\") /oxgc) {
                my $qq_string = '';
                while ($string !~ /\G \z/oxgc) {
                    if    ($string =~ /\G (\\\\)    /oxgc) { $qq_string .= $1; }
                    elsif ($string =~ /\G (\\\")    /oxgc) { $qq_string .= $1; }
                    elsif ($string =~ /\G \"        /oxgc)                                       { $e_string .= e_chdir('','"','"',$qq_string); next E_STRING_LOOP; }
                    elsif ($string =~ /\G ($q_char) /oxgc) { $qq_string .= $1; }
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }
        }

# split
        elsif ($string =~ m{\G \b (split) \b (?! \s* => ) }oxgc) {
            $slash = 'm//';

            my $e_string = 'Char::Ebig5plus::split';

            while ($string =~ /\G ( \s+ | \( | \#.* ) /oxgc) {
                $e_string .= $1;
            }

# end of split
            if    ($string =~ /\G (?= [,;\)\}\]] )          /oxgc) { return $e_string;                               }

# split scalar value
            elsif ($string =~ /\G ( [\$\@\&\*] $qq_scalar ) /oxgc) { $e_string .= e_string($1);  next E_STRING_LOOP; }

# split literal space
            elsif ($string =~ /\G \b qq       (\#) [ ] (\#) /oxgc) { $e_string .= qq  {qq$1 $2}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b qq (\s*) (\() [ ] (\)) /oxgc) { $e_string .= qq{$1qq$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b qq (\s*) (\{) [ ] (\}) /oxgc) { $e_string .= qq{$1qq$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b qq (\s*) (\[) [ ] (\]) /oxgc) { $e_string .= qq{$1qq$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b qq (\s*) (\<) [ ] (\>) /oxgc) { $e_string .= qq{$1qq$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b qq (\s*) (\S) [ ] (\2) /oxgc) { $e_string .= qq{$1qq$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b q        (\#) [ ] (\#) /oxgc) { $e_string .= qq   {q$1 $2}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b q  (\s*) (\() [ ] (\)) /oxgc) { $e_string .= qq {$1q$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b q  (\s*) (\{) [ ] (\}) /oxgc) { $e_string .= qq {$1q$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b q  (\s*) (\[) [ ] (\]) /oxgc) { $e_string .= qq {$1q$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b q  (\s*) (\<) [ ] (\>) /oxgc) { $e_string .= qq {$1q$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G \b q  (\s*) (\S) [ ] (\2) /oxgc) { $e_string .= qq {$1q$2 $3}; next E_STRING_LOOP; }
            elsif ($string =~ /\G                ' [ ] '    /oxgc) { $e_string .= qq     {' '};  next E_STRING_LOOP; }
            elsif ($string =~ /\G                " [ ] "    /oxgc) { $e_string .= qq     {" "};  next E_STRING_LOOP; }

# split qq//
            elsif ($string =~ /\G \b (qq) \b /oxgc) {
                if ($string =~ /\G (\#) ((?:$qq_char)*?) (\#) /oxgc)                        { $e_string .= e_split('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # qq# #  --> qr # #
                else {
                    while ($string !~ /\G \z/oxgc) {
                        if    ($string =~ /\G (\s+|\#.*)                             /oxgc) { $e_string .= $1; }
                        elsif ($string =~ /\G (\()          ((?:$qq_paren)*?)   (\)) /oxgc) { $e_string .= e_split('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # qq ( ) --> qr ( )
                        elsif ($string =~ /\G (\{)          ((?:$qq_brace)*?)   (\}) /oxgc) { $e_string .= e_split('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # qq { } --> qr { }
                        elsif ($string =~ /\G (\[)          ((?:$qq_bracket)*?) (\]) /oxgc) { $e_string .= e_split('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # qq [ ] --> qr [ ]
                        elsif ($string =~ /\G (\<)          ((?:$qq_angle)*?)   (\>) /oxgc) { $e_string .= e_split('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # qq < > --> qr < >
                        elsif ($string =~ /\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) /oxgc) { $e_string .= e_split('qr','{','}',$2,''); next E_STRING_LOOP; } # qq | | --> qr { }
                        elsif ($string =~ /\G (\S)          ((?:$qq_char)*?)    (\1) /oxgc) { $e_string .= e_split('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # qq * * --> qr * *
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }
            }

# split qr//
            elsif ($string =~ /\G \b (qr) \b /oxgc) {
                if ($string =~ /\G (\#) ((?:$qq_char)*?) (\#) ([imosxpadlu]*) /oxgc)                        { $e_string .= e_split  ('qr',$1,$3,$2,$4);   next E_STRING_LOOP; } # qr# #
                else {
                    while ($string !~ /\G \z/oxgc) {
                        if    ($string =~ /\G (\s+|\#.*)                                             /oxgc) { $e_string .= $1; }
                        elsif ($string =~ /\G (\()          ((?:$qq_paren)*?)   (\)) ([imosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # qr ( )
                        elsif ($string =~ /\G (\{)          ((?:$qq_brace)*?)   (\}) ([imosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # qr { }
                        elsif ($string =~ /\G (\[)          ((?:$qq_bracket)*?) (\]) ([imosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # qr [ ]
                        elsif ($string =~ /\G (\<)          ((?:$qq_angle)*?)   (\>) ([imosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # qr < >
                        elsif ($string =~ /\G (\')          ((?:$qq_char)*?)    (\') ([imosxpadlu]*) /oxgc) { $e_string .= e_split_q('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # qr ' '
                        elsif ($string =~ /\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) ([imosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr','{','}',$2,$4); next E_STRING_LOOP; } # qr | | --> qr { }
                        elsif ($string =~ /\G (\S)          ((?:$qq_char)*?)    (\1) ([imosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # qr * *
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }
            }

# split q//
            elsif ($string =~ /\G \b (q) \b /oxgc) {
                if ($string =~ /\G (\#) ((?:\\\#|\\\\|$q_char)*?) (\#) /oxgc)                    { $e_string .= e_split_q('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # q# #  --> qr # #
                else {
                    while ($string !~ /\G \z/oxgc) {
                        if    ($string =~ /\G (\s+|\#.*)                                  /oxgc) { $e_string .= $1; }
                        elsif ($string =~ /\G (\() ((?:\\\\|\\\)|\\\(|$q_paren)*?)   (\)) /oxgc) { $e_string .= e_split_q('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # q ( ) --> qr ( )
                        elsif ($string =~ /\G (\{) ((?:\\\\|\\\}|\\\{|$q_brace)*?)   (\}) /oxgc) { $e_string .= e_split_q('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # q { } --> qr { }
                        elsif ($string =~ /\G (\[) ((?:\\\\|\\\]|\\\[|$q_bracket)*?) (\]) /oxgc) { $e_string .= e_split_q('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # q [ ] --> qr [ ]
                        elsif ($string =~ /\G (\<) ((?:\\\\|\\\>|\\\<|$q_angle)*?)   (\>) /oxgc) { $e_string .= e_split_q('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # q < > --> qr < >
                        elsif ($string =~ /\G ([*\-:?\\^|])       ((?:$q_char)*?)    (\1) /oxgc) { $e_string .= e_split_q('qr','{','}',$2,''); next E_STRING_LOOP; } # q | | --> qr { }
                        elsif ($string =~ /\G (\S) ((?:\\\\|\\\1|     $q_char)*?)    (\1) /oxgc) { $e_string .= e_split_q('qr',$1,$3,$2,'');   next E_STRING_LOOP; } # q * * --> qr * *
                    }
                    die "$__FILE__: Can't find string terminator anywhere before EOF";
                }
            }

# split m//
            elsif ($string =~ /\G \b (m) \b /oxgc) {
                if ($string =~ /\G (\#) ((?:$qq_char)*?) (\#) ([cgimosxpadlu]*) /oxgc)                        { $e_string .= e_split  ('qr',$1,$3,$2,$4);   next E_STRING_LOOP; } # m# #  --> qr # #
                else {
                    while ($string !~ /\G \z/oxgc) {
                        if    ($string =~ /\G (\s+|\#.*)                                               /oxgc) { $e_string .= $1; }
                        elsif ($string =~ /\G (\()          ((?:$qq_paren)*?)   (\)) ([cgimosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # m ( ) --> qr ( )
                        elsif ($string =~ /\G (\{)          ((?:$qq_brace)*?)   (\}) ([cgimosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # m { } --> qr { }
                        elsif ($string =~ /\G (\[)          ((?:$qq_bracket)*?) (\]) ([cgimosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # m [ ] --> qr [ ]
                        elsif ($string =~ /\G (\<)          ((?:$qq_angle)*?)   (\>) ([cgimosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # m < > --> qr < >
                        elsif ($string =~ /\G (\')          ((?:$qq_char)*?)    (\') ([cgimosxpadlu]*) /oxgc) { $e_string .= e_split_q('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # m ' ' --> qr ' '
                        elsif ($string =~ /\G ([*\-:?\\^|]) ((?:$qq_char)*?)    (\1) ([cgimosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr','{','}',$2,$4); next E_STRING_LOOP; } # m | | --> qr { }
                        elsif ($string =~ /\G (\S)          ((?:$qq_char)*?)    (\1) ([cgimosxpadlu]*) /oxgc) { $e_string .= e_split  ('qr',$1, $3, $2,$4); next E_STRING_LOOP; } # m * * --> qr * *
                    }
                    die "$__FILE__: Search pattern not terminated";
                }
            }

# split ''
            elsif ($string =~ /\G (\') /oxgc) {
                my $q_string = '';
                while ($string !~ /\G \z/oxgc) {
                    if    ($string =~ /\G (\\\\)    /oxgc) { $q_string .= $1; }
                    elsif ($string =~ /\G (\\\')    /oxgc) { $q_string .= $1; } # splitqr'' --> split qr''
                    elsif ($string =~ /\G \'        /oxgc)                      { $e_string .= e_split_q(q{ qr},"'","'",$q_string,''); next E_STRING_LOOP; } # ' ' --> qr ' '
                    elsif ($string =~ /\G ($q_char) /oxgc) { $q_string .= $1; }
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }

# split ""
            elsif ($string =~ /\G (\") /oxgc) {
                my $qq_string = '';
                while ($string !~ /\G \z/oxgc) {
                    if    ($string =~ /\G (\\\\)    /oxgc) { $qq_string .= $1; }
                    elsif ($string =~ /\G (\\\")    /oxgc) { $qq_string .= $1; } # splitqr"" --> split qr""
                    elsif ($string =~ /\G \"        /oxgc)                       { $e_string .= e_split(q{ qr},'"','"',$qq_string,''); next E_STRING_LOOP; } # " " --> qr " "
                    elsif ($string =~ /\G ($q_char) /oxgc) { $qq_string .= $1; }
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }

# split //
            elsif ($string =~ /\G (\/) /oxgc) {
                my $regexp = '';
                while ($string !~ /\G \z/oxgc) {
                    if    ($string =~ /\G (\\\\)               /oxgc) { $regexp .= $1; }
                    elsif ($string =~ /\G (\\\/)               /oxgc) { $regexp .= $1; } # splitqr// --> split qr//
                    elsif ($string =~ /\G \/ ([cgimosxpadlu]*) /oxgc)                    { $e_string .= e_split(q{ qr}, '/','/',$regexp,$1); next E_STRING_LOOP; } # / / --> qr / /
                    elsif ($string =~ /\G ($q_char)            /oxgc) { $regexp .= $1; }
                }
                die "$__FILE__: Search pattern not terminated";
            }
        }

# qq//
        elsif ($string =~ /\G \b (qq) \b /oxgc) {
            my $ope = $1;
            if ($string =~ /\G (\#) ((?:$qq_char)*?) (\#) /oxgc) { # qq# #
                $e_string .= e_qq($ope,$1,$3,$2);
            }
            else {
                my $e = '';
                while ($string !~ /\G \z/oxgc) {
                    if    ($string =~ /\G (\s+|\#.*)                    /oxgc) { $e .= $1; }
                    elsif ($string =~ /\G (\() ((?:$qq_paren)*?)   (\)) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qq ( )
                    elsif ($string =~ /\G (\{) ((?:$qq_brace)*?)   (\}) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qq { }
                    elsif ($string =~ /\G (\[) ((?:$qq_bracket)*?) (\]) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qq [ ]
                    elsif ($string =~ /\G (\<) ((?:$qq_angle)*?)   (\>) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qq < >
                    elsif ($string =~ /\G (\S) ((?:$qq_char)*?)    (\1) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qq * *
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }
        }

# qx//
        elsif ($string =~ /\G \b (qx) \b /oxgc) {
            my $ope = $1;
            if ($string =~ /\G (\#) ((?:$qq_char)*?) (\#) /oxgc) { # qx# #
                $e_string .= e_qq($ope,$1,$3,$2);
            }
            else {
                my $e = '';
                while ($string !~ /\G \z/oxgc) {
                    if    ($string =~ /\G (\s+|\#.*)                    /oxgc) { $e .= $1; }
                    elsif ($string =~ /\G (\() ((?:$qq_paren)*?)   (\)) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qx ( )
                    elsif ($string =~ /\G (\{) ((?:$qq_brace)*?)   (\}) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qx { }
                    elsif ($string =~ /\G (\[) ((?:$qq_bracket)*?) (\]) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qx [ ]
                    elsif ($string =~ /\G (\<) ((?:$qq_angle)*?)   (\>) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qx < >
                    elsif ($string =~ /\G (\') ((?:$qq_char)*?)    (\') /oxgc) { $e_string .= $e . e_q ($ope,$1,$3,$2); next E_STRING_LOOP; } # qx ' '
                    elsif ($string =~ /\G (\S) ((?:$qq_char)*?)    (\1) /oxgc) { $e_string .= $e . e_qq($ope,$1,$3,$2); next E_STRING_LOOP; } # qx * *
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }
        }

# q//
        elsif ($string =~ /\G \b (q) \b /oxgc) {
            my $ope = $1;
            if ($string =~ /\G (\#) ((?:\\\#|\\\\|$q_char)*?) (\#) /oxgc) { # q# #
                $e_string .= e_q($ope,$1,$3,$2);
            }
            else {
                my $e = '';
                while ($string !~ /\G \z/oxgc) {
                    if    ($string =~ /\G (\s+|\#.*)                                  /oxgc) { $e .= $1; }
                    elsif ($string =~ /\G (\() ((?:\\\\|\\\)|\\\(|$q_paren)*?)   (\)) /oxgc) { $e_string .= $e . e_q($ope,$1,$3,$2); next E_STRING_LOOP; } # q ( )
                    elsif ($string =~ /\G (\{) ((?:\\\\|\\\}|\\\{|$q_brace)*?)   (\}) /oxgc) { $e_string .= $e . e_q($ope,$1,$3,$2); next E_STRING_LOOP; } # q { }
                    elsif ($string =~ /\G (\[) ((?:\\\\|\\\]|\\\[|$q_bracket)*?) (\]) /oxgc) { $e_string .= $e . e_q($ope,$1,$3,$2); next E_STRING_LOOP; } # q [ ]
                    elsif ($string =~ /\G (\<) ((?:\\\\|\\\>|\\\<|$q_angle)*?)   (\>) /oxgc) { $e_string .= $e . e_q($ope,$1,$3,$2); next E_STRING_LOOP; } # q < >
                    elsif ($string =~ /\G (\S) ((?:\\\\|\\\1|     $q_char)*?)    (\1) /oxgc) { $e_string .= $e . e_q($ope,$1,$3,$2); next E_STRING_LOOP; } # q * *
                }
                die "$__FILE__: Can't find string terminator anywhere before EOF";
            }
        }

# ''
        elsif ($string =~ /\G (?<![\w\$\@\%\&\*]) (\') ((?:\\\'|\\\\|$q_char)*?) (\') /oxgc) { $e_string .= e_q('',$1,$3,$2);  }

# ""
        elsif ($string =~ /\G (\") ((?:$qq_char)*?) (\") /oxgc)                              { $e_string .= e_qq('',$1,$3,$2); }

# ``
        elsif ($string =~ /\G (\`) ((?:$qq_char)*?) (\`) /oxgc)                              { $e_string .= e_qq('',$1,$3,$2); }

# <<= <=> <= < operator
        elsif ($string =~ /\G (<<=|<=>|<=|<) (?= \s* [A-Za-z_0-9'"`\$\@\&\*\(\+\-] )/oxgc)   { $e_string .= $1;                }

# <FILEHANDLE>
        elsif ($string =~ /\G (<[\$]?[A-Za-z_][A-Za-z_0-9]*>) /oxgc)                         { $e_string .= $1;                }

# <WILDCARD>   --- glob
        elsif ($string =~ /\G < ((?:$q_char)+?) > /oxgc) {
            $e_string .= 'Char::Ebig5plus::glob("' . $1 . '")';
        }

# << (bit shift)   --- not here document
        elsif ($string =~ /\G ( << \s* ) (?= [0-9\$\@\&] ) /oxgc) { $slash = 'm//'; $e_string .= $1; }

# <<'HEREDOC'
        elsif ($string =~ /\G ( << '([a-zA-Z_0-9]*)' ) /oxgc) {
            $slash = 'm//';
            my $here_quote = $1;
            my $delimiter  = $2;

            # get here document
            if ($here_script eq '') {
                $here_script = CORE::substr $_, pos $_;
                $here_script =~ s/.*?\n//oxm;
            }
            if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
                push @heredoc, $1 . qq{\n$delimiter\n};
                push @heredoc_delimiter, $delimiter;
            }
            else {
                die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
            }
            $e_string .= $here_quote;
        }

# <<\HEREDOC
        elsif ($string =~ /\G ( << \\([a-zA-Z_0-9]+) ) /oxgc) {
            $slash = 'm//';
            my $here_quote = $1;
            my $delimiter  = $2;

            # get here document
            if ($here_script eq '') {
                $here_script = CORE::substr $_, pos $_;
                $here_script =~ s/.*?\n//oxm;
            }
            if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
                push @heredoc, $1 . qq{\n$delimiter\n};
                push @heredoc_delimiter, $delimiter;
            }
            else {
                die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
            }
            $e_string .= $here_quote;
        }

# <<"HEREDOC"
        elsif ($string =~ /\G ( << "([a-zA-Z_0-9]*)" ) /oxgc) {
            $slash = 'm//';
            my $here_quote = $1;
            my $delimiter  = $2;

            # get here document
            if ($here_script eq '') {
                $here_script = CORE::substr $_, pos $_;
                $here_script =~ s/.*?\n//oxm;
            }
            if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
                push @heredoc, e_heredoc($1) . qq{\n$delimiter\n};
                push @heredoc_delimiter, $delimiter;
            }
            else {
                die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
            }
            $e_string .= $here_quote;
        }

# <<HEREDOC
        elsif ($string =~ /\G ( << ([a-zA-Z_0-9]+) ) /oxgc) {
            $slash = 'm//';
            my $here_quote = $1;
            my $delimiter  = $2;

            # get here document
            if ($here_script eq '') {
                $here_script = CORE::substr $_, pos $_;
                $here_script =~ s/.*?\n//oxm;
            }
            if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
                push @heredoc, e_heredoc($1) . qq{\n$delimiter\n};
                push @heredoc_delimiter, $delimiter;
            }
            else {
                die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
            }
            $e_string .= $here_quote;
        }

# <<`HEREDOC`
        elsif ($string =~ /\G ( << `([a-zA-Z_0-9]*)` ) /oxgc) {
            $slash = 'm//';
            my $here_quote = $1;
            my $delimiter  = $2;

            # get here document
            if ($here_script eq '') {
                $here_script = CORE::substr $_, pos $_;
                $here_script =~ s/.*?\n//oxm;
            }
            if ($here_script =~ s/\A (.*?) \n $delimiter \n //xms) {
                push @heredoc, e_heredoc($1) . qq{\n$delimiter\n};
                push @heredoc_delimiter, $delimiter;
            }
            else {
                die "$__FILE__: Can't find string terminator $delimiter anywhere before EOF";
            }
            $e_string .= $here_quote;
        }

        # any operator before div
        elsif ($string =~ /\G (
            -- | \+\+ |
            [\)\}\]]

            ) /oxgc) { $slash = 'div'; $e_string .= $1; }

        # any operator before m//
        elsif ($string =~ /\G (

            !~~ | !~ | != | ! |
            %= | % |
            &&= | && | &= | & |
            -= | -> | - |
            :\s*= |
            : |
            <<= | <=> | <= | < |
            == | => | =~ | = |
            >>= | >> | >= | > |
            \*\*= | \*\* | \*= | \* |
            \+= | \+ |
            \.\.\. | \.\. | \.= | \. |
            \/\/= | \/\/ |
            \/= | \/ |
            \? |
            \\ |
            \^= | \^ |
            \b x= |
            \|\|= | \|\| | \|= | \| |
            ~~ | ~ |
            \b(?: and | cmp | eq | ge | gt | le | lt | ne | not | or | xor | x )\b |
            \b(?: print )\b |

            [,;\(\{\[]

            ) /oxgc) { $slash = 'm//'; $e_string .= $1; }

        # other any character
        elsif ($string =~ /\G ($q_char) /oxgc) { $e_string .= $1; }

        # system error
        else {
            die "$__FILE__: oops, this shouldn't happen!";
        }
    }

    return $e_string;
}

#
# character class
#
sub character_class {
    my($char,$modifier) = @_;

    if ($char eq '.') {
        if ($modifier =~ /s/) {
            return '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE])';
        }
        else {
            return '(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x0A])';
        }
    }
    else {
        return Char::Ebig5plus::classic_character_class($char);
    }
}

#
# escape capture ($1, $2, $3, ...)
#
sub e_capture {

    return join '', '${Char::Ebig5plus::capture(', $_[0], ')}';
    return join '', '$',                 $_[0];
}

#
# escape transliteration (tr/// or y///)
#
sub e_tr {
    my($variable,$charclass,$e,$charclass2,$modifier) = @_;
    my $e_tr = '';
    $modifier ||= '';

    $slash = 'div';

    # quote character class 1
    $charclass  = q_tr($charclass);

    # quote character class 2
    $charclass2 = q_tr($charclass2);

    # /b /B modifier
    if ($modifier =~ tr/bB//d) {
        if ($variable eq '') {
            $e_tr = qq{tr$charclass$e$charclass2$modifier};
        }
        else {
            $e_tr = qq{$variable${bind_operator}tr$charclass$e$charclass2$modifier};
        }
    }
    else {
        if ($variable eq '') {
            $e_tr = qq{Char::Ebig5plus::tr(\$_,' =~ ',$charclass,$e$charclass2,'$modifier')};
        }
        else {
            $e_tr = qq{Char::Ebig5plus::tr($variable,'$bind_operator',$charclass,$e$charclass2,'$modifier')};
        }
    }

    # clear tr/// variable
    $tr_variable = '';
    $bind_operator = '';

    return $e_tr;
}

#
# quote for escape transliteration (tr/// or y///)
#
sub q_tr {
    my($charclass) = @_;

    # quote character class
    if ($charclass !~ m/'/oxms) {
        return e_q('',  "'", "'", $charclass); # --> q' '
    }
    elsif ($charclass !~ m{/}oxms) {
        return e_q('q',  '/', '/', $charclass); # --> q/ /
    }
    elsif ($charclass !~ m/\#/oxms) {
        return e_q('q',  '#', '#', $charclass); # --> q# #
    }
    elsif ($charclass !~ m/[\<\>]/oxms) {
        return e_q('q', '<', '>', $charclass); # --> q< >
    }
    elsif ($charclass !~ m/[\(\)]/oxms) {
        return e_q('q', '(', ')', $charclass); # --> q( )
    }
    elsif ($charclass !~ m/[\{\}]/oxms) {
        return e_q('q', '{', '}', $charclass); # --> q{ }
    }
    else {
        for my $char (qw( ! " $ % & * + . : = ? @ ^ ` | ~ ), "\x00".."\x1F", "\x7F", "\xFF") {
            if ($charclass !~ m/\Q$char\E/xms) {
                return e_q('q', $char, $char, $charclass);
            }
        }
    }

    return e_q('q', '{', '}', $charclass);
}

#
# escape q string (q//, '')
#
sub e_q {
    my($ope,$delimiter,$end_delimiter,$string) = @_;

    $slash = 'div';

    my @char = $string =~ m/ \G ($q_char) /oxmsg;
    for (my $i=0; $i <= $#char; $i++) {

        # escape last octet of multiple octet
        if ($char[$i] =~ m/\A ([\x80-\xFF].*) (\Q$delimiter\E|\Q$end_delimiter\E) \z/xms) {
            $char[$i] = $1 . '\\' . $2;
        }
        elsif (($char[$i] =~ m/\A ([\x80-\xFF].*) (\\) \z/xms) and defined($char[$i+1]) and ($char[$i+1] eq '\\')) {
            $char[$i] = $1 . '\\' . $2;
        }
    }
    if (defined($char[-1]) and ($char[-1] =~ m/\A ([\x80-\xFF].*) (\\) \z/xms)) {
        $char[-1] = $1 . '\\' . $2;
    }

    return join '', $ope, $delimiter, @char,   $end_delimiter;
    return join '', $ope, $delimiter, $string, $end_delimiter;
}

#
# escape qq string (qq//, "", qx//, ``)
#
sub e_qq {
    my($ope,$delimiter,$end_delimiter,$string) = @_;

    $slash = 'div';

    my $metachar = qr/[\@\\\|]/oxms; # '|' is for qx//, ``, open() and system()

    my $left_e  = 0;
    my $right_e = 0;
    my @char = $string =~ m{ \G (
        \\o\{ [0-7]+          \}   |
        \\x\{ [0-9A-Fa-f]+    \}   |
        \\N\{ [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \$ \s* \d+                 |
        \$ \s* \{ \s* \d+ \s* \}   |
        \$ \$ (?![\w\{])           |
        \$ \s* \$ \s* $qq_variable |
        \\?(?:$q_char)
    )}oxmsg;

    for (my $i=0; $i <= $#char; $i++) {

        # "\L\u" --> "\u\L"
        if (($char[$i] eq '\L') and ($char[$i+1] eq '\u')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # "\U\l" --> "\l\U"
        elsif (($char[$i] eq '\U') and ($char[$i+1] eq '\l')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # octal escape sequence
        elsif ($char[$i] =~ m/\A \\o \{ ([0-7]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::octchr($1);
        }

        # hexadecimal escape sequence
        elsif ($char[$i] =~ m/\A \\x \{ ([0-9A-Fa-f]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::hexchr($1);
        }

        # \N{CHARNAME} --> N{CHARNAME}
        elsif ($char[$i] =~ m/\A \\ ( N\{ ([^\x81-\xFE0-9\}][^\x81-\xFE\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        if (0) {
        }

        # escape last octet of multiple octet
        elsif ($char[$i] =~ m/\A ([\x80-\xFF].*) ($metachar|\Q$delimiter\E|\Q$end_delimiter\E) \z/xms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # \u \l \U \L \Q \E
        elsif ($char[$i] =~ m/\A ([<>]) \z/oxms) {
            if ($right_e < $left_e) {
                $char[$i] = '\\' . $char[$i];
            }
        }
        elsif ($char[$i] eq '\u') {

            # "STRING @{[ LIST EXPR ]} MORE STRING"
            #
            # 1.15. Interpolating Functions and Expressions Within Strings
            # in Chapter 1. Strings
            # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
            # (and so on)

            $char[$i] = '@{[Char::Ebig5plus::ucfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\l') {
            $char[$i] = '@{[Char::Ebig5plus::lcfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\U') {
            $char[$i] = '@{[Char::Ebig5plus::uc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\L') {
            $char[$i] = '@{[Char::Ebig5plus::lc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\Q') {
            $char[$i] = '@{[CORE::quotemeta qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\E') {
            if ($right_e < $left_e) {
                $char[$i] = '>]}';
                $right_e++;
            }
            else {
                $char[$i] = '';
            }
        }
        elsif ($char[$i] eq '\Q') {
            while (1) {
                if (++$i > $#char) {
                    last;
                }
                if ($char[$i] eq '\E') {
                    last;
                }
            }
        }
        elsif ($char[$i] eq '\E') {
        }

        # $0 --> $0
        elsif ($char[$i] =~ m/\A \$ 0 \z/oxms) {
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* 0 \s* \} \z/oxms) {
        }

        # $$ --> $$
        elsif ($char[$i] =~ m/\A \$\$ \z/oxms) {
        }

        # $1, $2, $3 --> $2, $3, $4 (only when multibyte anchoring is enable)
        elsif ($char[$i] =~ m/\A \$ ([1-9][0-9]*) \z/oxms) {
            $char[$i] = e_capture($1);
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* ([1-9][0-9]*) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
        }

        # $$foo[ ... ] --> $ $foo->[ ... ]
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \[ (?:$qq_bracket)*? \] ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
        }

        # $$foo{ ... } --> $ $foo->{ ... }
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \{ (?:$qq_brace)*? \} ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
        }

        # $$foo
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) \z/oxms) {
            $char[$i] = e_capture($1);
        }

        # ${ foo } --> ${ foo }
        elsif ($char[$i] =~ m/\A \$ \s* \{ \s* [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* \s* \} \z/oxms) {
        }

        # ${ ... }
        elsif ($char[$i] =~ m/\A \$ \s* \{ \s* ( .+ ) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
        }
    }

    # return string
    if ($left_e > $right_e) {
        return join '', $ope, $delimiter, @char, '>]}' x ($left_e - $right_e), $end_delimiter;
    }
    return     join '', $ope, $delimiter, @char,                               $end_delimiter;
}

#
# escape qw string (qw//)
#
sub e_qw {
    my($ope,$delimiter,$end_delimiter,$string) = @_;

    $slash = 'div';

    # choice again delimiter
    my %octet = map {$_ => 1} ($string =~ m/\G ([\x00-\xFF]) /oxmsg);
    if (not $octet{$end_delimiter}) {
        return join '', $ope, $delimiter, $string, $end_delimiter;
    }
    elsif (not $octet{')'}) {
        return join '', $ope, '(',        $string, ')';
    }
    elsif (not $octet{'}'}) {
        return join '', $ope, '{',        $string, '}';
    }
    elsif (not $octet{']'}) {
        return join '', $ope, '[',        $string, ']';
    }
    elsif (not $octet{'>'}) {
        return join '', $ope, '<',        $string, '>';
    }
    else {
        for my $char (qw( ! " $ % & * + - . / : = ? @ ^ ` | ~ ), "\x00".."\x1F", "\x7F", "\xFF") {
            if (not $octet{$char}) {
                return join '', $ope,      $char, $string, $char;
            }
        }
    }

    # qw/AAA BBB C'CC/ --> ('AAA', 'BBB', 'C\'CC')
    my @string = CORE::split(/\s+/, $string);
    for my $string (@string) {
        my @octet = $string =~ m/\G ([\x00-\xFF]) /oxmsg;
        for my $octet (@octet) {
            if ($octet =~ m/\A (['\\]) \z/oxms) {
                $octet = '\\' . $1;
            }
        }
        $string = join '', @octet;
    }
    return join '', '(', (join ', ', map { "'$_'" } @string), ')';
}

#
# escape here document (<<"HEREDOC", <<HEREDOC, <<`HEREDOC`)
#
sub e_heredoc {
    my($string) = @_;

    $slash = 'm//';

    my $metachar = qr/[\@\\|]/oxms; # '|' is for <<`HEREDOC`

    my $left_e  = 0;
    my $right_e = 0;
    my @char = $string =~ m{ \G (
        \\o\{ [0-7]+          \}   |
        \\x\{ [0-9A-Fa-f]+    \}   |
        \\N\{ [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \$ \s* \d+                 |
        \$ \s* \{ \s* \d+ \s* \}   |
        \$ \$ (?![\w\{])           |
        \$ \s* \$ \s* $qq_variable |
        \\?(?:$q_char)
    )}oxmsg;

    for (my $i=0; $i <= $#char; $i++) {

        # "\L\u" --> "\u\L"
        if (($char[$i] eq '\L') and ($char[$i+1] eq '\u')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # "\U\l" --> "\l\U"
        elsif (($char[$i] eq '\U') and ($char[$i+1] eq '\l')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # octal escape sequence
        elsif ($char[$i] =~ m/\A \\o \{ ([0-7]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::octchr($1);
        }

        # hexadecimal escape sequence
        elsif ($char[$i] =~ m/\A \\x \{ ([0-9A-Fa-f]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::hexchr($1);
        }

        # \N{CHARNAME} --> N{CHARNAME}
        elsif ($char[$i] =~ m/\A \\ ( N\{ ([^\x81-\xFE0-9\}][^\x81-\xFE\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        if (0) {
        }

        # escape character
        elsif ($char[$i] =~ m/\A ([\x80-\xFF].*) ($metachar) \z/oxms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # \u \l \U \L \Q \E
        elsif ($char[$i] =~ m/\A ([<>]) \z/oxms) {
            if ($right_e < $left_e) {
                $char[$i] = '\\' . $char[$i];
            }
        }
        elsif ($char[$i] eq '\u') {
            $char[$i] = '@{[Char::Ebig5plus::ucfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\l') {
            $char[$i] = '@{[Char::Ebig5plus::lcfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\U') {
            $char[$i] = '@{[Char::Ebig5plus::uc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\L') {
            $char[$i] = '@{[Char::Ebig5plus::lc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\Q') {
            $char[$i] = '@{[CORE::quotemeta qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\E') {
            if ($right_e < $left_e) {
                $char[$i] = '>]}';
                $right_e++;
            }
            else {
                $char[$i] = '';
            }
        }
        elsif ($char[$i] eq '\Q') {
            while (1) {
                if (++$i > $#char) {
                    last;
                }
                if ($char[$i] eq '\E') {
                    last;
                }
            }
        }
        elsif ($char[$i] eq '\E') {
        }

        # $0 --> $0
        elsif ($char[$i] =~ m/\A \$ 0 \z/oxms) {
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* 0 \s* \} \z/oxms) {
        }

        # $$ --> $$
        elsif ($char[$i] =~ m/\A \$\$ \z/oxms) {
        }

        # $1, $2, $3 --> $2, $3, $4 (only when multibyte anchoring is enable)
        elsif ($char[$i] =~ m/\A \$ ([1-9][0-9]*) \z/oxms) {
            $char[$i] = e_capture($1);
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* ([1-9][0-9]*) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
        }

        # $$foo[ ... ] --> $ $foo->[ ... ]
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \[ (?:$qq_bracket)*? \] ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
        }

        # $$foo{ ... } --> $ $foo->{ ... }
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \{ (?:$qq_brace)*? \} ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
        }

        # $$foo
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) \z/oxms) {
            $char[$i] = e_capture($1);
        }

        # ${ foo } --> ${ foo }
        elsif ($char[$i] =~ m/\A \$ \s* \{ \s* [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* \s* \} \z/oxms) {
        }

        # ${ ... }
        elsif ($char[$i] =~ m/\A \$ \s* \{ \s* ( .+ ) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
        }
    }

    # return string
    if ($left_e > $right_e) {
        return join '', @char, '>]}' x ($left_e - $right_e);
    }
    return     join '', @char;
}

#
# escape regexp (m//, qr//)
#
sub e_qr {
    my($ope,$delimiter,$end_delimiter,$string,$modifier) = @_;
    $modifier ||= '';

    if ($modifier =~ m/([adlu])/oxms) {
        my $line = 0;
        for (my $i=0; my($package,$filename,$use_line,$subroutine) = caller($i); $i++) {
            if ($filename ne __FILE__) {
                $line = $use_line + (CORE::substr($_,0,pos($_)) =~ tr/\n//) + 1;
                last;
            }
        }
        die qq{Unsupported modifier "$1" used at line $line.\n};
    }

    my $ignorecase = ($modifier =~ m/i/oxms) ? 1 : 0;

    $slash = 'div';

    my $metachar = qr/[\@\\|[\]{^]/oxms;

    # split regexp
    my @char = $string =~ m{\G(
        \\o\{ [0-7]+           \}  |
        \\    [0-7]{2,3}           |
        \\x\{ [0-9A-Fa-f]+     \}  |
        \\x   [0-9A-Fa-f]{1,2}     |
        \\c   [\x40-\x5F]          |
        \\N\{ [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \\p\{ [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \\P\{ [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \\  (?:$q_char)            |
        [\$\@] $qq_variable        |
        \$ \s* \d+                 |
        \$ \s* \{ \s* \d+ \s* \}   |
        \$ \$ (?![\w\{])           |
        \$ \s* \$ \s* $qq_variable |
        \[\:\^ (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] |
        \[\:   (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] |
        \[\^                       |
        \(\?                       |
            (?:$q_char)
    )}oxmsg;

    # choice again delimiter
    if ($delimiter =~ m/ [\@:] /oxms) {
        my %octet = map {$_ => 1} @char;
        if (not $octet{')'}) {
            $delimiter     = '(';
            $end_delimiter = ')';
        }
        elsif (not $octet{'}'}) {
            $delimiter     = '{';
            $end_delimiter = '}';
        }
        elsif (not $octet{']'}) {
            $delimiter     = '[';
            $end_delimiter = ']';
        }
        elsif (not $octet{'>'}) {
            $delimiter     = '<';
            $end_delimiter = '>';
        }
        else {
            for my $char (qw( ! " $ % & * + - . / = ? ^ ` | ~ ), "\x00".."\x1F", "\x7F", "\xFF") {
                if (not $octet{$char}) {
                    $delimiter     = $char;
                    $end_delimiter = $char;
                    last;
                }
            }
        }
    }

    my $left_e  = 0;
    my $right_e = 0;
    for (my $i=0; $i <= $#char; $i++) {

        # "\L\u" --> "\u\L"
        if (($char[$i] eq '\L') and ($char[$i+1] eq '\u')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # "\U\l" --> "\l\U"
        elsif (($char[$i] eq '\U') and ($char[$i+1] eq '\l')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # octal escape sequence
        elsif ($char[$i] =~ m/\A \\o \{ ([0-7]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::octchr($1);
        }

        # hexadecimal escape sequence
        elsif ($char[$i] =~ m/\A \\x \{ ([0-9A-Fa-f]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::hexchr($1);
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

        if (0) {
        }

        # escape last octet of multiple octet
        elsif ($char[$i] =~ m/\A \\? ([\x80-\xFF].*) ($metachar|\Q$delimiter\E|\Q$end_delimiter\E) \z/xms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # join separated multiple octet
        elsif ($char[$i] =~ m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms) {
            if (   ($i+3 <= $#char) and (grep(m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms, @char[$i+1..$i+3]) == 3) and (eval(sprintf '"%s%s%s%s"', @char[$i..$i+3]) =~ m/\A $q_char \z/oxms)) {
                $char[$i] .= join '', splice @char, $i+1, 3;
            }
            elsif (($i+2 <= $#char) and (grep(m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms, @char[$i+1..$i+2]) == 2) and (eval(sprintf '"%s%s%s"',   @char[$i..$i+2]) =~ m/\A $q_char \z/oxms)) {
                $char[$i] .= join '', splice @char, $i+1, 2;
            }
            elsif (($i+1 <= $#char) and (grep(m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms, $char[$i+1      ]) == 1) and (eval(sprintf '"%s%s"',     @char[$i..$i+1]) =~ m/\A $q_char \z/oxms)) {
                $char[$i] .= join '', splice @char, $i+1, 1;
            }
        }

        # open character class [...]
        elsif ($char[$i] eq '[') {
            my $left = $i;

            # [] make die "unmatched [] in regexp ..."
            # (and so on)

            if ($char[$i+1] eq ']') {
                $i++;
            }

            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # open character class [^...]
        elsif ($char[$i] eq '[^') {
            my $left = $i;

            # [^] make die "unmatched [] in regexp ..."
            # (and so on)

            if ($char[$i+1] eq ']') {
                $i++;
            }

            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [^...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_not_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # rewrite character class or escape character
        elsif (my $char = character_class($char[$i],$modifier)) {
            $char[$i] = $char;
        }

        # /i modifier
        elsif ($ignorecase and ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) and (Char::Ebig5plus::uc($char[$i]) ne Char::Ebig5plus::lc($char[$i]))) {
            $char[$i] = '[' . Char::Ebig5plus::uc($char[$i]) . Char::Ebig5plus::lc($char[$i]) . ']';
        }

        # \u \l \U \L \Q \E
        elsif ($char[$i] =~ m/\A [<>] \z/oxms) {
            if ($right_e < $left_e) {
                $char[$i] = '\\' . $char[$i];
            }
        }
        elsif ($char[$i] eq '\u') {
            $char[$i] = '@{[Char::Ebig5plus::ucfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\l') {
            $char[$i] = '@{[Char::Ebig5plus::lcfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\U') {
            $char[$i] = '@{[Char::Ebig5plus::uc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\L') {
            $char[$i] = '@{[Char::Ebig5plus::lc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\Q') {
            $char[$i] = '@{[CORE::quotemeta qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\E') {
            if ($right_e < $left_e) {
                $char[$i] = '>]}';
                $right_e++;
            }
            else {
                $char[$i] = '';
            }
        }
        elsif ($char[$i] eq '\Q') {
            while (1) {
                if (++$i > $#char) {
                    last;
                }
                if ($char[$i] eq '\E') {
                    last;
                }
            }
        }
        elsif ($char[$i] eq '\E') {
        }

        # $0 --> $0
        elsif ($char[$i] =~ m/\A \$ 0 \z/oxms) {
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* 0 \s* \} \z/oxms) {
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$ --> $$
        elsif ($char[$i] =~ m/\A \$\$ \z/oxms) {
        }

        # $1, $2, $3 --> $2, $3, $4 (only when multibyte anchoring is enable)
        elsif ($char[$i] =~ m/\A \$ ([1-9][0-9]*) \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* ([1-9][0-9]*) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$foo[ ... ] --> $ $foo->[ ... ]
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \[ (?:$qq_bracket)*? \] ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$foo{ ... } --> $ $foo->{ ... }
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \{ (?:$qq_brace)*? \} ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$foo
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # ${ foo }
        elsif ($char[$i] =~ m/\A \$ \s* \{ ( \s* [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* \s* ) \} \z/oxms) {
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # ${ ... }
        elsif ($char[$i] =~ m/\A \$ \s* \{ \s* ( .+ ) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $scalar or @array
        elsif ($char[$i] =~ m/\A [\$\@].+ /oxms) {
            $char[$i] = e_string($char[$i]);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # quote character before ? + * {
        elsif (($i >= 1) and ($char[$i] =~ m/\A [\?\+\*\{] \z/oxms)) {
            if ($char[$i-1] =~ m/\A (?:[\x00-\xFF]|\\[0-7]{2,3}|\\x[0-9-A-Fa-f]{1,2}) \z/oxms) {
            }
            else {
                $char[$i-1] = '(?:' . $char[$i-1] . ')';
            }
        }
    }

    # make regexp string
    $modifier =~ tr/i//d;
    if ($left_e > $right_e) {
        return join '', $ope, $delimiter, "$your_gap(?:", @char, '>]}' x ($left_e - $right_e), ')', $m_matched, $end_delimiter, $modifier;
    }
    return     join '', $ope, $delimiter, "$your_gap(?:", @char,                               ')', $m_matched, $end_delimiter, $modifier;
}

#
# escape regexp (m'', qr'')
#
sub e_qr_q {
    my($ope,$delimiter,$end_delimiter,$string,$modifier) = @_;
    $modifier ||= '';

    if ($modifier =~ m/([adlu])/oxms) {
        my $line = 0;
        for (my $i=0; my($package,$filename,$use_line,$subroutine) = caller($i); $i++) {
            if ($filename ne __FILE__) {
                $line = $use_line + (CORE::substr($_,0,pos($_)) =~ tr/\n//) + 1;
                last;
            }
        }
        die qq{Unsupported modifier "$1" used at line $line.\n};
    }

    my $ignorecase = ($modifier =~ m/i/oxms) ? 1 : 0;

    $slash = 'div';

    # split regexp
    my @char = $string =~ m{\G(
        \[\:\^ [a-z]+ \:\] |
        \[\:   [a-z]+ \:\] |
        \[\^               |
        [\$\@/\\]          |
        \\?    (?:$q_char)
    )}oxmsg;

    # unescape character
    for (my $i=0; $i <= $#char; $i++) {
        if (0) {
        }

        # escape last octet of multiple octet
        elsif ($char[$i] =~ m/\A ([\x80-\xFF].*) ([\\|\[\{\^]|\Q$delimiter\E|\Q$end_delimiter\E) \z/xms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # open character class [...]
        elsif ($char[$i] eq '[') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # open character class [^...]
        elsif ($char[$i] eq '[^') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [^...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_not_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # escape $ @ / and \
        elsif ($char[$i] =~ m{\A [\$\@/\\] \z}oxms) {
            $char[$i] = '\\' . $char[$i];
        }

        # rewrite character class or escape character
        elsif (my $char = character_class($char[$i],$modifier)) {
            $char[$i] = $char;
        }

        # /i modifier
        elsif ($ignorecase and ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) and (Char::Ebig5plus::uc($char[$i]) ne Char::Ebig5plus::lc($char[$i]))) {
            $char[$i] = '[' . Char::Ebig5plus::uc($char[$i]) . Char::Ebig5plus::lc($char[$i]) . ']';
        }

        # quote character before ? + * {
        elsif (($i >= 1) and ($char[$i] =~ m/\A [\?\+\*\{] \z/oxms)) {
            if ($char[$i-1] =~ m/\A [\x00-\xFF] \z/oxms) {
            }
            else {
                $char[$i-1] = '(?:' . $char[$i-1] . ')';
            }
        }
    }

    $modifier =~ tr/i//d;
    $delimiter     = '/';
    $end_delimiter = '/';
    return join '', $ope, $delimiter, "$your_gap(?:", @char, ')', $m_matched, $end_delimiter, $modifier;
}

#
# escape regexp (s/here//)
#
sub e_s1 {
    my($ope,$delimiter,$end_delimiter,$string,$modifier) = @_;
    $modifier ||= '';

    my $ignorecase = ($modifier =~ m/i/oxms) ? 1 : 0;

    $slash = 'div';

    my $metachar = qr/[\@\\|[\]{^]/oxms;

    # split regexp
    my @char = $string =~ m{\G(
        \\g \s* \{ \s* - \s* [1-9][0-9]* \s* \} |
        \\g \s* \{ \s*       [1-9][0-9]* \s* \} |
        \\g \s*              [1-9][0-9]*        |
        \\o\{                [0-7]+          \} |
        \\                   [1-9][0-9]*        |
        \\                   [0-7]{2,3}         |
        \\x\{                [0-9A-Fa-f]+    \} |
        \\x                  [0-9A-Fa-f]{1,2}   |
        \\c                  [\x40-\x5F]        |
        \\N\{                [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \\p\{                [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \\P\{                [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \\  (?:$q_char)                         |
        [\$\@] $qq_variable                     |
        \$ \s* \d+                              |
        \$ \s* \{ \s* \d+ \s* \}                |
        \$ \$ (?![\w\{])                        |
        \$ \s* \$ \s* $qq_variable              |
        \[\:\^ (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] |
        \[\:   (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] |
        \[\^                                    |
        \(\?                                    |
            (?:$q_char)
    )}oxmsg;

    # choice again delimiter
    if ($delimiter =~ m/ [\@:] /oxms) {
        my %octet = map {$_ => 1} @char;
        if (not $octet{')'}) {
            $delimiter     = '(';
            $end_delimiter = ')';
        }
        elsif (not $octet{'}'}) {
            $delimiter     = '{';
            $end_delimiter = '}';
        }
        elsif (not $octet{']'}) {
            $delimiter     = '[';
            $end_delimiter = ']';
        }
        elsif (not $octet{'>'}) {
            $delimiter     = '<';
            $end_delimiter = '>';
        }
        else {
            for my $char (qw( ! " $ % & * + - . / = ? ^ ` | ~ ), "\x00".."\x1F", "\x7F", "\xFF") {
                if (not $octet{$char}) {
                    $delimiter     = $char;
                    $end_delimiter = $char;
                    last;
                }
            }
        }
    }

    # count '('
    my $parens = grep { $_ eq '(' } @char;

    my $left_e  = 0;
    my $right_e = 0;
    for (my $i=0; $i <= $#char; $i++) {

        # "\L\u" --> "\u\L"
        if (($char[$i] eq '\L') and ($char[$i+1] eq '\u')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # "\U\l" --> "\l\U"
        elsif (($char[$i] eq '\U') and ($char[$i+1] eq '\l')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # octal escape sequence
        elsif ($char[$i] =~ m/\A \\o \{ ([0-7]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::octchr($1);
        }

        # hexadecimal escape sequence
        elsif ($char[$i] =~ m/\A \\x \{ ([0-9A-Fa-f]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::hexchr($1);
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

        if (0) {
        }

        # escape last octet of multiple octet
        elsif ($char[$i] =~ m/\A \\? ([\x80-\xFF].*) ($metachar|\Q$delimiter\E|\Q$end_delimiter\E) \z/xms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # join separated multiple octet
        elsif ($char[$i] =~ m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms) {
            if (   ($i+3 <= $#char) and (grep(m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms, @char[$i+1..$i+3]) == 3) and (eval(sprintf '"%s%s%s%s"', @char[$i..$i+3]) =~ m/\A $q_char \z/oxms)) {
                $char[$i] .= join '', splice @char, $i+1, 3;
            }
            elsif (($i+2 <= $#char) and (grep(m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms, @char[$i+1..$i+2]) == 2) and (eval(sprintf '"%s%s%s"',   @char[$i..$i+2]) =~ m/\A $q_char \z/oxms)) {
                $char[$i] .= join '', splice @char, $i+1, 2;
            }
            elsif (($i+1 <= $#char) and (grep(m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms, $char[$i+1      ]) == 1) and (eval(sprintf '"%s%s"',     @char[$i..$i+1]) =~ m/\A $q_char \z/oxms)) {
                $char[$i] .= join '', splice @char, $i+1, 1;
            }
        }

        # open character class [...]
        elsif ($char[$i] eq '[') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # open character class [^...]
        elsif ($char[$i] eq '[^') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [^...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_not_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # rewrite character class or escape character
        elsif (my $char = character_class($char[$i],$modifier)) {
            $char[$i] = $char;
        }

        # /i modifier
        elsif ($ignorecase and ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) and (Char::Ebig5plus::uc($char[$i]) ne Char::Ebig5plus::lc($char[$i]))) {
            $char[$i] = '[' . Char::Ebig5plus::uc($char[$i]) . Char::Ebig5plus::lc($char[$i]) . ']';
        }

        # \u \l \U \L \Q \E
        elsif ($char[$i] =~ m/\A [<>] \z/oxms) {
            if ($right_e < $left_e) {
                $char[$i] = '\\' . $char[$i];
            }
        }
        elsif ($char[$i] eq '\u') {
            $char[$i] = '@{[Char::Ebig5plus::ucfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\l') {
            $char[$i] = '@{[Char::Ebig5plus::lcfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\U') {
            $char[$i] = '@{[Char::Ebig5plus::uc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\L') {
            $char[$i] = '@{[Char::Ebig5plus::lc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\Q') {
            $char[$i] = '@{[CORE::quotemeta qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\E') {
            if ($right_e < $left_e) {
                $char[$i] = '>]}';
                $right_e++;
            }
            else {
                $char[$i] = '';
            }
        }
        elsif ($char[$i] eq '\Q') {
            while (1) {
                if (++$i > $#char) {
                    last;
                }
                if ($char[$i] eq '\E') {
                    last;
                }
            }
        }
        elsif ($char[$i] eq '\E') {
        }

        # \0 --> \0
        elsif ($char[$i] =~ m/\A \\ \s* 0 \z/oxms) {
        }

        # \g{N}, \g{-N}
        #
        # P.108 Using Simple Patterns
        # in Chapter 7: In the World of Regular Expressions
        # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition

        # \g{-1}, \g{-2}, \g{-3} --> \g{-1}, \g{-2}, \g{-3}
        elsif ($char[$i] =~ m/\A \\g \s* \{ \s* - \s* ([1-9][0-9]*) \s* \} \z/oxms) {
        }

        # \g{1}, \g{2}, \g{3} --> \g{2}, \g{3}, \g{4} (only when multibyte anchoring is enable)
        elsif ($char[$i] =~ m/\A \\g \s* \{ \s* ([1-9][0-9]*) \s* \} \z/oxms) {
            if ($1 <= $parens) {
                $char[$i] = '\\g{' . ($1 + 1) . '}';
            }
        }

        # \g1, \g2, \g3 --> \g2, \g3, \g4 (only when multibyte anchoring is enable)
        elsif ($char[$i] =~ m/\A \\g \s* ([1-9][0-9]*) \z/oxms) {
            if ($1 <= $parens) {
                $char[$i] = '\\g' . ($1 + 1);
            }
        }

        # \1, \2, \3 --> \2, \3, \4 (only when multibyte anchoring is enable)
        elsif ($char[$i] =~ m/\A \\ \s* ([1-9][0-9]*) \z/oxms) {
            if ($1 <= $parens) {
                $char[$i] = '\\' . ($1 + 1);
            }
        }

        # $0 --> $0
        elsif ($char[$i] =~ m/\A \$ 0 \z/oxms) {
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* 0 \s* \} \z/oxms) {
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$ --> $$
        elsif ($char[$i] =~ m/\A \$\$ \z/oxms) {
        }

        # $1, $2, $3 --> $2, $3, $4 (only when multibyte anchoring is enable)
        elsif ($char[$i] =~ m/\A \$ ([1-9][0-9]*) \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* ([1-9][0-9]*) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$foo[ ... ] --> $ $foo->[ ... ]
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \[ (?:$qq_bracket)*? \] ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$foo{ ... } --> $ $foo->{ ... }
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \{ (?:$qq_brace)*? \} ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$foo
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # ${ foo }
        elsif ($char[$i] =~ m/\A \$ \s* \{ ( \s* [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* \s* ) \} \z/oxms) {
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # ${ ... }
        elsif ($char[$i] =~ m/\A \$ \s* \{ \s* ( .+ ) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $scalar or @array
        elsif ($char[$i] =~ m/\A [\$\@].+ /oxms) {
            $char[$i] = e_string($char[$i]);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # quote character before ? + * {
        elsif (($i >= 1) and ($char[$i] =~ m/\A [\?\+\*\{] \z/oxms)) {
            if ($char[$i-1] =~ m/\A (?:[\x00-\xFF]|\\[0-7]{2,3}|\\x[0-9-A-Fa-f]{1,2}) \z/oxms) {
            }
            else {
                $char[$i-1] = '(?:' . $char[$i-1] . ')';
            }
        }
    }

    # make regexp string
    my $capture_your_gap = '';
    $capture_your_gap = "($your_gap)";
    $modifier =~ tr/i//d;
    if ($left_e > $right_e) {
        return join '', $ope, $delimiter, $capture_your_gap, '(?:', @char, '>]}' x ($left_e - $right_e), ')', $s_matched, $end_delimiter, $modifier;
    }
    return     join '', $ope, $delimiter, $capture_your_gap, '(?:', @char,                               ')', $s_matched, $end_delimiter, $modifier;
}

#
# escape regexp (s'here'')
#
sub e_s1_q {
    my($ope,$delimiter,$end_delimiter,$string,$modifier) = @_;
    $modifier ||= '';

    my $ignorecase = ($modifier =~ m/i/oxms) ? 1 : 0;

    $slash = 'div';

    # split regexp
    my @char = $string =~ m{\G(
        \[\:\^ [a-z]+ \:\] |
        \[\:   [a-z]+ \:\] |
        \[\^               |
        [\$\@/\\]          |
        \\?    (?:$q_char)
    )}oxmsg;

    # unescape character
    for (my $i=0; $i <= $#char; $i++) {
        if (0) {
        }

        # escape last octet of multiple octet
        elsif ($char[$i] =~ m/\A ([\x80-\xFF].*) ([\\|\[\{\^]|\Q$delimiter\E|\Q$end_delimiter\E) \z/xms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # open character class [...]
        elsif ($char[$i] eq '[') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # open character class [^...]
        elsif ($char[$i] eq '[^') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [^...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_not_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # escape $ @ / and \
        elsif ($char[$i] =~ m{\A [\$\@/\\] \z}oxms) {
            $char[$i] = '\\' . $char[$i];
        }

        # rewrite character class or escape character
        elsif (my $char = character_class($char[$i],$modifier)) {
            $char[$i] = $char;
        }

        # /i modifier
        elsif ($ignorecase and ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) and (Char::Ebig5plus::uc($char[$i]) ne Char::Ebig5plus::lc($char[$i]))) {
            $char[$i] = '[' . Char::Ebig5plus::uc($char[$i]) . Char::Ebig5plus::lc($char[$i]) . ']';
        }

        # quote character before ? + * {
        elsif (($i >= 1) and ($char[$i] =~ m/\A [\?\+\*\{] \z/oxms)) {
            if ($char[$i-1] =~ m/\A [\x00-\xFF] \z/oxms) {
            }
            else {
                $char[$i-1] = '(?:' . $char[$i-1] . ')';
            }
        }
    }

    $modifier =~ tr/i//d;
    $delimiter     = '/';
    $end_delimiter = '/';
    my $capture_your_gap = '';
    $capture_your_gap = "($your_gap)";
    return join '', $ope, $delimiter, $capture_your_gap, '(?:', @char, ')', $s_matched, $end_delimiter, $modifier;
}

#
# escape regexp (s''here')
#
sub e_s2_q {
    my($ope,$delimiter,$end_delimiter,$string) = @_;

    $slash = 'div';

    my @char = $string =~ m/ \G (\\\\|[\$\@\/\\]|$q_char) /oxmsg;
    for (my $i=0; $i <= $#char; $i++) {
        if (0) {
        }

        # escape last octet of multiple octet
        elsif ($char[$i] =~ m/\A ([\x80-\xFF].*) (\Q$delimiter\E|\Q$end_delimiter\E) \z/xms) {
            $char[$i] = $1 . '\\' . $2;
        }
        elsif (($char[$i] =~ m/\A ([\x80-\xFF].*) (\\) \z/xms) and defined($char[$i+1]) and ($char[$i+1] eq '\\')) {
            $char[$i] = $1 . '\\' . $2;
        }

        # not escape \\
        elsif ($char[$i] =~ m{\A \\\\ \z}oxms) {
        }

        # escape $ @ / and \
        elsif ($char[$i] =~ m{\A [\$\@/\\] \z}oxms) {
            $char[$i] = '\\' . $char[$i];
        }
    }
    if (defined($char[-1]) and ($char[-1] =~ m/\A ([\x80-\xFF].*) (\\) \z/xms)) {
        $char[-1] = $1 . '\\' . $2;
    }

    return join '', $ope, $delimiter, @char,   $end_delimiter;
}

#
# escape regexp (s/here/and here/modifier)
#
sub e_sub {
    my($variable,$delimiter1,$pattern,$end_delimiter1,$delimiter2,$replacement,$end_delimiter2,$modifier) = @_;
    $modifier ||= '';

    if ($modifier =~ m/([adlu])/oxms) {
        my $line = 0;
        for (my $i=0; my($package,$filename,$use_line,$subroutine) = caller($i); $i++) {
            if ($filename ne __FILE__) {
                $line = $use_line + (CORE::substr($_,0,pos($_)) =~ tr/\n//) + 1;
                last;
            }
        }
        die qq{Unsupported modifier "$1" used at line $line.\n};
    }

    if ($variable eq '') {
        $variable      = '$_';
        $bind_operator = ' =~ ';
    }

    $slash = 'div';

    # P.128 Start of match (or end of previous match): \G
    # in Chapter 3: Overview of Regular Expression Features and Flavors
    # P.312 Iterative Matching: Scalar Context, with /g
    # in Chapter 7: Perl
    # of ISBN 0-596-00272-6 Mastering Regular Expressions, Second edition

    my $e_modifier = $modifier =~ tr/e//d;
    my $r_modifier = $modifier =~ tr/r//d;

    my $my = '';
    if ($variable =~ s/\A \( \s* ( (?: local \b | my \b | our \b | state \b )? .+ ) \) \z/$1/oxms) {
        $my = $variable;
        $variable =~ s/ (?: local \b | my \b | our \b | state \b ) \s* //oxms;
        $variable =~ s/ = .+ \z//oxms;
    }

    (my $variable_basename = $variable) =~ s/ [\[\{].* \z//oxms;
    $variable_basename =~ s/ \s+ \z//oxms;

    # quote replacement string
    my $e_replacement = '';
    if ($e_modifier >= 1) {
        $e_replacement = e_qq('', '', '', $replacement);
        $e_modifier--;
    }
    else {
        if ($delimiter2 eq "'") {
            $e_replacement = e_s2_q('qq', '/',         '/',             $replacement);
        }
        else {
            $e_replacement = e_qq  ('qq', $delimiter2, $end_delimiter2, $replacement);
        }
    }

    my $local = '';
    if ($variable_basename =~ /::/) {
        $local = 'local';
    }
    else{
        $local = 'my';
    }

    my $sub = '';

    # with /r
    if ($r_modifier) {
        if (0) {
        }

        # s///gr with multibyte anchoring
        elsif ($modifier =~ m/g/oxms) {
            $sub = sprintf(
                #      1  2    3   4  5              6   7 8                 9  10   11  1213    14      15           16            17      18     19          20         21               22
                q<eval{%s %s_t=%s; %s %s_a=''; while(%s_t%s%s){ local $^W=0; %s %s_r=%s; %s%s_t="%s_a${1}%s_r$'"; pos(%s_t)=length "%s_a${1}%s_r"; %s_a=substr(%s_t,0,pos(%s_t)); } return %s_t}>,

                $local,                                                                       #  1
                    $variable_basename,                                                       #  2
                $variable,                                                                    #  3
                $local,                                                                       #  4
                    $variable_basename,                                                       #  5
                    $variable_basename,                                                       #  6
                $bind_operator,                                                               #  7
                ($delimiter1 eq "'") ?                                                        #  8
                e_s1_q('m', $delimiter1, $end_delimiter1, $pattern, $modifier) :              #  :
                e_s1  ('m', $delimiter1, $end_delimiter1, $pattern, $modifier),               #  :
                $local,                                                                       #  9
                    $variable_basename,                                                       # 10
                $e_replacement,                                                               # 11
                sprintf('%s_r=eval %s_r; ', $variable_basename, $variable_basename) x $e_modifier, # 12
                    $variable_basename,                                                       # 13
                    $variable_basename,                                                       # 14
                    $variable_basename,                                                       # 15
                    $variable_basename,                                                       # 16
                    $variable_basename,                                                       # 17
                    $variable_basename,                                                       # 18
                    $variable_basename,                                                       # 19
                    $variable_basename,                                                       # 20
                    $variable_basename,                                                       # 21
                    $variable_basename,                                                       # 22
            );
        }

        # s///gr without multibyte anchoring
        elsif ($modifier =~ m/g/oxms) {
            $sub = sprintf(
                #      1  2    3         4   5 6                 7  8    9   1011      12           13              14              15
                q<eval{%s %s_t=%s; while(%s_t%s%s){ local $^W=0; %s %s_r=%s; %s%s_t="$`%s_r$'"; pos(%s_t)=length "$`%s_r"; } return %s_t}>,

                $local,                                                                       #  1
                    $variable_basename,                                                       #  2
                $variable,                                                                    #  3
                    $variable_basename,                                                       #  4
                $bind_operator,                                                               #  5
                ($delimiter1 eq "'") ?                                                        #  6
                e_s1_q('m', $delimiter1, $end_delimiter1, $pattern, $modifier) :              #  :
                e_s1  ('m', $delimiter1, $end_delimiter1, $pattern, $modifier),               #  :
                $local,                                                                       #  7
                    $variable_basename,                                                       #  8
                $e_replacement,                                                               #  9
                sprintf('%s_r=eval %s_r; ', $variable_basename, $variable_basename) x $e_modifier, # 10
                    $variable_basename,                                                       # 11
                    $variable_basename,                                                       # 12
                    $variable_basename,                                                       # 13
                    $variable_basename,                                                       # 14
                    $variable_basename,                                                       # 15
            );
        }

        # s///r
        else {

            my $prematch = q{$`};
            $prematch = q{${1}};

            $sub = sprintf(
                #  1 2 3                        4  5    6   7  8 9           10
                q<(%s%s%s) ? eval{ local $^W=0; %s %s_r=%s; %s"%s%s_r$'" } : %s>,

                $variable,                                                                    #  1
                $bind_operator,                                                               #  2
                ($delimiter1 eq "'") ?                                                        #  3
                e_s1_q('m', $delimiter1, $end_delimiter1, $pattern, $modifier) :              #  :
                e_s1  ('m', $delimiter1, $end_delimiter1, $pattern, $modifier),               #  :
                $local,                                                                       #  4
                    $variable_basename,                                                       #  5
                $e_replacement,                                                               #  6
                sprintf('%s_r=eval %s_r; ', $variable_basename, $variable_basename) x $e_modifier, #  7
                $prematch,                                                                    #  8
                    $variable_basename,                                                       #  9
                $variable,                                                                    # 10
            );
        }

        # $var !~ s///r doesn't make sense
        if ($bind_operator =~ m/ !~ /oxms) {
            $sub = q{die("$0: Using !~ with s///r doesn't make sense"), } . $sub;
        }
    }

    # without /r
    else {
        if (0) {
        }

        # s///g with multibyte anchoring
        elsif ($modifier =~ m/g/oxms) {
            $sub = sprintf(
                #      1  2       3  4              5 6 7                 8  9    10  1112  13      14           15          16      17     18          19       20    21             22
                q<eval{%s %s_n=0; %s %s_a=''; while(%s%s%s){ local $^W=0; %s %s_r=%s; %s%s="%s_a${1}%s_r$'"; pos(%s)=length "%s_a${1}%s_r"; %s_a=substr(%s,0,pos(%s)); %s_n++} return %s_n}>,

                $local,                                                                       #  1
                    $variable_basename,                                                       #  2
                $local,                                                                       #  3
                    $variable_basename,                                                       #  4
                $variable,                                                                    #  5
                $bind_operator,                                                               #  6
                ($delimiter1 eq "'") ?                                                        #  7
                e_s1_q('m', $delimiter1, $end_delimiter1, $pattern, $modifier) :              #  :
                e_s1  ('m', $delimiter1, $end_delimiter1, $pattern, $modifier),               #  :
                $local,                                                                       #  8
                    $variable_basename,                                                       #  9
                $e_replacement,                                                               # 10
                sprintf('%s_r=eval %s_r; ', $variable_basename, $variable_basename) x $e_modifier, # 11
                $variable,                                                                    # 12
                    $variable_basename,                                                       # 13
                    $variable_basename,                                                       # 14
                $variable,                                                                    # 15
                    $variable_basename,                                                       # 16
                    $variable_basename,                                                       # 17
                    $variable_basename,                                                       # 18
                $variable,                                                                    # 19
                $variable,                                                                    # 20
                    $variable_basename,                                                       # 21
                    $variable_basename,                                                       # 22
            );
        }

        # s///g without multibyte anchoring
        elsif ($modifier =~ m/g/oxms) {
            $sub = sprintf(
                #      1  2             3 4 5                 6  7    8   9 10    11           12            13     14             15
                q<eval{%s %s_n=0; while(%s%s%s){ local $^W=0; %s %s_r=%s; %s%s="$`%s_r$'"; pos(%s)=length "$`%s_r"; %s_n++} return %s_n}>,

                $local,                                                                       #  1
                    $variable_basename,                                                       #  2
                $variable,                                                                    #  3
                $bind_operator,                                                               #  4
                ($delimiter1 eq "'") ?                                                        #  5
                e_s1_q('m', $delimiter1, $end_delimiter1, $pattern, $modifier) :              #  :
                e_s1  ('m', $delimiter1, $end_delimiter1, $pattern, $modifier),               #  :
                $local,                                                                       #  6
                    $variable_basename,                                                       #  7
                $e_replacement,                                                               #  8
                sprintf('%s_r=eval %s_r; ', $variable_basename, $variable_basename) x $e_modifier, #  9
                $variable,                                                                    # 10
                    $variable_basename,                                                       # 11
                $variable,                                                                    # 12
                    $variable_basename,                                                       # 13
                    $variable_basename,                                                       # 14
                    $variable_basename,                                                       # 15
            );
        }

        # s///
        else {

            my $prematch = q{$`};
            $prematch = q{${1}};

            $sub = sprintf(
                #  1 2 3                        4  5    6   7 8   9 10
                q<(%s%s%s) ? eval{ local $^W=0; %s %s_r=%s; %s%s="%s%s_r$'"; 1 } : undef>,

                $variable,                                                                    #  1
                $bind_operator,                                                               #  2
                ($delimiter1 eq "'") ?                                                        #  3
                e_s1_q('m', $delimiter1, $end_delimiter1, $pattern, $modifier) :              #  :
                e_s1  ('m', $delimiter1, $end_delimiter1, $pattern, $modifier),               #  :
                $local,                                                                       #  4
                    $variable_basename,                                                       #  5
                $e_replacement,                                                               #  6
                sprintf('%s_r=eval %s_r; ', $variable_basename, $variable_basename) x $e_modifier, #  7
                $variable,                                                                    #  8
                $prematch,                                                                    #  9
                    $variable_basename,                                                       # 10
            );
        }
    }

    # (my $foo = $bar) =~ s///   -->   (my $foo = $bar, eval { ... })[1]
    if ($my ne '') {
        $sub = "($my, $sub)[1]";
    }

    # clear s/// variable
    $sub_variable = '';
    $bind_operator = '';

    return $sub;
}

#
# escape chdir (q//, '')
#
sub e_chdir_q {
    my($ope,$delimiter,$end_delimiter,$string) = @_;

    if ($^W) {
        if (Char::Ebig5plus::_MSWin32_5Cended_path($string)) {
            if ($] !~ /^5\.005/) {
                warn <<END;
$__FILE__: Can't chdir to '$string'

chdir does not work with chr(0x5C) at end of path
http://bugs.activestate.com/show_bug.cgi?id=81839
END
            }
        }
    }

    return e_q($ope,$delimiter,$end_delimiter,$string);
}

#
# escape chdir (qq//, "")
#
sub e_chdir {
    my($ope,$delimiter,$end_delimiter,$string) = @_;

    if ($^W) {
        if (Char::Ebig5plus::_MSWin32_5Cended_path($string)) {
            if ($] !~ /^5\.005/) {
                warn <<END;
$__FILE__: Can't chdir to '$string'

chdir does not work with chr(0x5C) at end of path
http://bugs.activestate.com/show_bug.cgi?id=81839
END
            }
        }
    }

    return e_qq($ope,$delimiter,$end_delimiter,$string);
}

#
# escape regexp of split qr//
#
sub e_split {
    my($ope,$delimiter,$end_delimiter,$string,$modifier) = @_;
    $modifier ||= '';

    if ($modifier =~ m/([adlu])/oxms) {
        my $line = 0;
        for (my $i=0; my($package,$filename,$use_line,$subroutine) = caller($i); $i++) {
            if ($filename ne __FILE__) {
                $line = $use_line + (CORE::substr($_,0,pos($_)) =~ tr/\n//) + 1;
                last;
            }
        }
        die qq{Unsupported modifier "$1" used at line $line.\n};
    }

    my $ignorecase = ($modifier =~ m/i/oxms) ? 1 : 0;

    $slash = 'div';

    my $metachar = qr/[\@\\|[\]{^]/oxms;

    # split regexp
    my @char = $string =~ m{\G(
        \\o\{ [0-7]+           \}  |
        \\    [0-7]{2,3}           |
        \\x\{ [0-9A-Fa-f]+     \}  |
        \\x   [0-9A-Fa-f]{1,2}     |
        \\c   [\x40-\x5F]          |
        \\N\{ [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \\p\{ [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \\P\{ [^\x81-\xFE0-9\}][^\x81-\xFE\}]* \} |
        \\  (?:$q_char)            |
        [\$\@] $qq_variable        |
        \$ \s* \d+                 |
        \$ \s* \{ \s* \d+ \s* \}   |
        \$ \$ (?![\w\{])           |
        \$ \s* \$ \s* $qq_variable |
        \[\:\^ (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] |
        \[\:   (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] |
        \[\^                       |
        \(\?                       |
            (?:$q_char)
    )}oxmsg;

    my $left_e  = 0;
    my $right_e = 0;
    for (my $i=0; $i <= $#char; $i++) {

        # "\L\u" --> "\u\L"
        if (($char[$i] eq '\L') and ($char[$i+1] eq '\u')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # "\U\l" --> "\l\U"
        elsif (($char[$i] eq '\U') and ($char[$i+1] eq '\l')) {
            @char[$i,$i+1] = @char[$i+1,$i];
        }

        # octal escape sequence
        elsif ($char[$i] =~ m/\A \\o \{ ([0-7]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::octchr($1);
        }

        # hexadecimal escape sequence
        elsif ($char[$i] =~ m/\A \\x \{ ([0-9A-Fa-f]+) \} \z/oxms) {
            $char[$i] = Char::Ebig5plus::hexchr($1);
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

        if (0) {
        }

        # escape last octet of multiple octet
        elsif ($char[$i] =~ m/\A \\? ([\x80-\xFF].*) ($metachar|\Q$delimiter\E|\Q$end_delimiter\E) \z/xms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # join separated multiple octet
        elsif ($char[$i] =~ m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms) {
            if (   ($i+3 <= $#char) and (grep(m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms, @char[$i+1..$i+3]) == 3) and (eval(sprintf '"%s%s%s%s"', @char[$i..$i+3]) =~ m/\A $q_char \z/oxms)) {
                $char[$i] .= join '', splice @char, $i+1, 3;
            }
            elsif (($i+2 <= $#char) and (grep(m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms, @char[$i+1..$i+2]) == 2) and (eval(sprintf '"%s%s%s"',   @char[$i..$i+2]) =~ m/\A $q_char \z/oxms)) {
                $char[$i] .= join '', splice @char, $i+1, 2;
            }
            elsif (($i+1 <= $#char) and (grep(m/\A (?: \\ [0-7]{2,3} | \\x [0-9A-Fa-f]{1,2}) \z/oxms, $char[$i+1      ]) == 1) and (eval(sprintf '"%s%s"',     @char[$i..$i+1]) =~ m/\A $q_char \z/oxms)) {
                $char[$i] .= join '', splice @char, $i+1, 1;
            }
        }

        # open character class [...]
        elsif ($char[$i] eq '[') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # open character class [^...]
        elsif ($char[$i] eq '[^') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [^...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_not_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # rewrite character class or escape character
        elsif (my $char = character_class($char[$i],$modifier)) {
            $char[$i] = $char;
        }

        # P.794 29.2.161. split
        # in Chapter 29: Functions
        # of ISBN 0-596-00027-8 Programming Perl Third Edition.
        # said "The //m modifier is assumed when you split on the pattern /^/",
        # but perl5.008 is not so. Therefore, this software adds //m.
        # (and so on)

        # split(m/^/) --> split(m/^/m)
        elsif (($char[$i] eq '^') and ($modifier !~ m/m/oxms)) {
            $modifier .= 'm';
        }

        # /i modifier
        elsif ($ignorecase and ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) and (Char::Ebig5plus::uc($char[$i]) ne Char::Ebig5plus::lc($char[$i]))) {
            $char[$i] = '[' . Char::Ebig5plus::uc($char[$i]) . Char::Ebig5plus::lc($char[$i]) . ']';
        }

        # \u \l \U \L \Q \E
        elsif ($char[$i] =~ m/\A ([<>]) \z/oxms) {
            if ($right_e < $left_e) {
                $char[$i] = '\\' . $char[$i];
            }
        }
        elsif ($char[$i] eq '\u') {
            $char[$i] = '@{[Char::Ebig5plus::ucfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\l') {
            $char[$i] = '@{[Char::Ebig5plus::lcfirst qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\U') {
            $char[$i] = '@{[Char::Ebig5plus::uc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\L') {
            $char[$i] = '@{[Char::Ebig5plus::lc qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\Q') {
            $char[$i] = '@{[CORE::quotemeta qq<';
            $left_e++;
        }
        elsif ($char[$i] eq '\E') {
            if ($right_e < $left_e) {
                $char[$i] = '>]}';
                $right_e++;
            }
            else {
                $char[$i] = '';
            }
        }
        elsif ($char[$i] eq '\Q') {
            while (1) {
                if (++$i > $#char) {
                    last;
                }
                if ($char[$i] eq '\E') {
                    last;
                }
            }
        }
        elsif ($char[$i] eq '\E') {
        }

        # $0 --> $0
        elsif ($char[$i] =~ m/\A \$ 0 \z/oxms) {
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* 0 \s* \} \z/oxms) {
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$ --> $$
        elsif ($char[$i] =~ m/\A \$\$ \z/oxms) {
        }

        # $1, $2, $3 --> $2, $3, $4 (only when multibyte anchoring is enable)
        elsif ($char[$i] =~ m/\A \$ ([1-9][0-9]*) \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }
        elsif ($char[$i] =~ m/\A \$ \{ \s* ([1-9][0-9]*) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$foo[ ... ] --> $ $foo->[ ... ]
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \[ (?:$qq_bracket)*? \] ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$foo{ ... } --> $ $foo->{ ... }
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) ( \{ (?:$qq_brace)*? \} ) \z/oxms) {
            $char[$i] = e_capture($1.'->'.$2);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $$foo
        elsif ($char[$i] =~ m/\A \$ ( \$ [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* ) \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # ${ foo }
        elsif ($char[$i] =~ m/\A \$ \s* \{ ( \s* [A-Za-z_][A-Za-z0-9_]*(?: ::[A-Za-z_][A-Za-z0-9_]*)* \s* ) \} \z/oxms) {
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $1 . ')]}';
            }
        }

        # ${ ... }
        elsif ($char[$i] =~ m/\A \$ \s* \{ \s* ( .+ ) \s* \} \z/oxms) {
            $char[$i] = e_capture($1);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # $scalar or @array
        elsif ($char[$i] =~ m/\A [\$\@].+ /oxms) {
            $char[$i] = e_string($char[$i]);
            if ($ignorecase) {
                $char[$i] = '@{[Char::Ebig5plus::ignorecase(' . $char[$i] . ')]}';
            }
        }

        # quote character before ? + * {
        elsif (($i >= 1) and ($char[$i] =~ m/\A [\?\+\*\{] \z/oxms)) {
            if ($char[$i-1] =~ m/\A (?:[\x00-\xFF]|\\[0-7]{2,3}|\\x[0-9-A-Fa-f]{1,2}) \z/oxms) {
            }
            else {
                $char[$i-1] = '(?:' . $char[$i-1] . ')';
            }
        }
    }

    # make regexp string
    $modifier =~ tr/i//d;
    if ($left_e > $right_e) {
        return join '', $ope, $delimiter, @char, '>]}' x ($left_e - $right_e), $end_delimiter, $modifier;
    }
    return     join '', $ope, $delimiter, @char,                               $end_delimiter, $modifier;
}

#
# escape regexp of split qr''
#
sub e_split_q {
    my($ope,$delimiter,$end_delimiter,$string,$modifier) = @_;
    $modifier ||= '';

    if ($modifier =~ m/([adlu])/oxms) {
        my $line = 0;
        for (my $i=0; my($package,$filename,$use_line,$subroutine) = caller($i); $i++) {
            if ($filename ne __FILE__) {
                $line = $use_line + (CORE::substr($_,0,pos($_)) =~ tr/\n//) + 1;
                last;
            }
        }
        die qq{Unsupported modifier "$1" used at line $line.\n};
    }

    my $ignorecase = ($modifier =~ m/i/oxms) ? 1 : 0;

    $slash = 'div';

    # split regexp
    my @char = $string =~ m{\G(
        \[\:\^ [a-z]+ \:\] |
        \[\:   [a-z]+ \:\] |
        \[\^               |
        \\?    (?:$q_char)
    )}oxmsg;

    # unescape character
    for (my $i=0; $i <= $#char; $i++) {
        if (0) {
        }

        # escape last octet of multiple octet
        elsif ($char[$i] =~ m/\A ([\x80-\xFF].*) ([\\|\[\{\^]|\Q$delimiter\E|\Q$end_delimiter\E) \z/xms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # open character class [...]
        elsif ($char[$i] eq '[') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # open character class [^...]
        elsif ($char[$i] eq '[^') {
            my $left = $i;
            if ($char[$i+1] eq ']') {
                $i++;
            }
            while (1) {
                if (++$i > $#char) {
                    die "$__FILE__: unmatched [] in regexp";
                }
                if ($char[$i] eq ']') {
                    my $right = $i;

                    # [^...]
                    splice @char, $left, $right-$left+1, Char::Ebig5plus::charlist_not_qr(@char[$left+1..$right-1], $modifier);

                    $i = $left;
                    last;
                }
            }
        }

        # rewrite character class or escape character
        elsif (my $char = character_class($char[$i],$modifier)) {
            $char[$i] = $char;
        }

        # split(m/^/) --> split(m/^/m)
        elsif (($char[$i] eq '^') and ($modifier !~ m/m/oxms)) {
            $modifier .= 'm';
        }

        # /i modifier
        elsif ($ignorecase and ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) and (Char::Ebig5plus::uc($char[$i]) ne Char::Ebig5plus::lc($char[$i]))) {
            $char[$i] = '[' . Char::Ebig5plus::uc($char[$i]) . Char::Ebig5plus::lc($char[$i]) . ']';
        }

        # quote character before ? + * {
        elsif (($i >= 1) and ($char[$i] =~ m/\A [\?\+\*\{] \z/oxms)) {
            if ($char[$i-1] =~ m/\A [\x00-\xFF] \z/oxms) {
            }
            else {
                $char[$i-1] = '(?:' . $char[$i-1] . ')';
            }
        }
    }

    $modifier =~ tr/i//d;
    return join '', $ope, $delimiter, @char, $end_delimiter, $modifier;
}

#
# escape require
#
sub e_require {
    my($module) = @_;

    my $expr = _pathof($module);

    return qq<Char::Ebig5plus::require '$expr';>;
}

#
# escape use without import
#
sub e_use_noimport {
    my($module) = @_;

    my $expr = _pathof($module);

    my $fh = gensym();
    for my $realfilename (_realfilename($expr)) {

        if (open($fh, $realfilename)) {
            local $/ = undef; # slurp mode
            my $script = <$fh>;
            close($fh) or die "$__FILE__: Can't close file: $realfilename";

            if ($script =~ m/^ \s* use \s+ Char::Big5Plus \s* ([^\x81-\xFE;]*) ; \s* \n? $/oxms) {
                return qq<BEGIN { Char::Ebig5plus::require '$expr'; }>;
            }
            last;
        }
    }

    return qq<use $module ();>;
}

#
# escape no without unimport
#
sub e_no_nounimport {
    my($module) = @_;

    my $expr = _pathof($module);

    my $fh = gensym();
    for my $realfilename (_realfilename($expr)) {

        if (open($fh, $realfilename)) {
            local $/ = undef; # slurp mode
            my $script = <$fh>;
            close($fh) or die "$__FILE__: Can't close file: $realfilename";

            if ($script =~ m/^ \s* use \s+ Char::Big5Plus \s* ([^\x81-\xFE;]*) ; \s* \n? $/oxms) {
                return qq<BEGIN { Char::Ebig5plus::require '$expr'; }>;
            }
            last;
        }
    }

    return qq<no $module ();>;
}

#
# escape use with import no parameter
#
sub e_use_noparam {
    my($module) = @_;

    my $expr = _pathof($module);

    my $fh = gensym();
    for my $realfilename (_realfilename($expr)) {

        if (open($fh, $realfilename)) {
            local $/ = undef; # slurp mode
            my $script = <$fh>;
            close($fh) or die "$__FILE__: Can't close file: $realfilename";

            if ($script =~ m/^ \s* use \s+ Char::Big5Plus \s* ([^\x81-\xFE;]*) ; \s* \n? $/oxms) {

                # P.326 UNIVERSAL: The Ultimate Ancestor Class
                # in Chapter 12: Objects
                # of ISBN 0-596-00027-8 Programming Perl Third Edition.
                # (and so on)

                return qq[BEGIN { Char::Ebig5plus::require '$expr'; $module->import() if $module->can('import'); }];
            }
            last;
        }
    }

    return qq<use $module;>;
}

#
# escape no with unimport no parameter
#
sub e_no_noparam {
    my($module) = @_;

    my $expr = _pathof($module);

    my $fh = gensym();
    for my $realfilename (_realfilename($expr)) {

        if (open($fh, $realfilename)) {
            local $/ = undef; # slurp mode
            my $script = <$fh>;
            close($fh) or die "$__FILE__: Can't close file: $realfilename";

            if ($script =~ m/^ \s* use \s+ Char::Big5Plus \s* ([^\x81-\xFE;]*) ; \s* \n? $/oxms) {

                # P.326 UNIVERSAL: The Ultimate Ancestor Class
                # in Chapter 12: Objects
                # of ISBN 0-596-00027-8 Programming Perl Third Edition.
                # (and so on)

                return qq[BEGIN { Char::Ebig5plus::require '$expr'; $module->unimport() if $module->can('unimport'); }];
            }
            last;
        }
    }

    return qq<no $module;>;
}

#
# escape use with import parameters
#
sub e_use {
    my($module,$list) = @_;

    my $expr = _pathof($module);

    my $fh = gensym();
    for my $realfilename (_realfilename($expr)) {

        if (open($fh, $realfilename)) {
            local $/ = undef; # slurp mode
            my $script = <$fh>;
            close($fh) or die "$__FILE__: Can't close file: $realfilename";

            if ($script =~ m/^ \s* use \s+ Char::Big5Plus \s* ([^\x81-\xFE;]*) ; \s* \n? $/oxms) {
                return qq[BEGIN { Char::Ebig5plus::require '$expr'; $module->import($list) if $module->can('import'); }];
            }
            last;
        }
    }

    return qq<use $module $list;>;
}

#
# escape no with unimport parameters
#
sub e_no {
    my($module,$list) = @_;

    my $expr = _pathof($module);

    my $fh = gensym();
    for my $realfilename (_realfilename($expr)) {

        if (open($fh, $realfilename)) {
            local $/ = undef; # slurp mode
            my $script = <$fh>;
            close($fh) or die "$__FILE__: Can't close file: $realfilename";

            if ($script =~ m/^ \s* use \s+ Char::Big5Plus \s* ([^\x81-\xFE;]*) ; \s* \n? $/oxms) {
                return qq[BEGIN { Char::Ebig5plus::require '$expr'; $module->unimport($list) if $module->can('unimport'); }];
            }
            last;
        }
    }

    return qq<no $module $list;>;
}

#
# file path of module
#
sub _pathof {
    my($expr) = @_;

    if ($^O eq 'MacOS') {
        $expr =~ s#::#:#g;
    }
    else {
        $expr =~ s#::#/#g;
    }
    $expr .= '.pm' if $expr !~ m/ \.pm \z/oxmsi;

    return $expr;
}

#
# real file name of module
#
sub _realfilename {
    my($expr) = @_;

    if ($^O eq 'MacOS') {
        return map {"$_$expr"} @INC;
    }
    else {
        return map {"$_/$expr"} @INC;
    }
}


1;

__END__

=pod

=head1 NAME

Char::Big5Plus - Source code filter to escape Big5Plus

=head1 SYNOPSIS

  use Char::Big5Plus;
  use Char::Big5Plus version;         --- require version
  use Char::Big5Plus qw(ord reverse); --- demand enhanced feature of ord and reverse
  use Char::Big5Plus version qw(ord reverse);

  # "no Char::Big5Plus;" not supported

  or

  $ perl Char/Big5Plus.pm Big5Plus_script.pl > Escaped_script.pl.e

  then

  $ perl Escaped_script.pl.e

  Big5Plus_script.pl  --- script written in Big5Plus
  Escaped_script.pl.e --- escaped script

  functions:
    Char::Big5Plus::ord(...);
    Char::Big5Plus::reverse(...);
    Char::Big5Plus::length(...);
    Char::Big5Plus::substr(...);
    Char::Big5Plus::index(...);
    Char::Big5Plus::rindex(...);

  emulate Perl5.6 on perl5.00503
    use warnings;
    use warnings::register;

  dummy functions:
    utf8::upgrade(...);
    utf8::downgrade(...);
    utf8::encode(...);
    utf8::decode(...);
    utf8::is_utf8(...);
    utf8::valid(...);
    bytes::chr(...);
    bytes::index(...);
    bytes::length(...);
    bytes::ord(...);
    bytes::rindex(...);
    bytes::substr(...);

=head1 ABSTRACT

Let's start with a bit of history: jperl 4.019+1.3 introduced Big5Plus support.
You could apply chop() and regexps even to complex CJK characters.

JPerl in CPAN Perl Ports (Binary Distributions)

said before,

  As of Perl 5.8.0 it is suggested that instead of JPerl (which is
  based on a quite old release of Perl) you should just use Perl 5.8.0,
  since it can do all that JPerl did, and more.

But was it really so?

In this country, Big5Plus is widely used on mainframe I/O, the personal computer,
and the cellular phone. This software treats Big5Plus directly, but doesn't treat
Latin-1. Therefor there is not UTF8 flag.

A difficult solution makes the problem more difficult.
Shall we escape from the encode problem?

=head1 Yet Another Future Of

JPerl is very useful software. -- Oops, note, this "JPerl" means Japanized or
Japanese Perl, so is unrelated to Java and JVM. Therefore, I named this software
better, fitter Char::Big5Plus.

Now, the last version of JPerl is 5.005_04 and is not maintained now.

Japanization modifier WATANABE Hirofumi said,

  "Because WATANABE am tired I give over maintaing JPerl."

at Slide #15: "The future of JPerl" of

L<ftp://ftp.oreilly.co.jp/pcjp98/watanabe/jperlconf.ppt>

in The Perl Confernce Japan 1998.

When I heard it, I thought that someone excluding me would maintain JPerl.
And I slept every night hanging a sock. Night and day, I kept having hope.
After 10 years, I noticed that white beard exists in the sock :-)

This software is a source code filter to escape Perl script encoded by Big5Plus
given from STDIN or command line parameter. The character code is never converted
by escaping the script. Neither the value of the character nor the length of the
character string change even if it escapes.

What's this software good for ...

=over 2

=item * Upper Compatibility like Perl4 to Perl5

=item * Maximum Portability like jcode.pl

=item * Handles Raw Big5Plus, No UTF8 flag like JPerl

=item * Remains One Interpreter like Encode module

=item * Code Set Independent like Ruby

=item * There's more than one way to do it like Perl itself

=back

Let's make yet another future by JPerl's future.

=head1 JRE: JPerl Runtime Environment

  +---------------------------------------+
  |        JPerl Application Script       | Your Script
  +---------------------------------------+
  |  Source Code Filter, Runtime Routine  | ex. Char/Big5Plus.pm, Char/Ebig5plus.pm
  +---------------------------------------+
  |          PVM 5.00503 or later         | ex. perl 5.00503
  +---------------------------------------+

A Perl Virtual Machine (PVM) enables a set of computer software programs and
data structures to use a virtual machine model for the execution of other
computer programs and scripts. The model used by a PVM accepts a form of
computer intermediate language commonly referred to as Perl byteorientedcode.
This language conceptually represents the instruction set of a byte-oriented,
capability architecture.

=head1 Basic Idea Of Source Code Filter

I discovered this mail again recently.

[Tokyo.pm] jus Benkyoukai

http://mail.pm.org/pipermail/tokyo-pm/1999-September/001854.html

save as: SJIS.pm

  package SJIS;
  use Filter::Util::Call;
  sub multibyte_filter {
      my $status;
      if (($status = filter_read()) > 0 ) {
          s/([\x81-\x9f\xe0-\xef])([\x40-\x7e\x80-\xfc])/
              sprintf("\\x%02x\\x%02x",ord($1),ord($2))
          /eg;
      }
      $status;
  }
  sub import {
      filter_add(\&multibyte_filter);
  }
  1;

I am glad that I could confirm my idea is not so wrong.

=head1 Software Composition

   Char/Big5Plus.pm               --- source code filter to escape Big5Plus
   Char/Ebig5plus.pm              --- run-time routines for Char/Big5Plus.pm
   perl55.bat            --- find and run perl5.5  without %PATH% settings
   perl56.bat            --- find and run perl5.6  without %PATH% settings
   perl58.bat            --- find and run perl5.8  without %PATH% settings
   perl510.bat           --- find and run perl5.10 without %PATH% settings
   perl512.bat           --- find and run perl5.12 without %PATH% settings
   perl514.bat           --- find and run perl5.14 without %PATH% settings
   perl64.bat            --- find and run perl64   without %PATH% settings
   strict.pm_            --- dummy strict.pm
   warnings.pm_          --- poor warnings.pm
   warnings/register.pm_ --- poor warnings/register.pm

   Rename and install strict.pm_ of this distribution to strict.pm if your system
   doesn't have strict.pm.

=head1 Upper Compatibility By Escaping

This software adds the function by 'Escaping' it always, and nothing of the
past is broken. Therefore, 'Possible job' never becomes 'Impossible job'.
This approach is effective in the field where the retreat is never permitted.
Modern Perl/perl can not always solve the problem. Often, it means an
incompatible upgrade part to traditional Perl should be rewound.

=head1 Escaping Your Script (You do)

You need write 'use Char::Big5Plus;' in your script.

  ---------------------------------
  Before      After
  ---------------------------------
  (nothing)   use Char::Big5Plus;
  ---------------------------------

=head1 Escaping Multiple Octet Code (Char/Big5Plus.pm provides)

Insert chr(0x5c) before  @  [  \  ]  ^  `  {  |  and  }  in multiple octet of

=over 2

=item * string in single quote ('', q{}, <<'END' and qw{})

=item * string in double quote ("", qq{}, <<END, <<"END", ``, qx{} and <<`END`)

=item * regexp in single quote (m'', s''', split(''), split(m'') and qr'')

=item * regexp in double quote (//, m//, ??, s///, split(//), split(m//) and qr//)

=item * character in tr/// (tr/// and y///)

=back

  ex. Japanese Katakana "SO" like [ `/ ] code is "\x83\x5C" in SJIS
 
                  see     hex dump
  -----------------------------------------
  source script   "`/"    [83 5c]
  -----------------------------------------
 
  Here, use SJIS;
                          hex dump
  -----------------------------------------
  escaped script  "`\/"   [83 [5c] 5c]
  -----------------------------------------
                    ^--- escape by SJIS software
 
  by the by       see     hex dump
  -----------------------------------------
  your eye's      "`/\"   [83 5c] [5c]
  -----------------------------------------
  perl eye's      "`\/"   [83] \[5c]
  -----------------------------------------
 
                          hex dump
  -----------------------------------------
  in the perl     "`/"    [83] [5c]
  -----------------------------------------

=head1 Escaping Character Classes (Char/Ebig5plus.pm provides)

The character classes are redefined as follows to backward compatibility.

  ---------------------------------------------------------------------------
  Before        After
  ---------------------------------------------------------------------------
   .            (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x0A])
                (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE])     (/s modifier)
  \d            [0-9]
  \s            [\x09\x0A\x0C\x0D\x20]
  \w            [0-9A-Z_a-z]
  \D            (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE0-9])
  \S            (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x0A\x0C\x0D\x20])
  \W            (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE0-9A-Z_a-z])
  \h            [\x09\x20]
  \v            [\x0C\x0A\x0D]
  \H            (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x20])
  \V            (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x0C\x0A\x0D])
  \C            [\x00-\xFF]
  \X            X (so, just 'X')
  \R            (?:\x0D\x0A|[\x0A\x0D])
  \N            (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x0A])
  ---------------------------------------------------------------------------

Also POSIX-style character classes.

  ---------------------------------------------------------------------------
  Before        After
  ---------------------------------------------------------------------------
  [:alnum:]     [\x30-\x39\x41-\x5A\x61-\x7A]
  [:alpha:]     [\x41-\x5A\x61-\x7A]
  [:ascii:]     [\x00-\x7F]
  [:blank:]     [\x09\x20]
  [:cntrl:]     [\x00-\x1F\x7F]
  [:digit:]     [\x30-\x39]
  [:graph:]     [\x21-\x7F]
  [:lower:]     [\x61-\x7A]
  [:print:]     [\x20-\x7F]
  [:punct:]     [\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E]
  [:space:]     [\x09\x0A\x0B\x0C\x0D\x20]
  [:upper:]     [\x41-\x5A]
  [:word:]      [\x30-\x39\x41-\x5A\x5F\x61-\x7A]
  [:xdigit:]    [\x30-\x39\x41-\x46\x61-\x66]
  [:^alnum:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x30-\x39\x41-\x5A\x61-\x7A])
  [:^alpha:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x41-\x5A\x61-\x7A])
  [:^ascii:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x00-\x7F])
  [:^blank:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x20])
  [:^cntrl:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x00-\x1F\x7F])
  [:^digit:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x30-\x39])
  [:^graph:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x21-\x7F])
  [:^lower:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x61-\x7A])
  [:^print:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x20-\x7F])
  [:^punct:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E])
  [:^space:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x09\x0A\x0B\x0C\x0D\x20])
  [:^upper:]    (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x41-\x5A])
  [:^word:]     (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x30-\x39\x41-\x5A\x5F\x61-\x7A])
  [:^xdigit:]   (?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE\x30-\x39\x41-\x46\x61-\x66])
  ---------------------------------------------------------------------------

Also \b and \B are redefined as follows to backward compatibility.

  ---------------------------------------------------------------------------
  Before      After
  ---------------------------------------------------------------------------
  \b          (?:\A(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[0-9A-Z_a-z])|(?<=[0-9A-Z_a-z])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]|\z))
  \B          (?:(?<=[0-9A-Z_a-z])(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]))
  ---------------------------------------------------------------------------

=head1 Escaping Built-in Functions (Char/Big5Plus.pm and Char/Ebig5plus.pm provide)

Insert 'Char::Ebig5plus::' at head of function name. Char/Ebig5plus.pm provides your script Char::Ebig5plus::*
functions.

  ---------------------------------
  Before      After
  ---------------------------------
  length      length
  substr      substr
  pos         pos
  split       Char::Ebig5plus::split
  tr///       Char::Ebig5plus::tr
  tr///b      tr///
  tr///B      tr///
  y///        Char::Ebig5plus::tr
  y///b       tr///
  y///B       tr///
  chop        Char::Ebig5plus::chop
  index       Char::Ebig5plus::index
  rindex      Char::Ebig5plus::rindex
  lc          Char::Ebig5plus::lc
  lcfirst     Char::Ebig5plus::lcfirst
  uc          Char::Ebig5plus::uc
  ucfirst     Char::Ebig5plus::ucfirst
  chr         Char::Ebig5plus::chr
  glob        Char::Ebig5plus::glob
  lstat       Char::Ebig5plus::lstat
  opendir     Char::Ebig5plus::opendir
  stat        Char::Ebig5plus::stat
  unlink      Char::Ebig5plus::unlink
  chdir       Char::Ebig5plus::chdir
  do          Char::Ebig5plus::do
  require     Char::Ebig5plus::require
  ---------------------------------

  ------------------------------------------------------------------------------------------------------------------------
  Before                   After
  ------------------------------------------------------------------------------------------------------------------------
  use Perl::Module;        BEGIN { Char::Ebig5plus::require 'Perl/Module.pm'; Perl::Module->import() if Perl::Module->can('import'); }
  use Perl::Module @list;  BEGIN { Char::Ebig5plus::require 'Perl/Module.pm'; Perl::Module->import(@list) if Perl::Module->can('import'); }
  use Perl::Module ();     BEGIN { Char::Ebig5plus::require 'Perl/Module.pm'; }
  no Perl::Module;         BEGIN { Char::Ebig5plus::require 'Perl/Module.pm'; Perl::Module->unimport() if Perl::Module->can('unimport'); }
  no Perl::Module @list;   BEGIN { Char::Ebig5plus::require 'Perl/Module.pm'; Perl::Module->unimport(@list) if Perl::Module->can('unimport'); }
  no Perl::Module ();      BEGIN { Char::Ebig5plus::require 'Perl/Module.pm'; }
  ------------------------------------------------------------------------------------------------------------------------

=head1 Un-Escaping bytes::* Functions (Char/Big5Plus.pm provides)

Char/Big5Plus.pm remove 'bytes::' at head of function name.

  ------------------------------------
  Before           After
  ------------------------------------
  bytes::chr       chr
  bytes::index     index
  bytes::length    length
  bytes::ord       ord
  bytes::rindex    rindex
  bytes::substr    substr
  ------------------------------------

=head1 Un-Escaping \ Of \N, \p, \P and \X (Char/Big5Plus.pm provides)

Char/Big5Plus.pm remove '\' at head of alphanumeric regexp metasymbols \N, \p, \P
and \X. By this method, you can avoid the trap of the abstraction.

  ------------------------------------
  Before           After
  ------------------------------------
  \N{CHARNAME}     N{CHARNAME}
  \p{L}            p{L}
  \p{^L}           p{^L}
  \p{\^L}          p{\^L}
  \pL              pL
  \P{L}            P{L}
  \P{^L}           P{^L}
  \P{\^L}          P{\^L}
  \PL              PL
  \X               X
  ------------------------------------

=head1 Escaping File Test Operators (Char/Big5Plus.pm and Char/Ebig5plus.pm provide)

Insert 'Char::Ebig5plus::' instead of '-' of operator.

  ---------------------------------
  Before      After
  ---------------------------------
  -r          Char::Ebig5plus::r
  -w          Char::Ebig5plus::w
  -x          Char::Ebig5plus::x
  -o          Char::Ebig5plus::o
  -R          Char::Ebig5plus::R
  -W          Char::Ebig5plus::W
  -X          Char::Ebig5plus::X
  -O          Char::Ebig5plus::O
  -e          Char::Ebig5plus::e
  -z          Char::Ebig5plus::z
  -f          Char::Ebig5plus::f
  -d          Char::Ebig5plus::d
  -l          Char::Ebig5plus::l
  -p          Char::Ebig5plus::p
  -S          Char::Ebig5plus::S
  -b          Char::Ebig5plus::b
  -c          Char::Ebig5plus::c
  -t          -t
  -u          Char::Ebig5plus::u
  -g          Char::Ebig5plus::g
  -k          Char::Ebig5plus::k
  -T          Char::Ebig5plus::T
  -B          Char::Ebig5plus::B
  -s          Char::Ebig5plus::s
  -M          Char::Ebig5plus::M
  -A          Char::Ebig5plus::A
  -C          Char::Ebig5plus::C
  ---------------------------------

As of Perl 5.00503, as a form of purely syntactic sugar, you can stack file
test operators, in a way that -w -x $file is equivalent to -x $file && -w _ .

  if ( -w -r $file ) {
      print "The file is both readable and writable!\n";
  }

=head1 Escaping Built-in Standard Module (Char/Ebig5plus.pm provides)

Char/Ebig5plus.pm does "BEGIN { unshift @INC, '/Perl/site/lib/Char::Big5Plus' }" at head.
Store the standard module modified for Char::Big5Plus software in this directory to
override built-in standard modules.

=head1 Escaping Standard Module Content (You do)

You need copy built-in standard module to /Perl/site/lib/Char::Big5Plus and change
'use utf8;' to 'use Char::Big5Plus;' in its. You need help yourself for now.

Back to and see 'Escaping Your Script'. Enjoy hacking!!

=head1 Escaping Function Name (You do)

You need write 'Char::Big5Plus::' at head of function name when you want character
oriented function. See 'Character Oriented Functions'.

  ---------------------------------
  Before      After
  ---------------------------------
  ord         Char::Big5Plus::ord
  reverse     Char::Big5Plus::reverse
  length      Char::Big5Plus::length
  substr      Char::Big5Plus::substr
  index       Char::Big5Plus::index
  rindex      Char::Big5Plus::rindex
  ---------------------------------

=head1 Character Oriented Functions

=over 2

=item * Order Of Character

  $ord = Char::Big5Plus::ord($string);

  This function returns the numeric value ASCII or Big5Plus of the first character
  of $string, not Unicode. The return value is always unsigned.

  If you import ord "use Char::Big5Plus qw(ord);", ord of your script will be rewritten in
  Char::Big5Plus::ord. Char::Big5Plus::ord is not compatible with ord of JPerl.

=item * Reverse List Or String

  @reverse = Char::Big5Plus::reverse(@list);
  $reverse = Char::Big5Plus::reverse(@list);

  In list context, this function returns a list value consisting of the elements of
  @list in the opposite order. The function can be used to create descending
  sequences:

  for (Char::Big5Plus::reverse(1 .. 10)) { ... }

  Because of the way hashes flatten into lists when passed as a @list, reverse can
  also be used to invert a hash, presuming the values are unique:

  %barfoo = Char::Big5Plus::reverse(%foobar);

  In scalar context, the function concatenates all the elements of LIST and then
  returns the reverse of that resulting string, character by character.

  If you import reverse "use Char::Big5Plus qw(reverse);", reverse of your script will be
  rewritten in Char::Big5Plus::reverse. Char::Big5Plus::reverse is not compatible with reverse of
  JPerl.

=item * Length By Big5Plus Character

  $length = Char::Big5Plus::length($string);
  $length = Char::Big5Plus::length();

  This function returns the length in characters of the scalar value $string. If
  $string is omitted, it returns the Char::Big5Plus::length of $_.

  Do not try to use length to find the size of an array or hash. Use scalar @array
  for the size of an array, and scalar keys %hash for the number of key/value pairs
  in a hash. (The scalar is typically omitted when redundant.)

  To find the length of a string in bytes rather than characters, say simply:

  $bytes = length($string);

=item * Substr By Big5Plus Character

  $substr = Char::Big5Plus::substr($string,$offset,$length,$replacement);
  $substr = Char::Big5Plus::substr($string,$offset,$length);
  $substr = Char::Big5Plus::substr($string,$offset);

  This function extracts a substring out of the string given by $string and returns
  it. The substring is extracted starting at $offset characters from the front of
  the string.
  If $offset is negative, the substring starts that far from the end of the string
  instead. If $length is omitted, everything to the end of the string is returned.
  If $length is negative, the length is calculated to leave that many characters off
  the end of the string. Otherwise, $length indicates the length of the substring to
  extract, which is sort of what you'd expect.

  An alternative to using Char::Big5Plus::substr as an lvalue is to specify the $replacement
  string as the fourth argument. This allows you to replace parts of the $string and
  return what was there before in one operation, just as you can with splice. The next
  example also replaces the last character of $var with "Curly" and puts that replaced
  character into $oldstr: 

  $oldstr = Char::Big5Plus::substr($var, -1, 1, "Curly");

  If you assign something shorter than the length of your substring, the string will
  shrink, and if you assign something longer than the length, the string will grow to
  accommodate it. To keep the string the same length, you may need to pad or chop your
  value using sprintf or the x operator. If you attempt to assign to an unallocated
  area past the end of the string, Char::Big5Plus::substr raises an exception.

  To prepend the string "Larry" to the current value of $_, use:

  Char::Big5Plus::substr($var, 0, 0, "Larry");

  To instead replace the first character of $_ with "Moe", use:

  Char::Big5Plus::substr($var, 0, 1, "Moe");

  And finally, to replace the last character of $var with "Curly", use:

  Char::Big5Plus::substr($var, -1, 1, "Curly");

=item * Index By Big5Plus Character

  $index = Char::Big5Plus::index($string,$substring,$offset);
  $index = Char::Big5Plus::index($string,$substring);

  This function searches for one string within another. It returns the position of
  the first occurrence of $substring in $string. The $offset, if specified, says how
  many characters from the start to skip before beginning to look. Positions are
  based at 0. If the substring is not found, the function returns one less than the
  base, ordinarily -1. To work your way through a string, you might say:

  $pos = -1;
  while (($pos = Char::Big5Plus::index($string, $lookfor, $pos)) > -1) {
      print "Found at $pos\n";
      $pos++;
  }

=item * Rindex By Big5Plus Character

  $rindex = Char::Big5Plus::rindex($string,$substring,$position);
  $rindex = Char::Big5Plus::rindex($string,$substring);

  This function works just like Char::Big5Plus::index except that it returns the position of
  the last occurrence of $substring in $string (a reverse index). The function
  returns -1 if not $substring is found. $position, if specified, is the rightmost
  position that may be returned. To work your way through a string backward, say:

  $pos = Char::Big5Plus::length($string);
  while (($pos = Char::Big5Plus::rindex($string, $lookfor, $pos)) >= 0) {
      print "Found at $pos\n";
      $pos--;
  }

=back

=head1 Perl5.6 Emulation On perl5.005

  Using warnings pragma on perl5.00503 by rename files.

  warnings.pm_ --> warnings.pm
  warnings/register.pm_ --> warnings/register.pm

=head1 Ignore Pragmas And Modules

  -----------------------------------------------------------
  Before                    After
  -----------------------------------------------------------
  use strict;               use strict; no strict qw(refs);
  use 5.12.0;               use 5.12.0; no strict qw(refs);
  require utf8;             # require utf8;
  require bytes;            # require bytes;
  require charnames;        # require charnames;
  require I18N::Japanese;   # require I18N::Japanese;
  require I18N::Collate;    # require I18N::Collate;
  require I18N::JExt;       # require I18N::JExt;
  require File::DosGlob;    # require File::DosGlob;
  require Wild;             # require Wild;
  require Wildcard;         # require Wildcard;
  require Japanese;         # require Japanese;
  use utf8;                 # use utf8;
  use bytes;                # use bytes;
  use charnames;            # use charnames;
  use I18N::Japanese;       # use I18N::Japanese;
  use I18N::Collate;        # use I18N::Collate;
  use I18N::JExt;           # use I18N::JExt;
  use File::DosGlob;        # use File::DosGlob;
  use Wild;                 # use Wild;
  use Wildcard;             # use Wildcard;
  use Japanese;             # use Japanese;
  no utf8;                  # no utf8;
  no bytes;                 # no bytes;
  no charnames;             # no charnames;
  no I18N::Japanese;        # no I18N::Japanese;
  no I18N::Collate;         # no I18N::Collate;
  no I18N::JExt;            # no I18N::JExt;
  no File::DosGlob;         # no File::DosGlob;
  no Wild;                  # no Wild;
  no Wildcard;              # no Wildcard;
  no Japanese;              # no Japanese;
  -----------------------------------------------------------

  Comment out pragma to ignore utf8 environment, and Char/Ebig5plus.pm provides these
  functions.

=over 2

=item * Dummy utf8::upgrade

  $num_octets = utf8::upgrade($string);

  Returns the number of octets necessary to represent the string.

=item * Dummy utf8::downgrade

  $success = utf8::downgrade($string[, FAIL_OK]);

  Returns true always.

=item * Dummy utf8::encode

  utf8::encode($string);

  Returns nothing.

=item * Dummy utf8::decode

  $success = utf8::decode($string);

  Returns true always.

=item * Dummy utf8::is_utf8

  $flag = utf8::is_utf8(STRING);

  Returns false always.

=item * Dummy utf8::valid

  $flag = utf8::valid(STRING);

  Returns true always.

=item * Dummy bytes::chr

  This function is same as chr.

=item * Dummy bytes::index

  This function is same as index.

=item * Dummy bytes::length

  This function is same as length.

=item * Dummy bytes::ord

  This function is same as ord.

=item * Dummy bytes::rindex

  This function is same as rindex.

=item * Dummy bytes::substr

  This function is same as substr.

=back

=head1 Environment Variable

 This software uses the flock function for exclusive control. The execution of the
 program is blocked until it becomes possible to read or write the file.
 You can have it not block in the flock function by defining environment variable
 SJIS_NONBLOCK.
 
 Example:
 
   SET SJIS_NONBLOCK=1
 
 (The value '1' doesn't have the meaning)

=head1 BUGS AND LIMITATIONS

Please patches and report problems to author are welcome.

=over 2

=item * format

Function "format" can't handle multiple octet code same as original Perl.

=item * chdir

Function chdir() can always be executed with perl5.005.

There are the following limitations for DOS-like system(any of MSWin32, NetWare,
symbian, dos).

On perl5.006 or perl5.00800, if path is ended by chr(0x5C), it needs jacode.pl
library.

On perl5.008001 or later, perl5.010, perl5.012, perl5.014, if path is ended by
chr(0x5C), it needs Win32 module. Chdir succeeds when a short path name can be
acquired according to Win32::GetShortPathName(). However, the current directory
is a short path name.

see also,
Bug #81839
chdir does not work with chr(0x5C) at end of path
http://bugs.activestate.com/show_bug.cgi?id=81839

=item * Look-behind Assertion

The look-behind assertion like (?<=[A-Z]) is not prevented from matching trail
octet of the previous multiple octet code.

=item * Char::Big5Plus::substr As Lvalue

Char::Big5Plus::substr differs from CORE::substr, and cannot be used as a lvalue.
To change part of a string, you can use the optional fourth argument which is the
replacement string.

Char::Big5Plus::substr($string, 13, 4, "JPerl");

=item * Special Variables $` And $& Doesn't Function

  Because ...

  Script
    'AAABBBCCC' =~ /BBB/;

  is escaped to
    'AAABBBCCC' =~ /\G(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE])*?(?:BBB)@Char::Ebig5plus::m_matched/;

  For multibyte anchoring,
    <\G(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE])*?> is added.

  Result
    $' = ''       (expect 'AAA')
    $& = 'AAABBB' (expect 'BBB')
    $` = 'CCC'

  Solution ...

  Script
    'AAABBBCCC' =~ /(BBB)/;

  Enclose the entire regular expression with ( ... ) for capturing.

  is escaped to
    'AAABBBCCC' =~ /\G(?:[\x81-\xFE][\x00-\xFF]|[^\x81-\xFE])*?(?:(BBB))@Char::Ebig5plus::m_matched/;

  Result
    $1 = 'BBB'

  $1 does function instead of $&.

  Or

  P.357 Avoiding the pre-match copy
  in Chapter 7: Perl
  of ISBN 0-596-00289-0 Mastering Regular Expressions, Second edition

  -------------------------------------------------
  Variable   Mimicked with
  -------------------------------------------------
  $`         substr(target, 0, $-[0])
  $&         substr(target, $-[0], $+[0], - $-[0])
  $'         substr(target, $+[0])
  -------------------------------------------------

=item * Limitation Of Regular Expression

This software has limitation from \G in multibyte anchoring. On perl5.006,
perl5.008, perl5.010, perl5.012, and perl5.014, it doesn't match in the place
in which it should match at over 32,767 octets. Moreover, at that time, neither
the error nor warning are displayed.

see also,
Bug #89792
\G can't treat over 32,767 octets
http://bugs.activestate.com/show_bug.cgi?id=89792

=item * Modifier /a /d /l And /u Of Regular Expression

The concept of this software is not to use two or more encoding methods at the
same time. Therefore, modifier /a /d /l and /u are not supported.
\d means [0-9] always.

=back

=head1 AUTHOR

INABA Hitoshi E<lt>ina@cpan.orgE<gt>

This project was originated by INABA Hitoshi.

=head1 LICENSE AND COPYRIGHT

This software is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

This software is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=head1 My Goal

P.401 See chapter 15: Unicode
of ISBN 0-596-00027-8 Programming Perl Third Edition.

Before the introduction of Unicode support in perl, The eq operator
just compared the byte-strings represented by two scalars. Beginning
with perl 5.8, eq compares two byte-strings with simultaneous
consideration of the UTF8 flag.

  Information processing model beginning with perl 5.8
 
    +----------------------+---------------------+
    |     Text strings     |                     |
    +----------+-----------|    Binary strings   |
    |   UTF8   |  Latin-1  |                     |
    +----------+-----------+---------------------+
    | UTF8     |            Not UTF8             |
    | Flagged  |            Flagged              |
    +--------------------------------------------+
    http://perl-users.jp/articles/advent-calendar/2010/casual/4
 
    You should memorize this figure.
 
    (Why is only Latin-1 special?)

This change consequentially made a big gap between a past script and
new script. Both scripts cannot re-use the code mutually any longer.
Because a new method puts a strain in the programmer, it will still take
time to replace all the in existence scripts.

The biggest problem of new method is that the UTF8 flag can't synchronize
to real encode of string. Thus you must debug about UTF8 flag, before
your script. How to solve it by returning to a past method, I will quote
page 402 of Programming Perl, 3rd ed. again.

  Information processing model beginning with this software
 
    +-----------------------------------+
    |           Octet Strings           | aka Binary strings
    +-----------------------------------+
    |         Character Strings         | aka Text strings
    +-----------------------------------+
    |      ASCII Compatible Encoding    | ex. Big5Plus
    +-----------------------------------+
                (No UTF8 Flag)
 
    You need not memorize this figure.

Ideally, I'd like to achieve these five Goals:

=over 2

=item * Goal #1:

Old byte-oriented programs should not spontaneously break on the old
byte-oriented data they used to work on.

It has already been achieved by Big5Plus designed for combining with
old byte-oriented ASCII.

=item * Goal #2:

Old byte-oriented programs should magically start working on the new
character-oriented data when appropriate.

Still now, 1 octet is counted with 1 by embedded functions length,
substr, index, rindex and pos that handle length and position of string.
In this part, there is no change. The length of 1 character of 2 octet
code is 2.

On the other hand, the regular expression in the script is added the
multibyte anchoring processing with this software, instead of you.

figure of Goal #1 and Goal #2.

                               GOAL#1  GOAL#2
                        (a)     (b)     (c)     (d)     (e)
      +--------------+-------+-------+-------+-------+-------+
      | data         |  Old  |  Old  |  New  |  Old  |  New  |
      +--------------+-------+-------+-------+-------+-------+
      | script       |  Old  |      Old      |      New      |
      +--------------+-------+---------------+---------------+
      | interpreter  |  Old  |              New              |
      +--------------+-------+-------------------------------+
      Old --- Old byte-oriented
      New --- New character-oriented

There is a combination from (a) to (e) in data, script and interpreter
of old and new. Let's add the Encode module and this software did not
exist at time of be written this document and JPerl did exist.

                        (a)     (b)     (c)     (d)     (e)
                                      JPerl           Encode,Char::Big5Plus
      +--------------+-------+-------+-------+-------+-------+
      | data         |  Old  |  Old  |  New  |  Old  |  New  |
      +--------------+-------+-------+-------+-------+-------+
      | script       |  Old  |      Old      |      New      |
      +--------------+-------+---------------+---------------+
      | interpreter  |  Old  |              New              |
      +--------------+-------+-------------------------------+
      Old --- Old byte-oriented
      New --- New character-oriented

The reason why JPerl is very excellent is that it is at the position of
(c). That is, it is not necessary to do a special description to the
script to process new character-oriented string.

Contrasting is Encode module and describing "use Char::Big5Plus;" on this software,
in this case, a new description is necessary.

=item * Goal #3:

Programs should run just as fast in the new character-oriented mode
as in the old byte-oriented mode.

It is impossible. Because the following time is necessary.

(1) Time of escape script for old byte-oriented perl.

(2) Time of processing regular expression by escaped script while
    multibyte anchoring.

=item * Goal #4:

Perl should remain one language, rather than forking into a
byte-oriented Perl and a character-oriented Perl.

JPerl forked the perl interpreter so as not to fork the Perl language.
But the Perl core team might not hope for the perl interpreter's fork.
As a result, the Perl language forked, and the community was reduced
through necessity.

A character-oriented perl is not necessary to make it specially,
because a byte-oriented perl can already treat the binary data.
This software is only an application program of Perl, a filter program.
If perl can be executed, this software will be able to be executed.

And you will get support from the Perl community, when you solve the
problem by the Perl script.

=item * Goal #5:

JPerl users will be able to maintain JPerl by Perl.

May the JPerl be with you, always.

=back

Back when Programming Perl, 3rd ed. was written, UTF8 flag was not born
and Perl is designed to make the easy jobs easy. This software provide
programming environment like at that time.

=head1 SEE ALSO

 Programming Perl, Second Edition
 By Larry Wall, Tom Christiansen, Randal L. Schwartz
 January 1900 (really so?)
 Pages: 670
 ISBN 10: 1-56592-149-6 | ISBN 13: 9781565921498
 http://shop.oreilly.com/product/9781565921498.do

 Programming Perl, Third Edition
 By Larry Wall, Tom Christiansen, Jon Orwant
 Third Edition  July 2000
 Pages: 1104
 ISBN 10: 0-596-00027-8 | ISBN 13: 9780596000271
 http://shop.oreilly.com/product/9780596000271.do

 Perl Cookbook, Second Edition
 By Tom Christiansen, Nathan Torkington
 Second Edition  August 2003
 Pages: 964
 ISBN 10: 0-596-00313-7 | ISBN 13: 9780596003135
 http://shop.oreilly.com/product/9780596003135.do

 Perl in a Nutshell, Second Edition
 By Stephen Spainhour, Ellen Siever, Nathan Patwardhan
 Second Edition  June 2002
 Pages: 760
 Series: In a Nutshell
 ISBN 10: 0-596-00241-6 | ISBN 13: 9780596002411
 http://shop.oreilly.com/product/9780596002411.do

 Learning Perl on Win32 Systems
 By Randal L. Schwartz, Erik Olson, Tom Christiansen
 August 1997
 Pages: 306
 ISBN 10: 1-56592-324-3 | ISBN 13: 9781565923249
 http://shop.oreilly.com/product/9781565923249.do

 Learning Perl, Fifth Edition
 By Randal L. Schwartz, Tom Phoenix, brian d foy
 June 2008
 Pages: 352
 Print ISBN:978-0-596-52010-6 | ISBN 10: 0-596-52010-7
 Ebook ISBN:978-0-596-10316-3 | ISBN 10: 0-596-10316-6
 http://shop.oreilly.com/product/9780596520113.do

 Perl RESOURCE KIT UNIX EDITION
 Futato, Irving, Jepson, Patwardhan, Siever
 ISBN 10: 1-56592-370-7
 http://shop.oreilly.com/product/9781565923706.do

 Understanding Japanese Information Processing
 By Ken Lunde
 January 1900
 Pages: 470
 ISBN 10: 1-56592-043-0 | ISBN 13: 9781565920439
 http://shop.oreilly.com/product/9781565920439.do

 CJKV Information Processing
 Chinese, Japanese, Korean & Vietnamese Computing
 By Ken Lunde
 First Edition  January 1999
 Pages: 1128
 ISBN 10: 1-56592-224-7 | ISBN 13: 9781565922242
 http://shop.oreilly.com/product/9781565922242.do

 Mastering Regular Expressions, Second Edition
 By Jeffrey E. F. Friedl
 Second Edition  July 2002
 Pages: 484
 ISBN 10: 0-596-00289-0 | ISBN 13: 9780596002893
 http://shop.oreilly.com/product/9780596002893.do

 Mastering Regular Expressions, Third Edition
 By Jeffrey E. F. Friedl
 Third Edition  August 2006
 Pages: 542
 ISBN 10: 0-596-52812-4 | ISBN 13:9780596528126
 http://shop.oreilly.com/product/9780596528126.do

 Regular Expressions Cookbook
 By Jan Goyvaerts, Steven Levithan
 May 2009
 Pages: 512
 ISBN 10:0-596-52068-9 | ISBN 13: 978-0-596-52068-7
 http://shop.oreilly.com/product/9780596520694.do

 PERL PUROGURAMINGU
 Larry Wall, Randal L.Schwartz, Yoshiyuki Kondo
 December 1997
 ISBN 4-89052-384-7
 http://www.context.co.jp/~cond/books/old-books.html

 JIS KANJI JITEN
 Kouji Shibano
 Pages: 1456
 ISBN 4-542-20129-5
 http://www.webstore.jsa.or.jp/lib/lib.asp?fn=/manual/mnl01_12.htm

 UNIX MAGAZINE
 1993 Aug
 Pages: 172
 T1008901080816 ZASSHI 08901-8
 http://ascii.asciimw.jp/books/books/detail/978-4-7561-5008-0.shtml

 MacPerl Power and Ease
 By Vicki Brown, Chris Nandor
 April 1998
 Pages: 350
 ISBN 10: 1881957322 | ISBN 13: 978-1881957324
 http://www.amazon.com/Macperl-Power-Ease-Vicki-Brown/dp/1881957322

 Other Tools
 http://search.cpan.org/dist/jacode/
 http://search.cpan.org/dist/Char/

 BackPAN
 http://backpan.perl.org/authors/id/I/IN/INA/

=head1 ACKNOWLEDGEMENTS

This software was made referring to software and the document that the
following hackers or persons had made. 
I am thankful to all persons.

 Rick Yamashita, Shift_JIS
 ttp://furukawablog.spaces.live.com/Blog/cns!1pmWgsL289nm7Shn7cS0jHzA!2225.entry (dead link)
 ttp://shino.tumblr.com/post/116166805/1981-us-jis
 (add 'h' at head)
 http://www.wdic.org/w/WDIC/%E3%82%B7%E3%83%95%E3%83%88JIS

 Larry Wall, Perl
 http://www.perl.org/

 Kazumasa Utashiro, jcode.pl
 ftp://ftp.iij.ad.jp/pub/IIJ/dist/utashiro/perl/
 http://log.utashiro.com/pub/2006/07/jkondo_a580.html

 Jeffrey E. F. Friedl, Mastering Regular Expressions
 http://regex.info/

 SADAHIRO Tomoyuki, The right way of using Shift_JIS
 http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm

 Yukihiro "Matz" Matsumoto, YAPC::Asia2006 Ruby on Perl(s)
 http://www.rubyist.net/~matz/slides/yapc2006/

 jscripter, For jperl users
 http://homepage1.nifty.com/kazuf/jperl.html

 Bruce., Unicode in Perl
 http://www.rakunet.org/tsnet/TSabc/18/546.html

 Hiroaki Izumi, Perl5.8/Perl5.10 is not useful on the Windows.
 http://www.aritia.jp/hizumi/perl/perlwin.html

 TSUKAMOTO Makio, Perl memo/file path of Windows
 http://digit.que.ne.jp/work/wiki.cgi?Perl%E3%83%A1%E3%83%A2%2FWindows%E3%81%A7%E3%81%AE%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%83%91%E3%82%B9

 chaichanPaPa, Matching Shift_JIS file name
 http://d.hatena.ne.jp/chaichanPaPa/20080802/1217660826

 SUZUKI Norio, Jperl
 http://homepage2.nifty.com/kipp/perl/jperl/

 WATANABE Hirofumi, Jperl
 http://www.cpan.org/src/5.0/jperl/
 http://search.cpan.org/~watanabe/
 ftp://ftp.oreilly.co.jp/pcjp98/watanabe/jperlconf.ppt

 Chuck Houpt, Michiko Nozu, MacJPerl
 http://habilis.net/macjperl/index.j.html

 Kenichi Ishigaki, Pod-PerldocJp, Welcome to modern Perl world
 http://search.cpan.org/dist/Pod-PerldocJp/
 http://gihyo.jp/dev/serial/01/modern-perl/0031
 http://gihyo.jp/dev/serial/01/modern-perl/0032
 http://gihyo.jp/dev/serial/01/modern-perl/0033

 Dan Kogai, Encode module
 http://search.cpan.org/dist/Encode/
 http://www.archive.org/details/YAPCAsia2006TokyoPerl58andUnicodeMythsFactsandChanges (video)
 http://yapc.g.hatena.ne.jp/jkondo/ (audio)

 Juerd, Perl Unicode Advice
 http://juerd.nl/site.plp/perluniadvice

 daily dayflower, 2008-06-25 perluniadvice
 http://d.hatena.ne.jp/dayflower/20080625/1214374293

 Jesse Vincent, Compatibility is a virtue
 http://www.nntp.perl.org/group/perl.perl5.porters/2010/05/msg159825.html

 Tokyo-pm archive
 http://mail.pm.org/pipermail/tokyo-pm/
 http://mail.pm.org/pipermail/tokyo-pm/1999-September/001844.html
 http://mail.pm.org/pipermail/tokyo-pm/1999-September/001854.html

 ruby-list
 http://blade.nagaokaut.ac.jp/ruby/ruby-list/index.shtml
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/2440
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/2446
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/2569
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/9427
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/9431
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/10500
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/10501
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/10502
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/12385
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/12392
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/12393
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/19156

=cut
