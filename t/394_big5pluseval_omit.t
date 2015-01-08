# This file is encoded in Char::Big5Plus.
die "This file is not encoded in Char::Big5Plus.\n" if q{��} ne "\x82\xa0";

use Char::Big5Plus;

print "1..12\n";

# Char::Big5Plus::eval (omit) has Char::Big5Plus::eval "..."
$_ = <<'END';
Char::Big5Plus::eval " if ('�A�\' !~ /A/) { return 1 } else { return 0 } "
END
if (Char::Big5Plus::eval) {
    print qq{ok - 1 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 1 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has Char::Big5Plus::eval qq{...}
$_ = <<'END';
Char::Big5Plus::eval qq{ if ('�A�\' !~ /A/) { return 1 } else { return 0 } }
END
if (Char::Big5Plus::eval) {
    print qq{ok - 2 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 2 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has Char::Big5Plus::eval '...'
$_ = <<'END';
Char::Big5Plus::eval ' if (qq{�A�\} !~ /A/) { return 1 } else { return 0 } '
END
if (Char::Big5Plus::eval) {
    print qq{ok - 3 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 3 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has Char::Big5Plus::eval q{...}
$_ = <<'END';
Char::Big5Plus::eval q{ if ('�A�\' !~ /A/) { return 1 } else { return 0 } }
END
if (Char::Big5Plus::eval) {
    print qq{ok - 4 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 4 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has Char::Big5Plus::eval $var
$_ = <<'END';
Char::Big5Plus::eval $var2
END
my $var2 = q{ if ('�A�\' !~ /A/) { return 1 } else { return 0 } };
if (Char::Big5Plus::eval) {
    print qq{ok - 5 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 5 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has Char::Big5Plus::eval (omit)
$_ = <<'END';
$_ = "if ('�A�\' !~ /A/) { return 1 } else { return 0 }";
Char::Big5Plus::eval
END
if (Char::Big5Plus::eval) {
    print qq{ok - 6 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 6 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has Char::Big5Plus::eval {...}
$_ = <<'END';
Char::Big5Plus::eval { if ('�A�\' !~ /A/) { return 1 } else { return 0 } }
END
if (Char::Big5Plus::eval) {
    print qq{ok - 7 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 7 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has "..."
$_ = <<'END';
if ('�A�\' !~ /A/) { return "1" } else { return "0" }
END
if (Char::Big5Plus::eval) {
    print qq{ok - 8 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 8 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has qq{...}
$_ = <<'END';
if ('�A�\' !~ /A/) { return qq{1} } else { return qq{0} }
END
if (Char::Big5Plus::eval) {
    print qq{ok - 9 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 9 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has '...'
$_ = <<'END';
if ('�A�\' !~ /A/) { return '1' } else { return '0' }
END
if (Char::Big5Plus::eval) {
    print qq{ok - 10 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 10 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has q{...}
$_ = <<'END';
if ('�A�\' !~ /A/) { return q{1} } else { return q{0} }
END
if (Char::Big5Plus::eval) {
    print qq{ok - 11 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 11 $^X @{[__FILE__]}\n};
}

# Char::Big5Plus::eval (omit) has $var
$_ = <<'END';
if ('�A�\' !~ /A/) { return $var1 } else { return $var0 }
END
my $var1 = 1;
my $var0 = 0;
if (Char::Big5Plus::eval) {
    print qq{ok - 12 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 12 $^X @{[__FILE__]}\n};
}

__END__
