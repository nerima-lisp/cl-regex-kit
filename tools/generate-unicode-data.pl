#!/usr/bin/env perl
use strict;
use warnings;

use File::Basename qw(dirname);
use File::Spec;
use Getopt::Long qw(GetOptions);

sub usage {
    return <<'USAGE';
Usage: generate-unicode-data.pl --ucd-dir DIR (--check|--write)

Generate the checked-in Unicode tables from an official Unicode UCD directory.
The UCD is intentionally not vendored in this repository.
USAGE
}

my ($ucd_dir, $check, $write);
GetOptions(
    'ucd-dir=s' => \$ucd_dir,
    'check'     => \$check,
    'write'     => \$write,
) or die usage();

die usage() unless defined $ucd_dir && (($check ? 1 : 0) + ($write ? 1 : 0)) == 1;

my $root = File::Spec->rel2abs(File::Spec->catdir(dirname(__FILE__), '..'));
$ucd_dir = File::Spec->rel2abs($ucd_dir);
die "UCD directory does not exist: $ucd_dir\n" unless -d $ucd_dir;

sub trim {
    my ($text) = @_;
    $text =~ s/^\s+//;
    $text =~ s/\s+$//;
    return $text;
}

sub slurp {
    my ($path) = @_;
    open my $fh, '<:raw', $path or die "Cannot read $path: $!\n";
    local $/;
    my $text = <$fh>;
    close $fh or die "Cannot close $path: $!\n";
    return $text;
}

sub merge_ranges {
    my (@ranges) = @_;
    @ranges = sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @ranges;
    my @merged;
    for my $range (@ranges) {
        if (@merged && $range->[0] <= $merged[-1][1] + 1) {
            $merged[-1][1] = $range->[1] if $range->[1] > $merged[-1][1];
        } else {
            push @merged, [@$range];
        }
    }
    return @merged;
}

sub parse_property_ranges {
    my ($path, $property, $value) = @_;
    open my $fh, '<:raw', $path or die "Cannot read $path: $!\n";
    my @ranges;
    while (my $line = <$fh>) {
        $line =~ s/\r?\n\z//;
        $line =~ s/#.*\z//;
        next if $line =~ /^\s*\z/;
        my @fields = split /\s*;\s*/, $line, -1;
        next unless @fields >= 2;
        my $range = trim(shift @fields);
        my $name = trim(shift @fields);
        my $field_value = trim(join(';', @fields));
        next unless $name eq $property;
        next if defined($value) && $field_value ne $value;
        my ($start, $end) = $range =~ /^([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\z/
            or die "Invalid range $range in $path\n";
        $end = $start unless defined $end;
        push @ranges, [hex($start), hex($end)];
    }
    close $fh or die "Cannot close $path: $!\n";
    return [merge_ranges(@ranges)];
}

sub parse_word_break_ranges {
    my ($path) = @_;
    open my $fh, '<:raw', $path or die "Cannot read $path: $!\n";
    my @ranges;
    while (my $line = <$fh>) {
        $line =~ s/\r?\n\z//;
        $line =~ s/#.*\z//;
        next if $line =~ /^\s*\z/;
        my @fields = split /\s*;\s*/, $line, -1;
        next unless @fields >= 2;
        my $range = trim(shift @fields);
        my $class = trim(shift @fields);
        my ($start, $end) = $range =~ /^([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\z/
            or die "Invalid range $range in $path\n";
        $end = $start unless defined $end;
        push @ranges, [hex($start), hex($end), $class];
    }
    close $fh or die "Cannot close $path: $!\n";
    @ranges = sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @ranges;
    my @merged;
    for my $range (@ranges) {
        if (@merged && $range->[2] eq $merged[-1][2]
            && $range->[0] <= $merged[-1][1] + 1) {
            $merged[-1][1] = $range->[1] if $range->[1] > $merged[-1][1];
        } else {
            push @merged, [@$range];
        }
    }
    return \@merged;
}

sub union_find_find {
    my ($parent, $node) = @_;
    $parent->{$node} = $node unless exists $parent->{$node};
    if ($parent->{$node} != $node) {
        $parent->{$node} = union_find_find($parent, $parent->{$node});
    }
    return $parent->{$node};
}

sub union_find_union {
    my ($parent, $left, $right) = @_;
    my $left_root = union_find_find($parent, $left);
    my $right_root = union_find_find($parent, $right);
    $parent->{$left_root} = $right_root unless $left_root == $right_root;
}

sub parse_case_folding {
    my ($path) = @_;
    open my $fh, '<:raw', $path or die "Cannot read $path: $!\n";
    my %parent;
    while (my $line = <$fh>) {
        $line =~ s/\r?\n\z//;
        $line =~ s/#.*\z//;
        next if $line =~ /^\s*\z/;
        my @fields = split /\s*;\s*/, $line, -1;
        next unless @fields >= 3;
        my $source = hex(trim($fields[0]));
        my $status = trim($fields[1]);
        next unless $status eq 'C' || $status eq 'S';
        my ($target) = split /\s+/, trim($fields[2]);
        union_find_union(\%parent, $source, hex($target));
    }
    close $fh or die "Cannot close $path: $!\n";

    my %groups;
    for my $code_point (keys %parent) {
        my $root = union_find_find(\%parent, $code_point);
        push @{$groups{$root}}, 0 + $code_point;
    }

    my @entries;
    for my $members (values %groups) {
        next unless @$members > 1;
        @$members = sort { $a <=> $b } @$members;
        for my $source (@$members) {
            my @targets = grep { $_ != $source } @$members;
            push @entries, [$source, \@targets];
        }
    }
    @entries = sort { $a->[0] <=> $b->[0] } @entries;
    return \@entries;
}

sub range_spec {
    my ($ranges) = @_;
    return join ' ', map {
        $_->[0] == $_->[1]
            ? sprintf('%x', $_->[0])
            : sprintf('%x-%x', $_->[0], $_->[1])
    } @$ranges;
}

sub lisp_code_point {
    my ($code_point) = @_;
    return sprintf('#x%04X', $code_point);
}

sub replace_definition {
    my ($text, $symbol, $replacement) = @_;
    my $pattern = qr{^\(defparameter \Q$symbol\E\s+.*?(?=^\(|\z)}ms;
    my $replacements = ($$text =~ s{$pattern}{$replacement}e);
    die "Could not replace exactly one definition for $symbol\n" unless $replacements == 1;
}

sub update_file {
    my ($path, $content, $generated) = @_;
    if ($check) {
        return if $content eq slurp($path);
        push @$generated, $path;
        return;
    }
    open my $fh, '>:raw', $path or die "Cannot write $path: $!\n";
    print {$fh} $content or die "Cannot write $path: $!\n";
    close $fh or die "Cannot close $path: $!\n";
}

sub update_age_header {
    my ($content) = @_;
    $content =~ s/^;;; Generated from .*$/;;; Generated from the Unicode 17.0.0 UCD DerivedAge.txt./m;
    return $content;
}

my $derived_age_path = File::Spec->catfile($ucd_dir, 'DerivedAge.txt');
my $age17 = parse_property_ranges($derived_age_path, '17.0');
my $age1_path = File::Spec->catfile($root, 'src', 'unicode-age-data-1.lisp');
my $age1 = update_age_header(slurp($age1_path));
my $age17_block = "    (\"V17_0\"\n"
    . join('', map { sprintf("      (%s . %s)\n", lisp_code_point($_->[0]), lisp_code_point($_->[1])) } @$age17)
    . "      )\n";
if ($age1 =~ /^    \("V17_0"\n.*?^      \)\n/ms) {
    $age1 =~ s/^    \("V17_0"\n.*?^      \)\n/$age17_block/ms;
} else {
    $age1 =~ s/^    \("V1_1"\n/$age17_block    ("V1_1"\n/m
        or die "Could not locate the V1_1 age block\n";
}

my @generated;
update_file($age1_path, $age1, \@generated);
for my $part (qw(unicode-age-data.lisp unicode-age-data-2.lisp unicode-age-data-3.lisp)) {
    my $path = File::Spec->catfile($root, 'src', $part);
    update_file($path, update_age_header(slurp($path)), \@generated);
}

my $case_path = File::Spec->catfile($root, 'src', 'unicode-case-folding-data.lisp');
my $case = <<'HEADER';
;;;; src/unicode-case-folding-data.lisp

(in-package #:cl-regex-kit)

(defparameter *unicode-simple-case-folding*
  (let ((table (make-hash-table)))
    (dolist (entry
             '(
HEADER
for my $entry (@{parse_case_folding(File::Spec->catfile($ucd_dir, 'CaseFolding.txt'))}) {
    $case .= sprintf("    (%s (%s))\n", lisp_code_point($entry->[0]),
        join(' ', map { lisp_code_point($_) } @{$entry->[1]}));
}
$case .= <<'FOOTER';
                ))
      (destructuring-bind (source targets) entry
        (setf (gethash (code-char source) table)
              (mapcar #'code-char targets))))
    table)
  "Unicode 17.0.0 simple case-folding pairs generated from the official UCD CaseFolding.txt.")
FOOTER
update_file($case_path, $case, \@generated);

my @extra_specs = (
    ['IDCOMPATMATHCONTINUE', 'PropList.txt', 'ID_Compat_Math_Continue'],
    ['IDCOMPATMATHSTART', 'PropList.txt', 'ID_Compat_Math_Start'],
    ['INCB', 'DerivedCoreProperties.txt', 'InCB', 'Extend'],
    ['MODIFIERCOMBININGMARK', 'PropList.txt', 'Modifier_Combining_Mark'],
    ['OTHERALPHABETIC', 'PropList.txt', 'Other_Alphabetic'],
    ['OTHERDEFAULTIGNORABLECODEPOINT', 'PropList.txt', 'Other_Default_Ignorable_Code_Point'],
    ['OTHERIDCONTINUE', 'PropList.txt', 'Other_ID_Continue'],
    ['OTHERIDSTART', 'PropList.txt', 'Other_ID_Start'],
    ['OTHERLOWERCASE', 'PropList.txt', 'Other_Lowercase'],
    ['OTHERMATH', 'PropList.txt', 'Other_Math'],
    ['OTHERUPPERCASE', 'PropList.txt', 'Other_Uppercase'],
);
my $extra_path = File::Spec->catfile($root, 'src', 'unicode-extra-binary-property-data.lisp');
my $extra = <<'HEADER';
;;; Generated from the Unicode 17.0.0 UCD PropList.txt and DerivedCoreProperties.txt.
;;; Do not edit manually; regenerate with tools/generate-unicode-data.pl.

(in-package #:cl-regex-kit)

(defparameter +unicode-extra-binary-property-ranges+
  '(
HEADER
for my $spec (@extra_specs) {
    my ($name, $file, $property, $value) = @$spec;
    my $ranges = parse_property_ranges(File::Spec->catfile($ucd_dir, $file), $property, $value);
    $extra .= sprintf("    (\"%s\"\n", $name);
    $extra .= join('', map { sprintf("      (%s . %s)\n", lisp_code_point($_->[0]), lisp_code_point($_->[1])) } @$ranges);
    $extra .= "      )\n";
}
$extra .= "    ))\n";
update_file($extra_path, $extra, \@generated);

my @binary_specs = (
    ['emoji', 'emoji-data.txt', 'Emoji'],
    ['dash', 'PropList.txt', 'Dash'],
    ['hyphen', 'PropList.txt', 'Hyphen'],
    ['pattern-syntax', 'PropList.txt', 'Pattern_Syntax'],
    ['quotation-mark', 'PropList.txt', 'Quotation_Mark'],
    ['sentence-terminal', 'PropList.txt', 'Sentence_Terminal'],
    ['terminal-punctuation', 'PropList.txt', 'Terminal_Punctuation'],
    ['unified-ideograph', 'PropList.txt', 'Unified_Ideograph'],
    ['grapheme-link', 'DerivedCoreProperties.txt', 'InCB', 'Linker'],
    ['logical-order-exception', 'PropList.txt', 'Logical_Order_Exception'],
    ['other-grapheme-extend', 'PropList.txt', 'Other_Grapheme_Extend'],
    ['prepended-concatenation-mark', 'PropList.txt', 'Prepended_Concatenation_Mark'],
    ['radical', 'PropList.txt', 'Radical'],
    ['emoji-component', 'emoji-data.txt', 'Emoji_Component'],
    ['emoji-modifier', 'emoji-data.txt', 'Emoji_Modifier'],
    ['emoji-modifier-base', 'emoji-data.txt', 'Emoji_Modifier_Base'],
    ['emoji-presentation', 'emoji-data.txt', 'Emoji_Presentation'],
    ['extended-pictographic', 'emoji-data.txt', 'Extended_Pictographic'],
    ['xid-start', 'DerivedCoreProperties.txt', 'XID_Start'],
    ['xid-continue', 'DerivedCoreProperties.txt', 'XID_Continue'],
    ['grapheme-base', 'DerivedCoreProperties.txt', 'Grapheme_Base'],
    ['diacritic', 'PropList.txt', 'Diacritic'],
    ['extender', 'PropList.txt', 'Extender'],
    ['bidi-control', 'PropList.txt', 'Bidi_Control'],
    ['deprecated', 'PropList.txt', 'Deprecated'],
    ['ids-binary-operator', 'PropList.txt', 'IDS_Binary_Operator'],
    ['pattern-white-space', 'PropList.txt', 'Pattern_White_Space'],
    ['variation-selector', 'PropList.txt', 'Variation_Selector'],
);

my $binary_path = File::Spec->catfile($root, 'src', 'unicode-binary-property-range-data.lisp');
my $binary = slurp($binary_path);
$binary =~ s/Unicode 16\.0/Unicode 17.0.0/g;
$binary =~ s/Generated from regex-syntax's UCD tables/Generated from the Unicode 17.0.0 UCD property files/g;
$binary =~ s/Generated from the official UCD property files/Generated from the Unicode 17.0.0 UCD property files/g;
for my $spec (@binary_specs) {
    my ($name, $file, $property, $value) = @$spec;
    my $ranges = parse_property_ranges(File::Spec->catfile($ucd_dir, $file), $property, $value);
    my $symbol = "+$name-ranges+";
    my $replacement = "(defparameter $symbol (decode-code-point-ranges\n    \"" . range_spec($ranges) . "\"))\n";
    replace_definition(\$binary, $symbol, $replacement);
}

my %word_break_classes = (
    'ALetter'            => ':ALETTER',
    'CR'                 => ':CR',
    'Double_Quote'       => ':DOUBLE-QUOTE',
    'Extend'             => ':EXTEND',
    'ExtendNumLet'       => ':EXTENDNUMLET',
    'Format'             => ':FORMAT',
    'Hebrew_Letter'      => ':HEBREW-LETTER',
    'Katakana'           => ':KATAKANA',
    'LF'                 => ':LF',
    'MidLetter'          => ':MIDLETTER',
    'MidNum'             => ':MIDNUM',
    'MidNumLet'          => ':MIDNUMLET',
    'Newline'            => ':NEWLINE',
    'Numeric'            => ':NUMERIC',
    'Regional_Indicator' => ':REGIONAL-INDICATOR',
    'Single_Quote'       => ':SINGLE-QUOTE',
    'WSegSpace'          => ':WSEGSPACE',
    'ZWJ'                => ':ZWJ',
);
my $word_break_ranges = parse_word_break_ranges(File::Spec->catfile($ucd_dir, 'WordBreakProperty.txt'));
my $word_break_body = join('', map {
    die "Unknown Word_Break value $_->[2]\n" unless exists $word_break_classes{$_->[2]};
    sprintf("      %s %s %s\n", lisp_code_point($_->[0]), lisp_code_point($_->[1]), $word_break_classes{$_->[2]})
} @$word_break_ranges);
my $word_break_pattern = qr{(\(defparameter \+unicode-word-break-ranges\+[ \t]*\n[ \t]*#\(\n).*?(\n[ \t]*\)\))}s;
my $word_break_replacements = ($binary =~ s{$word_break_pattern}{$1 . $word_break_body . $2}e);
die "Could not replace the Unicode Word_Break range table\n" unless $word_break_replacements == 1;
$binary =~ s/Unicode 16\.0/Unicode 17.0.0/g;
$binary =~ s/Generated from Unicode 16\.0 WordBreakProperty\.txt/Generated from Unicode 17.0.0 WordBreakProperty.txt/g;
$binary =~ s/Unicode 16\.0 Word_Break/Unicode 17.0.0 Word_Break/g;
update_file($binary_path, $binary, \@generated);

if ($check && @generated) {
    print "Stale generated Unicode files:\n", join('', map { "  $_\n" } @generated);
    exit 1;
}
print $write ? "Updated Unicode 17.0.0 data files.\n" : "Unicode 17.0.0 data files are up to date.\n";
