(in-package #:cl-regex-kit/test)

(it
  "supports RE2-style control escapes, quoted literals, and Unicode metadata"
  (let ((text (format nil "~C~C~C" (code-char 7) #\Page (code-char 11))))
    (expect (match-string (match "\\a\\f\\v" text) text) :to-equal text))
  (expect
    (match-string (match "\\Q[a-z]+\\E" "--[a-z]+--") "--[a-z]+--")
    :to-equal
    "[a-z]+")
  (expect
    (match-string (match "\\p{Greek}+" "--αβ--") "--αβ--")
    :to-equal
    "αβ")
  (expect
    (match-string (match "\\p{Script=Greek}+" "--αβ--") "--αβ--")
    :to-equal
    "αβ")
  (expect
    (match-string (match "\\p{scx=Greek}+" "--αβ--") "--αβ--")
    :to-equal
    "αβ")
  (let ((prolonged-sound-mark (string (code-char #x30fc))))
    (expect (match "\\p{sc=Hiragana}" prolonged-sound-mark) :to-be-null)
    (expect (match "\\p{scx=Hiragana}" prolonged-sound-mark) :not :to-be-null)
    (expect (match "\\p{scx=Katakana}" prolonged-sound-mark) :not :to-be-null)
    (expect (match "\\p{scx=Hira}" prolonged-sound-mark) :not :to-be-null)
    (expect (match "\\p{sc=Grek}+" "--αβ--") :not :to-be-null))
  (expect
    (match-string (match "\\p{Block=Greek_And_Coptic}+" "--αβ--") "--αβ--")
    :to-equal
    "αβ")
  (expect
    (match-string
      (match "\\p{Age=V15_0}" (string (code-char #x1fae8)))
      (string (code-char #x1fae8)))
    :to-equal
    (string (code-char #x1fae8)))
  (let ((unicode-15-1-character (string (code-char #x2ebf0))))
    (expect (match "\\p{Age=15.1}" unicode-15-1-character) :to-be-truthy)
    (expect (match "\\p{Age=V15_1}" unicode-15-1-character) :to-be-truthy)
    (expect (match "\\p{Age=v151}" unicode-15-1-character) :to-be-truthy)
    (expect (match "\\p{Age=V15_0}" unicode-15-1-character) :to-be-null))
  (let ((unicode-17-character (string (code-char #x088f))))
    (expect (match "\\p{Age=17.0}" unicode-17-character) :to-be-truthy)
    (expect (match "\\p{Age=V17_0}" unicode-17-character) :to-be-truthy)
    (expect (match "\\p{Age=16.0}" unicode-17-character) :to-be-null))
  (let ((unicode-17-emoji (string (code-char #x1f6d8))))
    (expect (match "\\p{Emoji}" unicode-17-emoji) :to-be-truthy)
    (expect (match "\\p{EPres}" unicode-17-emoji) :to-be-truthy))
  (expect
    (match (format nil "(?i)~C" (code-char #xa7ce))
           (string (code-char #xa7cf)))
    :to-be-truthy)
  (expect (match "\\p{ASCII_Hex_Digit}+" "1aF") :to-be-truthy)
  (expect (match "\\p{Hex_Digit}" (string (code-char #xff26))) :to-be-truthy)
  (expect (match "\\p{Cased}" "A") :to-be-truthy)
  (expect (match "\\p{Case_Ignorable}" (string (code-char #x0027))) :to-be-truthy)
  (expect
    (match "\\p{Default_Ignorable_Code_Point}" (string (code-char #x200d)))
    :to-be-truthy)
  (expect (match "\\p{Ideographic}" (string (code-char #x4e00))) :to-be-truthy)
  (expect (match "\\p{Math}" "+") :to-be-truthy)
  (expect (match "\\p{Soft_Dotted}" "i") :to-be-truthy)
  (expect (match "\\p{Bidi_Mirrored}" "(") :to-be-truthy)
  (expect (match "\\p{Bidi_M}" "(") :to-be-truthy)
  (expect (match "\\p{Join_Control}" (string (code-char #x200c))) :to-be-truthy)
  (expect (match "\\p{Bidi_C}" (string (code-char #x061c))) :to-be-truthy)
  (expect (match "\\p{Dep}" (string (code-char #x0149))) :to-be-truthy)
  (expect
    (match "\\p{IDS_Binary_Operator}" (string (code-char #x31ef)))
    :to-be-truthy)
  (expect (match "\\p{IDS_Tri}" (string (code-char #x2ff2))) :to-be-truthy)
  (expect (match "\\p{IDS_Uni}" (string (code-char #x2ffe))) :to-be-truthy)
  (expect (match "\\p{NChar}" (string (code-char #xfdd0))) :to-be-truthy)
  (expect (match "\\p{Pat_WS}" (string (code-char #x200e))) :to-be-truthy)
  (expect (match "\\p{RI}" (string (code-char #x1f1e6))) :to-be-truthy)
  (expect (match "\\p{VS}" (string (code-char #xfe0f))) :to-be-truthy)
  (expect (match "\\p{Emoji}" (string (code-char #x1f600))) :to-be-truthy)
  (expect (match "\\p{EComp}" (string (code-char #x200d))) :to-be-truthy)
  (expect (match "\\p{EMod}" (string (code-char #x1f3fb))) :to-be-truthy)
  (expect (match "\\p{EBase}" (string (code-char #x1f44d))) :to-be-truthy)
  (expect (match "\\p{EPres}" (string (code-char #x1f600))) :to-be-truthy)
  (expect (match "\\p{ExtPict}" (string (code-char #x1f600))) :to-be-truthy)
  (expect (match "\\p{IDS}" (string (code-char #x2118))) :to-be-truthy)
  (expect (match "\\p{IDC}" (string (code-char #x00b7))) :to-be-truthy)
  (expect (match "\\p{ID_Start}" "1") :to-be-null)
  (expect (match "\\p{Dash}" (string (code-char #x2014))) :to-be-truthy)
  (expect (match "\\p{Pattern_Syntax}" "[") :to-be-truthy)
  (expect (match "\\p{Quotation_Mark}" (string (code-char #x201c))) :to-be-truthy)
  (expect
    (match "\\p{Sentence_Terminal}" (string (code-char #x3002)))
    :to-be-truthy)
  (expect (match "\\p{Terminal_Punctuation}" ",") :to-be-truthy)
  (expect (match "\\p{Unified_Ideograph}" "漢") :to-be-truthy)
  (expect (match "\\p{Grapheme_Link}" (string (code-char #x094d))) :to-be-truthy)
  (expect (match "\\p{LOE}" (string (code-char #x0e40))) :to-be-truthy)
  (expect
    (match "\\p{Other_Grapheme_Extend}" (string (code-char #x200c)))
    :to-be-truthy)
  (expect (match "\\p{PCM}" (string (code-char #x0600))) :to-be-truthy)
  (expect (match "\\p{Radical}" (string (code-char #x2f00))) :to-be-truthy)
  (expect (match "\\p{Radical}" (string (code-char #x2e9a))) :to-be nil)
  (expect (match "\\p{GCB=Extend}" (string (code-char #x0301))) :to-be-truthy)
  (expect (match "\\p{Grapheme_Cluster_Break=Other}" "a") :to-be-truthy)
  (expect (match "\\p{WB=ALetter}" "a") :to-be-truthy)
  (expect (match "\\p{Word_Break=Numeric}" "1") :to-be-truthy)
  (expect (match "\\p{SB=ATerm}" ".") :to-be-truthy)
  (progn
    (expect (match "\\p{Sentence_Break=Upper}" "A") :to-be-truthy)
    (expect (match "\\p{gcb=RI}" (string (code-char #x1f1e6))) :to-be-truthy)
    (expect (match "\\p{wb=LE}" "a") :to-be-truthy)
    (expect (match "\\p{sb=AT}" ".") :to-be-truthy)
    (expect (match "\\P{wb=LE}" "1") :to-be-truthy))
  (expect
    (match "\\p{Grapheme_Extend}" (string (code-char #x200c)))
    :to-be-truthy)
  (expect (match "\\p{Changes_When_Casefolded}" "ß") :to-be-truthy)
  (expect (match "\\p{CWL}" "A") :to-be-truthy)
  (expect (match "\\p{CWT}" "a") :to-be-truthy)
  (expect (match "\\p{CWU}" "a") :to-be-truthy)
  (expect (match "\\p{CWCM}" "A") :to-be-truthy)
  (expect (match "\\p{Diacritic}" (string (code-char #x0301))) :to-be-truthy)
  (expect (match "\\p{Extender}" (string (code-char #x30fc))) :to-be-truthy)
  (expect (match "\\p{Grapheme_Base}" "a") :to-be-truthy)
  (expect (match "\\p{Grapheme_Base}" (string (code-char #x0301))) :to-be-null)
  (expect (match "\\p{XID_Start}" "A") :to-be-truthy)
  (expect (match "\\p{XID_Continue}" (string (code-char #x0301))) :to-be-truthy))

(it-unicode-property-positive-cases
 "matches \\p{~A} against U+~4,'0X"
 (("Dia" #x0301) ("Ext" #x30fc) ("GrBase" #x0061) ("Ideo" #x4e00)
  ("IDSB" #x31ef) ("IDST" #x2ff2) ("IDSU" #x2ffe) ("JoinC" #x200c)
  ("PatSyn" #x005b) ("QMark" #x201c) ("SD" #x0069) ("STerm" #x3002)
  ("Term" #x002c) ("UIdeo" #x6f22) ("XIDS" #x0041) ("XIDC" #x0301)
  ("ID_Compat_Math_Continue" #x00b2) ("ID_Compat_Math_Start" #x2202)
  ("InCB" #x0300) ("Indic_Conjunct_Break" #x0300)
  ("Modifier_Combining_Mark" #x0654) ("MCM" #x0654)
  ("Other_Alphabetic" #x0345) ("OAlpha" #x0345)
  ("Other_Default_Ignorable_Code_Point" #x034f) ("ODI" #x034f)
  ("Other_ID_Continue" #x00b7) ("OIDC" #x00b7)
  ("Other_ID_Start" #x1885) ("OIDS" #x1885)
  ("Other_Lowercase" #x00aa) ("OLower" #x00aa)
  ("Other_Math" #x005e) ("OMath" #x005e)
  ("Other_Uppercase" #x2160) ("OUpper" #x2160)))

(it-unicode-property-negative-cases
 "does not match \\p{~A} against U+~4,'0X"
 (("Dash" #x0041) ("PatSyn" #x0061) ("Term" #x0061)
  ("RI" #x1f600) ("EMod" #x1f600) ("XIDC" #x002d)))

(it-unicode-property-alias-cases
 "matches portable Unicode general-category alias \\p{~A}"
 (("L" #x0041 #x0031)
  ("M" #x0301 #x0041)
  ("N" #x0031 #x0041)
  ("P" #x0021 #x0041)
  ("S" #x002B #x0041)
  ("Z" #x0020 #x0041)
  ("C" #x0000 #x0041)
  ("Lowercase_Letter" #x0061 #x0041)
  ("Titlecase_Letter" #x01C5 #x0061)
  ("Modifier_Letter" #x02B0 #x0061)
  ("Other_Letter" #x05D0 #x0061)
  ("Nonspacing_Mark" #x0301 #x0061)
  ("Spacing_Mark" #x0903 #x0061)
  ("Enclosing_Mark" #x20DD #x0061)
  ("Decimal_Digit_Number" #x0031 #x0061)
  ("Letter_Number" #x2160 #x0061)
  ("Other_Number" #x00BD #x0061)
  ("Connector_Punctuation" #x005F #x0061)
  ("Dash_Punctuation" #x002D #x0061)
  ("Open_Punctuation" #x0028 #x0061)
  ("Close_Punctuation" #x0029 #x0061)
  ("Initial_Punctuation" #x00AB #x0061)
  ("Final_Punctuation" #x00BB #x0061)
  ("Other_Punctuation" #x0021 #x0061)
  ("Control" #x0000 #x0041)
  ("Format" #x200C #x0041)
  ("Private_Use" #xE000 #x0041)))

(it-unicode-property-alias-cases
 "matches long major-category alias \\p{~A}"
 (("Letter" #x0041 #x0031)
  ("Mark" #x0301 #x0041)
  ("Number" #x0031 #x0041)
  ("Punct" #x0021 #x0041)
  ("Symbol" #x002B #x0041)
  ("Separator" #x0020 #x0041)
  ("Other" #x0000 #x0041)
  ("Unassigned" #x0378 #x0041)))

(it-unicode-property-normalization-cases
 "normalizes property alias \\p{~A}"
 (("Any" #x1f600 t)
  ("ASCII" #x007f t)
  ("ASCII" #x0080 nil)
  ("Assigned" #x0041 t)
  ("Assigned" #x0378 nil)
  ("InGreek_And_Coptic" #x03b1 t)
  ("Block=Greek_And_Coptic" #x0041 nil)
  ("Script_Extensions=Hira" #x30fc t)
  ("Age=15_1" #x2ebf0 t)
  ("ASCII_Hex_Digit" #x0067 nil)))

(it-invalid-unicode-property-cases
 "rejects invalid Unicode property selector ~A"
 (("Age=15") ("Age=V15_99") ("Block=Not_A_Block")))

(it
  "interprets RE2 octal escapes as byte characters"
  (let ((text (format nil "A~C" (code-char 0))))
    (expect (match-string (match "\\101\\000" text) text) :to-equal text))
  (expect
    (match-string (match "[\\101-\\103]+" "--ABC--") "--ABC--")
    :to-equal
    "ABC"))
