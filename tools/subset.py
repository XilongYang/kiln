"""Subsets sys.argv[1] down to the characters appearing in the files named
by sys.argv[3:], writing the result to sys.argv[2] and printing the
matching CSS unicode-range value. Exits with status 2 (and no output) if
the font has no glyph for any of those characters at all; an uncaught
exception (bad font file, etc.) exits 1 instead, so callers can tell
"genuinely unused" apart from "something broke".
"""

import sys

from fontTools import subset
from fontTools.ttLib import TTFont

font_path, output_path = sys.argv[1], sys.argv[2]
text_paths = sys.argv[3:]

text = ""
for p in text_paths:
    with open(p, encoding="utf-8") as f:
        text += f.read()

font = TTFont(font_path)
cmap = font.getBestCmap()
codepoints = sorted(set(ord(c) for c in text) & set(cmap.keys()))

if not codepoints:
    sys.exit(2)  # 2 = "font genuinely unused"; distinct from the default 1 an uncaught exception exits with

options = subset.Options()
options.flavor = "woff2"
subsetter = subset.Subsetter(options=options)
subsetter.populate(unicodes=codepoints)
subsetter.subset(font)
font.save(output_path)

ranges = []
start = prev = codepoints[0]
for cp in codepoints[1:]:
    if cp == prev + 1:
        prev = cp
        continue
    ranges.append((start, prev))
    start = prev = cp
ranges.append((start, prev))


def fmt(a, b):
    return "U+%X" % a if a == b else "U+%X-%X" % (a, b)


print(",".join(fmt(a, b) for a, b in ranges))
