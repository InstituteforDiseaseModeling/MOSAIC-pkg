---
name: reference-cross-platform-csv-digits
description: write.csv/as.character of the same double differs at the 15th digit between dugong/hedgehog (x86-64 Linux, long double) and the Apple Silicon Mac (no long double); byte-compare CSVs only on one platform
metadata:
  type: reference
---

On the Apple Silicon laptop `capabilities("long.double")` is FALSE (sizeof.longdouble = 8);
on the x86-64 VMs (dugong, hedgehog) R has 80-bit long double. R's 15-significant-digit
formatting (write.csv, as.character, format) decides the digit count with long double where
available, so the SAME double can print as `31.383849964287` on the Mac and
`31.3838499642869` on dugong.

Measured 2026-10-01: regenerating the 56 v2026-10.01 national prediction CSVs on the Mac from
the persisted ensemble RDS gave 47/56 files not byte-identical to the dugong-written originals
(max |diff| after re-parsing 5e-13), with identical md5s under old and new code on the Mac.

**How to apply:** to prove a change leaves a CSV byte-identical, compare old code vs new code
on the SAME machine (git-archive snapshot of the base commit + load_all), never "regenerated on
the Mac" vs "written on a VM". Do not regenerate frozen VM-written CSVs locally: the text
changes even when nothing else does.

Related: [[reference-json-roundtrip-member-reconstruction]] (the digits = I(17) JSON trap).
