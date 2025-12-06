#!/usr/bin/awk -f
{
    gsub(/[^0-9]/,"",$0)
    n = length($0)
    if (n < 2) { print ""; next }
    max = -1
    for (i = 1; i <= n-1; i++) {
        di = substr($0, i, 1) + 0
        for (j = i+1; j <= n; j++) {
            dj = substr($0, j, 1) + 0
            val = di * 10 + dj
            if (val > max) max = val
        }
    }
    printf "%02d\n", max
}