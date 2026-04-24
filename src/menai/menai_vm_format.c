/*
 * menai_vm_format.c — number-to-string formatting for the Menai VM.
 *
 * Implements shortest round-trip double formatting that matches Python's
 * str() output, using only the C standard library.  No Python API is used.
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_value.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_string.h"

#include "menai_vm_format.h"

/*
 * shortest_double — write the shortest round-trip decimal representation of
 * v into buf (which must be at least 32 bytes).  Does not handle inf, nan,
 * or negative zero — callers must check for those first.
 *
 * Uses the %.*g loop: try increasing precision until strtod(buf) == v.
 * Appends ".0" if the result contains neither '.' nor 'e', so that the
 * output always looks like a float rather than an integer.
 */
static void
shortest_double(double v, char *buf, size_t bufsz)
{
    for (int prec = 1; prec <= 17; prec++) {
        snprintf(buf, bufsz, "%.*g", prec, v);
        if (strtod(buf, NULL) == v) {
            break;
        }
    }

    if (strchr(buf, '.') == NULL && strchr(buf, 'e') == NULL) {
        size_t len = strlen(buf);
        buf[len] = '.';
        buf[len + 1] = '0';
        buf[len + 2] = '\0';
    }
}

/*
 * format_component — write a complex component value into buf.
 *
 * Integer-valued components (finite, within long range, and equal to their
 * floor) are formatted as integers — matching Python's complex str() which
 * produces "1+2j" rather than "1.0+2.0j".  All other finite values use the
 * shortest round-trip representation.  inf and nan are written literally.
 */
static void
format_component(double v, char *buf, size_t bufsz)
{
    if (isinf(v)) {
        snprintf(buf, bufsz, "%s", v > 0.0 ? "inf" : "-inf");
        return;
    }

    if (isnan(v)) {
        snprintf(buf, bufsz, "nan");
        return;
    }

    if (v >= (double)LONG_MIN && v <= (double)LONG_MAX && v == (double)(long)v) {
        snprintf(buf, bufsz, "%ld", (long)v);
        return;
    }

    shortest_double(v, buf, bufsz);
}

/*
 * menai_format_float — format a double as a Menai string, matching Python's
 * str(float) output exactly.
 *
 * Returns a new MenaiValue * (MenaiString), or NULL on allocation failure.
 */
MenaiValue *
menai_format_float(double v)
{
    char buf[32];

    if (isinf(v)) {
        snprintf(buf, sizeof(buf), "%s", v > 0.0 ? "inf" : "-inf");
    } else if (isnan(v)) {
        snprintf(buf, sizeof(buf), "nan");
    } else if (v == 0.0 && signbit(v)) {
        snprintf(buf, sizeof(buf), "-0.0");
    } else {
        shortest_double(v, buf, sizeof(buf));
    }

    return menai_string_from_utf8(buf, (ssize_t)strlen(buf));
}

/*
 * menai_format_complex — format a complex number as a Menai string, matching
 * Python's str(complex).strip('()') output exactly.
 *
 * Returns a new MenaiValue * (MenaiString), or NULL on allocation failure.
 */
MenaiValue *
menai_format_complex(double real, double imag)
{
    char rbuf[32];
    char ibuf[32];
    char out[128];

    format_component(real, rbuf, sizeof(rbuf));
    format_component(imag, ibuf, sizeof(ibuf));

    int show_real = !(real == 0.0 && !signbit(real));

    if (show_real) {
        if (imag >= 0.0 || isnan(imag)) {
            snprintf(out, sizeof(out), "%s+%sj", rbuf, ibuf);
        } else {
            snprintf(out, sizeof(out), "%s%sj", rbuf, ibuf);
        }
    } else {
        snprintf(out, sizeof(out), "%sj", ibuf);
    }

    return menai_string_from_utf8(out, (ssize_t)strlen(out));
}
