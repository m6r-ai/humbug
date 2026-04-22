/*
 * menai_vm_string_tables.h — Unicode character property tables for the Menai VM.
 *
 * Data is from Unicode 15.0.
 *
 * Provides:
 *   unicode_is_whitespace(cp)       — whitespace predicate
 *   unicode_simple_upcase(cp)       — single-codepoint uppercase mapping
 *   unicode_simple_downcase(cp)     — single-codepoint lowercase mapping
 *   unicode_upcase_expansion(cp)    — multi-codepoint uppercase expansion
 */

#ifndef MENAI_VM_STRING_TABLES_H
#define MENAI_VM_STRING_TABLES_H

#include <stdint.h>

/* ---------------------------------------------------------------------------
 * unicode_is_whitespace
 * ------------------------------------------------------------------------- */

typedef struct {
    uint32_t lo;
    uint32_t hi;
} MenaiCpRange;

static const MenaiCpRange menai_whitespace_ranges[] = {
    { 0x0009, 0x000D }, /* HT, LF, VT, FF, CR */
    { 0x0020, 0x0020 }, /* SPACE */
    { 0x0085, 0x0085 }, /* NEXT LINE */
    { 0x00A0, 0x00A0 }, /* NO-BREAK SPACE */
    { 0x1680, 0x1680 }, /* OGHAM SPACE MARK */
    { 0x2000, 0x200A }, /* EN QUAD .. HAIR SPACE */
    { 0x2028, 0x2029 }, /* LINE SEPARATOR, PARAGRAPH SEPARATOR */
    { 0x202F, 0x202F }, /* NARROW NO-BREAK SPACE */
    { 0x205F, 0x205F }, /* MEDIUM MATHEMATICAL SPACE */
    { 0x3000, 0x3000 }, /* IDEOGRAPHIC SPACE */
    { 0xFEFF, 0xFEFF }, /* BOM / ZERO WIDTH NO-BREAK SPACE */
};

#define MENAI_WHITESPACE_RANGE_COUNT \
    ((int)(sizeof(menai_whitespace_ranges) / sizeof(menai_whitespace_ranges[0])))

static inline int unicode_is_whitespace(uint32_t cp)
{
    int lo = 0;
    int hi = MENAI_WHITESPACE_RANGE_COUNT - 1;
    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        if (cp < menai_whitespace_ranges[mid].lo) {
            hi = mid - 1;
        } else if (cp > menai_whitespace_ranges[mid].hi) {
            lo = mid + 1;
        } else {
            return 1;
        }
    }

    return 0;
}

/* ---------------------------------------------------------------------------
 * unicode_simple_upcase / unicode_simple_downcase
 *
 * Covers ASCII, Latin-1 Supplement, Latin Extended-A/B, Greek, Cyrillic,
 * Armenian, and Georgian (BMP case mappings from Unicode 15.0).
 *
 * Strategy:
 *   1. ASCII fast path.
 *   2. Binary search over MenaiCaseRange tables for blocks where the mapping
 *      is a constant offset (delta added to codepoint gives mapped codepoint).
 *   3. Binary search over MenaiCasePair tables for irregular singleton
 *      mappings that do not fit a constant-delta range.
 *   4. Existing Latin-1 switch for the remaining Latin-1 cases.
 *
 * All tables must be sorted by .lo / .from for binary search to be correct.
 * ------------------------------------------------------------------------- */

typedef struct { uint32_t lo; uint32_t hi; int32_t delta; } MenaiCaseRange;
typedef struct { uint32_t from; uint32_t to; } MenaiCasePair;

/* ---- Binary search helpers ----------------------------------------------- */

static inline uint32_t menai_search_case_ranges(
        uint32_t cp,
        const MenaiCaseRange *ranges, int count)
{
    int lo = 0, hi = count - 1;
    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        if (cp < ranges[mid].lo) {
            hi = mid - 1;
        } else if (cp > ranges[mid].hi) {
            lo = mid + 1;
        } else {
            return (uint32_t)((int32_t)cp + ranges[mid].delta);
        }
    }

    return 0;
}

static inline uint32_t menai_search_case_pairs(
        uint32_t cp,
        const MenaiCasePair *pairs, int count)
{
    int lo = 0, hi = count - 1;
    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        if (cp < pairs[mid].from) {
            hi = mid - 1;
        } else if (cp > pairs[mid].from) {
            lo = mid + 1;
        } else {
            return pairs[mid].to;
        }
    }

    return 0;
}

/* ---- Upcase range table --------------------------------------------------
 *
 * Each entry covers a contiguous run of lowercase codepoints that all map to
 * uppercase by adding delta (delta is negative: uppercase < lowercase).
 * Sorted by .lo.
 *
 * Latin Extended-A (U+0100-U+017F): mostly even=upper, odd=lower pairs.
 * Because the even and odd codepoints interleave, we cannot express the whole
 * block as a single range without also matching uppercase codepoints.  We
 * therefore list each lowercase codepoint as a unit range [cp, cp, -1].
 * Exceptions (irregular mappings) go in menai_upcase_pairs instead.
 *
 * Latin Extended-B (U+0180-U+024F): mostly irregular; unit ranges for the
 * even/odd paired sub-blocks, irregular ones in menai_upcase_pairs.
 *
 * Greek (U+03B1-U+03CB): two true ranges with delta -0x20, plus one unit
 * range for final sigma (U+03C2, delta -0x21).
 *
 * Cyrillic (U+0430-U+045F): two true ranges; supplementary paired letters
 * listed as unit ranges.
 *
 * Armenian (U+0561-U+0586): one true range, delta -0x30.
 *
 * Georgian Mkhedruli (U+2D00-U+2D25, U+2D27, U+2D2D): true range + units,
 * delta -0x1C60.
 */
static const MenaiCaseRange menai_upcase_ranges[] = {
    /* Latin Extended-A: even=upper, odd=lower, delta=-1 */
    { 0x0101, 0x0101, -1 }, /* ā → Ā */
    { 0x0103, 0x0103, -1 }, /* ă → Ă */
    { 0x0105, 0x0105, -1 }, /* ą → Ą */
    { 0x0107, 0x0107, -1 }, /* ć → Ć */
    { 0x0109, 0x0109, -1 }, /* ĉ → Ĉ */
    { 0x010B, 0x010B, -1 }, /* ċ → Ċ */
    { 0x010D, 0x010D, -1 }, /* č → Č */
    { 0x010F, 0x010F, -1 }, /* ď → Ď */
    { 0x0111, 0x0111, -1 }, /* đ → Đ */
    { 0x0113, 0x0113, -1 }, /* ē → Ē */
    { 0x0115, 0x0115, -1 }, /* ĕ → Ĕ */
    { 0x0117, 0x0117, -1 }, /* ė → Ė */
    { 0x0119, 0x0119, -1 }, /* ę → Ę */
    { 0x011B, 0x011B, -1 }, /* ě → Ě */
    { 0x011D, 0x011D, -1 }, /* ĝ → Ĝ */
    { 0x011F, 0x011F, -1 }, /* ğ → Ğ */
    { 0x0121, 0x0121, -1 }, /* ġ → Ġ */
    { 0x0123, 0x0123, -1 }, /* ģ → Ģ */
    { 0x0125, 0x0125, -1 }, /* ĥ → Ĥ */
    { 0x0127, 0x0127, -1 }, /* ħ → Ħ */
    { 0x0129, 0x0129, -1 }, /* ĩ → Ĩ */
    { 0x012B, 0x012B, -1 }, /* ī → Ī */
    { 0x012D, 0x012D, -1 }, /* ĭ → Ĭ */
    { 0x012F, 0x012F, -1 }, /* į → Į */
    /* U+0130 (İ) is uppercase; U+0131 (ı) → U+0049 (I): pair table */
    { 0x0133, 0x0133, -1 }, /* ĳ → Ĳ */
    { 0x0135, 0x0135, -1 }, /* ĵ → Ĵ */
    { 0x0137, 0x0137, -1 }, /* ķ → Ķ */
    /* U+0138 (ĸ): no uppercase */
    /* U+0139-U+0148: odd=upper, even=lower */
    { 0x013A, 0x013A, -1 }, /* ĺ → Ĺ */
    { 0x013C, 0x013C, -1 }, /* ļ → Ļ */
    { 0x013E, 0x013E, -1 }, /* ľ → Ľ */
    { 0x0140, 0x0140, -1 }, /* ŀ → Ŀ */
    { 0x0142, 0x0142, -1 }, /* ł → Ł */
    { 0x0144, 0x0144, -1 }, /* ń → Ń */
    { 0x0146, 0x0146, -1 }, /* ņ → Ņ */
    { 0x0148, 0x0148, -1 }, /* ň → Ň */
    /* U+0149 (ŉ): multi-char uppercase — expansion table */
    /* U+014A-U+0177: even=upper, odd=lower */
    { 0x014B, 0x014B, -1 }, /* ŋ → Ŋ */
    { 0x014D, 0x014D, -1 }, /* ō → Ō */
    { 0x014F, 0x014F, -1 }, /* ŏ → Ŏ */
    { 0x0151, 0x0151, -1 }, /* ő → Ő */
    { 0x0153, 0x0153, -1 }, /* œ → Œ */
    { 0x0155, 0x0155, -1 }, /* ŕ → Ŕ */
    { 0x0157, 0x0157, -1 }, /* ŗ → Ŗ */
    { 0x0159, 0x0159, -1 }, /* ř → Ř */
    { 0x015B, 0x015B, -1 }, /* ś → Ś */
    { 0x015D, 0x015D, -1 }, /* ŝ → Ŝ */
    { 0x015F, 0x015F, -1 }, /* ş → Ş */
    { 0x0161, 0x0161, -1 }, /* š → Š */
    { 0x0163, 0x0163, -1 }, /* ţ → Ţ */
    { 0x0165, 0x0165, -1 }, /* ť → Ť */
    { 0x0167, 0x0167, -1 }, /* ŧ → Ŧ */
    { 0x0169, 0x0169, -1 }, /* ũ → Ũ */
    { 0x016B, 0x016B, -1 }, /* ū → Ū */
    { 0x016D, 0x016D, -1 }, /* ŭ → Ŭ */
    { 0x016F, 0x016F, -1 }, /* ů → Ů */
    { 0x0171, 0x0171, -1 }, /* ű → Ű */
    { 0x0173, 0x0173, -1 }, /* ų → Ų */
    { 0x0175, 0x0175, -1 }, /* ŵ → Ŵ */
    { 0x0177, 0x0177, -1 }, /* ŷ → Ŷ */
    /* U+0178 (Ÿ) → U+00FF: Latin-1 switch */
    /* U+0179-U+017E: odd=upper, even=lower */
    { 0x017A, 0x017A, -1 }, /* ź → Ź */
    { 0x017C, 0x017C, -1 }, /* ż → Ż */
    { 0x017E, 0x017E, -1 }, /* ž → Ž */
    /* U+017F (ſ) → U+0053 (S): pair table */

    /* Latin Extended-B: paired sub-blocks with delta=-1 */
    { 0x0183, 0x0183, -1 }, /* ƃ → Ƃ */
    { 0x0185, 0x0185, -1 }, /* ƅ → Ƅ */
    { 0x0188, 0x0188, -1 }, /* ƈ → Ƈ */
    { 0x018C, 0x018C, -1 }, /* ƌ → Ƌ */
    { 0x0192, 0x0192, -1 }, /* ƒ → Ƒ */
    { 0x0199, 0x0199, -1 }, /* ƙ → Ƙ */
    { 0x01A1, 0x01A1, -1 }, /* ơ → Ơ */
    { 0x01A3, 0x01A3, -1 }, /* ƣ → Ƣ */
    { 0x01A5, 0x01A5, -1 }, /* ƥ → Ƥ */
    { 0x01A8, 0x01A8, -1 }, /* ƨ → Ƨ */
    { 0x01AD, 0x01AD, -1 }, /* ƭ → Ƭ */
    { 0x01B0, 0x01B0, -1 }, /* ư → Ư */
    { 0x01B4, 0x01B4, -1 }, /* ƴ → Ƴ */
    { 0x01B6, 0x01B6, -1 }, /* ƶ → Ƶ */
    { 0x01B9, 0x01B9, -1 }, /* ƹ → Ƹ */
    { 0x01BD, 0x01BD, -1 }, /* ƽ → Ƽ */
    /* Digraph title/lowercase → uppercase */
    { 0x01C5, 0x01C5, -1 }, /* Dž → DŽ */
    { 0x01C6, 0x01C6, -2 }, /* dž → DŽ */
    { 0x01C8, 0x01C8, -1 }, /* Lj → LJ */
    { 0x01C9, 0x01C9, -2 }, /* lj → LJ */
    { 0x01CB, 0x01CB, -1 }, /* Nj → NJ */
    { 0x01CC, 0x01CC, -2 }, /* nj → NJ */
    /* U+01CD-U+01DC: odd=upper, even=lower */
    { 0x01CE, 0x01CE, -1 }, /* ǎ → Ǎ */
    { 0x01D0, 0x01D0, -1 }, /* ǐ → Ǐ */
    { 0x01D2, 0x01D2, -1 }, /* ǒ → Ǒ */
    { 0x01D4, 0x01D4, -1 }, /* ǔ → Ǔ */
    { 0x01D6, 0x01D6, -1 }, /* ǖ → Ǖ */
    { 0x01D8, 0x01D8, -1 }, /* ǘ → Ǘ */
    { 0x01DA, 0x01DA, -1 }, /* ǚ → Ǚ */
    { 0x01DC, 0x01DC, -1 }, /* ǜ → Ǜ */
    /* U+01DD (ǝ) → U+018E (Ǝ): pair table */
    /* U+01DE-U+01EE: even=upper, odd=lower */
    { 0x01DF, 0x01DF, -1 }, /* ǟ → Ǟ */
    { 0x01E1, 0x01E1, -1 }, /* ǡ → Ǡ */
    { 0x01E3, 0x01E3, -1 }, /* ǣ → Ǣ */
    { 0x01E5, 0x01E5, -1 }, /* ǥ → Ǥ */
    { 0x01E7, 0x01E7, -1 }, /* ǧ → Ǧ */
    { 0x01E9, 0x01E9, -1 }, /* ǩ → Ǩ */
    { 0x01EB, 0x01EB, -1 }, /* ǫ → Ǫ */
    { 0x01ED, 0x01ED, -1 }, /* ǭ → Ǭ */
    { 0x01EF, 0x01EF, -1 }, /* ǯ → Ǯ */
    /* U+01F0 (ǰ): multi-char uppercase — expansion table */
    /* U+01F2 (Dz title) → U+01F1 (DZ); U+01F3 (dz) → U+01F1 (DZ): pair table */
    { 0x01F5, 0x01F5, -1 }, /* ǵ → Ǵ */
    /* U+01F8-U+021E: even=upper, odd=lower */
    { 0x01F9, 0x01F9, -1 }, /* ǹ → Ǹ */
    { 0x01FB, 0x01FB, -1 }, /* ǻ → Ǻ */
    { 0x01FD, 0x01FD, -1 }, /* ǽ → Ǽ */
    { 0x01FF, 0x01FF, -1 }, /* ǿ → Ǿ */
    { 0x0201, 0x0201, -1 }, /* ȁ → Ȁ */
    { 0x0203, 0x0203, -1 }, /* ȃ → Ȃ */
    { 0x0205, 0x0205, -1 }, /* ȅ → Ȅ */
    { 0x0207, 0x0207, -1 }, /* ȇ → Ȇ */
    { 0x0209, 0x0209, -1 }, /* ȉ → Ȉ */
    { 0x020B, 0x020B, -1 }, /* ȋ → Ȋ */
    { 0x020D, 0x020D, -1 }, /* ȍ → Ȍ */
    { 0x020F, 0x020F, -1 }, /* ȏ → Ȏ */
    { 0x0211, 0x0211, -1 }, /* ȑ → Ȑ */
    { 0x0213, 0x0213, -1 }, /* ȓ → Ȓ */
    { 0x0215, 0x0215, -1 }, /* ȕ → Ȕ */
    { 0x0217, 0x0217, -1 }, /* ȗ → Ȗ */
    { 0x0219, 0x0219, -1 }, /* ș → Ș */
    { 0x021B, 0x021B, -1 }, /* ț → Ț */
    { 0x021D, 0x021D, -1 }, /* ȝ → Ȝ */
    { 0x021F, 0x021F, -1 }, /* ȟ → Ȟ */
    /* U+0220 (Ƞ) is uppercase; U+019E (ƞ) → U+0220: pair table */
    /* U+0222-U+0232: even=upper, odd=lower */
    { 0x0223, 0x0223, -1 }, /* ȣ → Ȣ */
    { 0x0225, 0x0225, -1 }, /* ȥ → Ȥ */
    { 0x0227, 0x0227, -1 }, /* ȧ → Ȧ */
    { 0x0229, 0x0229, -1 }, /* ȩ → Ȩ */
    { 0x022B, 0x022B, -1 }, /* ȫ → Ȫ */
    { 0x022D, 0x022D, -1 }, /* ȭ → Ȭ */
    { 0x022F, 0x022F, -1 }, /* ȯ → Ȯ */
    { 0x0231, 0x0231, -1 }, /* ȱ → Ȱ */
    { 0x0233, 0x0233, -1 }, /* ȳ → Ȳ */
    /* U+023C (ȼ) → U+023B (Ȼ): pair table */
    /* U+023F (ȿ) → U+2C7E; U+0240 (ɀ) → U+2C7F: pair table */
    { 0x0242, 0x0242, -1 }, /* ɂ → Ɂ */
    /* U+0246-U+024E: even=upper, odd=lower */
    { 0x0247, 0x0247, -1 }, /* ɇ → Ɇ */
    { 0x0249, 0x0249, -1 }, /* ɉ → Ɉ */
    { 0x024B, 0x024B, -1 }, /* ɋ → Ɋ */
    { 0x024D, 0x024D, -1 }, /* ɍ → Ɍ */
    { 0x024F, 0x024F, -1 }, /* ɏ → Ɏ */

    /* Greek lowercase → uppercase */
    { 0x03B1, 0x03C1, -0x20 }, /* α..ρ → Α..Ρ */
    { 0x03C2, 0x03C2, -0x21 }, /* ς → Σ (final sigma) */
    { 0x03C3, 0x03CB, -0x20 }, /* σ..ϋ → Σ..Ϋ */
    /* Coptic letters in Greek block: even=upper, odd=lower, delta=-1 */
    { 0x03D9, 0x03D9, -1 }, /* ϙ → Ϙ */
    { 0x03DB, 0x03DB, -1 }, /* ϛ → Ϛ */
    { 0x03DD, 0x03DD, -1 }, /* ϝ → Ϝ */
    { 0x03DF, 0x03DF, -1 }, /* ϟ → Ϟ */
    { 0x03E1, 0x03E1, -1 }, /* ϡ → Ϡ */
    { 0x03E3, 0x03E3, -1 }, /* ϣ → Ϣ */
    { 0x03E5, 0x03E5, -1 }, /* ϥ → Ϥ */
    { 0x03E7, 0x03E7, -1 }, /* ϧ → Ϧ */
    { 0x03E9, 0x03E9, -1 }, /* ϩ → Ϩ */
    { 0x03EB, 0x03EB, -1 }, /* ϫ → Ϫ */
    { 0x03ED, 0x03ED, -1 }, /* ϭ → Ϭ */
    { 0x03EF, 0x03EF, -1 }, /* ϯ → Ϯ */
    { 0x03F8, 0x03F8, -1 }, /* ϸ → Ϸ */
    { 0x03FB, 0x03FB, -1 }, /* ϻ → Ϻ */

    /* Cyrillic lowercase → uppercase */
    { 0x0430, 0x044F, -0x20 }, /* а..я → А..Я */
    { 0x0450, 0x045F, -0x50 }, /* ѐ..џ → Ѐ..Џ */
    /* Cyrillic supplementary: even=upper, odd=lower, delta=-1 */
    { 0x0461, 0x0461, -1 }, /* ѡ → Ѡ */
    { 0x0463, 0x0463, -1 }, /* ѣ → Ѣ */
    { 0x0465, 0x0465, -1 }, /* ѥ → Ѥ */
    { 0x0467, 0x0467, -1 }, /* ѧ → Ѧ */
    { 0x0469, 0x0469, -1 }, /* ѩ → Ѩ */
    { 0x046B, 0x046B, -1 }, /* ѫ → Ѫ */
    { 0x046D, 0x046D, -1 }, /* ѭ → Ѭ */
    { 0x046F, 0x046F, -1 }, /* ѯ → Ѯ */
    { 0x0471, 0x0471, -1 }, /* ѱ → Ѱ */
    { 0x0473, 0x0473, -1 }, /* ѳ → Ѳ */
    { 0x0475, 0x0475, -1 }, /* ѵ → Ѵ */
    { 0x0477, 0x0477, -1 }, /* ѷ → Ѷ */
    { 0x0479, 0x0479, -1 }, /* ѹ → Ѹ */
    { 0x047B, 0x047B, -1 }, /* ѻ → Ѻ */
    { 0x047D, 0x047D, -1 }, /* ѽ → Ѽ */
    { 0x047F, 0x047F, -1 }, /* ѿ → Ѿ */
    { 0x0481, 0x0481, -1 }, /* ҁ → Ҁ */
    { 0x048B, 0x048B, -1 }, /* ҋ → Ҋ */
    { 0x048D, 0x048D, -1 }, /* ҍ → Ҍ */
    { 0x048F, 0x048F, -1 }, /* ҏ → Ҏ */
    { 0x0491, 0x0491, -1 }, /* ґ → Ґ */
    { 0x0493, 0x0493, -1 }, /* ғ → Ғ */
    { 0x0495, 0x0495, -1 }, /* ҕ → Ҕ */
    { 0x0497, 0x0497, -1 }, /* җ → Җ */
    { 0x0499, 0x0499, -1 }, /* ҙ → Ҙ */
    { 0x049B, 0x049B, -1 }, /* қ → Қ */
    { 0x049D, 0x049D, -1 }, /* ҝ → Ҝ */
    { 0x049F, 0x049F, -1 }, /* ҟ → Ҟ */
    { 0x04A1, 0x04A1, -1 }, /* ҡ → Ҡ */
    { 0x04A3, 0x04A3, -1 }, /* ң → Ң */
    { 0x04A5, 0x04A5, -1 }, /* ҥ → Ҥ */
    { 0x04A7, 0x04A7, -1 }, /* ҧ → Ҧ */
    { 0x04A9, 0x04A9, -1 }, /* ҩ → Ҩ */
    { 0x04AB, 0x04AB, -1 }, /* ҫ → Ҫ */
    { 0x04AD, 0x04AD, -1 }, /* ҭ → Ҭ */
    { 0x04AF, 0x04AF, -1 }, /* ү → Ү */
    { 0x04B1, 0x04B1, -1 }, /* ұ → Ұ */
    { 0x04B3, 0x04B3, -1 }, /* ҳ → Ҳ */
    { 0x04B5, 0x04B5, -1 }, /* ҵ → Ҵ */
    { 0x04B7, 0x04B7, -1 }, /* ҷ → Ҷ */
    { 0x04B9, 0x04B9, -1 }, /* ҹ → Ҹ */
    { 0x04BB, 0x04BB, -1 }, /* һ → Һ */
    { 0x04BD, 0x04BD, -1 }, /* ҽ → Ҽ */
    { 0x04BF, 0x04BF, -1 }, /* ҿ → Ҿ */
    /* U+04C1-U+04CE: odd=upper, even=lower */
    { 0x04C2, 0x04C2, -1 }, /* ӂ → Ӂ */
    { 0x04C4, 0x04C4, -1 }, /* ӄ → Ӄ */
    { 0x04C6, 0x04C6, -1 }, /* ӆ → Ӆ */
    { 0x04C8, 0x04C8, -1 }, /* ӈ → Ӈ */
    { 0x04CA, 0x04CA, -1 }, /* ӊ → Ӊ */
    { 0x04CC, 0x04CC, -1 }, /* ӌ → Ӌ */
    { 0x04CE, 0x04CE, -1 }, /* ӎ → Ӎ */
    /* U+04CF (ӏ) → U+04C0 (Ӏ): pair table */
    /* U+04D0-U+04FF: even=upper, odd=lower */
    { 0x04D1, 0x04D1, -1 }, /* ӑ → Ӑ */
    { 0x04D3, 0x04D3, -1 }, /* ӓ → Ӓ */
    { 0x04D5, 0x04D5, -1 }, /* ӕ → Ӕ */
    { 0x04D7, 0x04D7, -1 }, /* ӗ → Ӗ */
    { 0x04D9, 0x04D9, -1 }, /* ә → Ә */
    { 0x04DB, 0x04DB, -1 }, /* ӛ → Ӛ */
    { 0x04DD, 0x04DD, -1 }, /* ӝ → Ӝ */
    { 0x04DF, 0x04DF, -1 }, /* ӟ → Ӟ */
    { 0x04E1, 0x04E1, -1 }, /* ӡ → Ӡ */
    { 0x04E3, 0x04E3, -1 }, /* ӣ → Ӣ */
    { 0x04E5, 0x04E5, -1 }, /* ӥ → Ӥ */
    { 0x04E7, 0x04E7, -1 }, /* ӧ → Ӧ */
    { 0x04E9, 0x04E9, -1 }, /* ө → Ө */
    { 0x04EB, 0x04EB, -1 }, /* ӫ → Ӫ */
    { 0x04ED, 0x04ED, -1 }, /* ӭ → Ӭ */
    { 0x04EF, 0x04EF, -1 }, /* ӯ → Ӯ */
    { 0x04F1, 0x04F1, -1 }, /* ӱ → Ӱ */
    { 0x04F3, 0x04F3, -1 }, /* ӳ → Ӳ */
    { 0x04F5, 0x04F5, -1 }, /* ӵ → Ӵ */
    { 0x04F7, 0x04F7, -1 }, /* ӷ → Ӷ */
    { 0x04F9, 0x04F9, -1 }, /* ӹ → Ӹ */
    { 0x04FB, 0x04FB, -1 }, /* ӻ → Ӻ */
    { 0x04FD, 0x04FD, -1 }, /* ӽ → Ӽ */
    { 0x04FF, 0x04FF, -1 }, /* ӿ → Ӿ */

    /* Armenian: U+0561-U+0586 → U+0531-U+0556, delta=-0x30 */
    { 0x0561, 0x0586, -0x30 },

    /* Georgian Mkhedruli → Asomtavruli, delta=-0x1C60 */
    { 0x2D00, 0x2D25, -0x1C60 },
    { 0x2D27, 0x2D27, -0x1C60 },
    { 0x2D2D, 0x2D2D, -0x1C60 },
};

#define MENAI_UPCASE_RANGE_COUNT \
    ((int)(sizeof(menai_upcase_ranges) / sizeof(menai_upcase_ranges[0])))

/* ---- Upcase pair table (irregular lowercase → uppercase) -----------------
 * Sorted by .from.
 */
static const MenaiCasePair menai_upcase_pairs[] = {
    { 0x0131, 0x0049 }, /* ı → I */
    { 0x017F, 0x0053 }, /* ſ → S */
    { 0x0180, 0x0243 }, /* ƀ → Ƀ */
    { 0x0195, 0x01F6 }, /* ƕ → Ƕ */
    { 0x019A, 0x023D }, /* ƚ → Ƚ */
    { 0x019E, 0x0220 }, /* ƞ → Ƞ */
    { 0x01BF, 0x01F7 }, /* ƿ → Ƿ */
    { 0x01DD, 0x018E }, /* ǝ → Ǝ */
    { 0x01F2, 0x01F1 }, /* Dz → DZ (title → upper) */
    { 0x01F3, 0x01F1 }, /* dz → DZ */
    { 0x023C, 0x023B }, /* ȼ → Ȼ */
    { 0x023F, 0x2C7E }, /* ȿ → Ȿ */
    { 0x0240, 0x2C7F }, /* ɀ → Ɀ */
    { 0x0253, 0x0181 }, /* ɓ → Ɓ */
    { 0x0254, 0x0186 }, /* ɔ → Ɔ */
    { 0x0256, 0x0189 }, /* ɖ → Ɖ */
    { 0x0257, 0x018A }, /* ɗ → Ɗ */
    { 0x0259, 0x018F }, /* ə → Ə */
    { 0x025B, 0x0190 }, /* ɛ → Ɛ */
    { 0x0260, 0x0193 }, /* ɠ → Ɠ */
    { 0x0263, 0x0194 }, /* ɣ → Ɣ */
    { 0x0268, 0x0197 }, /* ɨ → Ɨ */
    { 0x0269, 0x0196 }, /* ɩ → Ɩ */
    { 0x026F, 0x019C }, /* ɯ → Ɯ */
    { 0x0272, 0x019D }, /* ɲ → Ɲ */
    { 0x0275, 0x019F }, /* ɵ → Ɵ */
    { 0x0280, 0x01A6 }, /* ʀ → Ʀ */
    { 0x0283, 0x01A9 }, /* ʃ → Ʃ */
    { 0x0288, 0x01AE }, /* ʈ → Ʈ */
    { 0x0289, 0x0244 }, /* ʉ → Ʉ */
    { 0x028A, 0x01B1 }, /* ʊ → Ʊ */
    { 0x028B, 0x01B2 }, /* ʋ → Ʋ */
    { 0x028C, 0x0245 }, /* ʌ → Ʌ */
    { 0x0292, 0x01B7 }, /* ʒ → Ʒ */
    { 0x0371, 0x0370 }, /* ͱ → Ͱ */
    { 0x0373, 0x0372 }, /* ͳ → Ͳ */
    { 0x0377, 0x0376 }, /* ͷ → Ͷ */
    { 0x037B, 0x03FD }, /* ͻ → Ͻ */
    { 0x037C, 0x03FE }, /* ͼ → Ͼ */
    { 0x037D, 0x03FF }, /* ͽ → Ͽ */
    { 0x03AC, 0x0386 }, /* ά → Ά */
    { 0x03AD, 0x0388 }, /* έ → Έ */
    { 0x03AE, 0x0389 }, /* ή → Ή */
    { 0x03AF, 0x038A }, /* ί → Ί */
    { 0x03CC, 0x038C }, /* ό → Ό */
    { 0x03CD, 0x038E }, /* ύ → Ύ */
    { 0x03CE, 0x038F }, /* ώ → Ώ */
    { 0x03D0, 0x0392 }, /* ϐ → Β */
    { 0x03D1, 0x0398 }, /* ϑ → Θ */
    { 0x03D5, 0x03A6 }, /* ϕ → Φ */
    { 0x03D6, 0x03A0 }, /* ϖ → Π */
    { 0x03D7, 0x03CF }, /* ϗ → Ϗ */
    { 0x03F0, 0x039A }, /* ϰ → Κ */
    { 0x03F1, 0x03A1 }, /* ϱ → Ρ */
    { 0x03F2, 0x03F9 }, /* ϲ → Ϲ */
    { 0x03F3, 0x037F }, /* ϳ → Ϳ */
    { 0x03F5, 0x0395 }, /* ϵ → Ε */
    { 0x04CF, 0x04C0 }, /* ӏ → Ӏ */
    { 0x2C65, 0x023A }, /* ⱥ → Ⱥ */
    { 0x2C66, 0x023E }, /* ⱦ → Ⱦ */
};

#define MENAI_UPCASE_PAIR_COUNT ((int)(sizeof(menai_upcase_pairs) / sizeof(menai_upcase_pairs[0])))

/* ---- Downcase range table ------------------------------------------------
 * Each entry covers a contiguous run of uppercase codepoints that all map to
 * lowercase by adding delta (delta is positive: lowercase > uppercase).
 * Sorted by .lo.
 */
static const MenaiCaseRange menai_downcase_ranges[] = {
    /* Latin Extended-A: even=upper, odd=lower, delta=+1 */
    { 0x0100, 0x0100, 1 }, /* Ā → ā */
    { 0x0102, 0x0102, 1 }, /* Ă → ă */
    { 0x0104, 0x0104, 1 }, /* Ą → ą */
    { 0x0106, 0x0106, 1 }, /* Ć → ć */
    { 0x0108, 0x0108, 1 }, /* Ĉ → ĉ */
    { 0x010A, 0x010A, 1 }, /* Ċ → ċ */
    { 0x010C, 0x010C, 1 }, /* Č → č */
    { 0x010E, 0x010E, 1 }, /* Ď → ď */
    { 0x0110, 0x0110, 1 }, /* Đ → đ */
    { 0x0112, 0x0112, 1 }, /* Ē → ē */
    { 0x0114, 0x0114, 1 }, /* Ĕ → ĕ */
    { 0x0116, 0x0116, 1 }, /* Ė → ė */
    { 0x0118, 0x0118, 1 }, /* Ę → ę */
    { 0x011A, 0x011A, 1 }, /* Ě → ě */
    { 0x011C, 0x011C, 1 }, /* Ĝ → ĝ */
    { 0x011E, 0x011E, 1 }, /* Ğ → ğ */
    { 0x0120, 0x0120, 1 }, /* Ġ → ġ */
    { 0x0122, 0x0122, 1 }, /* Ģ → ģ */
    { 0x0124, 0x0124, 1 }, /* Ĥ → ĥ */
    { 0x0126, 0x0126, 1 }, /* Ħ → ħ */
    { 0x0128, 0x0128, 1 }, /* Ĩ → ĩ */
    { 0x012A, 0x012A, 1 }, /* Ī → ī */
    { 0x012C, 0x012C, 1 }, /* Ĭ → ĭ */
    { 0x012E, 0x012E, 1 }, /* Į → į */
    /* U+0130 (İ) → U+0131 (ı): delta=+1 gives 0x0131, per task spec */
    { 0x0130, 0x0130, 1 },
    { 0x0132, 0x0132, 1 }, /* Ĳ → ĳ */
    { 0x0134, 0x0134, 1 }, /* Ĵ → ĵ */
    { 0x0136, 0x0136, 1 }, /* Ķ → ķ */
    /* U+0139-U+0148: odd=upper, even=lower, delta=+1 */
    { 0x0139, 0x0139, 1 }, /* Ĺ → ĺ */
    { 0x013B, 0x013B, 1 }, /* Ļ → ļ */
    { 0x013D, 0x013D, 1 }, /* Ľ → ľ */
    { 0x013F, 0x013F, 1 }, /* Ŀ → ŀ */
    { 0x0141, 0x0141, 1 }, /* Ł → ł */
    { 0x0143, 0x0143, 1 }, /* Ń → ń */
    { 0x0145, 0x0145, 1 }, /* Ņ → ņ */
    { 0x0147, 0x0147, 1 }, /* Ň → ň */
    /* U+014A-U+0177: even=upper, odd=lower, delta=+1 */
    { 0x014A, 0x014A, 1 }, /* Ŋ → ŋ */
    { 0x014C, 0x014C, 1 }, /* Ō → ō */
    { 0x014E, 0x014E, 1 }, /* Ŏ → ŏ */
    { 0x0150, 0x0150, 1 }, /* Ő → ő */
    { 0x0152, 0x0152, 1 }, /* Œ → œ */
    { 0x0154, 0x0154, 1 }, /* Ŕ → ŕ */
    { 0x0156, 0x0156, 1 }, /* Ŗ → ŗ */
    { 0x0158, 0x0158, 1 }, /* Ř → ř */
    { 0x015A, 0x015A, 1 }, /* Ś → ś */
    { 0x015C, 0x015C, 1 }, /* Ŝ → ŝ */
    { 0x015E, 0x015E, 1 }, /* Ş → ş */
    { 0x0160, 0x0160, 1 }, /* Š → š */
    { 0x0162, 0x0162, 1 }, /* Ţ → ţ */
    { 0x0164, 0x0164, 1 }, /* Ť → ť */
    { 0x0166, 0x0166, 1 }, /* Ŧ → ŧ */
    { 0x0168, 0x0168, 1 }, /* Ũ → ũ */
    { 0x016A, 0x016A, 1 }, /* Ū → ū */
    { 0x016C, 0x016C, 1 }, /* Ŭ → ŭ */
    { 0x016E, 0x016E, 1 }, /* Ů → ů */
    { 0x0170, 0x0170, 1 }, /* Ű → ű */
    { 0x0172, 0x0172, 1 }, /* Ų → ų */
    { 0x0174, 0x0174, 1 }, /* Ŵ → ŵ */
    { 0x0176, 0x0176, 1 }, /* Ŷ → ŷ */
    /* U+0178 (Ÿ) → U+00FF: Latin-1 switch */
    /* U+0179-U+017E: odd=upper, even=lower, delta=+1 */
    { 0x0179, 0x0179, 1 }, /* Ź → ź */
    { 0x017B, 0x017B, 1 }, /* Ż → ż */
    { 0x017D, 0x017D, 1 }, /* Ž → ž */

    /* Latin Extended-B: paired sub-blocks with delta=+1 */
    { 0x0182, 0x0182, 1 }, /* Ƃ → ƃ */
    { 0x0184, 0x0184, 1 }, /* Ƅ → ƅ */
    { 0x0187, 0x0187, 1 }, /* Ƈ → ƈ */
    { 0x018B, 0x018B, 1 }, /* Ƌ → ƌ */
    { 0x0191, 0x0191, 1 }, /* Ƒ → ƒ */
    { 0x0198, 0x0198, 1 }, /* Ƙ → ƙ */
    { 0x01A0, 0x01A0, 1 }, /* Ơ → ơ */
    { 0x01A2, 0x01A2, 1 }, /* Ƣ → ƣ */
    { 0x01A4, 0x01A4, 1 }, /* Ƥ → ƥ */
    { 0x01A7, 0x01A7, 1 }, /* Ƨ → ƨ */
    { 0x01AC, 0x01AC, 1 }, /* Ƭ → ƭ */
    { 0x01AF, 0x01AF, 1 }, /* Ư → ư */
    { 0x01B3, 0x01B3, 1 }, /* Ƴ → ƴ */
    { 0x01B5, 0x01B5, 1 }, /* Ƶ → ƶ */
    { 0x01B8, 0x01B8, 1 }, /* Ƹ → ƹ */
    { 0x01BC, 0x01BC, 1 }, /* Ƽ → ƽ */
    /* Digraph uppercase → lowercase */
    { 0x01C4, 0x01C4, 2 }, /* DŽ → dž */
    { 0x01C5, 0x01C5, 1 }, /* Dž → dž */
    { 0x01C7, 0x01C7, 2 }, /* LJ → lj */
    { 0x01C8, 0x01C8, 1 }, /* Lj → lj */
    { 0x01CA, 0x01CA, 2 }, /* NJ → nj */
    { 0x01CB, 0x01CB, 1 }, /* Nj → nj */
    /* U+01CD-U+01DC: odd=upper, even=lower, delta=+1 */
    { 0x01CD, 0x01CD, 1 }, /* Ǎ → ǎ */
    { 0x01CF, 0x01CF, 1 }, /* Ǐ → ǐ */
    { 0x01D1, 0x01D1, 1 }, /* Ǒ → ǒ */
    { 0x01D3, 0x01D3, 1 }, /* Ǔ → ǔ */
    { 0x01D5, 0x01D5, 1 }, /* Ǖ → ǖ */
    { 0x01D7, 0x01D7, 1 }, /* Ǘ → ǘ */
    { 0x01D9, 0x01D9, 1 }, /* Ǚ → ǚ */
    { 0x01DB, 0x01DB, 1 }, /* Ǜ → ǜ */
    /* U+01DE-U+01EE: even=upper, odd=lower, delta=+1 */
    { 0x01DE, 0x01DE, 1 }, /* Ǟ → ǟ */
    { 0x01E0, 0x01E0, 1 }, /* Ǡ → ǡ */
    { 0x01E2, 0x01E2, 1 }, /* Ǣ → ǣ */
    { 0x01E4, 0x01E4, 1 }, /* Ǥ → ǥ */
    { 0x01E6, 0x01E6, 1 }, /* Ǧ → ǧ */
    { 0x01E8, 0x01E8, 1 }, /* Ǩ → ǩ */
    { 0x01EA, 0x01EA, 1 }, /* Ǫ → ǫ */
    { 0x01EC, 0x01EC, 1 }, /* Ǭ → ǭ */
    { 0x01EE, 0x01EE, 1 }, /* Ǯ → ǯ */
    { 0x01F1, 0x01F1, 2 }, /* DZ → dz */
    { 0x01F2, 0x01F2, 1 }, /* Dz → dz */
    { 0x01F4, 0x01F4, 1 }, /* Ǵ → ǵ */
    /* U+01F8-U+021E: even=upper, odd=lower, delta=+1 */
    { 0x01F8, 0x01F8, 1 }, /* Ǹ → ǹ */
    { 0x01FA, 0x01FA, 1 }, /* Ǻ → ǻ */
    { 0x01FC, 0x01FC, 1 }, /* Ǽ → ǽ */
    { 0x01FE, 0x01FE, 1 }, /* Ǿ → ǿ */
    { 0x0200, 0x0200, 1 }, /* Ȁ → ȁ */
    { 0x0202, 0x0202, 1 }, /* Ȃ → ȃ */
    { 0x0204, 0x0204, 1 }, /* Ȅ → ȅ */
    { 0x0206, 0x0206, 1 }, /* Ȇ → ȇ */
    { 0x0208, 0x0208, 1 }, /* Ȉ → ȉ */
    { 0x020A, 0x020A, 1 }, /* Ȋ → ȋ */
    { 0x020C, 0x020C, 1 }, /* Ȍ → ȍ */
    { 0x020E, 0x020E, 1 }, /* Ȏ → ȏ */
    { 0x0210, 0x0210, 1 }, /* Ȑ → ȑ */
    { 0x0212, 0x0212, 1 }, /* Ȓ → ȓ */
    { 0x0214, 0x0214, 1 }, /* Ȕ → ȕ */
    { 0x0216, 0x0216, 1 }, /* Ȗ → ȗ */
    { 0x0218, 0x0218, 1 }, /* Ș → ș */
    { 0x021A, 0x021A, 1 }, /* Ț → ț */
    { 0x021C, 0x021C, 1 }, /* Ȝ → ȝ */
    { 0x021E, 0x021E, 1 }, /* Ȟ → ȟ */
    /* U+0222-U+0232: even=upper, odd=lower, delta=+1 */
    { 0x0222, 0x0222, 1 }, /* Ȣ → ȣ */
    { 0x0224, 0x0224, 1 }, /* Ȥ → ȥ */
    { 0x0226, 0x0226, 1 }, /* Ȧ → ȧ */
    { 0x0228, 0x0228, 1 }, /* Ȩ → ȩ */
    { 0x022A, 0x022A, 1 }, /* Ȫ → ȫ */
    { 0x022C, 0x022C, 1 }, /* Ȭ → ȭ */
    { 0x022E, 0x022E, 1 }, /* Ȯ → ȯ */
    { 0x0230, 0x0230, 1 }, /* Ȱ → ȱ */
    { 0x0232, 0x0232, 1 }, /* Ȳ → ȳ */
    { 0x023B, 0x023B, 1 }, /* Ȼ → ȼ */
    { 0x0241, 0x0241, 1 }, /* Ɂ → ɂ */
    /* U+0246-U+024E: even=upper, odd=lower, delta=+1 */
    { 0x0246, 0x0246, 1 }, /* Ɇ → ɇ */
    { 0x0248, 0x0248, 1 }, /* Ɉ → ɉ */
    { 0x024A, 0x024A, 1 }, /* Ɋ → ɋ */
    { 0x024C, 0x024C, 1 }, /* Ɍ → ɍ */
    { 0x024E, 0x024E, 1 }, /* Ɏ → ɏ */

    /* Greek uppercase → lowercase */
    { 0x0391, 0x03A1, 0x20 }, /* Α..Ρ → α..ρ */
    { 0x03A3, 0x03AB, 0x20 }, /* Σ..Ϋ → σ..ϋ */
    /* Coptic letters in Greek block: even=upper, odd=lower, delta=+1 */
    { 0x03D8, 0x03D8, 1 }, /* Ϙ → ϙ */
    { 0x03DA, 0x03DA, 1 }, /* Ϛ → ϛ */
    { 0x03DC, 0x03DC, 1 }, /* Ϝ → ϝ */
    { 0x03DE, 0x03DE, 1 }, /* Ϟ → ϟ */
    { 0x03E0, 0x03E0, 1 }, /* Ϡ → ϡ */
    { 0x03E2, 0x03E2, 1 }, /* Ϣ → ϣ */
    { 0x03E4, 0x03E4, 1 }, /* Ϥ → ϥ */
    { 0x03E6, 0x03E6, 1 }, /* Ϧ → ϧ */
    { 0x03E8, 0x03E8, 1 }, /* Ϩ → ϩ */
    { 0x03EA, 0x03EA, 1 }, /* Ϫ → ϫ */
    { 0x03EC, 0x03EC, 1 }, /* Ϭ → ϭ */
    { 0x03EE, 0x03EE, 1 }, /* Ϯ → ϯ */
    { 0x03F7, 0x03F7, 1 }, /* Ϸ → ϸ */
    { 0x03FA, 0x03FA, 1 }, /* Ϻ → ϻ */

    /* Cyrillic uppercase → lowercase */
    { 0x0400, 0x040F, 0x50 }, /* Ѐ..Џ → ѐ..џ */
    { 0x0410, 0x042F, 0x20 }, /* А..Я → а..я */
    /* Cyrillic supplementary: even=upper, odd=lower, delta=+1 */
    { 0x0460, 0x0460, 1 }, /* Ѡ → ѡ */
    { 0x0462, 0x0462, 1 }, /* Ѣ → ѣ */
    { 0x0464, 0x0464, 1 }, /* Ѥ → ѥ */
    { 0x0466, 0x0466, 1 }, /* Ѧ → ѧ */
    { 0x0468, 0x0468, 1 }, /* Ѩ → ѩ */
    { 0x046A, 0x046A, 1 }, /* Ѫ → ѫ */
    { 0x046C, 0x046C, 1 }, /* Ѭ → ѭ */
    { 0x046E, 0x046E, 1 }, /* Ѯ → ѯ */
    { 0x0470, 0x0470, 1 }, /* Ѱ → ѱ */
    { 0x0472, 0x0472, 1 }, /* Ѳ → ѳ */
    { 0x0474, 0x0474, 1 }, /* Ѵ → ѵ */
    { 0x0476, 0x0476, 1 }, /* Ѷ → ѷ */
    { 0x0478, 0x0478, 1 }, /* Ѹ → ѹ */
    { 0x047A, 0x047A, 1 }, /* Ѻ → ѻ */
    { 0x047C, 0x047C, 1 }, /* Ѽ → ѽ */
    { 0x047E, 0x047E, 1 }, /* Ѿ → ѿ */
    { 0x0480, 0x0480, 1 }, /* Ҁ → ҁ */
    { 0x048A, 0x048A, 1 }, /* Ҋ → ҋ */
    { 0x048C, 0x048C, 1 }, /* Ҍ → ҍ */
    { 0x048E, 0x048E, 1 }, /* Ҏ → ҏ */
    { 0x0490, 0x0490, 1 }, /* Ґ → ґ */
    { 0x0492, 0x0492, 1 }, /* Ғ → ғ */
    { 0x0494, 0x0494, 1 }, /* Ҕ → ҕ */
    { 0x0496, 0x0496, 1 }, /* Җ → җ */
    { 0x0498, 0x0498, 1 }, /* Ҙ → ҙ */
    { 0x049A, 0x049A, 1 }, /* Қ → қ */
    { 0x049C, 0x049C, 1 }, /* Ҝ → ҝ */
    { 0x049E, 0x049E, 1 }, /* Ҟ → ҟ */
    { 0x04A0, 0x04A0, 1 }, /* Ҡ → ҡ */
    { 0x04A2, 0x04A2, 1 }, /* Ң → ң */
    { 0x04A4, 0x04A4, 1 }, /* Ҥ → ҥ */
    { 0x04A6, 0x04A6, 1 }, /* Ҧ → ҧ */
    { 0x04A8, 0x04A8, 1 }, /* Ҩ → ҩ */
    { 0x04AA, 0x04AA, 1 }, /* Ҫ → ҫ */
    { 0x04AC, 0x04AC, 1 }, /* Ҭ → ҭ */
    { 0x04AE, 0x04AE, 1 }, /* Ү → ү */
    { 0x04B0, 0x04B0, 1 }, /* Ұ → ұ */
    { 0x04B2, 0x04B2, 1 }, /* Ҳ → ҳ */
    { 0x04B4, 0x04B4, 1 }, /* Ҵ → ҵ */
    { 0x04B6, 0x04B6, 1 }, /* Ҷ → ҷ */
    { 0x04B8, 0x04B8, 1 }, /* Ҹ → ҹ */
    { 0x04BA, 0x04BA, 1 }, /* Һ → һ */
    { 0x04BC, 0x04BC, 1 }, /* Ҽ → ҽ */
    { 0x04BE, 0x04BE, 1 }, /* Ҿ → ҿ */
    /* U+04C0 (Ӏ) → U+04CF (ӏ): pair table */
    /* U+04C1-U+04CE: odd=upper, even=lower, delta=+1 */
    { 0x04C1, 0x04C1, 1 }, /* Ӂ → ӂ */
    { 0x04C3, 0x04C3, 1 }, /* Ӄ → ӄ */
    { 0x04C5, 0x04C5, 1 }, /* Ӆ → ӆ */
    { 0x04C7, 0x04C7, 1 }, /* Ӈ → ӈ */
    { 0x04C9, 0x04C9, 1 }, /* Ӊ → ӊ */
    { 0x04CB, 0x04CB, 1 }, /* Ӌ → ӌ */
    { 0x04CD, 0x04CD, 1 }, /* Ӎ → ӎ */
    /* U+04D0-U+04FE: even=upper, odd=lower, delta=+1 */
    { 0x04D0, 0x04D0, 1 }, /* Ӑ → ӑ */
    { 0x04D2, 0x04D2, 1 }, /* Ӓ → ӓ */
    { 0x04D4, 0x04D4, 1 }, /* Ӕ → ӕ */
    { 0x04D6, 0x04D6, 1 }, /* Ӗ → ӗ */
    { 0x04D8, 0x04D8, 1 }, /* Ә → ә */
    { 0x04DA, 0x04DA, 1 }, /* Ӛ → ӛ */
    { 0x04DC, 0x04DC, 1 }, /* Ӝ → ӝ */
    { 0x04DE, 0x04DE, 1 }, /* Ӟ → ӟ */
    { 0x04E0, 0x04E0, 1 }, /* Ӡ → ӡ */
    { 0x04E2, 0x04E2, 1 }, /* Ӣ → ӣ */
    { 0x04E4, 0x04E4, 1 }, /* Ӥ → ӥ */
    { 0x04E6, 0x04E6, 1 }, /* Ӧ → ӧ */
    { 0x04E8, 0x04E8, 1 }, /* Ө → ө */
    { 0x04EA, 0x04EA, 1 }, /* Ӫ → ӫ */
    { 0x04EC, 0x04EC, 1 }, /* Ӭ → ӭ */
    { 0x04EE, 0x04EE, 1 }, /* Ӯ → ӯ */
    { 0x04F0, 0x04F0, 1 }, /* Ӱ → ӱ */
    { 0x04F2, 0x04F2, 1 }, /* Ӳ → ӳ */
    { 0x04F4, 0x04F4, 1 }, /* Ӵ → ӵ */
    { 0x04F6, 0x04F6, 1 }, /* Ӷ → ӷ */
    { 0x04F8, 0x04F8, 1 }, /* Ӹ → ӹ */
    { 0x04FA, 0x04FA, 1 }, /* Ӻ → ӻ */
    { 0x04FC, 0x04FC, 1 }, /* Ӽ → ӽ */
    { 0x04FE, 0x04FE, 1 }, /* Ӿ → ӿ */

    /* Armenian: U+0531-U+0556 → U+0561-U+0586, delta=+0x30 */
    { 0x0531, 0x0556, 0x30 },

    /* Georgian Asomtavruli → Mkhedruli, delta=+0x1C60 */
    { 0x10A0, 0x10C5, 0x1C60 },
    { 0x10C7, 0x10C7, 0x1C60 },
    { 0x10CD, 0x10CD, 0x1C60 },
};

#define MENAI_DOWNCASE_RANGE_COUNT ((int)(sizeof(menai_downcase_ranges) / sizeof(menai_downcase_ranges[0])))

/* ---- Downcase pair table (irregular uppercase → lowercase) ---------------
 * Sorted by .from.
 */
static const MenaiCasePair menai_downcase_pairs[] = {
    { 0x0181, 0x0253 }, /* Ɓ → ɓ */
    { 0x0186, 0x0254 }, /* Ɔ → ɔ */
    { 0x0189, 0x0256 }, /* Ɖ → ɖ */
    { 0x018A, 0x0257 }, /* Ɗ → ɗ */
    { 0x018E, 0x01DD }, /* Ǝ → ǝ */
    { 0x018F, 0x0259 }, /* Ə → ə */
    { 0x0190, 0x025B }, /* Ɛ → ɛ */
    { 0x0193, 0x0260 }, /* Ɠ → ɠ */
    { 0x0194, 0x0263 }, /* Ɣ → ɣ */
    { 0x0196, 0x0269 }, /* Ɩ → ɩ */
    { 0x0197, 0x0268 }, /* Ɨ → ɨ */
    { 0x019C, 0x026F }, /* Ɯ → ɯ */
    { 0x019D, 0x0272 }, /* Ɲ → ɲ */
    { 0x019F, 0x0275 }, /* Ɵ → ɵ */
    { 0x01A6, 0x0280 }, /* Ʀ → ʀ */
    { 0x01A9, 0x0283 }, /* Ʃ → ʃ */
    { 0x01AE, 0x0288 }, /* Ʈ → ʈ */
    { 0x01B1, 0x028A }, /* Ʊ → ʊ */
    { 0x01B2, 0x028B }, /* Ʋ → ʋ */
    { 0x01B7, 0x0292 }, /* Ʒ → ʒ */
    { 0x01F6, 0x0195 }, /* Ƕ → ƕ */
    { 0x01F7, 0x01BF }, /* Ƿ → ƿ */
    { 0x0220, 0x019E }, /* Ƞ → ƞ */
    { 0x023A, 0x2C65 }, /* Ⱥ → ⱥ */
    { 0x023D, 0x019A }, /* Ƚ → ƚ */
    { 0x023E, 0x2C66 }, /* Ⱦ → ⱦ */
    { 0x0243, 0x0180 }, /* Ƀ → ƀ */
    { 0x0244, 0x0289 }, /* Ʉ → ʉ */
    { 0x0245, 0x028C }, /* Ʌ → ʌ */
    { 0x0370, 0x0371 }, /* Ͱ → ͱ */
    { 0x0372, 0x0373 }, /* Ͳ → ͳ */
    { 0x0376, 0x0377 }, /* Ͷ → ͷ */
    { 0x037F, 0x03F3 }, /* Ϳ → ϳ */
    { 0x0386, 0x03AC }, /* Ά → ά */
    { 0x0388, 0x03AD }, /* Έ → έ */
    { 0x0389, 0x03AE }, /* Ή → ή */
    { 0x038A, 0x03AF }, /* Ί → ί */
    { 0x038C, 0x03CC }, /* Ό → ό */
    { 0x038E, 0x03CD }, /* Ύ → ύ */
    { 0x038F, 0x03CE }, /* Ώ → ώ */
    { 0x03CF, 0x03D7 }, /* Ϗ → ϗ */
    { 0x03F4, 0x03B8 }, /* ϴ → θ */
    { 0x03F9, 0x03F2 }, /* Ϲ → ϲ */
    { 0x03FD, 0x037B }, /* Ͻ → ͻ */
    { 0x03FE, 0x037C }, /* Ͼ → ͼ */
    { 0x03FF, 0x037D }, /* Ͽ → ͽ */
    { 0x04C0, 0x04CF }, /* Ӏ → ӏ */
};

#define MENAI_DOWNCASE_PAIR_COUNT \
    ((int)(sizeof(menai_downcase_pairs) / sizeof(menai_downcase_pairs[0])))

static inline uint32_t unicode_simple_upcase(uint32_t cp)
{
    uint32_t result;

    /* ASCII a-z */
    if (cp >= 0x0061 && cp <= 0x007A) {
        return cp - 0x0020;
    }

    /* Extended BMP ranges and irregular pairs */
    if (cp >= 0x0100) {
        result = menai_search_case_ranges(cp, menai_upcase_ranges,
                                          MENAI_UPCASE_RANGE_COUNT);
        if (result) {
            return result;
        }

        result = menai_search_case_pairs(cp, menai_upcase_pairs,
                                         MENAI_UPCASE_PAIR_COUNT);
        if (result) {
            return result;
        }
    }

    /* Latin-1 Supplement lowercase letters.
     * U+00DF (ß) has a multi-character expansion; handled by
     * unicode_upcase_expansion() instead — return unchanged here. */
    switch (cp) {
        case 0x00E0: return 0x00C0; /* à → À */
        case 0x00E1: return 0x00C1; /* á → Á */
        case 0x00E2: return 0x00C2; /* â → Â */
        case 0x00E3: return 0x00C3; /* ã → Ã */
        case 0x00E4: return 0x00C4; /* ä → Ä */
        case 0x00E5: return 0x00C5; /* å → Å */
        case 0x00E6: return 0x00C6; /* æ → Æ */
        case 0x00E7: return 0x00C7; /* ç → Ç */
        case 0x00E8: return 0x00C8; /* è → È */
        case 0x00E9: return 0x00C9; /* é → É */
        case 0x00EA: return 0x00CA; /* ê → Ê */
        case 0x00EB: return 0x00CB; /* ë → Ë */
        case 0x00EC: return 0x00CC; /* ì → Ì */
        case 0x00ED: return 0x00CD; /* í → Í */
        case 0x00EE: return 0x00CE; /* î → Î */
        case 0x00EF: return 0x00CF; /* ï → Ï */
        case 0x00F0: return 0x00D0; /* ð → Ð */
        case 0x00F1: return 0x00D1; /* ñ → Ñ */
        case 0x00F2: return 0x00D2; /* ò → Ò */
        case 0x00F3: return 0x00D3; /* ó → Ó */
        case 0x00F4: return 0x00D4; /* ô → Ô */
        case 0x00F5: return 0x00D5; /* õ → Õ */
        case 0x00F6: return 0x00D6; /* ö → Ö */
        case 0x00F8: return 0x00D8; /* ø → Ø */
        case 0x00F9: return 0x00D9; /* ù → Ù */
        case 0x00FA: return 0x00DA; /* ú → Ú */
        case 0x00FB: return 0x00DB; /* û → Û */
        case 0x00FC: return 0x00DC; /* ü → Ü */
        case 0x00FD: return 0x00DD; /* ý → Ý */
        case 0x00FE: return 0x00DE; /* þ → Þ */
        case 0x00FF: return 0x0178; /* ÿ → Ÿ */
        default:     return cp;
    }
}

static inline uint32_t unicode_simple_downcase(uint32_t cp)
{
    uint32_t result;

    /* ASCII A-Z */
    if (cp >= 0x0041 && cp <= 0x005A) {
        return cp + 0x0020;
    }

    /* Extended BMP ranges and irregular pairs */
    if (cp >= 0x0100) {
        result = menai_search_case_ranges(cp, menai_downcase_ranges,
                                          MENAI_DOWNCASE_RANGE_COUNT);
        if (result) {
            return result;
        }

        result = menai_search_case_pairs(cp, menai_downcase_pairs,
                                         MENAI_DOWNCASE_PAIR_COUNT);
        if (result) {
            return result;
        }
    }

    /* Latin-1 Supplement uppercase letters. */
    switch (cp) {
    case 0x00C0: return 0x00E0; /* À → à */
    case 0x00C1: return 0x00E1; /* Á → á */
    case 0x00C2: return 0x00E2; /* Â → â */
    case 0x00C3: return 0x00E3; /* Ã → ã */
    case 0x00C4: return 0x00E4; /* Ä → ä */
    case 0x00C5: return 0x00E5; /* Å → å */
    case 0x00C6: return 0x00E6; /* Æ → æ */
    case 0x00C7: return 0x00E7; /* Ç → ç */
    case 0x00C8: return 0x00E8; /* È → è */
    case 0x00C9: return 0x00E9; /* É → é */
    case 0x00CA: return 0x00EA; /* Ê → ê */
    case 0x00CB: return 0x00EB; /* Ë → ë */
    case 0x00CC: return 0x00EC; /* Ì → ì */
    case 0x00CD: return 0x00ED; /* Í → í */
    case 0x00CE: return 0x00EE; /* Î → î */
    case 0x00CF: return 0x00EF; /* Ï → ï */
    case 0x00D0: return 0x00F0; /* Ð → ð */
    case 0x00D1: return 0x00F1; /* Ñ → ñ */
    case 0x00D2: return 0x00F2; /* Ò → ò */
    case 0x00D3: return 0x00F3; /* Ó → ó */
    case 0x00D4: return 0x00F4; /* Ô → ô */
    case 0x00D5: return 0x00F5; /* Õ → õ */
    case 0x00D6: return 0x00F6; /* Ö → ö */
    case 0x00D8: return 0x00F8; /* Ø → ø */
    case 0x00D9: return 0x00F9; /* Ù → ù */
    case 0x00DA: return 0x00FA; /* Ú → ú */
    case 0x00DB: return 0x00FB; /* Û → û */
    case 0x00DC: return 0x00FC; /* Ü → ü */
    case 0x00DD: return 0x00FD; /* Ý → ý */
    case 0x00DE: return 0x00FE; /* Þ → þ */
    case 0x0178: return 0x00FF; /* Ÿ → ÿ */
    default: return cp;
    }
}

/* ---------------------------------------------------------------------------
 * unicode_upcase_expansion
 *
 * Multi-codepoint uppercase mappings.  The expansion array is 0-terminated;
 * unused trailing slots are 0.
 * ------------------------------------------------------------------------- */

typedef struct {
    uint32_t cp;
    uint32_t expansion[3]; /* up to 3 codepoints, 0-terminated */
} MenaiUpcaseExpansion;

static const MenaiUpcaseExpansion menai_upcase_expansions[] = {
    { 0x00DF, { 0x0053, 0x0053, 0      } }, /* ß → SS  */
    { 0xFB00, { 0x0046, 0x0046, 0      } }, /* ﬀ → FF  */
    { 0xFB01, { 0x0046, 0x0049, 0      } }, /* ﬁ → FI  */
    { 0xFB02, { 0x0046, 0x004C, 0      } }, /* ﬂ → FL  */
    { 0xFB03, { 0x0046, 0x0046, 0x0049 } }, /* ﬃ → FFI */
    { 0xFB04, { 0x0046, 0x0046, 0x004C } }, /* ﬄ → FFL */
    { 0xFB05, { 0x0053, 0x0054, 0      } }, /* ﬅ → ST  */
    { 0xFB06, { 0x0053, 0x0054, 0      } }, /* ﬆ → ST  */
};

#define MENAI_UPCASE_EXPANSION_COUNT \
    ((int)(sizeof(menai_upcase_expansions) / sizeof(menai_upcase_expansions[0])))

static inline const MenaiUpcaseExpansion *unicode_upcase_expansion(uint32_t cp)
{
    int lo = 0;
    int hi = MENAI_UPCASE_EXPANSION_COUNT - 1;
    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        if (cp < menai_upcase_expansions[mid].cp) {
            hi = mid - 1;
        } else if (cp > menai_upcase_expansions[mid].cp) {
            lo = mid + 1;
        } else {
            return &menai_upcase_expansions[mid];
        }
    }

    return 0;
}

#endif /* MENAI_VM_STRING_TABLES_H */
