/*
 * natural.cpp
 */
#include <cctype>
#include <vector>

#include "c8.h"
#include "digit_array.h"

namespace c8 {
    const char digits_lower[] = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
    };
    const char digits_upper[] = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
    };

    /*
     * Convert a character to numeric value, checking against a specified base.
     */
    auto inline convert_char_to_natural_digit(char c, natural_digit base) -> natural_digit {
        if (!isdigit(c)) {
            if (base != 16) {
                throw invalid_argument("invalid digit");
            }

            if (!isxdigit(c)) {
                throw invalid_argument("invalid digit");
            }

            c = static_cast<char>(tolower(c));
            c = static_cast<char>(c - ('a' - '0' - 10));
        }

        natural_digit c_digit = static_cast<natural_digit>(c - '0');

        if (base == 8) {
            if (c_digit >= 8) {
                throw invalid_argument("invalid digit");
            }
        }

        return c_digit;
    }


    /*
     * Reserve a number of digits in this natural number.
     */
    inline auto natural::reserve(std::size_t new_digits) -> void {
        /*
         * If our digit array is already large enough (which is very likely) then
         * we don't need to do anything.
         */
        if (C8_LIKELY(digits_size_ >= new_digits)) {
            return;
        }

        /*
         * Allocate a new digit array and update book-keeping info.  The naked new
         * here isn't ideal, but we really don't want to have to zero the array and
         * there's no way to do that with std::make_unique<>.
         */
        digits_size_ = new_digits;
        large_digits_ = std::unique_ptr<natural_digit[]>(new natural_digit[new_digits]);
        digits_ = large_digits_.get();
    }

    /*
     * Expand the number of digits in this natural number.
     */
    inline auto natural::expand(std::size_t new_digits) -> void {
        /*
         * If our digit array is already large enough (which is very likely) then
         * we don't need to do anything.
         */
        if (C8_LIKELY(digits_size_ >= new_digits)) {
            return;
        }

        /*
         * Replace the old digit array with the new one.  The naked new here
         * isn't ideal, but we really don't want to have to zero the array and
         * there's no way to do that with std::make_unique<>.
         */
        auto d = std::unique_ptr<natural_digit[]>(new natural_digit[new_digits]);
        auto d_ptr = d.get();
        digit_array_copy(d_ptr, digits_, num_digits_);

        digits_size_ = new_digits;
        large_digits_ = std::move(d);
        digits_ = d_ptr;
    }

    /*
     * Copy the contents of a natural number into this one.
     */
    auto natural::copy_digits(const natural &v) -> void {
        digits_size_ = sizeof(small_digits_) / sizeof(natural_digit);
        digits_ = small_digits_;
        num_digits_ = v.num_digits_;
        if (C8_UNLIKELY(!num_digits_)) {
            return;
        }

        reserve(v.num_digits_);
        digit_array_copy(digits_, v.digits_, num_digits_);
    }

    /*
     * Steal the contents of a natural number into this one.
     */
    auto natural::steal_digits(natural &v) -> void {
        large_digits_ = std::move(v.large_digits_);

        /*
         * Are we currently using the default small buffer, or do we have one allocated?
         */
        if (C8_UNLIKELY(large_digits_.get())) {
            /*
             * We're using an allocated buffer so just move it.
             */
            digits_ = v.digits_;
            v.digits_ = v.small_digits_;
            digits_size_ = v.digits_size_;
            v.digits_size_ = sizeof(v.small_digits_) / sizeof(natural_digit);
        } else {
            /*
             * We're using the default buffer so copy the contents.
             */
            digit_array_copy(small_digits_, v.small_digits_, v.num_digits_);
            digits_ = small_digits_;
            digits_size_ = sizeof(small_digits_) / sizeof(natural_digit);
        }

        num_digits_ = v.num_digits_;
        v.num_digits_ = 0;
    }

    /*
     * Construct a natural number from an unsigned long long integer.
     */
    natural::natural(unsigned long long v) noexcept {
        num_digits_ = 0;
        digits_size_ = sizeof(small_digits_) / sizeof(natural_digit);
        digits_ = small_digits_;

        if (!v) {
            return;
        }

        natural_digit *this_digits = digits_;

        std::size_t i = 0;
        do {
            natural_digit m = static_cast<natural_digit>(-1);
            this_digits[i++] = static_cast<natural_digit>(v & m);
#if defined(C8_DIGIT_8_BITS) || defined(C8_DIGIT_16_BITS) || defined(C8_DIGIT_32_BITS)
            v >>= natural_digit_bits;
#else
            v = 0;
#endif
        } while (v);

        num_digits_ = i;
    }

    /*
     * Construct a natural number from a string.
     *
     * The string representation can have the usual C++-like hex, octal, or decimal
     * representations.
     */
    natural::natural(const std::string &v) {
        num_digits_ = 0;
        digits_size_ = sizeof(small_digits_) / sizeof(natural_digit);
        digits_ = small_digits_;

        std::size_t v_sz = v.size();
        if (C8_UNLIKELY(v_sz == 0)) {
            throw invalid_argument("zero size string");
        }

        /*
         * Is our first character a '0'?  If it is then we may have an octal or hex number.
         */
        std::size_t idx = 0;
        natural_digit base = 10;
        if (v[0] == '0') {
            idx = 1;
            base = 8;
            if (v_sz > 1) {
                auto c2 = v[1];
                if ((c2 == 'x') || (c2 == 'X')) {
                    idx = 2;
                    base = 16;
                }
            }
        }

        natural res;
        res.expand((((v_sz - idx) * 4) + 31) / 32);

        unsigned int chars_per_digit = 1;

        if (sizeof(natural_digit) > 1) {
            chars_per_digit = 3;
        }

        if (sizeof(natural_digit) > 2) {
            chars_per_digit = 7;
        }

        if (sizeof(natural_digit) > 4) {
            chars_per_digit = 15;
        }

        unsigned int char_count = static_cast<unsigned int>(v_sz - idx);
        for (unsigned int i = 0; i < char_count; i += chars_per_digit) {
            natural_digit base_div = base;

            natural_digit acc = convert_char_to_natural_digit(v[idx++], base);

            if ((char_count - i) < chars_per_digit) {
                chars_per_digit = char_count - i;
            }

            for (unsigned int j = 1; j < chars_per_digit; j++) {
                natural_digit c_digit = convert_char_to_natural_digit(v[idx++], base);
                acc = static_cast<natural_digit>((acc * base) + c_digit);
                base_div = static_cast<natural_digit>(base_div * base);
            }

            digit_array_multiply(res.digits_, res.num_digits_, res.digits_, res.num_digits_, &base_div, 1);
            if (acc) {
                digit_array_add(res.digits_, res.num_digits_, res.digits_, res.num_digits_, &acc, 1);
            }
        }

        *this = std::move(res);
    }

    /*
     * Copy constructor.
     */
    natural::natural(const natural &v) {
        copy_digits(v);
    }

    /*
     * Move constructor.
     */
    natural::natural(natural &&v) noexcept {
        steal_digits(v);
    }

    /*
     * Return true if this number is equal to another one, false if it's not.
     */
    auto natural::operator ==(const natural &v) const noexcept -> bool {
        return digit_array_compare_eq(digits_, num_digits_, v.digits_, v.num_digits_);
    }

    /*
     * Return true if this number is not equal to another one, false if it's equal.
     */
    auto natural::operator !=(const natural &v) const noexcept -> bool {
        return digit_array_compare_ne(digits_, num_digits_, v.digits_, v.num_digits_);
    }

    /*
     * Return true if this number is greater than another one, false if it's not.
     */
    auto natural::operator >(const natural &v) const noexcept -> bool {
        return digit_array_compare_gt(digits_, num_digits_, v.digits_, v.num_digits_);
    }

    /*
     * Return true if this number is greater than, or equal to, another one, false if it's not.
     */
    auto natural::operator >=(const natural &v) const noexcept -> bool {
        return digit_array_compare_ge(digits_, num_digits_, v.digits_, v.num_digits_);
    }

    /*
     * Return true if this number is less than another one, false if it's not.
     */
    auto natural::operator <(const natural &v) const noexcept -> bool {
        return digit_array_compare_lt(digits_, num_digits_, v.digits_, v.num_digits_);
    }

    /*
     * Return true if this number is less than, or equal to, another one, false if it's not.
     */
    auto natural::operator <=(const natural &v) const noexcept -> bool {
        return digit_array_compare_le(digits_, num_digits_, v.digits_, v.num_digits_);
    }

    /*
     * Copy assignment operator.
     */
    auto natural::operator =(const natural &v) -> natural & {
        /*
         * Are we assigning to ourself?  If we are then we don't need to do anything.
         */
        if (C8_UNLIKELY(this == &v)) {
            return *this;
        }

        /*
         * Delete the old contents of this natural number and copy the original's digits.
         */
        copy_digits(v);
        return *this;
    }

    /*
     * Move assignment operator.
     */
    auto natural::operator =(natural &&v) noexcept -> natural & {
        /*
         * Are we assigning to ourself?  If we are then we don't have to do anything.
         */
        if (C8_UNLIKELY(this == &v)) {
            return *this;
        }

        /*
         * Delete the old contents of this natural number and steal the original's digits.
         */
        steal_digits(v);
        return *this;
    }

    /*
     * Return the number of bits required to represent this natural number.
     */
    auto natural::size_bits() const noexcept -> std::size_t {
        return digit_array_size_bits(digits_, num_digits_);
    }

    /*
     * Add another natural number to this one.
     */
    auto natural::operator +(const natural &v) const -> natural {
        natural res;

        /*
         * We need to reserve some space for our result.  Our result can only be
         * one digit larger than the largest number we have as an operand.
         */
        std::size_t v_num_digits = v.num_digits_;
        std::size_t this_num_digits = num_digits_;
        std::size_t max_num_digits = (v_num_digits > this_num_digits) ? v_num_digits : this_num_digits;
        res.reserve(max_num_digits + 1);
        digit_array_add(res.digits_, res.num_digits_, digits_, this_num_digits, v.digits_, v_num_digits);
        return res;
    }

    /*
     * Add another natural number to this one.
     */
    auto natural::operator +=(const natural &v) -> natural & {
        /*
         * We need to reserve some space for our result.  Our result can only be
         * one digit larger than the largest number we have as an operand.
         */
        std::size_t v_num_digits = v.num_digits_;
        std::size_t this_num_digits = num_digits_;
        std::size_t max_num_digits = (v_num_digits > this_num_digits) ? v_num_digits : this_num_digits;
        expand(max_num_digits + 1);
        digit_array_add(digits_, num_digits_, digits_, this_num_digits, v.digits_, v_num_digits);
        return *this;
    }

    /*
     * Subtract another natural number from this one.
     */
    auto natural::operator -(const natural &v) const -> natural {
        natural res;

        std::size_t this_num_digits = num_digits_;
        res.reserve(this_num_digits);
        digit_array_subtract(res.digits_, res.num_digits_, digits_, this_num_digits, v.digits_, v.num_digits_);
        return res;
    }

    /*
     * Subtract another natural number from this one.
     */
    auto natural::operator -=(const natural &v) -> natural & {
        digit_array_subtract(digits_, num_digits_, digits_, num_digits_, v.digits_, v.num_digits_);
        return *this;
    }

    /*
     * Left shift this natural number by a bit count.
     */
    auto natural::operator <<(unsigned int count) const -> natural {
        natural res;

        std::size_t shift_digits = count / natural_digit_bits;
        std::size_t shift_bits = count % natural_digit_bits;

        std::size_t this_num_digits = num_digits_;
        res.reserve(this_num_digits + shift_digits + 1);
        digit_array_left_shift(res.digits_, res.num_digits_, digits_, this_num_digits, shift_digits, shift_bits);
        return res;
    }

    /*
     * Left shift this natural number by a bit count.
     */
    auto natural::operator <<=(unsigned int count) -> natural & {
        std::size_t shift_digits = count / natural_digit_bits;
        std::size_t shift_bits = count % natural_digit_bits;

        std::size_t this_num_digits = num_digits_;
        expand(this_num_digits + shift_digits + 1);
        digit_array_left_shift(digits_, num_digits_, digits_, this_num_digits, shift_digits, shift_bits);
        return *this;
    }

    /*
     * Right shift this natural number by a bit count.
     */
    auto natural::operator >>(unsigned int count) const -> natural {
        natural res;

        std::size_t shift_digits = count / natural_digit_bits;
        std::size_t shift_bits = count % natural_digit_bits;

        std::size_t this_num_digits = num_digits_;
        res.reserve(this_num_digits);
        digit_array_right_shift(res.digits_, res.num_digits_, digits_, this_num_digits, shift_digits, shift_bits);
        return res;
    }

    /*
     * Right shift this natural number by a bit count.
     */
    auto natural::operator >>=(unsigned int count) -> natural & {
        std::size_t shift_digits = count / natural_digit_bits;
        std::size_t shift_bits = count % natural_digit_bits;

        std::size_t this_num_digits = num_digits_;
        digit_array_right_shift(digits_, num_digits_, digits_, this_num_digits, shift_digits, shift_bits);
        return *this;
    }

    /*
     * Multiply this natural number with another one.
     */
    auto natural::operator *(const natural &v) const -> natural {
        natural res;

        std::size_t v_num_digits = v.num_digits_;
        std::size_t this_num_digits = num_digits_;
        std::size_t res_num_digits = this_num_digits + v_num_digits;
        res.reserve(res_num_digits);
        digit_array_multiply(res.digits_, res.num_digits_, digits_, this_num_digits, v.digits_, v_num_digits);
        return res;
    }

    /*
     * Multiply this natural number with another one.
     */
    auto natural::operator *=(const natural &v) -> natural & {
        std::size_t v_num_digits = v.num_digits_;
        std::size_t this_num_digits = num_digits_;
        std::size_t res_num_digits = this_num_digits + v_num_digits;
        expand(res_num_digits);
        digit_array_multiply(digits_, num_digits_, digits_, this_num_digits, v.digits_, v_num_digits);
        return *this;
    }

    /*
     * Divide this natural number by another one, returning the quotient and remainder.
     */
    auto natural::divide_modulus(const natural &v) const -> std::pair<natural, natural> {
        std::pair<natural, natural> p;

        std::size_t this_num_digits = num_digits_;
        p.first.reserve(this_num_digits);
        std::size_t v_num_digits = v.num_digits_;
        p.second.reserve(v_num_digits);
        digit_array_divide_modulus(p.first.digits_, p.first.num_digits_, p.second.digits_, p.second.num_digits_,
                                   digits_, this_num_digits, v.digits_, v_num_digits);
        return p;
    }

    /*
     * Divide this natural number by another one, returning the quotient.
     */
    auto natural::operator /(const natural &v) const -> natural {
        natural quotient;

        std::size_t this_num_digits = num_digits_;
        quotient.reserve(this_num_digits);
        std::size_t v_num_digits = v.num_digits_;
        natural_digit remainder_digits[v_num_digits];
        std::size_t remainder_num_digits;
        digit_array_divide_modulus(quotient.digits_, quotient.num_digits_, remainder_digits, remainder_num_digits,
                                   digits_, this_num_digits, v.digits_, v_num_digits);
        return quotient;
    }

    /*
     * Divide this natural number by another one, returning the quotient.
     */
    auto natural::operator /=(const natural &v) -> natural & {
        std::size_t v_num_digits = v.num_digits_;
        natural_digit remainder_digits[v_num_digits];
        std::size_t remainder_num_digits;
        digit_array_divide_modulus(digits_, num_digits_, remainder_digits, remainder_num_digits,
                                   digits_, num_digits_, v.digits_, v_num_digits);
        return *this;
    }

    /*
     * Divide this natural number by another one, returning the remainder.
     */
    auto natural::operator %(const natural &v) const -> natural {
        natural remainder;

        std::size_t this_num_digits = num_digits_;
        natural_digit quotient_digits[this_num_digits];
        std::size_t v_num_digits = v.num_digits_;
        remainder.reserve(v_num_digits);
        std::size_t quotient_num_digits;
        digit_array_divide_modulus(quotient_digits, quotient_num_digits, remainder.digits_, remainder.num_digits_,
                                   digits_, this_num_digits, v.digits_, v_num_digits);
        return remainder;
    }

    /*
     * Divide this natural number by another one, returning the remainder.
     */
    auto natural::operator %=(const natural &v) -> natural & {
        std::size_t this_num_digits = num_digits_;
        natural_digit quotient_digits[this_num_digits];
        std::size_t quotient_num_digits;
        digit_array_divide_modulus(quotient_digits, quotient_num_digits, digits_, num_digits_,
                                   digits_, this_num_digits, v.digits_, v.num_digits_);
        return *this;
    }

    /*
     * Find the greatest common divisor of this and another natural number.
     */
    auto natural::gcd(const natural &v) const -> natural {
        natural num2;

        /*
         * If either this number or v are zero then our GCD is just the non-zero value.
         */
        std::size_t v_num_digits = v.num_digits_;
        if (!v.num_digits_) {
            num2 = *this;
            return num2;
        }

        std::size_t this_num_digits = num_digits_;
        if (!num_digits_) {
            num2 = v;
            return num2;
        }

        /*
         * We're going to use Euclid's algorithm.  We need to know which is the larger
         * of our two numbers and we'll call that num1, while the smaller will be num2.
         */
        natural num1;
        if (digit_array_compare_lt(digits_, this_num_digits, v.digits_, v_num_digits)) {
            num1 = v;
            num2 = *this;
        } else {
            num1 = *this;
            num2 = v;
        }

        /*
         * We never actually use the quotient - this is just somewhere to dump the output.
         */
        natural_digit quotient_digits[num1.num_digits_ - num2.num_digits_ + 1];
        std::size_t quotient_num_digits;

        /*
         * Euler's algorithm is essentially to take the modulus of num1 and num2 and check
         * if the result is zero.  If it is zero then num2 is the GCD.  If it's not zero then
         * try again, but replacing num1 with num2 and num2 with the modulus from the previous
         * attempt.  We iterate thus:
         *
         * r0 = num1 % num2
         * r1 = num2 % r0
         * r2 = r0 % r1
         * r3 = r1 % r2
         * ...
         *
         * We can implement a very efficient version of this with in-place modulus operations
         * if we observe that the dividend in each case is never used and can thus be overwritten.
         * When we do this we unroll into an iterating pair of operations:
         *
         * num1 = num1 % num2
         * num2 = num2 % num1
         */
        while (true) {
            digit_array_divide_modulus(quotient_digits, quotient_num_digits, num1.digits_, num1.num_digits_,
                                       num1.digits_, num1.num_digits_, num2.digits_, num2.num_digits_);
            if (!num1.num_digits_) {
                break;
            }

            digit_array_divide_modulus(quotient_digits, quotient_num_digits, num2.digits_, num2.num_digits_,
                                       num2.digits_, num2.num_digits_, num1.digits_, num1.num_digits_);
            if (!num2.num_digits_) {
                num2.steal_digits(num1);
                break;
            }
        }

        return num2;
    }

    /*
     * Convert this natural number to an unsigned long long.
     */
    auto natural::to_unsigned_long_long() const -> unsigned long long {
        /*
         * Will this number fit in an unsigned long long?  If not then throw an
         * exception.
         */
        if (size_bits() > (8 * sizeof(long long))) {
            throw overflow_error();
        }

        /*
         * Convert the value to our result format.
         */
        unsigned long long res = 0;
        std::size_t sz = num_digits_;
        if (sz > (sizeof(unsigned long long) / sizeof(natural_digit))) {
            sz = sizeof(unsigned long long) / sizeof(natural_digit);
        }

        const natural_digit *this_digits = digits_;
        for (unsigned int i = 0; i < sz; i++) {
            res |= (static_cast<unsigned long long>(this_digits[i]) << (i * natural_digit_bits));
        }

        return res;
    }

    /*
     * << operator to print a natural.
     */
    auto operator <<(std::ostream &outstr, const natural &v) -> std::ostream & {
        std::size_t v_num_digits = v.num_digits_;
        if (v_num_digits == 0) {
            outstr << '0';
            return outstr;
        }

        natural_digit base = 10;
        auto flags = outstr.flags();
        if (flags & std::ios_base::hex) {
            base = 16;
        } else if (flags & std::ios_base::oct) {
            base = 8;
        }

        const char *digits = digits_lower;
        if (flags & std::ios_base::uppercase) {
            digits = digits_upper;
        }

        if (flags & std::ios_base::showbase) {
            if (base != 10) {
                outstr << '0';
                if (base == 16) {
                    outstr << ((flags & std::ios_base::uppercase) ? 'X' : 'x');
                }
            }
        }

        natural_digit modulus_digit;
        std::size_t modulus_num_digits;

        /*
         * Allocate space for a result buffer.  We want this to be fast, so we go old-school
         * and use a C-style string (yes this isn't nice, but we're very careful).  We assume
         * a worst case of octal, so each 3 bits requires one result character.  We also want
         * a NUL terminator.  We also give ourselves one extra digit to handle any partial
         * digits, or the zero digit case.
         */
        std::size_t res_sz = ((v_num_digits * natural_digit_bits) / 3) + 2;
        char res[res_sz];
        res[--res_sz] = '\0';

        natural_digit rem_digits[v.num_digits_];
        std::size_t rem_num_digits;

        /*
         * We always have at least one digit, so compute that now.
         */
        digit_array_divide_modulus(rem_digits, rem_num_digits, &modulus_digit, modulus_num_digits,
                                   v.digits_, v.num_digits_, &base, 1);
        unsigned int d = 0;
        if (modulus_num_digits) {
            d = static_cast<unsigned int>(modulus_digit);
        }

        res[--res_sz] = digits[d];

        natural_digit base_div = base;

        if (sizeof(natural_digit) > 1) {
            base_div = static_cast<natural_digit>(base_div * base);
        }

        /*
         * Now compute any other characters.
         *
         * We try work in blocks of 2 characters at a time because this results in far
         * fewer arbitrary precision divides.
         */
        while (rem_num_digits) {
            modulus_digit = 0;
            digit_array_divide_modulus(rem_digits, rem_num_digits, &modulus_digit, modulus_num_digits,
                                       rem_digits, rem_num_digits, &base_div, 1);

            if (sizeof(natural_digit) > 1) {
                if ((rem_num_digits == 0) && (modulus_digit == 0)) {
                    break;
                }

                auto d = modulus_digit % base;
                modulus_digit = modulus_digit / base;
                res[--res_sz] = digits[d];
            }

            if ((rem_num_digits == 0) && (modulus_digit == 0)) {
                break;
            }

            res[--res_sz] = digits[modulus_digit];
        }

        /*
         * Output the digits we accumulated.
         */
        outstr << &res[res_sz];

        return outstr;
    }
}

auto x = L"fred";

class foo {
    int x;

    foo() {
        this.x = 3;
    }
}
