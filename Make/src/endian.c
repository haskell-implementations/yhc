#include <stdio.h>

/* Don't use signed numbers: their format is more complicated
 * than unsigned ones, in a way that we're not interested in.
 */
typedef unsigned int large_type;
typedef unsigned char byte_type;

/*
 * Although a test for endianness in C obviously
 * relies on behavior undefined by the C standards,
 * use a union because it is more understandable than
 * pointer-casts, and the C compiler may be more likely
 * to do what we want (see, for example,
 * http://gcc.gnu.org/onlinedocs/gcc-4.1.1/gcc/Optimize-Options.html#index-fstrict_002daliasing-542
 * ).
 */
union endianness_test {
    large_type i;
    byte_type byte[sizeof(large_type)];
};

int main(int argc, char** argv) {
    union endianness_test test;
    test.i = 1;
    /* If the last byte contains the '1' bit,
     * then the last byte is the least significant byte
     * and we are big-endian; otherwise we must be little-endian.
     * Test for big-endianness rather than little-endiannes
     * because the big-endian machines have apparently been
     * more consistent in ordering everything consistently
     * (through no fault of the fundamental notion of little-endianness.
     * "ON HOLY WARS AND A PLEA FOR PEACE" at
     * http://www.rdrop.com/~cary/html/endian_faq.html#danny_cohen
     * is a good, though lengthy, explanation of the issues.)
     */
    printf((test.byte[sizeof(large_type) - 1] != 0)
                    ? "1" : "0" );
    return 0;
}
