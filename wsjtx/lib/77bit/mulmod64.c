#include <stdint.h>

uint64_t mulmod64(uint64_t a, uint64_t b)
{
   #if defined(__x86_64__) || defined(_M_X64)
		__uint128_t p = ( (__uint128_t)a * (__uint128_t)b );
		return (uint64_t)p;
	#else
		uint32_t a_lo = (uint32_t)a;
		uint32_t a_hi = (uint32_t)(a >> 32);
		uint32_t b_lo = (uint32_t)b;
		uint32_t b_hi = (uint32_t)(b >> 32);

		uint64_t p0 = (uint64_t)a_lo * b_lo;
		uint64_t p1 = (uint64_t)a_lo * b_hi;
		uint64_t p2 = (uint64_t)a_hi * b_lo;
		uint64_t p3 = (uint64_t)a_hi * b_hi;

		// Combine partial products, keeping only the low 64 bits
		uint64_t mid = p1 + p2 + (p0 >> 32);
		uint64_t hi  = p3 + (mid >> 32);
		uint64_t lo  = (mid << 32) | (uint32_t)p0;

		return lo;   // low 64 bits of the full 128-bit product
	#endif

}
