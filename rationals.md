Certainly! Here is a concise summary of our technical discussion regarding type-level rational comparison and fixed-point bit-width validation in GHC.

---

# GHC Type-Level Rational & Fixed-Point Summary

This document outlines the logic for representing and validating rational numbers at the type level using `GHC.TypeLits`.

## 1. Comparing Fractions

To compare two type-level naturals representing fractions ($a/b$ and $c/d$), we avoid division (which loses precision) by using **cross-multiplication**.

### Logic

$$\frac{a}{b} > \frac{c}{d} \iff (a \cdot d) > (c \cdot b)$$

### Implementation

```haskell
type CompareFractions a b c d = CmpNat (a * d) (c * b)

```

---

## 2. Fixed-Point Validation (`SFixed I F`)

When checking if a literal $n/d$ "fits" into a fixed-point type with $I$ integer bits and $F$ fractional bits:

### Range Check (Integer Part)

Ensures the value does not overflow the allocated integer bits.

* **Unsigned Logic:** $(n \div d) < 2^I$
* **Signed Logic:** Requires an extra bit for the sign; range is roughly $-2^{I-1}$ to $2^{I-1}-1$.

### Precision Check (Fractional Part)

Determines if the literal can be represented **exactly** in binary.

* **Condition:** $(n \cdot 2^F) \pmod d == 0$
* **Note:** Many common decimals (like $3.14$) are not exact powers of 2 and will never "fit" perfectly; they must be rounded.

---

## 3. Inferring Required Bit-Widths

To calculate the minimum bits needed for a literal $n/d$:

| Bit Type | Calculation Logic | GHC TypeLits Formula |
| --- | --- | --- |
| **Integer ($I$)** | $\lceil \log_2(\text{value}) \rceil$ | `Log2 (n / d) + 1` |
| **Fractional ($F$)** | $\log_2(\text{denominator})$ | `Log2 d` (If $d$ is a power of 2) |

### The "3.14" Example ($157/50$)

* **Integer Bits:** $Log2(3) + 1 = 2$ bits.
* **Fractional Bits:** Since $50$ is not a power of $2$, it cannot be represented exactly. You must choose an $F$ based on desired precision (e.g., $F=7$ for $\approx 0.01$ accuracy).

---

## 4. Key Takeaways

* **Cross-multiply** for comparisons to maintain exactness.
* **Use `Log2**` for bit-width inference.
* **Binary Limitation:** Most decimal literals are approximations in fixed-point unless the denominator is a power of two.
