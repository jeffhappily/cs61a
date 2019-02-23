def longest_increasing_suffix(n):
    """Return the longest increasing suffix of a positive integer n.

    >>> longest_increasing_suffix(63134)
    134
    >>> longest_increasing_suffix(233)
    3
    >>> longest_increasing_suffix(5689)
    5689
    >>> longest_increasing_suffix(568901)
    # Note: 01 is the suffix, displayed as 1
    1
    """
    m, suffix, k = 10, 0, 1
    while n:
        n, last = n // 10, n % 10
        if remainder // 10 < last:
            m, suffix, k = _____________, last, 10 * k
        else:
            return suffix
    return suffix
