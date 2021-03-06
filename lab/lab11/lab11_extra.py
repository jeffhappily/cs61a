""" Optional Questions for Lab 11 """

from lab11 import *

# Q5
def hailstone(n):
    """
    >>> for num in hailstone(10):
    ...     print(num)
    ...
    10
    5
    16
    8
    4
    2
    1
    """
    if n == 1:
        yield n
        return

    yield n
    if n % 2 == 0:
        yield from hailstone(n//2)
    else:
        yield from hailstone(n*3+1)

# Q6
def repeated(t, k):
    """Return the first value in iterable T that appears K times in a row.

    >>> s = [3, 2, 1, 2, 1, 4, 4, 5, 5, 5]
    >>> repeated(trap(s, 7), 2)
    4
    >>> repeated(trap(s, 10), 3)
    5
    >>> print(repeated([4, None, None, None], 3))
    None
    """
    assert k > 1
    current = [None, 0]
    t = iter(t)

    try:
        while True:
            c = next(t)
            if c == current[0]:
                current[1] += 1
            else:
                current[0] = c
                current[1] = 1

            if current[1] == k:
                return current[0]
    except StopIteration:
        return None
# Q7
def merge(s0, s1):
    """Yield the elements of strictly increasing iterables s0 and s1, removing
    repeats. Assume that s0 and s1 have no repeats. s0 or s1 may be infinite
    sequences.

    >>> m = merge([0, 2, 4, 6, 8, 10, 12, 14], [0, 3, 6, 9, 12, 15])
    >>> type(m)
    <class 'generator'>
    >>> list(m)
    [0, 2, 3, 4, 6, 8, 9, 10, 12, 14, 15]
    >>> def big(n):
    ...    k = 0
    ...    while True: yield k; k += n
    >>> m = merge(big(2), big(3))
    >>> [next(m) for _ in range(11)]
    [0, 2, 3, 4, 6, 8, 9, 10, 12, 14, 15]
    """
    i0, i1 = iter(s0), iter(s1)
    e0, e1 = next(i0, None), next(i1, None)

    if e0 is None:
        yield e1
        yield from i1
    elif e1 is None:
        yield e0
        yield from i0
    else:
        if e0 == e1:
            yield e0
            yield from merge(i0, i1)
        elif e0 > e1:
            yield e1
            yield from merge(merge([e0], i0), i1)
        else:
            yield e0
            yield from merge(i0, merge([e1], i1))
        
# Q8
def remainders_generator(m):
    """
    Takes in an integer m, and yields m different remainder groups
    of m.

    >>> remainders_mod_four = remainders_generator(4)
    >>> for rem_group in remainders_mod_four:
    ...     for _ in range(3):
    ...         print(next(rem_group))
    0
    4
    8
    1
    5
    9
    2
    6
    10
    3
    7
    11
    """
    def iterator(start, k):
       yield start 
       yield from iterator(start+k, k)

    for i in range(m):
        yield iterator(i, m)

# Q9
def zip_generator(*iterables):
    """
    Takes in any number of iterables and zips them together.
    Returns a generator that outputs a series of lists, each
    containing the nth items of each iterable.
    >>> z = zip_generator([1, 2, 3], [4, 5, 6], [7, 8])
    >>> for i in z:
    ...     print(i)
    ...
    [1, 4, 7]
    [2, 5, 8]
    """
    iterables = list(map(lambda x: iter(x), iterables))
    
    while True:
        arr = []
        for itr in iterables:
            try:
                n = next(itr)
                arr.append(n)
            except StopIteration:
                return

        yield arr



