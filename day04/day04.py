from functools import reduce
from itertools import groupby

def to_digits(n):
    return [int(d) for d in str(n)]

def two_adjacent_are_same(digits):
    fold_fun = lambda acc, right: (acc[0] or acc[1] == right, right)
    return reduce(fold_fun, digits, (False, -1))[0]

def freaking_elf_forgot_a_detail(digits):
    g = groupby(digits)
    sizes = [len(list(groupped)) for _, groupped in g]
    return 2 in sizes

def never_decreases(digits):
    fold_fun = lambda acc, right: (acc[0] and acc[1] <= right, right)
    return reduce(fold_fun, digits, (True, -1))[0]


x = int(input("Input?"))
y = to_digits(x)
print(y)

# a = 125730
# b = 579381
# count = 0
# for n in range (a, b):
#     digits = to_digits(n)
#     if never_decreases(digits) and freaking_elf_forgot_a_detail(digits):
#         count +=1
# print(f"count={count}")
