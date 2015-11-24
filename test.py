def mccarthy_91(n):
    c = 1

    while c != 0:
        if n > 100:
            n = n-10
            c -= 1
        else:
            n = n + 11
            c += 1

    return n

print mccarthy_91(110)
print mccarthy_91(90)
print mccarthy_91(200)
print mccarthy_91(0)
