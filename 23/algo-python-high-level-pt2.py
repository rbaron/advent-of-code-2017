a = 1
b = 93
c = 93

b =  b * 100 + 100000
c = b + 17000

while g != 0:
    f = 1
    d = 2

    # exit when d == b
    while g != 0:
        e = 2

        # exit when find an e s.t.
        #  e - b = 0
        # will set f to 0 if
        # d * e == b at some point
        while g != 0:
            g = d * e - b

            if g == 0:
                f = 0

            e = e + 1
            g = e - b

        d = d + 1
        g = d - b

    if f == 0:
        h = h + 1

    g = b - c
    b = b + 17



