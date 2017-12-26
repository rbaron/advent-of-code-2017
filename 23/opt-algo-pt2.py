a = 1
b = 93
c = 0
d = 0
e = 0
f = 0
g = 0
h = 0

b =  b * 100 + 100000
c = b + 17000

while True:
    f = 1
    d = 2

    #while d <= b:
    #    e = 2

    #    # Innermost
    #    while e <= b:
    #        if d * e == b:
    #            f = 0
    #            break #opt
    #        e += 1

    #    d += 1

    #    # opt
    #    if f == 0:
    #        break

    # Set f to 0 if b is a composite. That is,
    # if exists some d s.t. b % d == 0
    while d <= (b ** 0.5):
        if b % d == 0:
            f = 0
            break
        d += 1

    # This is counting the number of
    # composite numbers between b && c,
    # in increments of 17
    if f == 0:
        h += 1

    if b == c:
        break

    b += 17

print(h)
