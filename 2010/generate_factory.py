import itertools
itertools.permutations([1,2,3])

def label(i, x = -1):
    if i == x :
        return 'X'
    else:
        return str(i/2) + ('R' if i%2 else 'L')

def generate( ls, x):
    len_ls = len(ls)
    assert ( len_ls % 2 == 0 )
    print label(ls.index(x)) + ':'
    for i in range( len_ls / 2 ):
        n = 2*i
        res = label( ls[n], x) + label( ls[n+1] , x)
        res += "0#"
        res += 'X' if x == n   else label( ls.index(n))
        res += 'X' if x == n+1 else label( ls.index(n+1))
        res += ':' if (n+2) == len_ls else ','
        print res
    print label( x)
