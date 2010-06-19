import itertools
itertools.permutations([1,2,3])

def label(i):
    return str(i/2) + ('R' if i%2 else 'L')

def generate( ls):
    len_ls = len(ls)
    assert ( len_ls % 2 == 0 )
    for i in range( len_ls / 2 ):
        n = 2*i
        res = label( ls[n]) + label( ls[n+1])
        res += "0#"
        res += label( ls.index(n)) + label( ls.index(n+1))
        res += ':' if (n+2) == len_ls else ','
        print res

