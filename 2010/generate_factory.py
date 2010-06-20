import itertools
import sys

def label(i, x = -1):
    if i == x :
        return 'X'
    else:
        return str(i/2) + ('R' if i%2 else 'L')

def generate( ls, x):
    len_ls = len(ls)
    assert ( len_ls % 2 == 0 )
    res = label(ls.index(x)) + ':' + '\n'
    for i in range( len_ls / 2 ):
        n = 2*i
        res += label( ls[n], x) + label( ls[n+1] , x)
        res += "0#"
        res += 'X' if x == n   else label( ls.index(n))
        res += 'X' if x == n+1 else label( ls.index(n+1))
        res += ':' if (n+2) == len_ls else ','
        res += '\n'
    res += label( x)
    return res




if __name__ == "__main__":
    if len(sys.argv) > 1:
        SIZE = int( sys.argv[1] ) * 2

        for ls in itertools.permutations( range(SIZE) ):
            for x in range(SIZE):
                print "======="
                print generate( ls, x )


