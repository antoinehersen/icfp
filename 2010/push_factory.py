import urllib,urllib2

text = "0L:\n1R1L0#X1R,\nX0R0#0R0L:\n0L"
parameters = {'contents' : text}
urllib.urlencode( parameters)

url = "http://icfpcontest.org/icfp10/instance/219/solve"
