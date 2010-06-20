import urllib,urllib2, httplib
import re

# jsessionid=5D6B7B9BE37C052D6FA80F02154D5192
def get_cookie( txt):
    p = re.compile('jsessionid=[A-Z0-9]*')
    m = p.search( txt)
    c =  m.group()
    return c.split('=')[1]

#Your Username: invalidmonkey
#Your password: 646428484954362363341124703581428857738992086448891824536895
# JSESSIONID=04C65D0779CF9776F64476D7452C5C0F

user_agent = 'Mozilla/4.0 (compatible; MSIE 5.5; Windows NT)'

def login():
    login_param = { 'j_username' : 'invalidmonkey',
                    'j_password' : '646428484954362363341124703581428857738992086448891824536895' }
    data = urllib.urlencode( login_param)
    url = "http://icfpcontest.org/icfp10/static/j_spring_security_check"

    headers = { 'User-Agent' : user_agent,
#                'Cookie' : "JSESSIONID=04C65D0779CF9776F64476D7452C5C0F",
                "Content-type": "application/x-www-form-urlencoded",
                "Accept": "text/plain"}
    req = urllib2.Request(url, data, headers)
    response = urllib2.urlopen(req)

    txt = response.read()
    return get_cookie( txt)

a_text = "1L:\n1R1L0#X1R,\nX0R0#0R0L:\n0L"

def submit_fuel( cookie, text):
    parameters = {'contents' : text}
    data = urllib.urlencode( parameters)

    cookie = "JSESSIONID=" + cookie
    url = "http://icfpcontest.org/icfp10/instance/219/solve"
    headers = { 'User-Agent' : user_agent,
                'Cookie' : cookie,
                "Content-type": "application/x-www-form-urlencoded",
                "Accept": "text/plain"}

    req = urllib2.Request(url, data, headers)
    response = urllib2.urlopen(req)
    return response

def get_ouptut( txt):
    p = re.compile('[012]{17}')
    m = p.search( txt)
    c =  m.group()
    return c

def is_illegal_prefix( txt):
    p = re.compile("this is an illegal prefix")
    m = p.search( txt)
    return False if m == None else True

if __name__ == "__main__":
    import generate_factory

    cookie = login()
    for size in range(20):
        gen = 0
        for ls in itertools.permutations( range(size) ):
            for x in range(size):
                factory = generate( ls, x )
                res = submit_fuel( cookie, factory)
                txt = res.read()
                prefix = get_ouptut(txt)
                print "{gen},{ls},{x},{prefix}".format(gen=gen,
                                                       ls=ls,
                                                       x=x,
                                                       prefix=prefix)
                gen +=1
                if !is_illegal_prefix(txt):
                    print txt
                    exit()
