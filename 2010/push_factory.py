import urllib,urllib2, httplib

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
    return response


text = "0L:\n1R1L0#X1R,\nX0R0#0R0L:\n0L"
parameters = {'contents' : text}
data = urllib.urlencode( parameters)

url = "http://icfpcontest.org/icfp10/instance/219/solve"


headers = { 'User-Agent' : user_agent,
            'Cookie' : "JSESSIONID=04C65D0779CF9776F64476D7452C5C0F",
            "Content-type": "application/x-www-form-urlencoded",
            "Accept": "text/plain"}

# conn = httplib.HTTPConnection("icfpcontest.org")

req = urllib2.Request(url, data, headers)
# response = urllib2.urlopen(req)
# the_page = response.read()
# response.info().items()


# import Cookie
# C = Cookie.SmartCookie()
# C["JSESSIONID"] = "04C65D0779CF9776F64476D7452C5C0F"

# print C.output(header="")


# import urllib
# import urllib2


