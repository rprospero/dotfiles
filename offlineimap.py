#!/usr/bin/python
import re, os

def get_authinfo_password(machine, login):
    s = 'machine %s login %s password "([^"]*)"' % (machine, login)
    p = re.compile(s)
    authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").readlines()
    for auth in authinfo:
        result = p.search(auth)# .group(1)
        if result:
            return result.group(1)
