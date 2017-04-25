#!/usr/bin/python
import re, os

# passwords={"Professional":"adam1106",
#            "Work":"adam1106",
#            "Personal":"yenzcyzlvnjxeefq"}

def get_authinfo_password(machine, login):
    s = "machine %s login %s password ([^ ]*)" % (machine, login)
    print(s)
    p = re.compile(s)
    authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").readlines()
    for auth in authinfo:
        result = p.search(auth)# .group(1)
        if result:
            return result.group(1)

print(get_authinfo_password("imap.gmail.com", "rprospero@gmail.com"))
