import rx
import subprocess
from datetime import datetime
from rx import Observable, Observer
from time import sleep
from ewmh import EWMH

ewmh = EWMH()

def get_disks(_):
    result = subprocess.check_output(["df"]).split(b"\n")
    result = [x.split() for x in result[1:]]
    return {x[0]: float(x[4][:-1])/100.0 for x in result if len(x) > 5}

def get_mail(_):
    result = subprocess.check_output(["/usr/bin/notmuch", "count",
                                      "tag:unread"])
    return int(result)

def get_mail_headings(_):
    result = subprocess.check_output(["/usr/bin/notmuch", "search",
                                      "tag:unread"])
    result = [";".join(str(x).split(";")[1:])
              for x in result.split(b"\n")[:15]]
    return "\n".join(result[::-1])

def get_cpu(_):
    with open("/proc/loadavg", "r") as infile:
        return float(infile.readline().split()[0])

def get_mem(_):
    with open("/proc/meminfo", "r") as infile:
        tot = int(infile.readline().split()[1])
        free = int(infile.readline().split()[1])
        return float(tot-free)/tot

def make_bar(x):
    height = x * 10
    return "^r(3x{})".format(int(height))

def decorateWindow(x):
    if not x:
        return ""
    if x.get_wm_class():
        kind, nameIcon = x.get_wm_class()
    else:
        kind = nameIcon = ""
    title = x.get_wm_name()
    if nameIcon == "Firefox":
        nameIcon = "^fn(FontAwesome)^fn() "
    elif nameIcon == "Emacs":
        nameIcon = "^fn(file icon)^fn() "
    elif nameIcon == "URxvt":
        nameIcon = "^fn(FontAwesome)^fn() "
    return "{}{}{}".format(kind, nameIcon, title)

def combine_frames(*frames):
    result = Observable.combine_latest(
        frames[0], frames[1],
        lambda x, y: "{} {}".format(x, y))
    for f in frames[2:]:
        result = Observable.combine_latest(
            result, f, lambda x, y: "{} {}".format(x, y))
    return result

disk_icon = "^fn(FontAwesome)^fn() "
cpu_icon = "^fn(material icons)^fn() "
mem_icon = "^fn(material icons)^fn() "
mail_icon = "^fn(material icons)^fn() "
cal_icon = "^fn(material icons)^fn() "


time = Observable.interval(6000) \
                 .map(lambda _: cal_icon+"{dt:%a} {dt:%b} {dt.day} {dt:%H}:{dt:%M}"
                      .format(dt=datetime.now())) \
                 .distinct_until_changed()

ys = Observable.interval(6000) \
               .map(get_disks) \

documents = ys.map(lambda x: x[b"Documents"]).map(make_bar)
sda2 = ys.map(lambda x: x[b"/dev/sda2"]).map(make_bar)

disks = Observable.combine_latest(sda2, documents,
                                  lambda x, y: disk_icon + x + " " + y) \
                  .distinct_until_changed()

cpu = Observable.interval(6000) \
      .map(get_cpu) \
      .map(lambda x: cpu_icon+make_bar(x))

mem = Observable.interval(6000) \
      .map(get_mem) \
      .map(lambda x: mem_icon+make_bar(x))

mail = Observable.interval(6000).map(lambda x, y: mail_icon+str(get_mail(x))) \
       .distinct_until_changed()
mail_headings = Observable.interval(6000).map(lambda x, y: get_mail_headings(x))

windows = Observable.interval(500) \
                    .map(lambda x: ewmh.getActiveWindow()) \
                    .distinct_until_changed() \
                    .filter(lambda x: x)

active = windows.map(decorateWindow)

left = combine_frames(time, active)

right = combine_frames(cpu, mem, mail, disks) \
       .map(lambda x: "^p(_CENTER){}".format(x)) \

header = Observable.combine_latest(
    left, right,
    lambda x, y: "{} ^p(_CENTER){}".format(x, y))

body = Observable.merge(
    mail_headings.distinct_until_changed().map(lambda x: "^cs()\n"+x),
    Observable.interval(1000).map(lambda x: ""))

bar = Observable.combine_latest(
    header, body,
    lambda x, y: "^tw(){}\n{}".format(x, y))\
    .distinct_until_changed() \
    .subscribe(lambda x: print(x, end=""))


while True:
    sleep(1)
