import os
import stat

def clean(path, files):
    if not os.path.exists(path):
        return
    for root, dirs, fs in os.walk(path):
        [os.unlink(root + os.sep + f) for f in fs if True in map(lambda x: f.endswith(x), files)]

def empty(path):
    if not os.path.exists(path):
        return
    for root, dirs, fs in os.walk(path, topdown=False):
        [os.chmod(root + os.sep + f, stat.S_IREAD | stat.S_IWRITE) for f in fs]
        [os.unlink(root + os.sep + f) for f in fs]
        [os.rmdir(root + os.sep + d) for d in dirs]
