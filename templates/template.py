# -*- coding:utf-8 -*-
import os
import sys
from collectscript import logutil

if hasattr(sys, 'frozen'):
    SELFPATH = logutil.scriptPath(sys.executable)
else:
    SELFPATH = logutil.scriptPath(__file__)
LOGCONFIG = os.path.join(SELFPATH, 'logging.conf')

LOGNAME = "XXX"
logutil.registerLogger(LOGNAME)


class XXX(object):
    def __init__(self):
        self._log = logutil.LogUtil().logger(LOGNAME)


if __name__ == '__main__':
    logutil.logConf(LOGCONFIG)
    logutil.LogUtil(LOGCONFIG)
