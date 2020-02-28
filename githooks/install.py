#!/usr/bin/env python3
import subprocess, os, stat
from shutil import copyfile

cwd = os.getcwd()
githooksFolder = os.path.join(cwd, "githooks")
gitFolder = subprocess.run(["git", "rev-parse", "--git-dir"], capture_output=True, text=True).stdout.strip()
destination = os.path.join(cwd, gitFolder, "hooks")
origins = [i.name for i in os.scandir(githooksFolder) if i.name != "install.py"]

for f in origins:
  absdest = os.path.join(destination, f)
  copyfile(os.path.join(githooksFolder, f), absdest)
  os.chmod(absdest, os.stat(absdest).st_mode | stat.S_IEXEC)
