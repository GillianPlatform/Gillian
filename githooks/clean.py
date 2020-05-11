#!/usr/bin/env python3
import os
from common import *

for f in hook_names:
  absdest = os.path.join(destination, f)
  try:
    os.remove(absdest)
  except:
    pass