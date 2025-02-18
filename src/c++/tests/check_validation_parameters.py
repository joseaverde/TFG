#!/usr/bin/env  python3
# *-* encoding=utf8 *-*

CWD  : Final[str] = os.path.dirname(__file__)
ROOT : Final[str] = os.path.dirname(CWD)
sys.path.append(ROOT)
sys.path.append(os.path.join(ROOT, "build", "Release", "modules"))


