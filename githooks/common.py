import subprocess, os, stat
from shutil import copyfile

possible_hooks = [
  "applypatch-msg",
  "pre-applypatch",
  "post-applypatch",
  "pre-commit",
  "pre-merge-commit",
  "prepare-commit-msg",
  "commit-msg",
  "post-commit",
  "pre-rebase",
  "post-checkout",
  "post-merge",
  "pre-push",
  "pre-receive",
  "update",
  "post-receive",
  "post-update",
  "push-to-checkout",
  "pre-auto-gc",
  "post-rewrite",
  "sendemail-validate",
  "fsmonitor-watchman",
  "p4-pre-submit",
  "post-index-change"
]

cwd = os.getcwd()
githooksFolder = os.path.join(cwd, "githooks")
gitFolder = subprocess.run(["git", "rev-parse", "--git-dir"], stdout=subprocess.PIPE, text=True).stdout.strip()
destination = os.path.join(cwd, gitFolder, "hooks")
hook_names = [i.name for i in os.scandir(githooksFolder) if i.name in possible_hooks]