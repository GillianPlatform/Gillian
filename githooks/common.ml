

let possible_hooks = [
  "applypatch-msg";
  "pre-applypatch";
  "post-applypatch";
  "pre-commit";
  "pre-merge-commit";
  "prepare-commit-msg";
  "commit-msg";
  "post-commit";
  "pre-rebase";
  "post-checkout";
  "post-merge";
  "pre-push";
  "pre-receive";
  "update";
  "post-receive";
  "post-update";
  "push-to-checkout";
  "pre-auto-gc";
  "post-rewrite";
  "sendemail-validate";
  "fsmonitor-watchman";
  "p4-pre-submit";
  "post-index-change"
]
open Shexp_process
open Shexp_process.Infix

let cwd = Sys.getcwd ()

let githooksFolder = Filename.concat cwd "githooks"
let gitFolder = String.trim (eval (
  (run "git" ["rev-parse"; "--git-dir"])
  |- read_all))
let destination = Filename.concat gitFolder "hooks"
let hooks_names =
  let existing = Sys.readdir githooksFolder in
  Stdlib.List.map Filename.basename possible_hooks |>
    Stdlib.List.filter (fun f -> Array.mem f existing)