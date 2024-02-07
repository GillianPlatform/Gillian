

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
open Feather
open Feather.Infix

let cwd = Sys.getcwd ()

let githooksFolder = Filename.concat cwd "githooks"
let gitFolder =
  process "git" ["rev-parse"; "--git-dir"]
  |> collect stdout
  |> String.trim
let destination = Filename.concat gitFolder "hooks"
let hooks_names =
  let existing = Sys.readdir githooksFolder in
  Stdlib.List.map Filename.basename possible_hooks |>
    Stdlib.List.filter (fun f -> Array.mem f existing)