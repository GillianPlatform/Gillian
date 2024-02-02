#!/usr/bin/env ocaml

#use "topfind";;
#thread

#require "feather";;
#use "./githooks/common.ml";;
#require "fileutils"

let abshooks = Stdlib.List.map (Filename.concat destination) hooks_names

let () =
  Stdlib.List.iter (Printf.printf "Cleaning %s\n") abshooks

let () = FileUtil.rm abshooks

