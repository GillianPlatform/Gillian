type ('info, 'category) t = {
  name : string;
  path : string;
  category : 'category;
  info : 'info;
}

let pp fmt test = Fmt.pf fmt "%s" test.path

let make ~name ~path ~category ~info = { name; path; info; category }
