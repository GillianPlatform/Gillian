module Result = struct
  let ( let+ ) f o = Result.map o f

  let ( let* ) o f = Result.bind o f
end
