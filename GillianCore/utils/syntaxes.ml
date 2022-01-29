module Result = struct
  let ( let+ ) f o = Result.map o f
  let ( let* ) o f = Result.bind o f
end

module Option = struct
  let ( let+ ) f o = Option.map o f
  let ( let* ) o f = Option.bind o f
end
