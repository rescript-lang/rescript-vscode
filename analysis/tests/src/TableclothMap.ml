let add = 3

module Of (M : sig end) = struct
  type t = int
end

module M = struct end

module Int = struct
  type t = Of(M).t
end
