let add = 3

module Of (M : sig end) = struct
  type _t = int
end

module M = struct end

module Int = struct
  type _t = Of(M)._t
end
