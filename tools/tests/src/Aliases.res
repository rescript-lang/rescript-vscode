@warning("-60")

module Exn = Js.Exn

module UtilsPromises = {
  include Js.Promise2

  module OldPromise = Js.Promise
}
