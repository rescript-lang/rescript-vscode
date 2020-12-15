let checkDocumentTimers = state => {
  let now = Unix.gettimeofday();
  let removed =
    Hashtbl.fold(
      (uri, timer, removed) =>
        if (now > timer) {
          [uri, ...removed];
        } else {
          removed;
        },
      state.TopTypes.documentTimers,
      [],
    );
  List.iter(uri => Hashtbl.remove(state.documentTimers, uri), removed);
  state;
};
