type cb = (msg: string) => void;

let subscribers: Array<cb> = [];
const errorLastNotified: Record<string, number> = {};

export const onErrorReported = (cb: (msg: string) => void) => {
  subscribers.push(cb);
  return () => {
    subscribers = subscribers.filter((s) => s !== cb);
  };
};

export const reportError = (identifier: string, msg: string) => {
  // Warn once per 15 min per error
  if (
    errorLastNotified[identifier] == null ||
    errorLastNotified[identifier] < Date.now() - 15 * 1000 * 60
  ) {
    errorLastNotified[identifier] = Date.now();
    subscribers.forEach((cb) => cb(msg));
  }
};
