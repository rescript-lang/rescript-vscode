/* TypeScript file generated from ImportMyBanner.res by genType. */
/* eslint-disable import/first */


import {make as makeNotChecked} from './MyBanner';

// In case of type error, check the type of 'make' in 'ImportMyBanner.re' and './MyBanner'.
export const makeTypeChecked: <a>(_1:{ readonly show: boolean; readonly message?: message }, _2:a) => JSX.Element = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = function <a>(Argshow: any, Argmessage: any, Arg2: any) {
  const result = makeTypeChecked({show:Argshow, message:Argmessage}, Arg2);
  return result
} as <a>(_1:{ readonly show: boolean; readonly message?: message }, _2:a) => JSX.Element;

// tslint:disable-next-line:interface-over-type-literal
export type message = { readonly text: string };
