/* TypeScript file generated from TestImport.res by genType. */
/* eslint-disable import/first */


import {TopLevelClass as innerStuffContentsNotChecked} from './exportNestedValues';

import {TopLevelClass as innerStuffContentsAsEmptyObjectNotChecked} from './exportNestedValues';

import {ValueStartingWithUpperCaseLetter as valueStartingWithUpperCaseLetterNotChecked} from './exportNestedValues';

import {default as defaultValueNotChecked} from './exportNestedValues';

import {TopLevelClass as makeNotChecked} from './MyBanner';

import {default as defaultValue2NotChecked} from './exportNestedValues';

// In case of type error, check the type of 'innerStuffContents' in 'TestImport.re' and './exportNestedValues'.
export const innerStuffContentsTypeChecked: { readonly x: number } = innerStuffContentsNotChecked.MiddleLevelElements.stuff.InnerStuff.innerStuffContents;

// Export 'innerStuffContents' early to allow circular import from the '.bs.js' file.
export const innerStuffContents: unknown = innerStuffContentsTypeChecked as { readonly x: number };

// In case of type error, check the type of 'innerStuffContentsAsEmptyObject' in 'TestImport.re' and './exportNestedValues'.
export const innerStuffContentsAsEmptyObjectTypeChecked: {} = innerStuffContentsAsEmptyObjectNotChecked.MiddleLevelElements.stuff.InnerStuff.innerStuffContents;

// Export 'innerStuffContentsAsEmptyObject' early to allow circular import from the '.bs.js' file.
export const innerStuffContentsAsEmptyObject: unknown = innerStuffContentsAsEmptyObjectTypeChecked as {};

// In case of type error, check the type of 'valueStartingWithUpperCaseLetter' in 'TestImport.re' and './exportNestedValues'.
export const valueStartingWithUpperCaseLetterTypeChecked: string = valueStartingWithUpperCaseLetterNotChecked;

// Export 'valueStartingWithUpperCaseLetter' early to allow circular import from the '.bs.js' file.
export const valueStartingWithUpperCaseLetter: unknown = valueStartingWithUpperCaseLetterTypeChecked as string;

// In case of type error, check the type of 'defaultValue' in 'TestImport.re' and './exportNestedValues'.
export const defaultValueTypeChecked: number = defaultValueNotChecked;

// Export 'defaultValue' early to allow circular import from the '.bs.js' file.
export const defaultValue: unknown = defaultValueTypeChecked as number;

// In case of type error, check the type of 'make' in 'TestImport.re' and './MyBanner'.
export const makeTypeChecked: <a>(_1:{ readonly show: boolean; readonly message?: message }, _2:a) => JSX.Element = makeNotChecked.MiddleLevelElements.MyBannerInternal;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = function <a>(Argshow: any, Argmessage: any, Arg2: any) {
  const result = makeTypeChecked({show:Argshow, message:Argmessage}, Arg2);
  return result
} as <a>(_1:{ readonly show: boolean; readonly message?: message }, _2:a) => JSX.Element;

// In case of type error, check the type of 'defaultValue2' in 'TestImport.re' and './exportNestedValues'.
export const defaultValue2TypeChecked: number = defaultValue2NotChecked;

// Export 'defaultValue2' early to allow circular import from the '.bs.js' file.
export const defaultValue2: unknown = defaultValue2TypeChecked as number;

// tslint:disable-next-line:interface-over-type-literal
export type message = { readonly text: string };
