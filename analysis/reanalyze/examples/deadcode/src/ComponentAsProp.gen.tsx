/* TypeScript file generated from ComponentAsProp.res by genType. */
/* eslint-disable import/first */


import * as React from 'react';

// @ts-ignore: Implicit any on import
import * as ComponentAsPropBS__Es6Import from './ComponentAsProp.bs';
const ComponentAsPropBS: any = ComponentAsPropBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type Props = {
  readonly button?: JSX.Element; 
  readonly description: JSX.Element; 
  readonly title: JSX.Element
};

/**  This is like declaring a normal ReasonReact component's `make` function, except the body is a the interop hook wrapJsForReason  */
export const make: React.ComponentType<{
  readonly button?: JSX.Element; 
  readonly description: JSX.Element; 
  readonly title: JSX.Element
}> = ComponentAsPropBS.make;
