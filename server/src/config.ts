import { Message } from "vscode-languageserver-protocol";

export type send = (msg: Message) => void;

export interface extensionConfiguration {
  askToStartBuild?: boolean;
  inlayHints?: {
    enable?: boolean;
    maxLength?: number | null;
  };
  codeLens?: boolean;
  binaryPath?: string | null;
  platformPath?: string | null;
  signatureHelp?: {
    enabled?: boolean;
    forConstructorPayloads?: boolean;
  };
  incrementalTypechecking?: {
    enable?: boolean;
    acrossFiles?: boolean;
    debugLogging?: boolean;
  };
  cache?: {
    projectConfig?: {
      enable?: boolean;
    };
  };
}

// All values here are temporary, and will be overridden as the server is
// initialized, and the current config is received from the client.
let config: { extensionConfiguration: extensionConfiguration } = {
  extensionConfiguration: {
    askToStartBuild: true,
    inlayHints: {
      enable: false,
      maxLength: 25,
    },
    codeLens: false,
    binaryPath: null,
    platformPath: null,
    signatureHelp: {
      enabled: true,
      forConstructorPayloads: true,
    },
    incrementalTypechecking: {
      enable: true,
      acrossFiles: false,
      debugLogging: false,
    },
    cache: {
      projectConfig: {
        enable: true,
      },
    },
  },
};

export default config;
