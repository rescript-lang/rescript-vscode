import { Message } from "vscode-languageserver-protocol";

export type send = (msg: Message) => void;

export interface extensionConfiguration {
  allowBuiltInFormatter: boolean;
  askToStartBuild: boolean;
  inlayHints: {
    enable: boolean;
    maxLength: number | null;
  };
  codeLens: boolean;
  binaryPath: string | null;
  platformPath: string | null;
  signatureHelp: {
    enabled: boolean;
  };
  incrementalTypechecking: {
    enabled: boolean;
    acrossFiles: boolean;
  };
}

// All values here are temporary, and will be overridden as the server is
// initialized, and the current config is received from the client.
let config: { extensionConfiguration: extensionConfiguration } = {
  extensionConfiguration: {
    allowBuiltInFormatter: false,
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
    },
    incrementalTypechecking: {
      enabled: false,
      acrossFiles: true,
    },
  },
};

export default config;
