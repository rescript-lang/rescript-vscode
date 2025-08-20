# Configuration

The ReScript Language Server support the folowing configuration.

These configurations are sent to the server on [initialization](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize)

```typescript
interface config {
  /**
   * Whether you want the extension to prompt for autostarting a ReScript build if a project is opened with no build running
   * @default true
   */
  askToStartBuild: boolean;

  /**
   * Inlay Hint config
   */
  inlayHints: {
    /**
     * Enable Inlay Hint
     * @defalt false
     */
    enable: boolean;
    /**
     * Maximum length of character for inlay hints. Set to null to have an unlimited length. Inlay hints that exceed the maximum length will not be shown
     * @defalt 25
     */
    maxLength: number | null;
  };
  /**
   * Enable CodeLens
   * @default false
   */
  codeLens: boolean;
  /**
   * Path to the directory where cross-platform ReScript binaries are. You can use it if you haven't or don't want to use the installed ReScript from node_modules in your project.
   * @default null
   */
  binaryPath: string | null;
  /**
   * Path to the directory where platform-specific ReScript binaries are. You can use it if you haven't or don't want to use the installed ReScript from node_modules in your project.
   * @default null
   */
  platformPath: string | null;

  /**
   * Signature Help config
   */
  signatureHelp: {
    /**
     * Enable Signature Help
     * @default true
     */
    enabled: boolean;
  };
}
```
