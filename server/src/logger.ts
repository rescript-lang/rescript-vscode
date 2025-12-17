import * as p from "vscode-languageserver-protocol";
import * as c from "./constants";

export type LogLevel = "error" | "warn" | "info" | "log";

const levelOrder: Record<LogLevel, number> = {
  log: 1,
  info: 2,
  warn: 3,
  error: 4,
};

export interface Logger {
  error(message: string): void;
  warn(message: string): void;
  info(message: string): void;
  log(message: string): void;
}

class NoOpLogger implements Logger {
  error(_message: string): void {}
  warn(_message: string): void {}
  info(_message: string): void {}
  log(_message: string): void {}
}

class LSPLogger implements Logger {
  private logLevel: LogLevel = "info";

  constructor(private send: (msg: p.Message) => void) {}

  setLogLevel(level: LogLevel): void {
    this.logLevel = level;
  }

  private shouldLog(level: LogLevel): boolean {
    return levelOrder[level] >= levelOrder[this.logLevel];
  }

  error(message: string): void {
    if (this.shouldLog("error")) {
      this.sendLogMessage(message, p.MessageType.Error);
    }
  }

  warn(message: string): void {
    if (this.shouldLog("warn")) {
      this.sendLogMessage(message, p.MessageType.Warning);
    }
  }

  info(message: string): void {
    if (this.shouldLog("info")) {
      this.sendLogMessage(message, p.MessageType.Info);
    }
  }

  log(message: string): void {
    if (this.shouldLog("log")) {
      this.sendLogMessage(message, p.MessageType.Log);
    }
  }

  private sendLogMessage(message: string, type: p.MessageType): void {
    const notification: p.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/logMessage",
      params: { type, message },
    };
    this.send(notification);
  }
}

// Default no-op instance
let instance: Logger = new NoOpLogger();

export function initializeLogger(send: (msg: p.Message) => void): void {
  instance = new LSPLogger(send);
}

export function setLogLevel(level: LogLevel): void {
  if (instance instanceof LSPLogger) {
    instance.setLogLevel(level);
  }
}

export function getLogger(): Logger {
  return instance;
}
