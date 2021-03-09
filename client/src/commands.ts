import * as fs from 'fs'
import { window } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';

export const createInterface = (client: LanguageClient) => {
	if (!client) {
		return window.showInformationMessage('Language server not running');
	}
	
	const editor = window.activeTextEditor;

	if (!editor) {
		return window.showInformationMessage('No active editor');
	}

	if (fs.existsSync(editor.document.uri.fsPath + 'i')) {
		return window.showInformationMessage('Interface file already exists');
	}

	client.sendRequest("custom:rescript-vscode.create_interface", {
		uri: editor.document.uri.toString(),
	})
};
