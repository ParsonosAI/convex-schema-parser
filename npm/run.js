#!/usr/bin/env node

const { spawn } = require('child_process');
const path = require('path');

const platform = process.platform;
const exeName = platform === 'win32' ? 'convex-schema-parser.exe' : 'convex-schema-parser';
const binaryPath = path.join(__dirname, exeName);
const args = process.argv.slice(2);

// Execute the binary, passing through all arguments and stdio.
const child = spawn(binaryPath, args, { stdio: 'inherit' });

child.on('error', (err) => {
	console.error('Failed to start the convex-schema-parser binary.', err);
});

child.on('exit', (code) => {
	process.exit(code);
});
