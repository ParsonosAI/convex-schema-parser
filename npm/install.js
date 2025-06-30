const https = require('https');
const fs = require('fs');
const path = require('path');
const decompress = require('decompress');

const { version, name, repository } = require('./package.json');
const repoUrl = repository.url.replace('git+', '').replace('.git', '');

const platform = process.platform;
const arch = process.arch;

function getAssetInfo() {
	let osName, exeName, archiveName;

	if (platform === 'darwin' && arch === 'x64') {
		osName = 'macos-latest';
		exeName = 'convex-schema-parser';
	} else if (platform === 'darwin' && arch === 'arm64') {
		osName = 'macos-latest';
		exeName = 'convex-schema-parser';
	} else if (platform === 'linux' && arch === 'x64') {
		osName = 'ubuntu-latest';
		exeName = 'convex-schema-parser';
	} else {
		console.error(`[ERROR] Unsupported platform: ${platform} ${arch}`);
		process.exit(1);
	}

	archiveName = `convex-schema-parser-${osName}-${arch}.tar.gz`;
	if (platform === 'win32') {
		archiveName = `convex-schema-parser-${osName}-${arch}.zip`;
	}

	return { osName, exeName, archiveName };
}

async function install() {
	const { exeName, archiveName } = getAssetInfo();
	const tag = `v${version}`;
	const url = `${repoUrl}/releases/download/${tag}/${archiveName}`;
	const binaryDir = __dirname;
	const binaryPath = path.join(binaryDir, exeName);

	console.log(`[DEBUG] Platform: ${platform}-${arch}`);
	console.log(`[DEBUG] Attempting to download binary from: ${url}`);

	const archivePath = path.join(binaryDir, archiveName);
	const file = fs.createWriteStream(archivePath);

	https.get(url, (res) => {
		console.log(`[DEBUG] GitHub response status code: ${res.statusCode}`);
		if (res.statusCode === 404) {
			console.error(`[ERROR] Asset not found at ${url}. Please check the release tag and asset names on GitHub.`);
			process.exit(1);
		}

		if (res.statusCode === 302) {
			console.log(`[DEBUG] Following redirect to: ${res.headers.location}`);
			https.get(res.headers.location, (res2) => res2.pipe(file));
		} else {
			res.pipe(file);
		}

		file.on('finish', () => {
			file.close(async () => {
				console.log('Download complete.');

				try {
					console.log(`[DEBUG] Extracting ${archivePath} to ${binaryDir}...`);
					await decompress(archivePath, binaryDir);
					console.log('Extraction complete.');

					if (fs.existsSync(binaryPath)) {
						console.log(`[DEBUG] Binary successfully extracted to: ${binaryPath}`);
					} else {
						console.error(`[ERROR] Decompression succeeded, but the executable was not found at ${binaryPath}. Check the archive structure.`);
						process.exit(1);
					}

					fs.unlinkSync(archivePath);

					if (platform !== 'win32') {
						fs.chmodSync(binaryPath, 0o755);
						console.log('Made binary executable.');
					}
					console.log(`${name} installed successfully.`);
				} catch (err) {
					console.error('Error extracting archive:', err);
					process.exit(1);
				}
			});
		});
	}).on('error', (err) => {
		fs.unlink(archivePath, () => { });
		console.error('Error downloading binary:', err.message);
		process.exit(1);
	});
}

install();
