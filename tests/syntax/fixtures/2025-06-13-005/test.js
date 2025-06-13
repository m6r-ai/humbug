// prerender.js

import fs from 'fs/promises';
import path from 'path';
import {URL} from 'url';
import puppeteer from 'puppeteer';
import yargs from 'yargs';
import {hideBin} from 'yargs/helpers';
import os from 'os';

const argv = yargs(hideBin(process.argv))
    .option('sitemap', {
        alias: 's',
        describe: 'Path to the sitemap.xml file',
        type: 'string',
        demandOption: true,
    })
    .option('output', {
        alias: 'o',
        describe: 'Directory to save rendered pages',
        type: 'string',
        demandOption: true,
    })
    .option('replace-url', {
        alias: 'r',
        describe: 'Replace URL in the form <new-url-prefix>=<old-url-prefix>',
        type: 'string',
        coerce: (arg) => {
            const match = arg.match(/^(.+?)=(.+)$/);
            if (!match) {
                throw new Error('Invalid format for --replace-url. Expected <new-url-prefix>=<old-url-prefix>');
            }
            return {newUrl: match[1], oldUrl: match[2]};
        },
    })
    .option('parallel-renders', {
        alias: 'p',
        describe: 'Number of parallel renders',
        type: 'number',
        default: os.cpus().length,
    })
    .option('max-retries', {
        alias: 'm',
        describe: 'Maximum number of retries for failed renders',
        type: 'number',
        default: 3,
    })
    .help('h')
    .alias('h', 'help')
    .argv;

/**
 * Read and parse the sitemap XML file.
 * @param {string} sitemapPath - Path to the sitemap file.
 * @returns {Promise<string[]>} List of URLs from the sitemap.
 */
async function parseSitemap(sitemapPath) {
    try {
        const content = await fs.readFile(sitemapPath, 'utf-8');
        const urls = [...content.matchAll(/<loc>(.*?)<\/loc>/g)].map(match => match[1]);
        return urls;
    } catch (error) {
        console.error(`Failed to read sitemap file: ${error.message}`);
        process.exit(1);
    }
}

/**
 * Create the directory structure based on URL.
 * @param {string} outputDir - Base output directory.
 * @param {string} urlPath - URL path to create directories for.
 */
async function createDirectories(outputDir, urlPath) {
    const fullPath = path.join(outputDir, urlPath);
    try {
        await fs.mkdir(fullPath, {recursive: true});
    } catch (error) {
        console.error(`Failed to create directory for ${urlPath}: ${error.message}`);
        process.exit(1);
    }
}

/**
 * Render a single page and save it to the file system.
 * @param {object} browser - Puppeteer browser instance.
 * @param {string} url - URL to render.
 * @param {string} outputPath - Path to save the rendered file.
 * @param {number} retries - Number of retries allowed.
 */
async function renderPage(browser, url, outputPath, retries) {
    const page = await browser.newPage();
    for (let attempt = 1; attempt <= retries; attempt++) {
        try {
            await page.goto(url, {waitUntil: 'networkidle2', timeout: 60000});
            const content = await page.content();
            await fs.writeFile(outputPath, content, 'utf-8');
            console.log(`Rendered: ${url}`);
            await page.close();
            return;
        } catch (error) {
            if (attempt < retries) {
                console.log(`Retrying (${attempt}/${retries}): ${url}`);
                await new Promise(resolve => setTimeout(resolve, 1000));
            } else {
                console.error(`Failed: ${url}: ${error.message}`);
                await page.close();
                process.exit(1);
            }
        }
    }
}

/**
 * Task queue to control the number of parallel renders.
 */
class TaskQueue {
    constructor(maxConcurrent) {
        this.maxConcurrent = maxConcurrent;
        this.running = 0;
        this.queue = [];
    }

    addTask(task) {
        this.queue.push(task);
        this.runNext();
    }

    async runNext() {
        if (this.running >= this.maxConcurrent || this.queue.length === 0) {
            return;
        }

        const task = this.queue.shift();
        this.running++;
        await task();
        this.running--;
        this.runNext();
    }
}

/**
 * Main function to pre-render pages from the sitemap.
 */
async function main() {
    const urls = await parseSitemap(argv.sitemap);
    const outputDir = argv.output;
    const replaceUrl = argv['replace-url'];
    const parallelRenders = argv['parallel-renders'];
    const maxRetries = argv['max-retries'];

    if (replaceUrl) {
        urls.forEach((url, index) => {
            urls[index] = url.replace(replaceUrl.oldUrl, replaceUrl.newUrl);
        });
    }

    try {
        await fs.mkdir(outputDir, {recursive: true});
    } catch (error) {
        console.error(`Failed to create output directory: ${error.message}`);
        process.exit(1);
    }

    const browser = await puppeteer.launch();
    const taskQueue = new TaskQueue(parallelRenders);

    urls.forEach(url => {
        const urlPath = new URL(url).pathname;
        const outputPath = path.join(outputDir, urlPath, 'index.html');

        taskQueue.addTask(async () => {
            await createDirectories(outputDir, urlPath);

            try {
                await fs.rm(outputPath, {force: true});
            } catch (error) {
                console.error(`Failed to delete existing file ${outputPath}: ${error.message}`);
                process.exit(1);
            }

            await renderPage(browser, url, outputPath, maxRetries);
        });
    });

    // Wait for all tasks to complete
    while (taskQueue.running > 0 || taskQueue.queue.length > 0) {
        await new Promise(resolve => setTimeout(resolve, 100));
    }

    await browser.close();
    console.log('All pages rendered successfully.');
    process.exit(0);
}

main();

